{-# LANGUAGE ConstrainedClassMethods, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GADTs, JavaScriptFFI, ScopedTypeVariables, TypeFamilies #-}
{-# language GeneralizedNewtypeDeriving, TypeApplications, AllowAmbiguousTypes, OverloadedStrings #-}
{-# language RankNTypes, DataKinds, KindSignatures, PolyKinds #-}
module Chili.Types where

import Control.Applicative (Applicative, Alternative)
import Control.Concurrent (forkIO)
import Control.Monad (Monad, MonadPlus)
import Control.Monad.Fix (mfix)
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO(..))
import Chili.Internal (debugPrint, debugStrLn)
import Chili.TDVar (TDVar, isDirtyTDVar, cleanTDVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar)
import Data.Aeson (FromJSON, ToJSON, decodeStrict, encode)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson.Types (Parser, Result(..), parse)
import Data.Char as Char (toLower)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.JSString as JS
import Data.JSString.Text (textToJSString, textFromJSString)
import qualified Data.Text as Text
-- import GHCJS.Prim (ToJSString(..), FromJSString(..))
-- import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import GHCJS.Buffer
import GHCJS.Foreign (jsNull)
import GHCJS.Foreign.Callback (OnBlocked(..), Callback, asyncCallback, asyncCallback1, syncCallback1)
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Nullable (Nullable(..), nullableToMaybe, maybeToNullable)
import GHCJS.Types (IsJSVal(..), JSVal(..), JSString(..),  nullRef, isNull, isUndefined)
import JavaScript.Web.MessageEvent (MessageEvent(..), MessageEventData(..))
import qualified JavaScript.Web.MessageEvent as MessageEvent
import qualified JavaScript.Web.WebSocket as WebSockets
import JavaScript.Web.WebSocket (WebSocket, WebSocketRequest(..), connect, send)

instance Eq JSVal where
  a == b = js_eq a b

foreign import javascript unsafe
  "$1===$2" js_eq :: JSVal  -> JSVal  -> Bool

maybeJSNullOrUndefined :: JSVal -> Maybe JSVal
maybeJSNullOrUndefined r | isNull r || isUndefined r = Nothing
maybeJSNullOrUndefined r = Just r

class InstanceOf ty where
  instanceOf :: (PToJSVal a) => a -> Bool

instance InstanceOf JSElement where
  instanceOf a = js_instanceOfJSElement (pToJSVal a)

foreign import javascript unsafe "$1 instanceof Element"
  js_instanceOfJSElement :: JSVal -> Bool

{-
fromJSValUnchecked :: (FromJSVal a) => JSVal a -> IO a
fromJSValUnchecked j =
    do x <- fromJSVal j
       case x of
         Nothing -> error "failed."
         (Just a) -> return a
-}

foreign import javascript unsafe "alert($1)"
  js_alert :: JSString -> IO ()

-- * JSNode

newtype JSNode = JSNode JSVal deriving Eq

unJSNode (JSNode o) = o

instance ToJSVal JSNode where
  toJSVal = toJSVal . unJSNode
  {-# INLINE toJSVal #-}

instance FromJSVal JSNode where
  fromJSVal = return . fmap JSNode . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance PFromJSVal JSNode where
  pFromJSVal = JSNode
  {-# INLINE pFromJSVal #-}

instance PToJSVal JSNode where
  pToJSVal (JSNode jsval) = jsval
  {-# INLINE pToJSVal #-}

-- | is this legit?
instance IsEventTarget JSNode where
  toEventTarget = EventTarget . unJSNode

instance InstanceOf JSNode where
  instanceOf a = js_instanceOfJSNode (pToJSVal a)

foreign import javascript unsafe "$1 instanceof Node"
  js_instanceOfJSNode :: JSVal -> Bool

-- * IsJSNode

class IsJSNode obj where
    toJSNode :: (IsJSNode obj) => obj -> JSNode

instance IsJSNode JSNode where
    toJSNode = id

fromJSNode :: forall o. (PFromJSVal o, InstanceOf o, IsJSNode o) => JSNode -> Maybe o
fromJSNode jsnode@(JSNode jsval) =
      if instanceOf @o jsnode
      then Just (pFromJSVal jsval)
      else Nothing

-- * IsJSNode

class (IsJSNode obj) => IsParentNode obj

instance IsParentNode JSElement
instance IsParentNode JSDocument
instance IsParentNode JSDocumentFragment

foreign import javascript unsafe "$1[\"append\"]($2)"
  js_append :: JSNode -> JSVal -> IO ()

appendNodeList :: (IsParentNode parent, MonadIO m) => parent -> JSNodeList -> m ()
appendNodeList parent nl = liftIO $
  do nlv <- toJSVal nl
     js_append (toJSNode parent) nlv

-- * EventTarget

newtype EventTarget = EventTarget { unEventTarget :: JSVal }

instance PToJSVal EventTarget where
  pToJSVal (EventTarget jsval) = jsval

instance Eq (EventTarget) where
  (EventTarget a) == (EventTarget b) = js_eq a b

instance ToJSVal EventTarget where
  toJSVal = return . unEventTarget
  {-# INLINE toJSVal #-}

instance FromJSVal EventTarget where
  fromJSVal = return . fmap EventTarget . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

-- * IsEventTarget

class IsEventTarget o where
    toEventTarget :: o -> EventTarget

fromEventTarget :: forall o. (PFromJSVal o, InstanceOf o, IsEventTarget o) => EventTarget -> Maybe o
fromEventTarget eventTarget@(EventTarget jsval) =
      if instanceOf @o eventTarget
      then Just (pFromJSVal jsval)
      else Nothing

instance IsEventTarget EventTarget where
  toEventTarget et = et

-- * JSNodeList

newtype JSNodeList = JSNodeList JSVal

unJSNodeList (JSNodeList o) = o

instance ToJSVal JSNodeList where
  toJSVal = return . unJSNodeList
  {-# INLINE toJSVal #-}

instance FromJSVal JSNodeList where
  fromJSVal = return . fmap JSNodeList . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsJSNode JSNodeList where
    toJSNode = JSNode . unJSNodeList

foreign import javascript unsafe "$1[\"item\"]($2)" js_item ::
        JSNodeList -> Word -> IO JSNode

-- | <https://developer.mozilla.org/en-US/docs/Web/API/NodeList.item Mozilla NodeList.item documentation>
item ::
     (MonadIO m) => JSNodeList -> Word -> m (Maybe JSNode)
item self index
  = liftIO
      ((js_item (self) index) >>= return . Just)

foreign import javascript unsafe "$1[\"length\"]" js_length ::
        JSNode -> IO Word

-- | <https://developer.mozilla.org/en-US/docs/Web/API/NodeList.item Mozilla NodeList.item documentation>
getLength :: (MonadIO m, IsJSNode self) => self -> m Word
getLength self
  = liftIO (js_length ( (toJSNode self))) -- >>= fromJSValUnchecked)

-- foreign import javascript unsafe "$1[\"length\"]" js_getLength ::
--         JSVal NodeList -> IO Word

foreign import javascript unsafe "$1[\"contains\"]($2)"
   js_contains :: JSNode -> JSNode -> IO Bool

contains :: (IsJSNode node, IsJSNode otherNode, MonadIO m) => node -> otherNode -> m Bool
contains node otherNode = liftIO $ js_contains (toJSNode node) (toJSNode otherNode)

-- * parentNode

foreign import javascript unsafe "$1[\"parentNode\"]"
        js_parentNode :: JSNode -> IO JSVal

parentNode :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
parentNode self =
    liftIO (fromJSVal =<< js_parentNode (toJSNode self))

-- * nodeType

foreign import javascript unsafe "$1[\"nodeType\"]"
  js_nodeType :: JSNode -> IO Int

nodeType :: (MonadIO m, IsJSNode self) => self -> m Int
nodeType self = liftIO (js_nodeType $ toJSNode self)

nodeTypeString :: Int -> String
nodeTypeString n =
  case n of
    1 -> "Element"
    2 -> "Attr"
    3 -> "Text"
    4 -> "CDATASection"
    5 -> "EntityReference"
    6 -> "Entity"
    7 -> "ProcessingInstruction"
    8 -> "Comment"
    9 -> "Document"
    10 -> "DocumentType"
    11 -> "DocumentFragment"
    12 -> "Notation"
    _  -> "NodeType=" ++ show n

-- * nodeName

foreign import javascript unsafe "$1[\"nodeName\"]"
  js_nodeName :: JSNode -> IO JSString

nodeName :: (MonadIO m, IsJSNode self) => self -> m JSString
nodeName self = liftIO (js_nodeName $ toJSNode self)

-- * JSDocumentFragment

newtype JSDocumentFragment = JSDocumentFragment { unJSDocumentFragment :: JSVal } deriving Eq

instance ToJSVal JSDocumentFragment where
  toJSVal = pure . unJSDocumentFragment
  {-# INLINE toJSVal #-}

instance FromJSVal JSDocumentFragment where
  fromJSVal = pure . fmap JSDocumentFragment . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance PFromJSVal JSDocumentFragment where
  pFromJSVal = JSDocumentFragment
  {-# INLINE pFromJSVal #-}

instance PToJSVal JSDocumentFragment where
  pToJSVal (JSDocumentFragment jsval) = jsval
  {-# INLINE pToJSVal #-}

instance IsJSNode JSDocumentFragment where
    toJSNode = JSNode . unJSDocumentFragment

instance IsEventTarget JSDocumentFragment where
    toEventTarget = EventTarget . unJSDocumentFragment

instance InstanceOf JSDocumentFragment where
  instanceOf a = js_instanceOfJSDocumentFragment (pToJSVal a)

foreign import javascript unsafe "$1 instanceof DocumentFragment"
  js_instanceOfJSDocumentFragment :: JSVal -> Bool

foreign import javascript unsafe "$1[\"firstElementChild\"]"
        js_firstElementChild :: JSDocumentFragment -> IO JSVal

firstElementChild :: (MonadIO m) => JSDocumentFragment -> m (Maybe JSElement)
firstElementChild df
  = liftIO ((js_firstElementChild df) >>= fromJSVal)



-- * JSDocument

newtype JSDocument = JSDocument JSVal

unJSDocument (JSDocument o) = o

instance ToJSVal JSDocument where
  toJSVal = pure . unJSDocument
  {-# INLINE toJSVal #-}

instance FromJSVal JSDocument where
  fromJSVal = pure . fmap JSDocument . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance PFromJSVal JSDocument where
  pFromJSVal = JSDocument
  {-# INLINE pFromJSVal #-}

instance PToJSVal JSDocument where
  pToJSVal (JSDocument jsval) = jsval
  {-# INLINE pToJSVal #-}

instance IsJSNode JSDocument where
    toJSNode = JSNode . unJSDocument

instance IsEventTarget JSDocument where
    toEventTarget = EventTarget . unJSDocument

instance InstanceOf JSDocument where
  instanceOf a = js_instanceOfJSDocument (pToJSVal a)

foreign import javascript unsafe "$1 instanceof Document"
  js_instanceOfJSDocument :: JSVal -> Bool

foreign import javascript unsafe "new window[\"Document\"]()"
        js_newDocument :: IO JSDocument

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document Mozilla Document documentation>
newJSDocument :: (MonadIO m) => m JSDocument
newJSDocument = liftIO js_newDocument

foreign import javascript unsafe "$1[\"implementation\"][\"createHTMLDocument\"]()"
       js_createHTMLDocument :: JSDocument -> IO JSDocument

-- foreign import javascript unsafe "document.implementation.createHTMLDocument()"
--        js_createHTMLDocument :: JSDocument -> IO JSDocument

-- | FIXME: actually use title when provided
createHTMLDocument :: JSDocument -> Maybe JSString -> IO JSDocument
createHTMLDocument d mTitle =
  js_createHTMLDocument d

foreign import javascript unsafe "$r = (typeof document === 'undefined') ? null : document"
  ghcjs_currentDocument :: IO JSVal

currentDocument :: (MonadIO m) => m (Maybe JSDocument)
currentDocument = liftIO $ fromJSVal =<< ghcjs_currentDocument

foreign import javascript unsafe "document = $1"
   js_setCurrentDocument :: JSDocument -> IO ()

setCurrentDocument :: (MonadIO m) => JSDocument -> m ()
setCurrentDocument doc = liftIO $ js_setCurrentDocument doc

foreign import javascript unsafe "$r = $1[\"document\"]"
        js_document :: JSWindow -> IO JSVal

document :: (MonadIO m) => JSWindow -> m (Maybe JSDocument)
document w = liftIO $ fromJSVal =<< js_document w

foreign import javascript unsafe "$r = $1[\"querySelector\"]"
        js_querySelector :: JSDocument -> JSString -> IO JSVal

querySelector :: (MonadIO m) => JSDocument -> JSString -> m (Maybe JSElement)
querySelector d sel = liftIO $ fromJSVal =<< js_querySelector d sel

foreign import javascript unsafe "$r = $1[\"body\"]"
        js_body :: JSDocument -> IO JSVal


body :: (MonadIO m) => JSDocument -> m (Maybe JSElement)
body d = liftIO $ fromJSVal =<< js_body d

-- | Commands for 'execCommand' and 'queryCommandState'
data Command
  = BackColor
  | Bold
  | ClearAuthenticationCache
  | ContentReadOnly
  | CopyC
  | CreateLink
  | CutC
  | DecreaseFontSize
  | DefaultParagraphSeparator
  | Delete
  | EnableAbsolutePositionEditor
  | EnableInlineTableEditing
  | EnableObjectResizing
  | FontName
  | FontSize
  | ForeColor
  | FormatBlock
  | ForwardDelete
  | Heading
  | HiliteColor
  | IncreaseFontSize
  | Indent
  | InsertBrOnReturn
  | InsertHorizontalRule
  | InsertHTML
  | InsertImage
  | InsertOrderedList
  | InsertUnorderedList
  | InsertParagraph
  | InsertText
  | Italic
  | JustifyCenter
  | JustifyFull
  | JustifyLeft
  | JustifyRight
  | Outdent
  | PasteC
  | Redo
  | RemoveFormat
  | SelectAll
  | StrikeThrough
  | Subscript
  | Superscript
  | Underline
  | Undo
  | Unlink
  | UseCSS -- deprecated
  | StyleWithCSS
    deriving (Eq, Ord, Read, Show)

commandStr :: Command -> JSString
commandStr BackColor        = "backColor"
commandStr Bold             = "bold"
commandStr ClearAuthenticationCache = "clearAuthenticationCache"
commandStr ContentReadOnly  = "contentReadOnly"
commandStr CopyC             = "copy"
commandStr CreateLink       = "createLink"
commandStr CutC              = "cut"
commandStr DecreaseFontSize = "decreaseFontSize"
commandStr DefaultParagraphSeparator = "defaultParagraphSeparator"
commandStr Delete = "delete"
commandStr EnableAbsolutePositionEditor = "enableAbsolutePositionEditor"
commandStr EnableInlineTableEditing = "enableInlineTableEditing"
commandStr EnableObjectResizing = "enableObjectResizing"
commandStr FontName = "fontName"
commandStr FontSize = "fontSize"
commandStr ForeColor = "foreColor"
commandStr FormatBlock = "formatBlock"
commandStr ForwardDelete = "forwardDelete"
commandStr Heading = "heading"
commandStr HiliteColor = "hiliteColor"
commandStr IncreaseFontSize = "increaseFontSize"
commandStr Indent = "indent"
commandStr InsertBrOnReturn = "insertBrOnReturn"
commandStr InsertHorizontalRule = "insertHorizontalRule"
commandStr InsertHTML = "insertHTML"
commandStr InsertImage = "insertImage"
commandStr InsertOrderedList = "insertorderedlist"
commandStr InsertUnorderedList = "insertUnorderedList"
commandStr InsertParagraph = "insertParagraph"
commandStr InsertText = "insertText"
commandStr Italic = "italic"
commandStr JustifyCenter = "justifyCenter"
commandStr JustifyFull = "justifyFull"
commandStr JustifyLeft = "justifyLeft"
commandStr JustifyRight = "justifyRight"
commandStr Outdent = "outdent"
commandStr PasteC = "paste"
commandStr Redo = "redo"
commandStr RemoveFormat = "removeFormat"
commandStr SelectAll = "selectAll"
commandStr StrikeThrough = "strikeThrough"
commandStr Subscript = "subscript"
commandStr Superscript = "superscript"
commandStr Underline = "underline"
commandStr Undo = "undo"
commandStr Unlink = "unlink"
commandStr UseCSS = "useCSS"
commandStr StyleWithCSS = "styleWithCSS"

foreign import javascript unsafe "$1[\"execCommand\"]($2,$3,$4)"
        js_execCommand :: JSDocument -> JSString -> Bool -> JSVal -> IO Bool

-- | TODO: many commands not implemented
execCommand :: (MonadIO m) => JSDocument -> Command -> Bool -> Maybe JSString -> m Bool
execCommand doc aCommand aShowDefaultUI aValueArgument  =
  liftIO $ js_execCommand doc (commandStr aCommand) aShowDefaultUI (maybe jsNull pToJSVal aValueArgument)

foreign import javascript unsafe "$1[\"queryCommandState\"]($2)"
        js_queryCommandState :: JSDocument -> JSString -> IO Bool

queryCommandState :: (MonadIO m) => JSDocument -> Command -> m Bool
queryCommandState doc aCommand = liftIO (js_queryCommandState doc (commandStr aCommand))


foreign import javascript unsafe "$1[\"queryCommandValue\"]($2)"
        js_queryCommandValue :: JSDocument -> JSString -> IO JSString

queryCommandValue :: (MonadIO m) => JSDocument -> Command -> m Text
queryCommandValue doc aCommand =
  do v <- liftIO (js_queryCommandValue doc (commandStr aCommand))
     pure (textFromJSString v)

-- * JSWindow

newtype JSWindow = JSWindow { unJSWindow ::  JSVal }

instance ToJSVal JSWindow where
  toJSVal = return . unJSWindow
  {-# INLINE toJSVal #-}

instance FromJSVal JSWindow where
  fromJSVal = return . fmap JSWindow . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventTarget JSWindow where
    toEventTarget = EventTarget . unJSWindow

foreign import javascript unsafe "$1 instanceof Window"
  js_instanceOfJSWindow :: JSVal -> Bool

instance InstanceOf JSWindow where
  instanceOf a = js_instanceOfJSWindow (pToJSVal a)

foreign import javascript unsafe "$r = (typeof window === 'undefined') ? null : window"
  js_window :: IO JSVal

window :: (MonadIO m) => m (Maybe JSWindow)
window = liftIO $ fromJSVal =<< js_window

foreign import javascript unsafe "window = $1"
   js_setWindow :: JSWindow -> IO ()

setWindow :: (MonadIO m) => JSWindow -> m ()
setWindow w = liftIO $ js_setWindow w

foreign import javascript unsafe "$1[\"devicePixelRatio\"]"
  js_devicePixelRatio :: JSWindow -> IO JSVal

devicePixelRatio :: (MonadIO m) => JSWindow -> m (Maybe Double)
devicePixelRatio w = liftIO (fromJSVal =<< js_devicePixelRatio w)

foreign import javascript unsafe "$1[\"getSelection\"]()"
  js_getSelection :: JSWindow -> IO Selection

getSelection :: (MonadIO m) => JSWindow -> m Selection
getSelection w = liftIO (js_getSelection w)

-- * JSElement

newtype JSElement = JSElement JSVal

unJSElement (JSElement o) = o

instance ToJSVal JSElement where
  toJSVal = return . unJSElement
  {-# INLINE toJSVal #-}

instance FromJSVal JSElement where
  fromJSVal = return . fmap JSElement . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance PFromJSVal JSElement where
  pFromJSVal = JSElement
  {-# INLINE pFromJSVal #-}

instance IsJSNode JSElement where
    toJSNode = JSNode . unJSElement

foreign import javascript unsafe "$1[\"clientLeft\"]"
        js_getClientLeft :: JSElement -> IO Double

getClientLeft :: (MonadIO m) => JSElement -> m Double
getClientLeft = liftIO . js_getClientLeft

foreign import javascript unsafe "$1[\"clientTop\"]"
        js_getClientTop :: JSElement -> IO Double

getClientTop :: (MonadIO m) => JSElement -> m Double
getClientTop = liftIO . js_getClientTop

foreign import javascript unsafe "$1[\"clientWidth\"]"
        js_getClientWidth :: JSElement -> IO Double

getClientWidth :: (MonadIO m) => JSElement -> m Double
getClientWidth = liftIO . js_getClientWidth

foreign import javascript unsafe "$1[\"clientHeight\"]"
        js_getClientHeight :: JSElement -> IO Double

getClientHeight :: (MonadIO m) => JSElement -> m Double
getClientHeight = liftIO . js_getClientHeight

-- * createJSElement

foreign import javascript unsafe "$1[\"createElement\"]($2)"
        js_createJSElement ::
        JSDocument -> JSString -> IO JSElement

-- | <https://developer.mozilla.org/en-US/docs/Web/API/JSDocument.createJSElement Mozilla JSDocument.createJSElement documentation>
createJSElement :: (MonadIO m) => JSDocument -> Text -> m (Maybe JSElement)
createJSElement document tagName
  = liftIO ((js_createJSElement document (textToJSString tagName))
            >>= return . Just)

-- * innerHTML

foreign import javascript unsafe "$1[\"innerHTML\"] = $2"
        js_setInnerHTML :: JSElement -> JSString -> IO ()

setInnerHTML :: (MonadIO m) => JSElement -> JSString -> m ()
setInnerHTML elm content = liftIO $ js_setInnerHTML elm content

foreign import javascript unsafe "$1[\"innerHTML\"]"
        js_getInnerHTML :: JSElement -> IO JSString

getInnerHTML :: (MonadIO m) => JSElement -> m JSString
getInnerHTML element = liftIO $ js_getInnerHTML element

foreign import javascript unsafe "$1[\"outerHTML\"] = $2"
        js_setOuterHTML :: JSElement -> JSString -> IO ()

setOuterHTML :: (MonadIO m) => JSElement -> JSString -> m ()
setOuterHTML elm content = liftIO $ js_setOuterHTML elm content

foreign import javascript unsafe "$1[\"outerHTML\"]"
        js_getOuterHTML :: JSElement -> IO JSString

getOuterHTML :: (MonadIO m) => JSElement -> m JSString
getOuterHTML element = liftIO $ js_getOuterHTML element

foreign import javascript unsafe "$r = $1[\"tagName\"]"
  js_tagName :: JSElement -> IO JSString

tagName :: (MonadIO m) => JSElement -> m Text
tagName e =
  do v <- liftIO $ js_tagName e
     pure (textFromJSString v)

-- * childNodes

foreign import javascript unsafe "$1[\"childNodes\"]"
        js_childNodes :: JSNode -> IO JSNodeList

childNodes :: (MonadIO m, IsJSNode self) => self -> m JSNodeList
childNodes self
    = liftIO (js_childNodes (toJSNode self))

-- * getElementsByName

foreign import javascript unsafe "$1[\"getElementsByName\"]($2)"
        js_getElementsByName ::
        JSDocument -> JSString -> IO JSNodeList

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document.getElementsByName Mozilla Document.getElementsByName documentation>
getElementsByName ::
                  (MonadIO m) =>
                    JSDocument -> JSString -> m (Maybe JSNodeList)
getElementsByName self elementName
  = liftIO
      ((js_getElementsByName self) elementName
       >>= return . Just)

foreign import javascript unsafe "$1[\"getElementsByClassName\"]($2)"
        js_getElementsByClassNameE ::
        JSElement -> JSString -> IO JSNodeList

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementsByClassName>
getElementsByClassNameE ::
                  (MonadIO m) =>
                    JSElement -> JSString -> m (Maybe JSNodeList)
getElementsByClassNameE elem elementName
  = liftIO
      ((js_getElementsByClassNameE elem) elementName
       >>= return . Just)

foreign import javascript unsafe "$1[\"getElementsByTagName\"]($2)"
        js_getElementsByTagName ::
        JSDocument -> JSString -> IO JSNodeList

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document.getElementsByTagName Mozilla Document.getElementsByTagName documentation>
getElementsByTagName ::
                     (MonadIO m) =>
                       JSDocument -> JSString -> m (Maybe JSNodeList)
getElementsByTagName self tagname
  = liftIO
      ((js_getElementsByTagName self tagname)
       >>= return . Just)

foreign import javascript unsafe "$1[\"getElementById\"]($2)"
        js_getElementsById ::
        JSDocument -> JSString -> IO (Nullable JSElement)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document.getElementsByTagName Mozilla Document.getElementsById documentation>
getElementById ::
                     (MonadIO m) =>
                       JSDocument -> JSString -> m (Maybe JSElement)
getElementById self ident =
  liftIO (nullableToMaybe <$> js_getElementsById self ident)

-- * insertAdjacentElement

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.insertBefore Mozilla Node.insertBefore documentation>

foreign import javascript unsafe "$1[\"insertAdjacentElement\"]($2, $3)"
        js_insertAdjacentElement :: JSNode -> JSString -> JSNode -> IO JSVal

data AdjacentPosition
  = BeforeBegin -- ^ Before the targetElement itself
  | AfterBegin  -- ^ Just inside the targetElement, before its first child.
  | BeforeEnd   -- ^ Just inside the targetElement, before its first child.
  | AfterEnd    -- ^ After the targetElement itself.
    deriving (Eq, Ord, Read, Show)

insertAdjacentElement :: (MonadIO m, IsJSNode targetElement, IsJSNode newNode) =>
               targetElement
            -> AdjacentPosition
            -> newNode
            -> m (Maybe JSNode)
insertAdjacentElement targetElement position newNode =
  liftIO $ fromJSVal =<< (js_insertAdjacentElement (toJSNode targetElement) (domStr position) (toJSNode newNode))
  where
    domStr :: AdjacentPosition -> JSString
    domStr BeforeBegin = "beforebegin"
    domStr AfterBegin  = "afterbegin"
    domStr BeforeEnd   = "beforeend"
    domStr AfterEnd    = "afterend"

-- * insertBefore

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.insertBefore Mozilla Node.insertBefore documentation>

foreign import javascript unsafe "$1[\"insertBefore\"]($2, $3)"
        js_insertBefore :: JSNode -> JSNode -> JSNode -> IO JSNode

insertBefore :: (MonadIO m, IsJSNode parentNode, IsJSNode newNode, IsJSNode referenceNode) =>
               parentNode
             -> newNode
            -> referenceNode
            -> m JSNode
insertBefore parentNode newNode referenceNode =
  liftIO $ (js_insertBefore (toJSNode parentNode) (toJSNode newNode) (toJSNode referenceNode))

-- * appendChild

foreign import javascript unsafe "$1[\"appendChild\"]($2)"
        js_appendChild :: JSNode -> JSNode -> IO JSNode

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.appendChild Mozilla Node.appendChild documentation>

appendChild :: (MonadIO m, IsJSNode self, IsJSNode newChild) =>
               self
            -> Maybe newChild
            -> m (Maybe JSNode)
appendChild self newChild
  = liftIO
      ((js_appendChild ( (toJSNode self))
          (maybe (JSNode jsNull) ( toJSNode) newChild))
         >>= return . Just)

foreign import javascript unsafe "$1[\"focus\"]()" js_focus :: JSElement -> IO ()

focus :: (MonadIO m) => JSElement -> m ()
focus e = liftIO (js_focus e)

foreign import javascript unsafe "$1[\"blur\"]()" js_blur :: JSElement -> IO ()

blur :: (MonadIO m) => JSElement -> m ()
blur e = liftIO (js_blur e)

-- * textContent

foreign import javascript unsafe "$1[\"textContent\"] = $2"
        js_setTextContent :: JSVal -> JSString -> IO ()

setTextContent :: (MonadIO m, IsJSNode self) =>
                  self
               -> Text
               -> m ()
setTextContent self content =
    liftIO $ (js_setTextContent (unJSNode (toJSNode self)) (textToJSString content))

foreign import javascript unsafe "$r = $1[\"textContent\"]"
        js_getTextContent :: JSVal -> IO JSString

getTextContent :: (MonadIO m, IsJSNode self) =>
                  self
               -> m Text
getTextContent self =
  liftIO $ (fmap textFromJSString $ js_getTextContent (unJSNode (toJSNode self)))


-- * replaceData

-- FIMXE: perhaps only a TextNode?
foreign import javascript unsafe "$1[\"replaceData\"]($2, $3, $4)" js_replaceData
    :: JSNode
    -> Word
    -> Word
    -> JSString
    -> IO ()

replaceData :: (MonadIO m, IsJSNode self) =>
               self
            -> Word
            -> Word
            -> Text
            -> m ()
replaceData self start length string =
    liftIO (js_replaceData (toJSNode self) start length (textToJSString string))

-- * remove

foreign import javascript unsafe "$1[\"remove\"]()"
        js_remove :: JSNode -> IO ()

remove :: (MonadIO m, IsJSNode self) => self -> m ()
remove self
  = liftIO (js_remove (toJSNode self))

-- * removeChild

foreign import javascript unsafe "$1[\"removeChild\"]($2)"
        js_removeChild :: JSNode -> JSNode -> IO JSNode

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.removeChild Mozilla Node.removeChild documentation>
removeChild ::  -- FIMXE: really a maybe?
            (MonadIO m, IsJSNode self, IsJSNode oldChild) =>
              self -> Maybe oldChild -> m (Maybe JSNode)
removeChild self oldChild
  = liftIO
      ((js_removeChild (toJSNode self)
          (maybe (JSNode jsNull) toJSNode oldChild))
         >>= return . Just)

-- * replaceChild

foreign import javascript unsafe "$1[\"replaceChild\"]($2, $3)"
        js_replaceChild :: JSNode -> JSNode -> JSNode -> IO JSNode

replaceChild ::
            (MonadIO m, IsJSNode self, IsJSNode newChild, IsJSNode oldChild) =>
              self -> newChild -> oldChild -> m (Maybe JSNode)
replaceChild self newChild oldChild
  = liftIO
      (js_replaceChild ((toJSNode self))
                       ((toJSNode) newChild)
                       ((toJSNode) oldChild)
         >>= return . Just)

-- * replaceWith

foreign import javascript unsafe "$1[\"replaceWith\"]($2)"
        js_replaceWith :: JSNode -> JSNode -> IO ()

replaceWith :: (IsJSNode oldNode, IsJSNode newNode, MonadIO m) => oldNode -> newNode -> m ()
replaceWith old new = liftIO $ js_replaceWith (toJSNode old) (toJSNode new)

-- * firstChild

foreign import javascript unsafe "$1[\"firstChild\"]"
        js_getFirstChild :: JSNode -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.firstChild Mozilla Node.firstChild documentation>
getFirstChild :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
getFirstChild self
  = liftIO ((js_getFirstChild ((toJSNode self))) >>= fromJSVal)

firstChild :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
firstChild = getFirstChild

-- * lastChild

foreign import javascript unsafe "$1[\"lastChild\"]"
        js_lastChild :: JSNode -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.firstChild Mozilla Node.firstChild documentation>
lastChild :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
lastChild self
  = liftIO ((js_lastChild ((toJSNode self))) >>= fromJSVal)


-- | remove all the children
removeChildren
    :: (MonadIO m, IsJSNode self) =>
       self
    -> m ()
removeChildren self =
    do mc <- getFirstChild self
       case mc of
         Nothing -> return ()
         (Just _) ->
             do removeChild self mc
                removeChildren self

-- * nextSibling

foreign import javascript unsafe "$1[\"nextSibling\"]"
        js_nextSibling :: JSNode -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.nextSibling Mozilla Node.nextSibling documentation>
nextSibling :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
nextSibling self
  = liftIO ((js_nextSibling ((toJSNode self))) >>= fromJSVal)


foreign import javascript unsafe "$1[\"previousSibling\"]"
        js_previousSibling :: JSNode -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.previousSibling Mozilla Node.nextSibling documentation>
previousSibling :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
previousSibling self
  = liftIO ((js_previousSibling ((toJSNode self))) >>= fromJSVal)


foreign import javascript unsafe "$1[\"setAttribute\"]($2, $3)"
        js_setAttribute :: JSElement -> JSString -> JSString -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.setAttribute Mozilla Element.setAttribute documentation>
setAttribute ::
             (MonadIO m) =>
               JSElement -> Text -> Text -> m ()
setAttribute self name value
  = liftIO
      (js_setAttribute self (textToJSString name) (textToJSString value))


foreign import javascript unsafe "$1[\"getAttribute\"]($2)"
        js_getAttribute :: JSElement -> JSString -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.setAttribute Mozilla Element.setAttribute documentation>
getAttribute :: (MonadIO m) =>
                JSElement
             -> JSString
             -> m (Maybe JSString)
getAttribute self name = liftIO (pFromJSVal <$> js_getAttribute self name)

foreign import javascript unsafe "$1[\"removeAttribute\"]($2)"
        js_removeAttribute :: JSElement -> JSString -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.removeAttribute Mozilla Element.removeAttribute documentation>
removeAttribute :: (MonadIO m) =>
                JSElement -> Text -> m ()
removeAttribute self name = liftIO (js_removeAttribute self (textToJSString name))

foreign import javascript unsafe "$1[\"style\"][$2] = $3"
        js_setStyle :: JSElement -> JSString -> JSVal -> IO ()

setStyle :: (MonadIO m, PToJSVal v) => JSElement -> JSString -> v -> m ()
setStyle self name value
  = liftIO
      (js_setStyle self name (pToJSVal value))

foreign import javascript unsafe "$1[$2] = $3"
        js_setProperty :: JSElement -> JSString -> JSVal -> IO ()

setProperty :: (MonadIO m, PToJSVal v) => JSElement -> Text -> v -> m ()
setProperty self name value
  = liftIO
      (js_setProperty self (textToJSString name) (pToJSVal value))

foreign import javascript unsafe "delete $1[$2]"
    js_delete :: JSVal -> JSString -> IO ()

deleteProperty :: (MonadIO m) => JSElement -> Text -> m ()
deleteProperty e n = liftIO (js_delete (unJSElement e) (textToJSString n))

foreign import javascript unsafe "$r = $1[\"checked\"]"
  js_getChecked :: JSElement -> IO Bool

getChecked :: (MonadIO m) => JSElement -> m Bool
getChecked e = liftIO $ js_getChecked e

foreign import javascript unsafe "$1[\"checked\"] = $2"
        js_setChecked :: JSElement -> Bool -> IO ()

setChecked :: (MonadIO m) => JSElement -> Bool -> m ()
setChecked e b = liftIO $ js_setChecked e b

{-
setCSS :: (MonadIO m) =>
          JSElement
       -> JSString
       -> JSString
       -> m ()
setCSS elem name value =
  liftIO $ js_setCSS elem name value
-}
-- * value

foreign import javascript unsafe "$1[\"value\"]"
        js_getValue :: JSNode -> IO JSString

getValue :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSString)
getValue self
  = liftIO ((js_getValue (toJSNode self)) >>= pure . Just)

foreign import javascript unsafe "$1[\"value\"] = $2"
        js_setValue :: JSNode -> JSString -> IO ()

setValue :: (MonadIO m, IsJSNode self) => self -> Text -> m ()
setValue self str =
    liftIO (js_setValue (toJSNode self) (textToJSString str))

foreign import javascript unsafe "$1[\"hasFocus\"]()"
        js_hasFocus :: JSDocument -> IO Bool

hasFocus :: (MonadIO m) => JSDocument -> m Bool
hasFocus doc = liftIO (js_hasFocus doc)

foreign import javascript unsafe "$1[\"matches\"]($2)"
        js_matches :: JSElement -> JSString -> IO Bool

matches :: (MonadIO m) => JSElement -> Text -> m Bool
matches e selectorStr = liftIO $ (js_matches e (textToJSString selectorStr))

foreign import javascript unsafe "$r = $1[\"activeElement\"]"
        js_getActiveElement :: JSDocument -> IO JSElement

getActiveElement :: (MonadIO m) => JSDocument -> m JSElement
getActiveElement d = liftIO (js_getActiveElement d)

-- * dataset

foreign import javascript unsafe "$1[\"dataset\"][$2]"
        js_getData :: JSNode -> JSString -> IO (Nullable JSString)

getData :: (MonadIO m, IsJSNode self) => self -> JSString -> m (Maybe JSString)
getData self name = liftIO (nullableToMaybe <$> js_getData (toJSNode self) name)
--getData self name = liftIO (fmap fromJSVal <$> maybeJSNullOrUndefined <$> (js_getData (toJSNode self) name))

foreign import javascript unsafe "$1[\"dataset\"][$2] = $3"
        js_setData :: JSNode -> JSString -> JSString -> IO ()

setData :: (MonadIO m, IsJSNode self) => self -> JSString -> JSString -> m ()
setData self name value = liftIO (js_setData (toJSNode self) name value)

-- * JSTextNode

newtype JSTextNode = JSTextNode JSVal -- deriving (Eq)

unJSTextNode (JSTextNode o) = o

instance ToJSVal JSTextNode where
  toJSVal = return . unJSTextNode
  {-# INLINE toJSVal #-}

instance FromJSVal JSTextNode where
  fromJSVal = return . fmap JSTextNode . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance PFromJSVal JSTextNode where
  pFromJSVal = JSTextNode
  {-# INLINE pFromJSVal #-}

instance PToJSVal JSTextNode where
  pToJSVal = unJSTextNode
  {-# INLINE pToJSVal #-}

instance IsJSNode JSTextNode where
    toJSNode = JSNode . unJSTextNode

foreign import javascript unsafe "$1 instanceof Text"
  js_instanceOfText :: JSVal -> Bool

instance InstanceOf JSTextNode where
  instanceOf a = js_instanceOfText (pToJSVal a)

-- * isEqualNode

foreign import javascript unsafe "$1[\"isEqualNode\"]($2)"
  js_isEqualNode :: JSNode -> JSNode -> IO Bool

isEqualNode :: (MonadIO m) => (IsJSNode obj1, IsJSNode obj2) => obj1 -> obj2 -> m Bool
isEqualNode obj1 obj2 = liftIO $ js_isEqualNode (toJSNode obj1) (toJSNode obj2)

-- * createTextNode

foreign import javascript unsafe "$1[\"createTextNode\"]($2)"
        js_createTextNode :: JSDocument -> JSString -> IO JSTextNode

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document.createTextNode Mozilla Document.createTextNode documentation>
createJSTextNode :: (MonadIO m) => JSDocument -> Text -> m (Maybe JSTextNode)
createJSTextNode document data'
  = liftIO
      ((js_createTextNode document
          (textToJSString data'))
         >>= return . Just)

foreign import javascript unsafe "$1[\"nodeValue\"]"
  js_nodeValue :: JSNode -> IO JSString

nodeValue :: (MonadIO m) => JSNode -> m JSString
nodeValue node = liftIO $ js_nodeValue node


foreign import javascript unsafe "$1[\"nodeValue\"] = $2"
  js_setNodeValue :: JSNode -> JSString -> IO ()

setNodeValue :: (MonadIO m) => JSNode -> JSString -> m ()
setNodeValue node val = liftIO $ js_setNodeValue node val

-- * Events

instance IsEventTarget JSElement where
    toEventTarget = EventTarget . unJSElement

class IsEvent ev where
  eventToJSString :: ev -> JSString

data Event
  = ReadyStateChange
  deriving (Eq, Show, Read)

instance IsEvent Event where
  eventToJSString ReadyStateChange = JS.pack "readystatechange"

data MouseEvent
  = Click
  | ContextMenu
  | DblClick
  | MouseDown
  | MouseEnter
  | MouseLeave
  | MouseMove
  | MouseOver
  | MouseOut
  | MouseUp
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance IsEvent MouseEvent where
  eventToJSString Click       = JS.pack "click"
  eventToJSString ContextMenu = JS.pack "contextmenu"
  eventToJSString DblClick    = JS.pack "dblclick"
  eventToJSString MouseDown   = JS.pack "mousedown"
  eventToJSString MouseEnter  = JS.pack "mouseenter"
  eventToJSString MouseLeave  = JS.pack "mouseleave"
  eventToJSString MouseMove   = JS.pack "mousemove"
  eventToJSString MouseOver   = JS.pack "mouseover"
  eventToJSString MouseOut    = JS.pack "mouseout"
  eventToJSString MouseUp     = JS.pack "mouseup"

data KeyboardEvent
  = KeyDown
  | KeyPress
  | KeyUp
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance IsEvent KeyboardEvent where
  eventToJSString KeyDown  = JS.pack "keydown"
  eventToJSString KeyPress = JS.pack "keypress"
  eventToJSString KeyUp    = JS.pack "keyup"

data FrameEvent
  = FrameAbort
  | BeforeUnload
  | FrameError
  | HashChange
  | FrameLoad
  | PageShow
  | PageHide
  | Resize
  | Scroll
  | Unload
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance IsEvent FrameEvent where
  eventToJSString = JS.pack . map Char.toLower . show

data FocusEvent
  = Blur
  | Focus
  | FocusIn  -- bubbles
  | FocusOut -- bubbles
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance IsEvent FocusEvent where
  eventToJSString Blur     = JS.pack "blur"
  eventToJSString Focus    = JS.pack "focus"
  eventToJSString FocusIn  = JS.pack "focusin"
  eventToJSString FocusOut = JS.pack "focusout"

data FormEvent
  = Change
  | Input
  | Invalid
  | Reset
--  | Search
--  | Select
  | Submit
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance IsEvent FormEvent where
  eventToJSString Change   = JS.pack "change"
  eventToJSString Input    = JS.pack "input"
  eventToJSString Invalid  = JS.pack "invalid"
  eventToJSString Reset    = JS.pack "reset"
--  eventToJSString Search   = JS.pack "search"
--  eventToJSString Select   = JS.pack "select"
  eventToJSString Submit   = JS.pack "submit"

data DragEvent
  = Drag
  | DragEnd
  | DragEnter
  | DragLeave
  | DragOver
  | DragStart
  | Drop
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance IsEvent DragEvent where
  eventToJSString Drag = JS.pack "drag"
  eventToJSString DragEnd = JS.pack "dragend"
  eventToJSString DragEnter = JS.pack "dragenter"
  eventToJSString DragLeave = JS.pack "dragleave"
  eventToJSString DragOver = JS.pack "dragover"
  eventToJSString DragStart = JS.pack "dragstart"
  eventToJSString Drop = JS.pack "drop"

data PrintEvent
  = AfterPrint
  | BeforePrint
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data MediaEvent
  = CanPlay
  | CanPlayThrough
  | DurationChange
  | Emptied
  | Ended
  | MediaError
  | LoadedData
  | LoadedMetaData
  | Pause
  | Play
  | Playing
  | RateChange
  | Seeked
  | Seeking
  | Stalled
  | Suspend
  | TimeUpdate
  | VolumeChange
  | Waiting
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ProgressEvent
  = LoadStart
  | Progress
  | ProgressAbort
  | ProgressError
  | ProgressLoad
  | Timeout
  | LoadEnd
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance IsEvent ProgressEvent where
  eventToJSString LoadStart     = JS.pack "loadstart"
  eventToJSString Progress      = JS.pack "progress"
  eventToJSString ProgressAbort = JS.pack "abort"
  eventToJSString ProgressError = JS.pack "error"
  eventToJSString ProgressLoad  = JS.pack "load"
  eventToJSString Timeout       = JS.pack "timeout"
  eventToJSString LoadEnd       = JS.pack "loadend"

data AnimationEvent
  = AnimationEnd
  | AnimationInteration
  | AnimationStart
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data TransitionEvent
  = TransitionEnd
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ServerSentEvent
  = ServerError
  | ServerMessage
  | Open
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

data MiscEvent
  = MiscMessage
  | Online
  | Offline
  | PopState
  | MiscShow
  | Storage
  | Toggle
  | Wheel
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data TouchEvent
  = TouchCancel
  | TouchEnd
  | TouchMove
  | TouchStart
  deriving (Eq, Ord, Show, Read)

data SelectionEvent
  = SelectionStart
  | SelectionChange
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance IsEvent SelectionEvent where
  eventToJSString SelectionStart  = JS.pack "selectionstart"
  eventToJSString SelectionChange = JS.pack "selectionchange"

data ElementScrollEvent
  = ElementScroll
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance IsEvent ElementScrollEvent where
  eventToJSString ElementScroll  = JS.pack "scroll"

-- https://developer.mozilla.org/en-US/docs/Web/API/ProgressEvent
-- data ProgressEvent =

-- | FIXME: is this type used anymore? Probably remove it.
data EventType
  = MouseEvent MouseEvent
  | KeyboardEvent KeyboardEvent
  | FrameEvent FrameEvent
  | FormEvent FormEvent
  | DragEvent DragEvent
  | ClipboardEvent ClipboardEvent
  | PrintEvent PrintEvent
  | MediaEvent MediaEvent
  | AnimationEvent AnimationEvent
  | TransitionEvent TransitionEvent
  | ServerSentEvent ServerSentEvent
  | MiscEvent MiscEvent
  | TouchEvent TouchEvent
  | SelectionEvent SelectionEvent
  | ElementScrollEvent ElementScrollEvent
  deriving (Eq, Ord, Show, Read)
-- * Event Objects

-- http://www.w3schools.com/jsref/dom_obj_event.asp

class IsEventObject obj where
  asEventObject        :: obj -> EventObject

-- * EventObject

newtype EventObject = EventObject { unEventObject :: JSVal }

instance Show EventObject where
  show _ = "EventObject"

instance ToJSVal EventObject where
  toJSVal = return . unEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal EventObject where
  fromJSVal = return . fmap EventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject EventObject where
  asEventObject = id

foreign import javascript unsafe "$1[\"defaultPrevented\"]" js_defaultPrevented ::
        EventObject -> IO Bool

defaultPrevented :: (IsEventObject obj, MonadIO m) => obj -> m Bool
defaultPrevented obj = liftIO (js_defaultPrevented (asEventObject obj))

foreign import javascript unsafe "$1[\"currentTarget\"]" js_currentTarget ::
        EventObject -> EventTarget

currentTarget :: (IsEventObject obj) => obj -> EventTarget
currentTarget obj = js_currentTarget (asEventObject obj)

foreign import javascript unsafe "$1[\"target\"]" js_target ::
        EventObject -> EventTarget

target :: (IsEventObject obj) => obj -> EventTarget
target obj = js_target (asEventObject obj)

foreign import javascript unsafe "$1[\"preventDefault\"]()" js_preventDefault ::
        EventObject -> IO ()

preventDefault :: (IsEventObject obj) => obj -> IO ()
preventDefault obj = (js_preventDefault (asEventObject obj))

foreign import javascript unsafe "$1[\"stopPropagation\"]()" js_stopPropagation ::
        EventObject -> IO ()

-- stopPropagation :: (IsEventObject obj, MonadIO m) => obj -> m ()
stopPropagation :: (IsEventObject obj) => obj -> IO ()
stopPropagation obj = (js_stopPropagation (asEventObject obj))

-- * MouseEventObject

newtype MouseEventObject = MouseEventObject { unMouseEventObject :: JSVal }

instance Show MouseEventObject where
  show _ = "MouseEventObject"

instance ToJSVal MouseEventObject where
  toJSVal = return . unMouseEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal MouseEventObject where
  fromJSVal = return . fmap MouseEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject MouseEventObject where
  asEventObject (MouseEventObject jsval) = EventObject jsval

foreign import javascript unsafe "$1[\"clientX\"]" clientX ::
        MouseEventObject -> Double

foreign import javascript unsafe "$1[\"clientY\"]" clientY ::
        MouseEventObject -> Double

foreign import javascript unsafe "$1[\"button\"]" button ::
        MouseEventObject -> Int

foreign import javascript unsafe "$1[\"shiftKey\"]" mouse_shiftKey ::
        MouseEventObject -> Bool

foreign import javascript unsafe "$1[\"ctrlKey\"]" mouse_ctrlKey ::
        MouseEventObject -> Bool

foreign import javascript unsafe "$1[\"altKey\"]" mouse_altKey ::
        MouseEventObject -> Bool

foreign import javascript unsafe "$1[\"metaKey\"]" mouse_metaKey ::
        MouseEventObject -> Bool

instance HasModifierKeys MouseEventObject where
  shiftKey = mouse_shiftKey
  ctrlKey  = mouse_ctrlKey
  altKey   = mouse_altKey
  metaKey  = mouse_metaKey

-- * KeyboardEventObject

newtype KeyboardEventObject = KeyboardEventObject { unKeyboardEventObject :: JSVal }

instance ToJSVal KeyboardEventObject where
  toJSVal = return . unKeyboardEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal KeyboardEventObject where
  fromJSVal = return . fmap KeyboardEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject KeyboardEventObject where
  asEventObject (KeyboardEventObject jsval) = EventObject jsval

foreign import javascript unsafe "$1[\"charCode\"]" charCode ::
        KeyboardEventObject -> Int

foreign import javascript unsafe "$1[\"keyCode\"]" keyCode ::
        KeyboardEventObject -> Int

foreign import javascript unsafe "$1[\"which\"]" which ::
        KeyboardEventObject -> Int


class HasModifierKeys obj where
  shiftKey :: obj -> Bool
  ctrlKey  :: obj -> Bool
  altKey   :: obj -> Bool
  metaKey  :: obj -> Bool

foreign import javascript unsafe "$1[\"shiftKey\"]" keyboard_shiftKey ::
        KeyboardEventObject -> Bool

foreign import javascript unsafe "$1[\"ctrlKey\"]" keyboard_ctrlKey ::
        KeyboardEventObject -> Bool

foreign import javascript unsafe "$1[\"altKey\"]" keyboard_altKey ::
        KeyboardEventObject -> Bool

foreign import javascript unsafe "$1[\"metaKey\"]" keyboard_metaKey ::
        KeyboardEventObject -> Bool

instance HasModifierKeys KeyboardEventObject where
  shiftKey = keyboard_shiftKey
  ctrlKey  = keyboard_ctrlKey
  altKey   = keyboard_altKey
  metaKey  = keyboard_metaKey

-- * FocusEventObject

newtype FocusEventObject = FocusEventObject { unFocusEventObject :: JSVal }

instance Show FocusEventObject where
  show _ = "FocusEventObject"

instance ToJSVal FocusEventObject where
  toJSVal = return . unFocusEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal FocusEventObject where
  fromJSVal = return . fmap FocusEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject FocusEventObject where
  asEventObject (FocusEventObject jsval) = EventObject jsval


-- * ProgressEventObject

newtype ProgressEventObject = ProgressEventObject { unProgressEventObject :: JSVal }

instance ToJSVal ProgressEventObject where
  toJSVal = return . unProgressEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal ProgressEventObject where
  fromJSVal = return . fmap ProgressEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

-- charCode :: (MonadIO m) => KeyboardEventObject -> IO

-- * ElementScrollObject

newtype ElementScrollEventObject = ElementScrollEventObject { unElementScrollEventObject :: JSVal }

instance Show ElementScrollEventObject where
  show _ = "ElementScrollEventObject"

instance ToJSVal ElementScrollEventObject where
  toJSVal = return . unElementScrollEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal ElementScrollEventObject where
  fromJSVal = return . fmap ElementScrollEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject ElementScrollEventObject where
  asEventObject (ElementScrollEventObject jsval) = EventObject jsval

foreign import javascript unsafe "$1[\"scrollTop\"]" scrollTop ::
        ElementScrollEventObject -> Int
foreign import javascript unsafe "$1[\"scrollLeft\"]" scrollLeft ::
        ElementScrollEventObject -> Int

-- * EventObjectOf

type family EventObjectOf event :: *
type instance EventObjectOf Event          = EventObject
type instance EventObjectOf MouseEvent     = MouseEventObject
type instance EventObjectOf KeyboardEvent  = KeyboardEventObject
type instance EventObjectOf FocusEvent     = FocusEventObject
type instance EventObjectOf FormEvent      = EventObject
type instance EventObjectOf ProgressEvent  = ProgressEventObject
type instance EventObjectOf ClipboardEvent = ClipboardEventObject
type instance EventObjectOf SelectionEvent = SelectionEventObject
type instance EventObjectOf DragEvent      = DragEventObject
type instance EventObjectOf ElementScrollEvent = ElementScrollEventObject

-- * CustomEvent

data CustomEvent (e :: k) = CustomEvent

instance KnownSymbol e => IsEvent (CustomEvent e) where
  eventToJSString (CustomEvent {}) = JS.pack $ symbolVal (Proxy :: Proxy e)

newtype CustomEventObject (e :: k) a = CustomEventObject { unCustomEventObject :: JSVal }

instance ToJSVal (CustomEventObject e a) where
  toJSVal = return . unCustomEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (CustomEventObject e a) where
  fromJSVal = return . fmap CustomEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (CustomEventObject e a) where
  asEventObject (CustomEventObject jsval) = EventObject jsval

foreign import javascript unsafe "new CustomEvent($1, { 'detail': $2, 'bubbles' : $3, 'cancelable' : $4})"
        js_newCustomEvent :: JSString -> JSVal -> Bool -> Bool -> IO JSVal

newCustomEvent :: (IsEvent (CustomEvent ev), FromJSVal a, ToJSVal a) => CustomEvent ev -> Maybe a -> Bool -> Bool -> IO (CustomEventObject ev a)
newCustomEvent ev detail bubbles cancelable =
  do let evStr = eventToJSString ev
     d <- toJSVal detail
     jsval <- js_newCustomEvent evStr d bubbles cancelable
     pure $ CustomEventObject jsval

foreign import javascript unsafe "$r = $1[\"detail\"]"
        js_detail :: CustomEventObject e a -> JSVal

detail :: (FromJSVal a) => CustomEventObject e a -> IO (Maybe a)
detail ceo = fromJSVal $ js_detail ceo

-- * addEventListener

-- FIXME: Element is overly restrictive
foreign import javascript unsafe "$1[\"addEventListener\"]($2, $3,\n$4)"
   js_addEventListener :: EventTarget -> JSString -> Callback (JSVal -> IO ()) -> Bool -> IO ()

addEventListener :: (MonadIO m, IsEventTarget self, IsEvent event, FromJSVal (EventObjectOf event)) =>
                  self
               -> event
               -> (EventObjectOf event -> IO ())
               -> Bool
               -> m ()
addEventListener self event callback useCapture = liftIO $
  do cb <- syncCallback1 ThrowWouldBlock callback'
     js_addEventListener (toEventTarget self) (eventToJSString event) cb useCapture
  where
    callback' = \ev ->
         do (Just eventObject) <- fromJSVal ev
            callback eventObject

foreign import javascript unsafe "$1[\"dispatchEvent\"]($2)"
  js_dispatchEvent :: EventTarget -> EventObject -> IO ()

dispatchEvent :: (MonadIO m, IsEventTarget eventTarget, IsEventObject eventObj) => eventTarget -> eventObj -> m ()
dispatchEvent et ev = liftIO $ js_dispatchEvent (toEventTarget et) (asEventObject ev)

{-
-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventTarget.addEventListener Mozilla EventTarget.addEventListener documentation>
addEventListener
  :: (MonadIO m, IsEventTarget self) =>
     self
  -> EventType
  -> Callback (IO ())
  -> Bool
  -> m ()
addEventListener self type' listener useCapture
  = liftIO
      (js_addEventListener (toEventTarget self)
         type''
         listener
--         (maybe jsNull pToJSVal listener)
         useCapture)
             where
               type'' = case type' of
                          Change -> JS.pack "change"
                          Click  -> JS.pack "click"
                          Input  -> JS.pack "input"
                          Blur   -> JS.pack "blur"
                          Keydown -> JS.pack "keydown"
                          Keyup   -> JS.pack "keyup"
                          Keypress -> JS.pack "keypress"
                          ReadyStateChange -> JS.pack "readystatechange"
                          EventTxt s -> s

-}


-- * DOMRect

newtype DOMClientRect = DomClientRect { unDomClientRect :: JSVal }

foreign import javascript unsafe "$1[\"width\"]" width ::
         DOMClientRect -> Double

foreign import javascript unsafe "$1[\"top\"]" rectTop ::
         DOMClientRect -> Double

foreign import javascript unsafe "$1[\"left\"]" rectLeft ::
         DOMClientRect -> Double

foreign import javascript unsafe "$1[\"right\"]" rectRight ::
         DOMClientRect -> Double

foreign import javascript unsafe "$1[\"bottom\"]" rectBottom ::
         DOMClientRect -> Double

foreign import javascript unsafe "$1[\"height\"]" height ::
         DOMClientRect -> Double

foreign import javascript unsafe "$1[\"getBoundingClientRect\"]()" js_getBoundingClientRect ::
  JSElement -> IO DOMClientRect

getBoundingClientRect :: (MonadIO m) => JSElement -> m DOMClientRect
getBoundingClientRect = liftIO . js_getBoundingClientRect

-- * XMLHttpRequest
newtype XMLHttpRequest = XMLHttpRequest { unXMLHttpRequest :: JSVal }

instance Eq (XMLHttpRequest) where
  (XMLHttpRequest a) == (XMLHttpRequest b) = js_eq a b

instance IsEventTarget XMLHttpRequest where
    toEventTarget = EventTarget . unXMLHttpRequest

foreign import javascript unsafe "$1 instanceof XMLHttpRequest"
  js_instanceOfXMLHttpRequest :: JSVal -> Bool

instance InstanceOf XMLHttpRequest where
  instanceOf a = js_instanceOfXMLHttpRequest (pToJSVal a)

{-
instance PToJSVal XMLHttpRequest where
  pToJSVal = unXMLHttpRequest
  {-# INLINE pToJSVal #-}

instance PFromJSVal XMLHttpRequest where
  pFromJSVal = XMLHttpRequest
  {-# INLINE pFromJSVal #-}
-}
instance ToJSVal XMLHttpRequest where
  toJSVal = return . unXMLHttpRequest
  {-# INLINE toJSVal #-}

instance FromJSVal XMLHttpRequest where
  fromJSVal = return . fmap XMLHttpRequest . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

foreign import javascript unsafe "new window[\"XMLHttpRequest\"]()"
        js_newXMLHttpRequest :: IO XMLHttpRequest

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest Mozilla XMLHttpRequest documentation>
newXMLHttpRequest :: (MonadIO m) => m XMLHttpRequest
newXMLHttpRequest
  = liftIO js_newXMLHttpRequest

foreign import javascript unsafe "$1[\"open\"]($2, $3, $4)"
        js_open ::
        XMLHttpRequest ->
          JSString -> JSString -> Bool -> {- JSString -> JSString -> -} IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.open Mozilla XMLHttpRequest.open documentation>
open ::
     (MonadIO m) =>
       XMLHttpRequest -> Text -> Text -> Bool -> m ()
open self method url async
  = liftIO (js_open self (textToJSString method) (textToJSString url) async)

foreign import javascript unsafe "$1[\"setRequestHeader\"]($2,$3)"
        js_setRequestHeader
            :: XMLHttpRequest
            -> JSString
            -> JSString
            -> IO ()

setRequestHeader :: (MonadIO m) =>
                    XMLHttpRequest
                 -> Text
                 -> Text
                 -> m ()
setRequestHeader self header value =
    liftIO (js_setRequestHeader self (textToJSString header) (textToJSString value))

-- foreign import javascript interruptible "h$dom$sendXHR($1, $2, $c);" js_send :: JSVal XMLHttpRequest -> JSVal () -> IO Int
{-
-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest#send() Mozilla XMLHttpRequest.send documentation>
send :: (MonadIO m) => XMLHttpRequest -> m ()
send self = liftIO $ js_send (unXMLHttpRequest self) jsNull >> return () -- >>= throwXHRError
-}

foreign import javascript unsafe "$1[\"send\"]()" js_send ::
        XMLHttpRequest -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest#send() Mozilla XMLHttpRequest.send documentation>
send :: (MonadIO m) => XMLHttpRequest -> m ()
send self =
    liftIO $ js_send self >> return () -- >>= throwXHRError

foreign import javascript unsafe "$1[\"send\"]($2)" js_sendString ::
        XMLHttpRequest -> JSString -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest#send() Mozilla XMLHttpRequest.send documentation>
sendString :: (MonadIO m) => XMLHttpRequest -> JSString -> m ()
sendString self str =
    liftIO $ js_sendString self str >> return () -- >>= throwXHRError

foreign import javascript unsafe "$1[\"send\"]($2)" js_sendArrayBuffer ::
        XMLHttpRequest -> JSVal -> IO ()

sendArrayBuffer :: (MonadIO m) => XMLHttpRequest -> Buffer -> m ()
sendArrayBuffer xhr buf =
    liftIO $ do ref <- fmap (pToJSVal . getArrayBuffer) (thaw buf)
                js_sendArrayBuffer xhr ref

foreign import javascript unsafe "$1[\"send\"]($2)" js_sendData ::
        XMLHttpRequest
    -> JSVal
    -> IO ()

foreign import javascript unsafe "$1[\"readyState\"]"
        js_getReadyState :: XMLHttpRequest -> IO Word

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.readyState Mozilla XMLHttpRequest.readyState documentation>
getReadyState :: (MonadIO m) => XMLHttpRequest -> m Word
getReadyState self
  = liftIO (js_getReadyState self)

foreign import javascript unsafe "$1[\"responseType\"]"
        js_getResponseType ::
        XMLHttpRequest -> IO JSString -- XMLHttpRequestResponseType

-- | <Https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.responseType Mozilla XMLHttpRequest.responseType documentation>
getResponseType ::
                (MonadIO m) => XMLHttpRequest -> m Text
getResponseType self
  = liftIO (textFromJSString <$> js_getResponseType self)


foreign import javascript unsafe "$1[\"responseType\"] = $2"
        js_setResponseType ::
        XMLHttpRequest -> JSString -> IO () -- XMLHttpRequestResponseType

setResponseType :: (MonadIO m) =>
                   XMLHttpRequest
                -> Text
                -> m ()
setResponseType self typ =
    liftIO $ js_setResponseType self (textToJSString typ)

data XMLHttpRequestResponseType = XMLHttpRequestResponseType
                                | XMLHttpRequestResponseTypeArraybuffer
                                | XMLHttpRequestResponseTypeBlob
                                | XMLHttpRequestResponseTypeDocument
                                | XMLHttpRequestResponseTypeJson
                                | XMLHttpRequestResponseTypeText
foreign import javascript unsafe "\"\""
        js_XMLHttpRequestResponseType :: JSVal -- XMLHttpRequestResponseType

foreign import javascript unsafe "\"arraybuffer\""
        js_XMLHttpRequestResponseTypeArraybuffer ::
        JSVal -- XMLHttpRequestResponseType

foreign import javascript unsafe "\"blob\""
        js_XMLHttpRequestResponseTypeBlob ::
        JSVal -- XMLHttpRequestResponseType

foreign import javascript unsafe "\"document\""
        js_XMLHttpRequestResponseTypeDocument ::
        JSVal -- XMLHttpRequestResponseType

foreign import javascript unsafe "\"json\""
        js_XMLHttpRequestResponseTypeJson ::
        JSVal -- XMLHttpRequestResponseType

foreign import javascript unsafe "\"text\""
        js_XMLHttpRequestResponseTypeText ::
        JSVal -- XMLHttpRequestResponseType

{-
instance PToJSVal XMLHttpRequestResponseType where
        pToJSVal XMLHttpRequestResponseType = js_XMLHttpRequestResponseType
        pToJSVal XMLHttpRequestResponseTypeArraybuffer
          = js_XMLHttpRequestResponseTypeArraybuffer
        pToJSVal XMLHttpRequestResponseTypeBlob
          = js_XMLHttpRequestResponseTypeBlob
        pToJSVal XMLHttpRequestResponseTypeDocument
          = js_XMLHttpRequestResponseTypeDocument
        pToJSVal XMLHttpRequestResponseTypeJson
          = js_XMLHttpRequestResponseTypeJson
        pToJSVal XMLHttpRequestResponseTypeText
          = js_XMLHttpRequestResponseTypeText
-}
instance ToJSVal XMLHttpRequestResponseType where
        toJSVal XMLHttpRequestResponseType
          = return js_XMLHttpRequestResponseType
        toJSVal XMLHttpRequestResponseTypeArraybuffer
          = return js_XMLHttpRequestResponseTypeArraybuffer
        toJSVal XMLHttpRequestResponseTypeBlob
          = return js_XMLHttpRequestResponseTypeBlob
        toJSVal XMLHttpRequestResponseTypeDocument
          = return js_XMLHttpRequestResponseTypeDocument
        toJSVal XMLHttpRequestResponseTypeJson
          = return js_XMLHttpRequestResponseTypeJson
        toJSVal XMLHttpRequestResponseTypeText
          = return js_XMLHttpRequestResponseTypeText

{-
instance PFromJSVal XMLHttpRequestResponseType where
        pFromJSVal x
          | x == js_XMLHttpRequestResponseType = XMLHttpRequestResponseType
        pFromJSVal x
          | x == js_XMLHttpRequestResponseTypeArraybuffer =
            XMLHttpRequestResponseTypeArraybuffer
        pFromJSVal x
          | x == js_XMLHttpRequestResponseTypeBlob =
            XMLHttpRequestResponseTypeBlob
        pFromJSVal x
          | x == js_XMLHttpRequestResponseTypeDocument =
            XMLHttpRequestResponseTypeDocument
        pFromJSVal x
          | x == js_XMLHttpRequestResponseTypeJson =
            XMLHttpRequestResponseTypeJson
        pFromJSVal x
          | x == js_XMLHttpRequestResponseTypeText =
            XMLHttpRequestResponseTypeText
-}
instance FromJSVal XMLHttpRequestResponseType where
--        fromJSValUnchecked = return . pFromJSVal
        fromJSVal x
            | x == js_XMLHttpRequestResponseType =
                return (Just XMLHttpRequestResponseType)
            | x == js_XMLHttpRequestResponseTypeArraybuffer =
                return (Just XMLHttpRequestResponseTypeArraybuffer)
            | x == js_XMLHttpRequestResponseTypeBlob =
                return (Just XMLHttpRequestResponseTypeBlob)
            | x == js_XMLHttpRequestResponseTypeDocument =
                return (Just XMLHttpRequestResponseTypeDocument)
            | x == js_XMLHttpRequestResponseTypeJson =
                return (Just XMLHttpRequestResponseTypeJson)
            | x == js_XMLHttpRequestResponseTypeText =
                return (Just XMLHttpRequestResponseTypeText)
            | otherwise = error "instance FromJSVal XMLHttpRequestResponseType"

foreign import javascript unsafe "$1[\"response\"]" js_getResponse
        :: XMLHttpRequest
        -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.response Mozilla XMLHttpRequest.response documentation>
getResponse :: (MonadIO m) =>
               XMLHttpRequest
            -> m JSVal
getResponse self =
    liftIO (js_getResponse self)

foreign import javascript unsafe "$1[\"responseText\"]"
        js_getResponseText :: XMLHttpRequest -> IO JSString

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.responseText Mozilla XMLHttpRequest.responseText documentation>
getResponseText ::
                (MonadIO m) => XMLHttpRequest -> m Text
getResponseText self
  = liftIO
      (textFromJSString <$> js_getResponseText self)

foreign import javascript unsafe "$1[\"status\"]" js_getStatus ::
        XMLHttpRequest -> IO Word

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.status Mozilla XMLHttpRequest.status documentation>
getStatus :: (MonadIO m) => XMLHttpRequest -> m Word
getStatus self = liftIO (js_getStatus self)

foreign import javascript unsafe "$1[\"statusText\"]"
        js_getStatusText :: XMLHttpRequest -> IO JSString

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.statusText Mozilla XMLHttpRequest.statusText documentation>
getStatusText ::
              (MonadIO m) => XMLHttpRequest -> m Text
getStatusText self
  = liftIO
      (textFromJSString <$> js_getStatusText self)

foreign import javascript unsafe "$1[\"responseURL\"]"
        js_getResponseURL :: XMLHttpRequest -> IO JSString


-- * WebSocket

sendRemoteWS :: (ToJSON remote) => WebSocket -> remote -> IO ()
sendRemoteWS ws remote =
  do let jstr = JS.pack (C.unpack $ encode remote)
     debugStrLn $ "send WS: " ++ JS.unpack jstr
     WebSockets.send jstr ws
     debugStrLn $ "sent."

initRemoteWS :: (ToJSON remote) => JS.JSString -> (MessageEvent -> IO ()) -> IO (remote -> IO ())
initRemoteWS url' onMessageHandler =
    do let request = WebSocketRequest { url       = url'
                                      , protocols = []
                                      , onClose   = Nothing
                                      , onMessage = Just onMessageHandler
                                      }
       ws <- WebSockets.connect request
       pure (sendRemoteWS ws)

-- * DragEventObject

newtype DragEventObject = DragEventObject { unDragEventObject :: JSVal }

instance Show DragEventObject where
  show _ = "DragEventObject"

instance ToJSVal DragEventObject where
  toJSVal = return . unDragEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal DragEventObject where
  fromJSVal = return . fmap DragEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject DragEventObject where
  asEventObject (DragEventObject jsval) = EventObject jsval

-- * SelectionEventObject

newtype SelectionEventObject = SelectionEventObject { unSelectionEventObject :: JSVal }

instance Show SelectionEventObject where
  show _ = "SelectionEventObject"

instance ToJSVal SelectionEventObject where
  toJSVal = return . unSelectionEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal SelectionEventObject where
  fromJSVal = return . fmap SelectionEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject SelectionEventObject where
  asEventObject (SelectionEventObject jsval) = EventObject jsval

-- * Selection

newtype Selection = Selection { unSelection ::  JSVal }

instance ToJSVal Selection where
  toJSVal = pure . unSelection
  {-# INLINE toJSVal #-}

instance FromJSVal Selection where
  fromJSVal = pure . fmap Selection . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

-- ** Properties

foreign import javascript unsafe "$1[\"rangeCount\"]"
        js_getRangeCount :: Selection -> IO Int

getRangeCount :: (MonadIO m) => Selection -> m Int
getRangeCount selection = liftIO (js_getRangeCount selection)

rangeCount :: (MonadIO m) => Selection -> m Int
rangeCount = getRangeCount

-- ** methods

foreign import javascript unsafe "$1[\"addRange\"]($2)"
  js_addRange :: Selection -> Range -> IO ()

addRange :: (MonadIO m) => Selection -> Range -> m ()
addRange selection range = liftIO $ (js_addRange selection range)

foreign import javascript unsafe "$r = $1[\"anchorNode\"]"
  js_anchorNode :: Selection -> IO JSNode

anchorNode :: (MonadIO m) => Selection -> m JSNode
anchorNode s = liftIO (js_anchorNode s)

foreign import javascript unsafe "$r = $1[\"anchorOffset\"]"
  js_anchorOffset :: Selection -> IO Int

anchorOffset :: (MonadIO m) => Selection -> m Int
anchorOffset s = liftIO (js_anchorOffset s)

foreign import javascript unsafe "$1[\"collapse\"]($2, $3)"
  js_collapse :: Selection -> JSNode -> Int -> IO ()

collapse :: (MonadIO m) => Selection -> Maybe JSNode -> Maybe Int -> m ()
collapse sel mNode mOffset = liftIO $ js_collapse sel (fromMaybe (JSNode jsNull) mNode) (fromMaybe 0 mOffset)

foreign import javascript unsafe "$1[\"collapseToStart\"]()"
  js_collapseToStart :: Selection -> IO ()

collapseToStart :: (MonadIO m) => Selection -> m ()
collapseToStart s = liftIO (js_collapseToStart s)

foreign import javascript unsafe "$1[\"collapseToEnd\"]()"
  js_collapseToEnd :: Selection -> IO ()

collapseToEnd :: (MonadIO m) => Selection -> m ()
collapseToEnd s = liftIO (js_collapseToEnd s)

foreign import javascript unsafe "$1[\"deleteFromDocument\"]()"
  js_deleteFromDocument :: Selection -> IO ()

deleteFromDocument :: (MonadIO m) => Selection -> m ()
deleteFromDocument sel = liftIO $ js_deleteFromDocument sel

foreign import javascript unsafe "$r = $1[\"isCollapsed\"]"
  js_isCollapsed :: Selection -> IO Bool

isCollapsed :: (MonadIO m) => Selection -> m Bool
isCollapsed sel = liftIO $ js_isCollapsed sel

foreign import javascript unsafe "$1[\"getRangeAt\"]($2)"
        js_getRangeAt :: Selection -> Int -> IO Range

getRangeAt :: (MonadIO m) => Selection -> Int -> m Range
getRangeAt selection index = liftIO (js_getRangeAt selection index)

foreign import javascript unsafe "$1[\"removeAllRanges\"]()"
  js_removeAllRanges :: Selection -> IO ()

removeAllRanges :: (MonadIO m) => Selection -> m ()
removeAllRanges s = liftIO $ js_removeAllRanges s

foreign import javascript unsafe "$1[\"toString\"]()"
 selectionToString :: Selection -> IO JSString

foreign import javascript unsafe "$1[\"containsNode\"]($2,$3)"
 js_containsNode :: Selection -> JSNode -> Bool -> IO Bool

containsNode :: (MonadIO m) => Selection -> JSNode -> Bool -> m Bool
containsNode sel node partialContainment = liftIO (js_containsNode sel node partialContainment)

-- * Range

newtype Range = Range { unRange ::  JSVal }

instance ToJSVal Range where
  toJSVal = pure . unRange
  {-# INLINE toJSVal #-}

instance FromJSVal Range where
  fromJSVal = pure . fmap Range . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

foreign import javascript unsafe "new Range()"
  js_newRange :: IO Range

newRange :: (MonadIO m) => m Range
newRange = liftIO js_newRange

foreign import javascript unsafe "$1[\"cloneContents\"]()"
  js_cloneContents :: Range -> IO JSVal

cloneContents :: (MonadIO m) => Range -> m JSDocumentFragment
cloneContents r = liftIO $ pFromJSVal <$> js_cloneContents r

foreign import javascript unsafe "$r = $1[\"commonAncestorContainer\"]"
  js_commonAncestorContainer :: Range -> IO JSNode

commonAncestorContainer :: (MonadIO m) => Range -> m JSNode
commonAncestorContainer r = liftIO $ js_commonAncestorContainer r

foreign import javascript unsafe "$1[\"deleteContents\"]()"
  js_deleteContents :: Range -> IO ()

deleteContents :: (MonadIO m) => Range -> m ()
deleteContents r = liftIO $ js_deleteContents r

foreign import javascript unsafe "$1[\"startContainer\"]"
        js_startContainer :: Range -> IO JSNode
{-
foreign import javascript unsafe "$[\"createRange\"]()"
  js_createRange :: JSDocument -> IO Range

createRange :: JSDocument -> IO Range
createRange d = js_createRange d
-}
foreign import javascript unsafe "$1[\"selectNode\"]($2)"
  js_selectNode :: Range -> JSNode -> IO ()

selectNode :: (MonadIO m, IsJSNode node) => Range -> node -> m ()
selectNode r n = liftIO (js_selectNode r (toJSNode n))

foreign import javascript unsafe "$1[\"selectNodeContents\"]($2)"
  js_selectNodeContents :: Range -> JSNode -> IO ()

selectNodeContents :: (MonadIO m, IsJSNode node) => Range -> node -> m ()
selectNodeContents r n = liftIO (js_selectNodeContents r (toJSNode n))

foreign import javascript unsafe "$1[\"insertNode\"]($2)"
  js_insertNode :: Range -> JSNode -> IO ()

insertNode :: (MonadIO m, IsJSNode node) => Range -> node -> m ()
insertNode r n = liftIO (js_insertNode r (toJSNode n))

foreign import javascript unsafe "$1[\"toString\"]()"
  js_rangeToString :: Range -> IO JSString

rangeToJSString :: Range -> IO JSString
rangeToJSString r = liftIO (js_rangeToString r)

startContainer :: (MonadIO m) => Range -> m JSNode
startContainer r = liftIO (js_startContainer r)

foreign import javascript unsafe "$1[\"startOffset\"]"
        js_startOffset :: Range -> IO Int

startOffset :: (MonadIO m) => Range -> m Int
startOffset r = liftIO (js_startOffset r)

foreign import javascript unsafe "$1[\"endContainer\"]"
        js_endContainer :: Range -> IO JSNode

endContainer :: (MonadIO m) => Range -> m JSNode
endContainer r = liftIO (js_endContainer r)

foreign import javascript unsafe "$1[\"endOffset\"]"
        js_endOffset :: Range -> IO Int

endOffset :: (MonadIO m) => Range -> m Int
endOffset r = liftIO (js_endOffset r)

foreign import javascript unsafe "$1[\"setStart\"]($2,$3)"
  js_setStart :: Range -> JSNode -> Int -> IO ()

setStart :: (MonadIO m, IsJSNode node) => Range -> node -> Int -> m ()
setStart r n i = liftIO $ js_setStart r (toJSNode n) i

foreign import javascript unsafe "$1[\"setStartBefore\"]($2)"
  js_setStartBefore :: Range -> JSNode -> IO ()

setStartBefore :: (MonadIO m, IsJSNode node) => Range -> node -> m ()
setStartBefore r n = liftIO $ js_setStartBefore r (toJSNode n)

foreign import javascript unsafe "$1[\"setEnd\"]($2,$3)"
  js_setEnd :: Range -> JSNode -> Int -> IO ()

setEnd :: (MonadIO m, IsJSNode node) => Range -> node -> Int -> m ()
setEnd r n i = liftIO $ js_setEnd r (toJSNode n) i

foreign import javascript unsafe "$1[\"setEndBefore\"]($2)"
  js_setEndBefore :: Range -> JSNode -> IO ()

setEndBefore :: (MonadIO m, IsJSNode node) => Range -> node -> m ()
setEndBefore r n = liftIO $ js_setEndBefore r (toJSNode n)

newtype ClientRects = ClientRects { unClientRects :: JSVal }

instance ToJSVal ClientRects where
  toJSVal = return . unClientRects
  {-# INLINE toJSVal #-}

instance FromJSVal ClientRects where
  fromJSVal = return . fmap ClientRects . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

foreign import javascript unsafe "$1[\"getClientRects\"]"
  js_getClientRects :: Range -> IO ClientRects

getClientRects :: (MonadIO m) => Range -> m ClientRects
getClientRects r = liftIO $ js_getClientRects r

newtype ClientRect = ClientRect { unClientRect :: JSVal }

instance ToJSVal ClientRect where
  toJSVal = return . unClientRect
  {-# INLINE toJSVal #-}

instance FromJSVal ClientRect where
  fromJSVal = return . fmap ClientRect . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

foreign import javascript unsafe "$1[$2]"
 js_clientRectIx :: ClientRects -> Int -> IO ClientRect

clientRectIx :: (MonadIO m) => ClientRects -> Int -> m ClientRect
clientRectIx crs i = liftIO $ js_clientRectIx crs i

foreign import javascript unsafe "$1[\"length\"]"
  clientRectsLength :: ClientRects -> Int

foreign import javascript unsafe "$1[\"left\"]"
  crLeft :: ClientRect -> Int

-- crLeft :: (MonadIO m) => ClientRect -> m Int
-- crLeft cr = liftIO $ js_crLeft cr

foreign import javascript unsafe "$1[\"top\"]"
  crTop :: ClientRect -> Int
{-
crTop :: (MonadIO m) => ClientRect -> m Int
crTop cr = liftIO $ js_crTop cr
-}

-- Note: probably returns ClientRectList -- similar to NodeList
-- foreign import javascript unsfae "$1[\"getClientRects\"]()"
--  js_getClientRects :: Range -> IO JSVal

-- * Pure HTML

data Attr model where
  Attr :: Text -> Text -> Attr model
  Prop :: Text -> Text -> Attr model
  OnCreate :: (JSElement -> TDVar model -> IO ()) -> Attr model
  EL :: (Show event, IsEvent event, FromJSVal (EventObjectOf event)) => event -> (EventObjectOf event -> TDVar model -> IO ()) -> Attr model

instance Show (Attr model) where
  show (Attr a v) = Text.unpack a <> " := " <> Text.unpack v
  show (Prop a v) = "." <> Text.unpack a <> " = " <> Text.unpack v
  show (OnCreate _ ) = "onCreate"
  show (EL e _) = "on" ++ show e

data Html model where
  Element :: Text -> [Attr model] -> [Html model] -> Html model
  CData   :: Text -> Html model
  Cntl    :: (Show event, IsEvent event, FromJSVal (EventObjectOf event)) =>
              Control event -> event -> (EventObjectOf event -> TDVar model -> IO ()) -> Html model

instance Show (Html model) where
  show (Element n attrs elems) = "Element " <> Text.unpack n <> " " <> show attrs <> " " <> show elems
  show (CData t) = "CData " <> Text.unpack t
  show (Cntl _ e _) = "Cntl " ++ show e

data Control event = forall model remote. (Show model) => Control
  { cmodel  :: model
  , cinit   :: (() -> IO ()) -> TDVar model -> IO ()
  , cview   :: (() -> IO ()) -> model -> Html model
  }

descendants :: [Html model] -> Int
descendants elems = sum [ descendants children | Element _n _attrs children <- elems] + (length elems)

-- I believe that if we try to `appendChild` several `CData` nodes
-- that the browser will consolidate them into a single node. That
-- throws off our mapping between the VDOM and the DOM. So, we need to
-- do the same
flattenCData :: [Html model] -> [Html model]
flattenCData (CData a : CData b : rest) = flattenCData (CData (a <> b) : rest)
flattenCData (h : t) = h : flattenCData t
flattenCData [] = []

type WithModel model = (model -> IO (Maybe model)) -> IO ()

type Loop = forall model remote. (Show model, ToJSON remote) =>
            JSDocument -> JSNode -> model -> ((remote -> IO ()) -> TDVar model -> IO ()) ->
            Maybe JS.JSString -> ((remote -> IO ()) -> MessageEvent -> TDVar model -> IO ()) -> ((remote -> IO ()) -> model -> Html model) -> IO (TDVar model)

foreign import javascript unsafe "window[\"setTimeout\"]($1, $2)" js_setTimeout ::
  Callback (IO ()) -> Int -> IO ()

{-
renderHtml loop withModel doc (Cntl (Control cmodel cinit cview) eventType eventHandler) =
  do (Just cBody) <- fmap toJSNode <$> createJSElement doc (Text.pack "span")
     tid <- liftIO $ forkIO $ loop doc cBody cmodel cinit cview
     addEventListener cBody eventType (\e -> withModel (eventHandler e)) False
     pure (Just cBody)
-}
{-
renderHtml loop withModel doc (Cntl (Control cmodel cview) eventType eventHandler) =
  do (Just cBody) <- fmap toJSNode <$> createJSElement doc (Text.pack "div")
     (Just html) <- renderHtml loop (\f -> f cmodel >> pure ()) doc (cview cmodel)
     appendChild cBody (Just html)
     addEventListener cBody eventType (\e -> withModel (eventHandler e)) False
     pure (Just cBody)
-}

{-
data Attr action where
  Attr  :: Text -> Text -> Attr action
  Event :: (FromJSVal (EventObjectOf event), IsEvent event) => event -> (EventObjectOf event -> IO action) -> Attr action

-- | FIXME: this instances is not really right, but was added for the sake of the test suite
instance Eq (Attr action) where
  (Attr k1 v1) == (Attr k2 v2) = (k1 == k2) && (v1 == v2)
  _ == _ = False

-- This issue with this is that we need to store state across calls to 'view'
data Widget action = forall model widgetAction. Widget
  { initWidget   :: model
  , widget :: model -> widgetAction -> (HTML action, model)
  }


data HTML action
  = Element { elementName        :: Text
            , elementAttrs       :: [Attr action]
            , elementKey         :: Maybe Text
            , elementDescendants :: Int
            , elementChildren    :: [HTML action]
            }
  | CDATA Bool Text
--   | W (Widget action)
--    deriving Eq

--   | Children [HTML action]

instance Show (Attr action) where
    show (Attr k v) = (Text.unpack k) <> " := " <> (Text.unpack v) <> " "
    show (Event _eventType _) = "Event " -- ++ show eventType ++ " <function>"

instance Show (HTML action) where
    show (Element tagName attrs _key _count children) =
        (Text.unpack tagName) <> " [" <> concat (map show attrs) <> "]\n" <> concat (map showChild children)
        where
          showChild c = "    " <> show c <> "\n"
    show (CDATA b txt) = Text.unpack txt

descendants :: [HTML action] -> Int
descendants elems = sum [ d | Element _ _ _ d _ <- elems] + (length elems)

renderHTML :: forall action m. (MonadIO m) => (action -> IO ()) -> JSDocument -> HTML action -> m (Maybe JSNode)
renderHTML _ doc (CDATA _ t) = fmap (fmap toJSNode) $ createJSTextNode doc t
renderHTML handle doc (Element tag {- events -} attrs _ _ children) =
    do me <- createJSElement doc tag
       case me of
         Nothing -> return Nothing
         (Just e) ->
             do mapM_ (\c -> appendChild e =<< renderHTML handle doc c) children
                mapM_ (doAttr e) attrs
                {-
                let events' = [ ev | ev@(Event ev f) <- attrs]
                    attrs'  = [ (k,v) | Attr k v <- attrs]
                liftIO $ mapM_ (\(k, v) -> setAttribute e k v) attrs'
                liftIO $ mapM_ (handleEvent e) events'
                -}
                return (Just $ toJSNode e)
    where
      doAttr elem (Attr k v)   = setAttribute elem k v
      doAttr elem (Event eventType toAction) =
           addEventListener elem eventType (\e -> handle =<< (toAction e)) False
{-
      handle' :: JSElement -> (Maybe JSString -> action) -> IO ()
      handle' elem toAction =
          do ms <- getValue elem
             handle (toAction ms)
-}
--       handleEvent :: JSElement -> Attr (event, EventObjectOf event -> action) -> IO ()
{-
      handleEvent elem (Event eventType toAction) =
        addEventListener elem eventType (\e -> handle =<< toAction e) False
-}
{-
          do cb <- asyncCallback (handle' elem toAction) -- FIXME: free ?
             addEventListener elem eventType cb False
-}
-}
-- * DataTransfer

newtype DataTransfer = DataTransfer { unDataTransfer :: JSVal }

instance Show DataTransfer where
  show _ = "DataTransfer"

instance ToJSVal DataTransfer where
  toJSVal = pure . unDataTransfer
  {-# INLINE toJSVal #-}

instance FromJSVal DataTransfer where
  fromJSVal = pure . fmap DataTransfer . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

foreign import javascript unsafe "$1[\"getData\"]($2)" js_getDataTransferData ::
        DataTransfer -> JSString -> IO JSString

getDataTransferData :: -- (MonadIO m) =>
           DataTransfer
        -> JSString -- ^ format
        -> IO JSString
getDataTransferData dt format = (js_getDataTransferData dt format)

foreign import javascript unsafe "$1[\"setData\"]($2, $3)" js_setDataTransferData ::
        DataTransfer -> JSString -> JSString -> IO ()

setDataTransferData :: DataTransfer
                    -> JSString -- ^ format
                    -> JSString -- ^ data
                    -> IO ()
setDataTransferData dataTransfer format data_ = (js_setDataTransferData dataTransfer format data_)

-- * DataTransferItem

newtype DataTransferItem = DataTransferItem { unDataTransferItem :: JSVal }

instance Show DataTransferItem where
  show _ = "DataTransferItem"

instance ToJSVal DataTransferItem where
  toJSVal = pure . unDataTransferItem
  {-# INLINE toJSVal #-}

instance FromJSVal DataTransferItem where
  fromJSVal = pure . fmap DataTransferItem . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

foreign import javascript unsafe "$1[\"kind\"]()" js_dataTransferItemKind ::
        DataTransferItem -> JSString

dataTransferItemKind :: DataTransferItem -> Text
dataTransferItemKind dti = textFromJSString $ js_dataTransferItemKind dti

foreign import javascript unsafe "$1[\"type\"]()" js_dataTransferItemType ::
        DataTransferItem -> JSString

dataTransferItemType :: DataTransferItem -> Text
dataTransferItemType dti = textFromJSString $ js_dataTransferItemType dti

-- * Clipboard

data ClipboardEvent
  = Copy
  | Cut
  | Paste
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance IsEvent ClipboardEvent where
  eventToJSString Copy  = JS.pack "copy"
  eventToJSString Cut   = JS.pack "cut"
  eventToJSString Paste = JS.pack "paste"

-- * ClipboardEventObject

newtype ClipboardEventObject = ClipboardEventObject { unClipboardEventObject :: JSVal }

instance Show ClipboardEventObject where
  show _ = "ClipboardEventObject"

instance ToJSVal ClipboardEventObject where
  toJSVal = pure . unClipboardEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal ClipboardEventObject where
  fromJSVal = pure . fmap ClipboardEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject ClipboardEventObject where
  asEventObject (ClipboardEventObject jsval) = EventObject jsval

foreign import javascript unsafe "$1[\"clipboardData\"]" clipboardData ::
        ClipboardEventObject -> IO DataTransfer

-- * Event

foreign import javascript unsafe "new Event($1, { 'bubbles' : $2, 'cancelable' : $3})"
        js_newEvent :: JSString -> Bool -> Bool -> IO JSVal

class (IsEvent ev) => MkEvent ev where
  mkEvent :: ev -> JSVal -> EventObjectOf ev

newEvent :: forall ev. (IsEvent ev, MkEvent ev) => ev -> Bool -> Bool -> IO (EventObjectOf ev)
newEvent ev bubbles cancelable =
  do let evStr = eventToJSString ev
     jsval <- js_newEvent evStr bubbles cancelable
     pure $ mkEvent ev jsval


-- * VDOM Events

data VDOMEvent
     = Redrawn
       deriving (Eq, Show)

instance IsEvent VDOMEvent where
  eventToJSString Redrawn = fromString "redrawn"

newtype VDOMEventObject = VDOMEventObject { unVDOMEventObject :: JSVal }

instance MkEvent VDOMEvent where
  mkEvent _ jsval = VDOMEventObject jsval

instance Show VDOMEventObject where
  show _ = "VDOMEventObject"

instance ToJSVal VDOMEventObject where
  toJSVal = return . unVDOMEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal VDOMEventObject where
  fromJSVal = return . fmap VDOMEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject VDOMEventObject where
  asEventObject (VDOMEventObject jsval) = EventObject jsval

type instance EventObjectOf VDOMEvent   = VDOMEventObject

-- * JSDom

newtype JSDOM = JSDOM JSVal

unJSDOM (JSDOM o) = o

instance ToJSVal JSDOM where
  toJSVal = pure . unJSDOM
  {-# INLINE toJSVal #-}

instance FromJSVal JSDOM where
  fromJSVal = pure . fmap JSDOM . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}


foreign import javascript unsafe "require('jsdom')"
   js_requireJSDOM :: IO JSVal

requireJSDOM :: (MonadIO m) => m (Maybe JSDOM)
requireJSDOM = liftIO $ fromJSVal =<< js_requireJSDOM

foreign import javascript unsafe "new $1.JSDOM($2).window"
   js_newJSDOM :: JSDOM -> JSString -> IO JSVal

newJSDOM :: (MonadIO m) => JSDOM -> JSString -> m (Maybe JSWindow)
newJSDOM jsdom html = liftIO $ fromJSVal =<< js_newJSDOM jsdom html

-- * MediaElement

class (PToJSVal o) => IsSrcObject o

newtype MediaStream = MediaStream { unMediaStream :: JSVal }

instance PToJSVal MediaStream where
  pToJSVal (MediaStream jsval) = jsval

instance IsSrcObject MediaStream

newtype MediaElement = MediaElement { unMediaElement :: JSVal }


-- | FIXME: this should somehow confirm that the element has the HTMLMediaElement interface
asMediaElement :: JSElement -> Maybe MediaElement
asMediaElement (JSElement v) = Just (MediaElement v)

foreign import javascript unsafe "$1[\"srcObject\"] = $2"
   js_setSrcObject :: MediaElement -> JSVal -> IO ()

setSrcObject :: (MonadIO m, IsSrcObject o) => MediaElement -> o -> m ()
setSrcObject me o = liftIO $ js_setSrcObject me (pToJSVal o)

-- * DOMTokenList

newtype DOMTokenList = DOMTokenList { unDOMTokenList :: JSVal }

instance ToJSVal DOMTokenList where
  toJSVal = return . unDOMTokenList
  {-# INLINE toJSVal #-}

instance FromJSVal DOMTokenList where
  fromJSVal = return . fmap DOMTokenList . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance PFromJSVal DOMTokenList where
  pFromJSVal = DOMTokenList
  {-# INLINE pFromJSVal #-}

foreign import javascript unsafe "$1[\"add\"]($2)"
        js_addToken1 :: DOMTokenList -> JSString -> IO ()

addToken1 :: (MonadIO m) => DOMTokenList -> JSString -> m ()
addToken1 dtl t = liftIO $ js_addToken1 dtl t

foreign import javascript unsafe "$1[\"remove\"]($2)"
        js_removeToken1 :: DOMTokenList -> JSString -> IO ()

removeToken1 :: (MonadIO m) => DOMTokenList -> JSString -> m ()
removeToken1 dtl t = liftIO $ js_removeToken1 dtl t

foreign import javascript unsafe "$1[\"replace\"]($2)"
        js_replaceToken :: DOMTokenList -> JSString -> JSString -> IO Bool

replaceToken :: (MonadIO m) => DOMTokenList -> JSString -> JSString -> m Bool
replaceToken dtl old new = liftIO $ js_replaceToken dtl old new

foreign import javascript unsafe "$r = $1[\"classList\"]"
        js_classList :: JSElement -> IO DOMTokenList

classList :: (MonadIO m) => JSElement -> m DOMTokenList
classList e = liftIO $ js_classList e

