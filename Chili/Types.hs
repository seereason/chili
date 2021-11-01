{-# LANGUAGE ConstrainedClassMethods, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GADTs, JavaScriptFFI, ScopedTypeVariables, TypeFamilies #-}
{-# language GeneralizedNewtypeDeriving, TypeApplications, AllowAmbiguousTypes, OverloadedStrings #-}
{-# language RankNTypes, DataKinds, KindSignatures, PolyKinds, TypeFamilyDependencies #-}
{-# language PatternSynonyms, UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
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
import Data.Char as Char (toLower)
import Data.Maybe (fromJust, fromMaybe, catMaybes)
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
import qualified JavaScript.Web.MessageEvent (MessageEvent(..), MessageEventData(..))
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
{-
To use this, we'd need to figure out how to call append with the spread operator,

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax

appendNodeList :: (IsParentNode parent, MonadIO m) => parent -> JSNodeList -> m ()
appendNodeList parent nl = liftIO $
  do nlv <- toJSVal nl
     js_append (toJSNode parent) nlv
-}

{-

This would work if we created an HTMLCollection datatype. But perhaps you want childNodes anyway?

foreign import javascript unsafe "$1[\"children\"]"
  js_children :: JSNode -> IO HTMLCollection

children :: (IsParentNode parent, MonadIO m) => parent -> m HTMLCollection
children parent = liftIO $
  do js_children (toJSNode parent)
-}
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

foreign import javascript unsafe "new EventTarget()"
        js_newEventTarget :: IO JSVal

newEventTarget :: (MonadIO m) => m EventTarget
newEventTarget = liftIO $ EventTarget <$> js_newEventTarget

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

-- * contenteditable

foreign import javascript unsafe "$1[\"contentEditable\"] = $2"
  js_setContentEditable :: JSElement -> Bool -> IO ()

setContentEditable :: (MonadIO m) => JSElement -> Bool -> m ()
setContentEditable e b = liftIO $ js_setContentEditable e b

foreign import javascript unsafe "$r = $1[\"contentEditable\"]"
  js_getContentEditable :: JSElement -> IO Bool

getContentEditable :: (MonadIO m) => JSElement -> m Bool
getContentEditable e = liftIO $ js_getContentEditable e

foreign import javascript unsafe "$1[\"contains\"]($2)"
   js_contains :: JSNode -> JSNode -> IO Bool

contains :: (IsJSNode node, IsJSNode otherNode, MonadIO m) => node -> otherNode -> m Bool
contains node otherNode = liftIO $ js_contains (toJSNode node) (toJSNode otherNode)

-- * cloneNode

foreign import javascript unsafe "$1[\"cloneNode\"]($2)"
  js_cloneNode :: JSNode -> Bool -> IO JSNode

cloneNode :: (MonadIO m, IsJSNode self) => self -> Bool -> m JSNode
cloneNode self deep =
  liftIO (js_cloneNode (toJSNode self) deep)

-- * parentNode

foreign import javascript unsafe "$1[\"parentNode\"]"
        js_parentNode :: JSNode -> IO JSVal

parentNode :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
parentNode self =
    liftIO (fromJSVal =<< js_parentNode (toJSNode self))

-- * parentElement

foreign import javascript unsafe "$1[\"parentElement\"]"
        js_parentElement :: JSNode -> IO JSVal

parentElement :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSElement)
parentElement self =
    liftIO (fromJSVal =<< js_parentElement (toJSNode self))

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
        js_firstElementChild :: JSVal -> IO JSVal

firstElementChild :: (MonadIO m, ToJSVal parent, IsParentNode parent) => parent -> m (Maybe JSElement)
firstElementChild p
  = liftIO (fromJSVal =<< js_firstElementChild =<< toJSVal p)

foreign import javascript unsafe "$1[\"lastElementChild\"]"
        js_lastElementChild :: JSVal -> IO JSVal

lastElementChild :: (MonadIO m, ToJSVal parent, IsParentNode parent) => parent -> m (Maybe JSElement)
lastElementChild p
  = liftIO (fromJSVal =<< js_lastElementChild =<< toJSVal p)


-- * JSDocument

newtype JSDocument = JSDocument JSVal

unJSDocument (JSDocument o) = o

class DocumentOrShadowRoot a
instance DocumentOrShadowRoot JSDocument

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

foreign import javascript unsafe "$r = $1[\"scrollX\"]"
  js_scrollX :: JSWindow -> IO Double

scrollX :: (MonadIO m) => JSWindow -> m Double
scrollX w = liftIO (js_scrollX w)

foreign import javascript unsafe "$r = $1[\"scrollY\"]"
  js_scrollY :: JSWindow -> IO Double

scrollY :: (MonadIO m) => JSWindow -> m Double
scrollY w = liftIO (js_scrollY w)

foreign import javascript unsafe "$r = $1[\"innerHeight\"]"
  js_innerHeight :: JSWindow -> IO Double

innerHeight :: (MonadIO m) => JSWindow -> m Double
innerHeight w = liftIO (js_innerHeight w)

foreign import javascript unsafe "$r = $1[\"innerWidth\"]"
  js_innerWidth :: JSWindow -> IO Double

innerWidth :: (MonadIO m) => JSWindow -> m Double
innerWidth w = liftIO (js_innerWidth w)

-------------------------
-- * JSElement
-------------------------

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
-- FIXME: can this actually return Nothing?
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

class (IsJSNode obj) => DocumentOrElement obj
instance DocumentOrElement JSDocument
instance DocumentOrElement JSElement

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
        JSNode -> JSString -> IO JSNodeList

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document.getElementsByTagName Mozilla Document.getElementsByTagName documentation>
getElementsByTagName :: (DocumentOrElement obj, MonadIO m) =>
                        obj
                     -> JSString
                     -> m (Maybe JSNodeList)
getElementsByTagName self tagname
  = liftIO ((js_getElementsByTagName (toJSNode self) tagname) >>= return . Just)

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

-- * nextElementSibling

foreign import javascript unsafe "$1[\"nextElementSibling\"]"
        js_nextElementSibling :: JSNode -> IO JSVal

-- | <https://developer.mozilla.org/en-us/docs/Web/API/NonDocumentTypeChildNode/nextElementSibling Mozilla nextElementSibling documentation>
nextElementSibling :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSElement)
nextElementSibling self
  = liftIO ((js_nextElementSibling ((toJSNode self))) >>= fromJSVal)


foreign import javascript unsafe "$1[\"previousSibling\"]"
        js_previousSibling :: JSNode -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.previousSibling Mozilla Node.previousSibling documentation>
previousSibling :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
previousSibling self
  = liftIO ((js_previousSibling ((toJSNode self))) >>= fromJSVal)

-- * previousElementSibling

foreign import javascript unsafe "$1[\"previousElementSibling\"]"
        js_previousElementSibling :: JSNode -> IO JSVal

-- | <https://developer.mozilla.org/en-us/docs/Web/API/NonDocumentTypeChildNode/previousElementSibling Mozilla previousElementSibling documentation>
previousElementSibling :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSElement)
previousElementSibling self
  = liftIO ((js_previousElementSibling ((toJSNode self))) >>= fromJSVal)


foreign import javascript unsafe "$1[\"setAttribute\"]($2, $3)"
        js_setAttribute :: JSElement -> JSString -> JSString -> IO ()

foreign import javascript safe "$1[\"setAttribute\"]($2, $3)"
        js_setAttributeSafe :: JSElement -> JSString -> JSString -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.setAttribute Mozilla Element.setAttribute documentation>
setAttribute ::
             (MonadIO m) =>
               JSElement -> Text -> Text -> m ()
setAttribute self name value
  = liftIO
      (js_setAttribute self (textToJSString name) (textToJSString value))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.setAttribute Mozilla Element.setAttribute documentation>
setAttributeSafe ::
             (MonadIO m) =>
               JSElement -> Text -> Text -> m ()
setAttributeSafe self name value
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

-- * ShadowRoot

newtype JSShadowRoot = JSShadowRoot { unJSShadowRoot :: JSVal }

instance DocumentOrShadowRoot JSShadowRoot

instance ToJSVal JSShadowRoot where
  toJSVal = return . unJSShadowRoot
  {-# INLINE toJSVal #-}

instance FromJSVal JSShadowRoot where
  fromJSVal = return . fmap JSShadowRoot . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance PFromJSVal JSShadowRoot where
  pFromJSVal = JSShadowRoot
  {-# INLINE pFromJSVal #-}

instance IsJSNode JSShadowRoot where
   toJSNode = JSNode . unJSShadowRoot


data ShadowRootMode
  = OpenRoot
  | ClosedRoot
    deriving (Eq, Ord, Read, Show, Enum)

foreign import javascript unsafe "$1[\"attachShadow\"]({mode: $2, delegatesFocus: $3})"
        js_attachShadow :: JSElement -> JSString -> Bool -> IO JSShadowRoot

attachShadow :: (MonadIO m) =>
                JSElement
             -> ShadowRootMode
             -> Bool  -- ^ delegate focus
             -> m JSShadowRoot
attachShadow root mode delegateFocus = liftIO $ js_attachShadow root modeStr delegateFocus
  where
    modeStr = case mode of
      OpenRoot   -> "open"
      ClosedRoot -> "closed"

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

-- * TextNode length

foreign import javascript unsafe "$1[\"length\"]"
        js_textNodeLength :: JSTextNode -> IO Int

textNodeLength :: (MonadIO m) => JSTextNode -> m Int
textNodeLength tn = liftIO (js_textNodeLength tn)

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

-- All EventObjects listed here https://developer.mozilla.org/en-US/docs/Web/API/Event
--                              https://developer.mozilla.org/en-US/docs/Web/Events

instance IsEventTarget JSElement where
    toEventTarget = EventTarget . unJSElement

-- | this type family maps types to unique symbols. Th compiler
-- enforces that RHS *must* uniquely identify the LHS.
type family UniqEventName (e :: k) = (s :: Symbol) | s -> e

-- | This is just 'Proxy' by a different name
data EventName (ev :: k) = EventName

-- | Return the event name (aka, event.type) as a 'String'.
--
-- The 'String' is determined by using 'UniqEventName' to convert `ev`
-- to a 'Symbol' and then converting the 'Symbol' to a 'String'.
eventName :: forall ev. (KnownSymbol (UniqEventName ev)) => EventName ev -> String
eventName _ = symbolVal (Proxy :: Proxy (UniqEventName ev))

-- | helper function so you can use TypeApplication with addEventListener
--
-- Instead of this:
--
--     addEventLister (EventName :: EventName Click) clickHandler True
--
-- you can write this:
--
--     addEventLister (ev @Click) clickHandler True
--
ev :: forall ev. (KnownSymbol (UniqEventName ev)) => EventName ev
ev = EventName

class IsEvent ev where
  eventToJSString :: ev -> JSString

data Event
  = LanguageChange
  | Open
  | ReadyStateChange
  deriving (Eq, Show, Read)

type instance UniqEventName LanguageChange   = "languagechange"
type instance UniqEventName Open             = "open"
type instance UniqEventName ReadyStateChange = "readystatechange"


data MouseEvent
  = AuxClick
  | Click
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

type instance UniqEventName AuxClick    = "auxclick"
type instance UniqEventName Click       = "click"
type instance UniqEventName ContextMenu = "contextmenu"
type instance UniqEventName DblClick    = "dblclick"
type instance UniqEventName MouseDown   = "mousedown"
type instance UniqEventName MouseEnter  = "mouseenter"
type instance UniqEventName MouseLeave  = "mouseleave"
type instance UniqEventName MouseMove   = "mousemove"
type instance UniqEventName MouseOver   = "mouseover"
type instance UniqEventName MouseOut    = "mouseout"
type instance UniqEventName MouseUp     = "mouseup"

instance IsEvent MouseEvent where
  eventToJSString AuxClick    = JS.pack "auxclick"
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

-- * HashChangeEvent

data HashChangeEvent
  = HashChange
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName HashChange      = "hashchange"

-- * HashChangeEventObject

newtype HashChangeEventObject (ev :: HashChangeEvent) = HashChangeEventObject { unHashChangeEventObject :: JSVal }

instance Show (HashChangeEventObject ev) where
  show _ = "HashChangeEventObject"

instance ToJSVal (HashChangeEventObject ev) where
  toJSVal = return . unHashChangeEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (HashChangeEventObject ev) where
  fromJSVal = return . fmap HashChangeEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (HashChangeEventObject ev) where
  type Ev (HashChangeEventObject ev) = ev
  asEventObject (HashChangeEventObject jsval) = EventObject jsval


-- * PrintingEvent

data PrintingEvent
  = AfterPrint
  | BeforePrint
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName AfterPrint         = "afterprint"
type instance UniqEventName BeforePrint        = "beforeprint"


-- | https://developer.mozilla.org/en-US/docs/Web/API/PromiseRejectionEvent

data PromiseRejectionEvent
  = RejectionHandled
  | UnhandledRejection
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName RejectionHandled   = "rejectionhandled"
type instance UniqEventName UnhandledRejection = "unhandledrejection"

-- * PromiseRejectionEventObject

newtype PromiseRejectionEventObject (ev :: PromiseRejectionEvent) = PromiseRejectionEventObject { unPromiseRejectionEventObject :: JSVal }

instance Show (PromiseRejectionEventObject ev) where
  show _ = "PromiseRejectionEventObject"

instance ToJSVal (PromiseRejectionEventObject ev) where
  toJSVal = return . unPromiseRejectionEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (PromiseRejectionEventObject ev) where
  fromJSVal = return . fmap PromiseRejectionEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (PromiseRejectionEventObject ev) where
  type Ev (PromiseRejectionEventObject ev) = ev
  asEventObject (PromiseRejectionEventObject jsval) = EventObject jsval


-- * ResourceEvent

data ResourceEvent
  = Error
  | Abort
  | Load
  | BeforeUnload
  | Unload
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName Error        = "error"
type instance UniqEventName Abort        = "abort"
type instance UniqEventName Load         = "load"
type instance UniqEventName BeforeUnload = "beforeunload"
type instance UniqEventName Unload       = "unload"

-- * MessageEvent

data MessageEvent
  = Message
  | MessageError
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName Message      = "message"
type instance UniqEventName MessageError = "messageerror"

-- * MessageEventObject

newtype MessageEventObject (ev :: MessageEvent) = MessageEventObject { unMessageEventObject :: JSVal }

instance Show (MessageEventObject ev) where
  show _ = "MessageEventObject"

instance ToJSVal (MessageEventObject ev) where
  toJSVal = return . unMessageEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (MessageEventObject ev) where
  fromJSVal = return . fmap MessageEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (MessageEventObject ev) where
  type Ev (MessageEventObject ev) = ev
  asEventObject (MessageEventObject jsval) = EventObject jsval

-- * NetworkEvent

data NetworkEvent
  = Online
  | Offline
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName Online  = "online"
type instance UniqEventName Offline = "offline"

-- * ViewEvent

data ViewEvent
  = FullScreenChange
  | FullScreenError
  | Resize
  | Scroll
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName FullScreenChange = "fullscreenchange"
type instance UniqEventName FullScreenError  = "fullscreenerror"
type instance UniqEventName Resize           = "resize"
type instance UniqEventName Scroll           = "scroll"

-- * PageTransitionEvent

data PageTransitionEvent
  = PageShow
  | PageHide
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName PageShow     = "pageshow"
type instance UniqEventName PageHide     = "pagehide"

-- * PageTransitionEventObject

newtype PageTransitionEventObject (ev :: PageTransitionEvent) = PageTransitionEventObject { unPageTransitionEventObject :: JSVal }

instance Show (PageTransitionEventObject ev) where
  show _ = "PageTransitionEventObject"

instance ToJSVal (PageTransitionEventObject ev) where
  toJSVal = return . unPageTransitionEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (PageTransitionEventObject ev) where
  fromJSVal = return . fmap PageTransitionEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (PageTransitionEventObject ev) where
  type Ev (PageTransitionEventObject ev) = ev
  asEventObject (PageTransitionEventObject jsval) = EventObject jsval

-- * PopStateEvent

data PopStateEvent
  = PopState
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName PopState     = "popstate"

-- * PopStateEventObject

newtype PopStateEventObject (ev :: PopStateEvent) = PopStateEventObject { unPopStateEventObject :: JSVal }

instance Show (PopStateEventObject ev) where
  show _ = "PopStateEventObject"

instance ToJSVal (PopStateEventObject ev) where
  toJSVal = return . unPopStateEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (PopStateEventObject ev) where
  fromJSVal = return . fmap PopStateEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (PopStateEventObject ev) where
  type Ev (PopStateEventObject ev) = ev
  asEventObject (PopStateEventObject jsval) = EventObject jsval

-- * InputEvent

data InputEvent
  = Input
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance IsEvent InputEvent where
  eventToJSString Input    = JS.pack "input"

type instance UniqEventName Input   = "input"

-- * InputEventObject

newtype InputEventObject (ev :: InputEvent) = InputEventObject { unInputEventObject :: JSVal }

instance Show (InputEventObject ev) where
  show _ = "InputEventObject"

instance ToJSVal (InputEventObject ev) where
  toJSVal = return . unInputEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (InputEventObject ev) where
  fromJSVal = return . fmap InputEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (InputEventObject ev) where
  type Ev (InputEventObject ev) = ev
  asEventObject (InputEventObject jsval) = EventObject jsval

foreign import javascript unsafe "$r = $1[\"data\"]" js_inputData ::
        InputEventObject ev -> Nullable JSString

inputData :: InputEventObject ev -> Maybe JSString
inputData ev = nullableToMaybe (js_inputData ev)

foreign import javascript unsafe "$r = $1[\"dataTransfer\"]" js_dataTransfer ::
        InputEventObject ev -> Nullable DataTransfer

dataTransfer :: InputEventObject ev -> Maybe DataTransfer
dataTransfer ev = nullableToMaybe (js_dataTransfer ev)

foreign import javascript unsafe "$r = $1[\"inputType\"]" inputType ::
        InputEventObject ev -> JSString

foreign import javascript unsafe "$r = $1[\"isComposing\"]" isComposing ::
        InputEventObject ev -> Bool

-- * FormEvent

data FormEvent
  = Change
  | Invalid
  | Reset
--  | Search
--  | Select
  | Submit
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance IsEvent FormEvent where
  eventToJSString Change   = JS.pack "change"
--  eventToJSString Input    = JS.pack "input"
  eventToJSString Invalid  = JS.pack "invalid"
  eventToJSString Reset    = JS.pack "reset"
--  eventToJSString Search   = JS.pack "search"
--  eventToJSString Select   = JS.pack "select"
  eventToJSString Submit   = JS.pack "submit"

type instance UniqEventName Change  = "change"
type instance UniqEventName Invalid = "invalid"
type instance UniqEventName Reset   = "reset"
type instance UniqEventName Submit  = "submit"

-- * MediaEvent

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


type instance UniqEventName CanPlay        = "canplay"
type instance UniqEventName CanPlayThrough = "canplaythrough"
type instance UniqEventName DurationChange = "durationchange"
type instance UniqEventName Emptied        = "emptied"
type instance UniqEventName Ended          = "ended"
type instance UniqEventName MediaError     = "mediaerror"
type instance UniqEventName LoadedData     = "loadeddata"
type instance UniqEventName LoadedMetaData = "loadedmetadata"
type instance UniqEventName Pause          = "pause"
type instance UniqEventName Play           = "play"
type instance UniqEventName Playing        = "playing"
type instance UniqEventName RateChange     = "ratechange"
type instance UniqEventName Seeked         = "seeked"
type instance UniqEventName Seeking        = "seeking"
type instance UniqEventName Stalled        = "stalled"
type instance UniqEventName Suspend        = "suspend"
type instance UniqEventName TimeUpdate     = "timeupdate"
type instance UniqEventName VolumeChange   = "volumechange"
type instance UniqEventName Waiting        = "waiting"

-- * AnimationEvent

data AnimationEvent
  = AnimationStart
  | AnimationCancel
  | AnimationEnd
  | AnimationIteration
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName AnimationStart     = "animationstart"
type instance UniqEventName AnimationCancel    = "animationcancel"
type instance UniqEventName AnimationEnd       = "animationend"
type instance UniqEventName AnimationIteration = "animationiteration"

-- * AnimationEventObject

newtype AnimationEventObject (ev :: AnimationEvent) = AnimationEventObject { unAnimationEventObject :: JSVal }

instance Show (AnimationEventObject ev) where
  show _ = "AnimationEventObject"

instance ToJSVal (AnimationEventObject ev) where
  toJSVal = return . unAnimationEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (AnimationEventObject ev) where
  fromJSVal = return . fmap AnimationEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (AnimationEventObject ev) where
  type Ev (AnimationEventObject ev) = ev
  asEventObject (AnimationEventObject jsval) = EventObject jsval



-- * TouchEvent

data TouchEvent
  = TouchCancel
  | TouchEnd
  | TouchMove
  | TouchStart
  deriving (Eq, Ord, Show, Read)

type instance UniqEventName TouchCancel = "touchcancel"
type instance UniqEventName TouchEnd    = "touchend"
type instance UniqEventName TouchMove   = "touchmove"
type instance UniqEventName TouchStart  = "touchstart"

-- * TouchEventObject

newtype TouchEventObject (ev :: TouchEvent) = TouchEventObject { unTouchEventObject :: JSVal }

instance Show (TouchEventObject ev) where
  show _ = "TouchEventObject"

instance ToJSVal (TouchEventObject ev) where
  toJSVal = return . unTouchEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (TouchEventObject ev) where
  fromJSVal = return . fmap TouchEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (TouchEventObject ev) where
  type Ev (TouchEventObject ev) = ev
  asEventObject (TouchEventObject jsval) = EventObject jsval

-- * TransitionEvent

data TransitionEvent
  = TransitionCancel
  | TransitionEnd
  | TransitionRun
  | TransitionStart
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName TransitionCancel = "transitioncancel"
type instance UniqEventName TransitionEnd    = "transitionend"
type instance UniqEventName TransitionRun    = "transitionrun"
type instance UniqEventName TransitionStart  = "transitionstart"

-- * TransitionEventObject

newtype TransitionEventObject (ev :: TransitionEvent) = TransitionEventObject { unTransitionEventObject :: JSVal }

instance Show (TransitionEventObject ev) where
  show _ = "TransitionEventObject"

instance ToJSVal (TransitionEventObject ev) where
  toJSVal = return . unTransitionEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (TransitionEventObject ev) where
  fromJSVal = return . fmap TransitionEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (TransitionEventObject ev) where
  type Ev (TransitionEventObject ev) = ev
  asEventObject (TransitionEventObject jsval) = EventObject jsval

-- * SelectionEvent

data SelectionEvent
  = Select
  | SelectStart
  | SelectionChange
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName Select          = "select"
type instance UniqEventName SelectStart     = "selectstart"
type instance UniqEventName SelectionChange = "selectionchange"

instance IsEvent SelectionEvent where
  eventToJSString Select          = JS.pack "select"
  eventToJSString SelectStart     = JS.pack "selectstart"
  eventToJSString SelectionChange = JS.pack "selectionchange"

-- * StorageEvent

data StorageEvent
  = Storage
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName Storage = "storage"

-- * StorageEventObject

newtype StorageEventObject (ev :: StorageEvent) = StorageEventObject { unStorageEventObject :: JSVal }

instance Show (StorageEventObject ev) where
  show _ = "StorageEventObject"

instance ToJSVal (StorageEventObject ev) where
  toJSVal = return . unStorageEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (StorageEventObject ev) where
  fromJSVal = return . fmap StorageEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (StorageEventObject ev) where
  type Ev (StorageEventObject ev) = ev
  asEventObject (StorageEventObject jsval) = EventObject jsval

-- * WheelEvent

data WheelEvent
  = Wheel
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName Wheel          = "wheel"

-- * WheelEventObject

newtype WheelEventObject (ev :: WheelEvent) = WheelEventObject { unWheelEventObject :: JSVal }

instance Show (WheelEventObject ev) where
  show _ = "WheelEventObject"

instance ToJSVal (WheelEventObject ev) where
  toJSVal = return . unWheelEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (WheelEventObject ev) where
  fromJSVal = return . fmap WheelEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (WheelEventObject ev) where
  type Ev (WheelEventObject ev) = ev
  asEventObject (WheelEventObject jsval) = EventObject jsval



-- * Event Objects

-- http://www.w3schools.com/jsref/dom_obj_event.asp

class IsEventObject obj where
  type Ev obj :: k
  asEventObject        :: obj -> EventObject (Ev obj)

-- * EventObject

newtype EventObject (ev :: k) = EventObject { unEventObject :: JSVal }

instance Show (EventObject ev) where
  show _ = "EventObject"

instance ToJSVal (EventObject ev) where
  toJSVal = return . unEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (EventObject ev) where
  fromJSVal = return . fmap EventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (EventObject ev) where
  type Ev (EventObject ev) = ev
  asEventObject (EventObject jsval) = EventObject jsval

foreign import javascript unsafe "$1[\"defaultPrevented\"]" js_defaultPrevented ::
        EventObject ev -> IO Bool

defaultPrevented :: (IsEventObject obj, MonadIO m) => obj -> m Bool
defaultPrevented obj = liftIO (js_defaultPrevented (asEventObject obj))

foreign import javascript unsafe "$1[\"currentTarget\"]" js_currentTarget ::
        EventObject ev -> EventTarget

currentTarget :: (IsEventObject obj) => obj -> EventTarget
currentTarget obj = js_currentTarget (asEventObject obj)

foreign import javascript unsafe "$1[\"target\"]" js_target ::
        EventObject ev -> EventTarget

target :: (IsEventObject obj) => obj -> EventTarget
target obj = js_target (asEventObject obj)

foreign import javascript unsafe "$1[\"preventDefault\"]()" js_preventDefault ::
        EventObject ev -> IO ()

preventDefault :: (IsEventObject obj) => obj -> IO ()
preventDefault obj = (js_preventDefault (asEventObject obj))

foreign import javascript unsafe "$1[\"stopPropagation\"]()" js_stopPropagation ::
        EventObject ev -> IO ()

-- stopPropagation :: (IsEventObject obj, MonadIO m) => obj -> m ()
stopPropagation :: (IsEventObject obj) => obj -> IO ()
stopPropagation obj = (js_stopPropagation (asEventObject obj))

-- * MouseEventObject

newtype MouseEventObject (ev :: MouseEvent) = MouseEventObject { unMouseEventObject :: JSVal }

instance Show (MouseEventObject ev) where
  show _ = "MouseEventObject"

instance ToJSVal (MouseEventObject ev) where
  toJSVal = return . unMouseEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (MouseEventObject ev) where
  fromJSVal = return . fmap MouseEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (MouseEventObject ev) where
  type Ev (MouseEventObject ev) = ev
  asEventObject (MouseEventObject jsval) = EventObject jsval

foreign import javascript unsafe "$1[\"clientX\"]" clientX ::
        MouseEventObject ev -> Double

foreign import javascript unsafe "$1[\"clientY\"]" clientY ::
        MouseEventObject ev -> Double

foreign import javascript unsafe "$1[\"button\"]" button ::
        MouseEventObject ev -> Int

foreign import javascript unsafe "$1[\"shiftKey\"]" mouse_shiftKey ::
        MouseEventObject ev -> Bool

foreign import javascript unsafe "$1[\"ctrlKey\"]" mouse_ctrlKey ::
        MouseEventObject ev -> Bool

foreign import javascript unsafe "$1[\"altKey\"]" mouse_altKey ::
        MouseEventObject ev -> Bool

foreign import javascript unsafe "$1[\"metaKey\"]" mouse_metaKey ::
        MouseEventObject ev -> Bool

instance HasModifierKeys (MouseEventObject ev) where
  shiftKey = mouse_shiftKey
  ctrlKey  = mouse_ctrlKey
  altKey   = mouse_altKey
  metaKey  = mouse_metaKey

-- * KeyboardEvent

data KeyboardEvent
  = KeyDown
  | KeyPress
  | KeyUp
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance IsEvent KeyboardEvent where
  eventToJSString KeyDown  = JS.pack "keydown"
  eventToJSString KeyPress = JS.pack "keypress"
  eventToJSString KeyUp    = JS.pack "keyup"

type instance UniqEventName KeyDown  = "keydown"
type instance UniqEventName KeyPress = "keypress"
type instance UniqEventName KeyUp    = "keyup"

-- * KeyboardEventObject

newtype KeyboardEventObject (ev :: KeyboardEvent) = KeyboardEventObject { unKeyboardEventObject :: JSVal }

instance ToJSVal (KeyboardEventObject ev) where
  toJSVal = return . unKeyboardEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (KeyboardEventObject ev) where
  fromJSVal = return . fmap KeyboardEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (KeyboardEventObject ev) where
  type Ev (KeyboardEventObject ev) = ev
  asEventObject (KeyboardEventObject jsval) = EventObject jsval

foreign import javascript unsafe "$1[\"charCode\"]" charCode ::
        (KeyboardEventObject ev) -> Int

foreign import javascript unsafe "$1[\"keyCode\"]" keyCode ::
        (KeyboardEventObject ev) -> Int

foreign import javascript unsafe "$1[\"which\"]" which ::
        (KeyboardEventObject ev) -> Int

foreign import javascript unsafe "$1[\"repeat\"]" repeat ::
        (KeyboardEventObject ev) -> Bool

class HasModifierKeys obj where
  shiftKey :: obj -> Bool
  ctrlKey  :: obj -> Bool
  altKey   :: obj -> Bool
  metaKey  :: obj -> Bool

foreign import javascript unsafe "$1[\"shiftKey\"]" keyboard_shiftKey ::
        (KeyboardEventObject ev) -> Bool

foreign import javascript unsafe "$1[\"ctrlKey\"]" keyboard_ctrlKey ::
        (KeyboardEventObject ev) -> Bool

foreign import javascript unsafe "$1[\"altKey\"]" keyboard_altKey ::
        (KeyboardEventObject ev) -> Bool

foreign import javascript unsafe "$1[\"metaKey\"]" keyboard_metaKey ::
        (KeyboardEventObject ev) -> Bool

instance HasModifierKeys (KeyboardEventObject ev) where
  shiftKey = keyboard_shiftKey
  ctrlKey  = keyboard_ctrlKey
  altKey   = keyboard_altKey
  metaKey  = keyboard_metaKey

-- * CloseEvent

data CloseEvent
  = Close
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName Close      = "close"

-- * CloseEventObject

newtype CloseEventObject (ev :: CloseEvent) = CloseEventObject { unCloseEventObject :: JSVal }

instance Show (CloseEventObject ev) where
  show _ = "CloseEventObject"

instance ToJSVal (CloseEventObject ev) where
  toJSVal = return . unCloseEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (CloseEventObject ev) where
  fromJSVal = return . fmap CloseEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (CloseEventObject ev) where
  type Ev (CloseEventObject ev) = ev
  asEventObject (CloseEventObject jsval) = EventObject jsval

-- * FocusEvent

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

type instance UniqEventName Blur     = "blur"
type instance UniqEventName Focus    = "focus"
type instance UniqEventName FocusIn  = "focusin"
type instance UniqEventName FocusOut = "focusout"

-- * FocusEventObject

newtype FocusEventObject (ev :: FocusEvent) = FocusEventObject { unFocusEventObject :: JSVal }

instance Show (FocusEventObject ev) where
  show _ = "FocusEventObject"

instance ToJSVal (FocusEventObject ev) where
  toJSVal = return . unFocusEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (FocusEventObject ev) where
  fromJSVal = return . fmap FocusEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (FocusEventObject ev) where
  type Ev (FocusEventObject ev) = ev
  asEventObject (FocusEventObject jsval) = EventObject jsval

-- * ProgressEvent

-- | note: "abort", "load", and "error" can sometimes return a 'ProgressEventObject'
-- instead of an 'EventObject'. But, we can not add constructors for that to
-- 'ProgressEvent' because we require that all constructors map to a unique
-- string. Instead you'll need to use the more generic 'Abort', 'Load', 'Error', and
-- manually cast the 'EventObject' to a 'ProgressEventObject' if you need access to the
-- extra 'ProgressEvent' parameters.
data ProgressEvent
  = LoadEnd
  | LoadStart
  | Progress
  | Timeout
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName LoadStart     = "loadstart"
type instance UniqEventName Progress      = "progress"
type instance UniqEventName Timeout       = "timeout"
type instance UniqEventName LoadEnd       = "loadend"

instance IsEvent ProgressEvent where
  eventToJSString LoadStart     = JS.pack "loadstart"
  eventToJSString Progress      = JS.pack "progress"
  eventToJSString Timeout       = JS.pack "timeout"
  eventToJSString LoadEnd       = JS.pack "loadend"

-- * ProgressEventObject

newtype ProgressEventObject (ev :: ProgressEvent) = ProgressEventObject { unProgressEventObject :: JSVal }

instance ToJSVal (ProgressEventObject ev) where
  toJSVal = return . unProgressEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (ProgressEventObject ev) where
  fromJSVal = return . fmap ProgressEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

foreign import javascript unsafe "$1[\"scrollTop\"]" scrollTop ::
        JSElement -> Int
foreign import javascript unsafe "$1[\"scrollLeft\"]" scrollLeft ::
        JSElement -> Int

-- * EventObjectOf

type family EventObjectOf (event :: k) :: o where
  EventObject (ev :: Event)                 = EventObject ev
  EventObject (ev :: AnimationEvent)        = AnimationEventObject ev
  EventObject (ev :: ClipboardEvent)        = ClipboardEventObject ev
  EventObject (ev :: CloseEvent)            = CloseEventObject ev
  EventObject (ev :: DragEvent)             = DragEventObject ev
  EventObject (ev :: FocusEvent)            = FocusEventObject ev
  EventObject (ev :: FormEvent)             = EventObject ev
  EventObject (ev :: HashChangeEvent)       = HashChangeEventObject ev
  EventObject (ev :: InputEvent)            = InputEventObject ev
  EventObject (ev :: KeyboardEvent)         = KeyboardEventObject ev
  EventObject (ev :: MediaEvent)            = EventObject ev
  EventObject (ev :: MessageEvent)          = MessageEventObject ev
  EventObject (ev :: MouseEvent)            = MouseEventObject ev
  EventObject (ev :: NetworkEvent)          = EventObject ev
  EventObject (ev :: PageTransitionEvent)   = PageTransitionEventObject ev
  EventObject (ev :: PopStateEvent)         = PopStateEventObject ev
  EventObject (ev :: PrintingEvent)         = EventObject ev
  EventObject (ev :: ProgressEvent)         = ProgressEventObject ev
  EventObject (ev :: PromiseRejectionEvent) = PromiseRejectionEventObject ev
  EventObject (ev :: ResourceEvent)         = EventObject ev
  EventObject (ev :: SelectionEvent)        = EventObject ev
  EventObject (ev :: StorageEvent)          = StorageEventObject ev
  EventObject (ev :: TouchEvent)            = TouchEventObject ev
  EventObject (ev :: TransitionEvent)       = TransitionEventObject ev
  EventObject (ev :: ViewEvent)             = EventObject ev
  EventObject (ev :: VDOMEvent)             = VDOMEventObject ev
  EventObject (ev :: WheelEvent)            = WheelEventObject ev
  EventObject e                             = CustomEventObject e (CustomEventDetail e)

-- * CustomEvent

-- | A type for CustomEvent objects. The phantom parameter `detail`
-- specifies the type of the detail field.
newtype CustomEventObject ev detail = CustomEventObject { unCustomEventObject :: JSVal }

instance ToJSVal (CustomEventObject ev detail) where
  toJSVal = return . unCustomEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (CustomEventObject ev detail) where
  fromJSVal = return . fmap CustomEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

-- type CustomEventObject' detail ev = CustomEventObject ev detail

instance IsEventObject (CustomEventObject ev detail) where
  type Ev (CustomEventObject ev detail) = ev
  asEventObject (CustomEventObject jsval) = EventObject jsval

foreign import javascript unsafe "new CustomEvent($1, { 'detail': $2, 'bubbles' : $3, 'cancelable' : $4})"
        js_newCustomEvent :: JSString -> JSVal -> Bool -> Bool -> IO JSVal

newCustomEvent :: (KnownSymbol (UniqEventName ev), FromJSVal detail, ToJSVal detail) => EventName ev -> Maybe detail -> Bool -> Bool -> IO (CustomEventObject ev detail)
newCustomEvent ev detail bubbles cancelable =
  do let evStr = JS.pack $ eventName ev
     d <- toJSVal detail
     jsval <- js_newCustomEvent evStr d bubbles cancelable
     pure $ CustomEventObject jsval

foreign import javascript unsafe "$r = $1[\"detail\"]"
        js_detail :: CustomEventObject e detail -> JSVal

detail :: (FromJSVal detail) => CustomEventObject e detail -> IO (Maybe detail)
detail ceo = fromJSVal $ js_detail ceo

-- | specify the type of the detail for a custom event
--
-- 'ev' is a event name
-- 'detail' is the type of the CustomEvent 'detail'
type family CustomEventDetail (ev :: k) = detail

-- * addEventListener

-- FIXME: Element is overly restrictive
foreign import javascript unsafe "$1[\"addEventListener\"]($2, $3,\n$4)"
   js_addEventListener :: EventTarget -> JSString -> Callback (JSVal -> IO ()) -> Bool -> IO ()

addEventListener :: forall m self k eventName. (MonadIO m, IsEventTarget self, KnownSymbol (UniqEventName (eventName :: k)), FromJSVal (EventObjectOf eventName)) =>
                  self
               -> EventName eventName
               -> (EventObjectOf eventName -> IO ())
               -> Bool
               -> m ()
addEventListener self event callback useCapture = liftIO $
  do cb <- syncCallback1 ThrowWouldBlock callback'
     let evStr = JS.pack $ eventName event
     js_addEventListener (toEventTarget self) evStr cb useCapture
  where
    callback' = \ev ->
         do (Just eventObject) <- fromJSVal ev
            callback eventObject

-- * addEventListener

-- FIXME: Element is overly restrictive
foreign import javascript unsafe "$1['addEventListener']($2, $3,{'capture':$4,'once':$5,'passive':$6})"
   js_addEventListenerOpt :: EventTarget -> JSString -> Callback (JSVal -> IO ()) -> Bool -> Bool -> Bool -> IO ()

addEventListenerOpt :: forall m self k eventName. (MonadIO m, IsEventTarget self, KnownSymbol (UniqEventName (eventName :: k)), FromJSVal (EventObjectOf eventName)) =>
                  self
               -> EventName eventName
               -> (EventObjectOf eventName -> IO ())
               -> (Bool, Bool, Bool)
               -> m ()
addEventListenerOpt self event callback (capture,once,passive) = liftIO $
  do cb <- syncCallback1 ThrowWouldBlock callback'
     let evStr = JS.pack $ eventName event
     js_addEventListenerOpt (toEventTarget self) evStr cb capture once passive
  where
    callback' = \ev ->
         do (Just eventObject) <- fromJSVal ev
            callback eventObject

foreign import javascript unsafe "$1[\"dispatchEvent\"]($2)"
  js_dispatchEvent :: EventTarget -> EventObject ev -> IO ()

dispatchEvent :: (MonadIO m, IsEventTarget eventTarget, IsEventObject eventObj) => eventTarget -> eventObj -> m ()
dispatchEvent et ev = liftIO $ js_dispatchEvent (toEventTarget et) (asEventObject ev)

-- * DOMRect

newtype DOMClientRect = DOMClientRect { unDomClientRect :: JSVal }

instance PFromJSVal DOMClientRect where
  pFromJSVal = DOMClientRect
  {-# INLINE pFromJSVal #-}

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

showDOMClientRect :: DOMClientRect -> String
showDOMClientRect rect = "{ rectLeft = " ++ show (rectLeft rect) ++ " , rectTop = " ++ show (rectTop rect) ++ " , rectRight = " ++ show (rectRight rect) ++ " , rectBottom = " ++ show (rectBottom rect)  ++ " , height = " ++ show (height rect) ++ " , width = " ++ show (width rect) ++ " }"

-- * offsetWidth

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/offsetWidth
foreign import javascript unsafe "$r = $1[\"offsetWidth\"]" js_offsetWidth ::
  JSElement -> IO Int

offsetWidth :: (MonadIO m) => JSElement -> m Int
offsetWidth = liftIO . js_offsetWidth

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

initRemoteWS :: (ToJSON remote) => JS.JSString -> (MessageEvent.MessageEvent -> IO ()) -> IO (remote -> IO ())
initRemoteWS url' onMessageHandler =
    do let request = WebSocketRequest { url       = url'
                                      , protocols = []
                                      , onClose   = Nothing
                                      , onMessage = Just onMessageHandler
                                      }
       ws <- WebSockets.connect request
       pure (sendRemoteWS ws)

-- * DragEvent

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
  eventToJSString Drag      = JS.pack "drag"
  eventToJSString DragEnd   = JS.pack "dragend"
  eventToJSString DragEnter = JS.pack "dragenter"
  eventToJSString DragLeave = JS.pack "dragleave"
  eventToJSString DragOver  = JS.pack "dragover"
  eventToJSString DragStart = JS.pack "dragstart"
  eventToJSString Drop      = JS.pack "drop"

type instance UniqEventName Drag      = "drag"
type instance UniqEventName DragEnd   = "dragend"
type instance UniqEventName DragEnter = "dragenter"
type instance UniqEventName DragLeave = "dragleave"
type instance UniqEventName DragOver  = "dragover"
type instance UniqEventName DragStart = "dragstart"
type instance UniqEventName Drop      = "drop"

-- * DragEventObject

newtype DragEventObject (ev :: DragEvent) = DragEventObject { unDragEventObject :: JSVal }

instance Show (DragEventObject ev) where
  show _ = "DragEventObject"

instance ToJSVal (DragEventObject ev) where
  toJSVal = return . unDragEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (DragEventObject ev) where
  fromJSVal = return . fmap DragEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (DragEventObject ev) where
  type Ev (DragEventObject ev) = ev
  asEventObject (DragEventObject jsval) = EventObject jsval

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

foreign import javascript unsafe "$r = $1[\"focusNode\"]"
  js_focusNode :: Selection -> IO JSNode

focusNode :: (MonadIO m) => Selection -> m JSNode
focusNode s = liftIO (js_focusNode s)

foreign import javascript unsafe "$r = $1[\"focusOffset\"]"
  js_focusOffset :: Selection -> IO Int

focusOffset :: (MonadIO m) => Selection -> m Int
focusOffset s = liftIO (js_focusOffset s)

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


foreign import javascript unsafe "$1[\"setBaseAndExtent\"]($2, $3, $4, $5)"
  js_setBaseAndExtent :: Selection -> JSNode -> Int -> JSNode -> Int -> IO ()

setBaseAndExtent :: (MonadIO m, IsJSNode an, IsJSNode fn) => Selection -> an -> Int -> fn -> Int -> m ()
setBaseAndExtent s an ao fn fo = liftIO $ js_setBaseAndExtent s (toJSNode an) ao (toJSNode fn) fo

foreign import javascript unsafe "$1[\"toString\"]()"
 js_selectionToString :: Selection -> IO JSString

selectionToString :: (MonadIO m) => Selection -> m JSString
selectionToString s = liftIO $ js_selectionToString s

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

foreign import javascript unsafe "$1[\"getBoundingClientRect\"]()" js_getRangeBoundingClientRect ::
  Range -> IO DOMClientRect

getRangeBoundingClientRect :: (MonadIO m) => Range -> m DOMClientRect
getRangeBoundingClientRect = liftIO . js_getRangeBoundingClientRect


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

foreign import javascript unsafe "$1[\"setStartAfter\"]($2)"
  js_setStartAfter :: Range -> JSNode -> IO ()

setStartAfter :: (MonadIO m, IsJSNode node) => Range -> node -> m ()
setStartAfter r n = liftIO $ js_setStartAfter r (toJSNode n)

foreign import javascript unsafe "$1[\"setEnd\"]($2,$3)"
  js_setEnd :: Range -> JSNode -> Int -> IO ()

setEnd :: (MonadIO m, IsJSNode node) => Range -> node -> Int -> m ()
setEnd r n i = liftIO $ js_setEnd r (toJSNode n) i

foreign import javascript unsafe "$1[\"setEndBefore\"]($2)"
  js_setEndBefore :: Range -> JSNode -> IO ()

setEndBefore :: (MonadIO m, IsJSNode node) => Range -> node -> m ()
setEndBefore r n = liftIO $ js_setEndBefore r (toJSNode n)

foreign import javascript unsafe "$1[\"setEndAfter\"]($2)"
  js_setEndAfter :: Range -> JSNode -> IO ()

setEndAfter :: (MonadIO m, IsJSNode node) => Range -> node -> m ()
setEndAfter r n = liftIO $ js_setEndAfter r (toJSNode n)


-- * HTMLCollection -- a bit like an array, but not

newtype HTMLCollection a = HTMLCollection { unHTMLCollection :: JSVal }

instance ToJSVal (HTMLCollection a) where
  toJSVal = pure . unHTMLCollection
  {-# INLINE toJSVal #-}

instance FromJSVal (HTMLCollection a) where
  fromJSVal = pure . fmap HTMLCollection . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

foreign import javascript unsafe "$1[\"item\"]($2)" js_collectionItem ::
        HTMLCollection a -> Int -> IO (Nullable a)

collectionItem :: (MonadIO m, PFromJSVal a) => HTMLCollection a -> Int -> m (Maybe a)
collectionItem col n =
  liftIO (nullableToMaybe <$> js_collectionItem col n)

foreign import javascript unsafe "$1[\"length\"]" js_collectionLength ::
        HTMLCollection a -> IO Int

collectionLength :: (MonadIO m) => HTMLCollection a -> m Int
collectionLength col =
  liftIO (js_collectionLength col)

collectionItems :: (MonadIO m, PFromJSVal a) => HTMLCollection a -> m [a]
collectionItems hc =
  do l <- collectionLength hc
     mItems <- mapM (\i -> collectionItem hc i) [0..(pred l)]
     pure $ catMaybes mItems


-- * ClientRect
{-
newtype ClientRects = ClientRects { unClientRects :: JSVal }

instance ToJSVal ClientRects where
  toJSVal = return . unClientRects
  {-# INLINE toJSVal #-}

instance FromJSVal ClientRects where
  fromJSVal = return . fmap ClientRects . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}
-}

foreign import javascript unsafe "$1[\"getClientRects\"]()"
  js_getClientRects :: JSVal -> IO (HTMLCollection DOMClientRect)

getElementClientRects :: (MonadIO m) => JSElement -> m (HTMLCollection DOMClientRect)
getElementClientRects e = liftIO $ js_getClientRects (unJSElement e)

getRangeClientRects :: (MonadIO m) => Range -> m (HTMLCollection DOMClientRect)
getRangeClientRects r = liftIO $ js_getClientRects (unRange r)

{-
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
-}
-- Note: probably returns ClientRectList -- similar to NodeList
-- foreign import javascript unsfae "$1[\"getClientRects\"]()"
--  js_getClientRects :: Range -> IO JSVal

-- * Pure HTML

data Attr model where
  Attr :: Text -> Text -> Attr model
  Prop :: Text -> Text -> Attr model
  OnCreate :: (JSElement -> TDVar model -> IO ()) -> Attr model
  EL :: (Show event, KnownSymbol (UniqEventName event), FromJSVal (EventObjectOf event)) => EventName event -> (EventObjectOf event -> TDVar model -> IO ()) -> Attr model

instance Show (Attr model) where
  show (Attr a v) = Text.unpack a <> " := " <> Text.unpack v
  show (Prop a v) = "." <> Text.unpack a <> " = " <> Text.unpack v
  show (OnCreate _ ) = "onCreate"
  show (EL e _) = "on" ++ eventName e

data Html model where
  Element :: Text -> [Attr model] -> [Html model] -> Html model
  CData   :: Text -> Html model
  Cntl    :: (Show event, KnownSymbol (UniqEventName event), FromJSVal (EventObjectOf event)) =>
              Control event -> EventName event -> (EventObjectOf event -> TDVar model -> IO ()) -> Html model

instance Show (Html model) where
  show (Element n attrs elems) = "Element " <> Text.unpack n <> " " <> show attrs <> " " <> show elems
  show (CData t) = "CData " <> Text.unpack t
  show (Cntl _ e _) = "Cntl " ++ eventName e

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
            Maybe JS.JSString -> ((remote -> IO ()) -> MessageEvent.MessageEvent -> TDVar model -> IO ()) -> ((remote -> IO ()) -> model -> Html model) -> IO (TDVar model)

foreign import javascript unsafe "window[\"setTimeout\"]($1, $2)" js_setTimeout ::
  Callback (IO ()) -> Int -> IO ()

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

instance PFromJSVal DataTransfer where
  pFromJSVal = DataTransfer
  {-# INLINE pFromJSVal #-}



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
foreign import javascript unsafe "$1[\"types\"]" js_getTypes ::
        DataTransfer -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DataTransfer.types Mozilla DataTransfer.types documentation> 
getTypes ::
         (MonadIO m) => DataTransfer -> m [JSString]
getTypes self = liftIO ((js_getTypes self) >>= fromJSValUnchecked)


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

type instance UniqEventName Copy  = "copy"
type instance UniqEventName Cut   = "cut"
type instance UniqEventName Paste = "paste"

instance IsEvent ClipboardEvent where
  eventToJSString Copy  = JS.pack "copy"
  eventToJSString Cut   = JS.pack "cut"
  eventToJSString Paste = JS.pack "paste"

-- * ClipboardEventObject

newtype ClipboardEventObject (ev :: ClipboardEvent) = ClipboardEventObject { unClipboardEventObject :: JSVal }

instance Show (ClipboardEventObject ev) where
  show _ = "ClipboardEventObject"

instance ToJSVal (ClipboardEventObject ev) where
  toJSVal = pure . unClipboardEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (ClipboardEventObject ev) where
  fromJSVal = pure . fmap ClipboardEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (ClipboardEventObject ev) where
  type Ev (ClipboardEventObject ev) = ev
  asEventObject (ClipboardEventObject jsval) = EventObject jsval

foreign import javascript unsafe "$1[\"clipboardData\"]" clipboardData ::
        ClipboardEventObject ev -> IO DataTransfer

-- * Event

foreign import javascript unsafe "new Event($1, { 'bubbles' : $2, 'cancelable' : $3})"
        js_newEvent :: JSString -> Bool -> Bool -> IO JSVal

class MkEvent ev where
  mkEvent :: EventName ev -> JSVal -> EventObjectOf ev

newEvent :: (MkEvent (ev :: k), KnownSymbol (UniqEventName (ev :: k))) => EventName ev -> Bool -> Bool -> IO (EventObjectOf ev)
newEvent ev bubbles cancelable =
  do let evStr = JS.pack $ eventName ev
     jsval <- js_newEvent evStr bubbles cancelable
     pure $ mkEvent ev jsval


-- * VDOM Events

data VDOMEvent
     = Redrawn
       deriving (Eq, Show)

type instance UniqEventName Redrawn = "redrawn"

instance IsEvent VDOMEvent where
  eventToJSString Redrawn = fromString "redrawn"

newtype VDOMEventObject ev = VDOMEventObject { unVDOMEventObject :: JSVal }

instance MkEvent Redrawn where
  mkEvent _ jsval = VDOMEventObject jsval

instance Show (VDOMEventObject ev) where
  show _ = "VDOMEventObject"

instance ToJSVal (VDOMEventObject ev) where
  toJSVal = return . unVDOMEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (VDOMEventObject ev) where
  fromJSVal = return . fmap VDOMEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (VDOMEventObject ev) where
  type Ev (VDOMEventObject ev) = ev
  asEventObject (VDOMEventObject jsval) = EventObject jsval

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

foreign import javascript unsafe "$1[\"contains\"]($2)"
        js_containsToken :: DOMTokenList -> JSString -> IO Bool

containsToken :: (MonadIO m) => DOMTokenList -> JSString -> m Bool
containsToken lst tkn = liftIO $ js_containsToken lst tkn

foreign import javascript unsafe "$r = $1[\"classList\"]"
        js_classList :: JSElement -> IO DOMTokenList

classList :: (MonadIO m) => JSElement -> m DOMTokenList
classList e = liftIO $ js_classList e

-- * CharacterData

-- | https:\/\/developer.mozilla.org\/en-US\/docs\/Web\/API\/CharacterData
class (IsJSNode obj) => CharacterData obj
instance CharacterData JSTextNode


foreign import javascript unsafe "$1[\"data\"]"
    js_data :: JSNode -> IO JSString


-- | object.data
getCharacterData :: (CharacterData obj) => obj -> IO JSString
getCharacterData o = js_data (toJSNode o)
