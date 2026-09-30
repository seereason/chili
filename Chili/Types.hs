{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#if __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif
{-# LANGUAGE ConstrainedClassMethods, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GADTs, ScopedTypeVariables, TypeFamilies #-}
{-# language GeneralizedNewtypeDeriving, TypeApplications, AllowAmbiguousTypes, OverloadedStrings #-}
{-# language RankNTypes, DataKinds, KindSignatures, PolyKinds, TypeFamilyDependencies #-}
{-# language PatternSynonyms, UndecidableInstances #-}
{-# language MultiParamTypeClasses, TypeOperators #-}
module Chili.Types where

#if defined(wasm32_HOST_ARCH)
import GHC.JS.Foreign.Callback (Callback(..))
import JavaScript.TypedArray.ArrayBuffer (SomeArrayBuffer(..))
#endif
import Control.Applicative (Applicative, Alternative)
import Control.Concurrent (forkIO)
import Control.Exception (Exception, throw)
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
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
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
import qualified JavaScript.TypedArray.ArrayBuffer as ArrayBuffer
import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer, MutableArrayBuffer)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import GHCJS.Buffer as Buffer
import GHCJS.Foreign (jsNull)
import GHC.JS.Foreign.Callback (OnBlocked(..), Callback, asyncCallback, asyncCallback1, syncCallback1)
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Nullable (Nullable(..), nullableToMaybe, maybeToNullable)
import GHCJS.Types (IsJSVal(..), JSVal(..), JSString(..),  nullRef, isNull, isUndefined)
import qualified JavaScript.Web.MessageEvent (MessageEvent(..), MessageEventData(..))
import qualified JavaScript.Web.MessageEvent as MessageEvent
import qualified JavaScript.Web.WebSocket as WebSockets
import JavaScript.Web.WebSocket (WebSocket, WebSocketRequest(..), connect, send)
import Safe

instance Eq JSVal where
  a == b = js_eq a b

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return (a1===a2); })($1,$2)" js_eq :: JSVal  -> JSVal  -> Bool
#else
foreign import javascript unsafe
  "((a1,a2) => { return (a1===a2); })" js_eq :: JSVal  -> JSVal  -> Bool
#endif

maybeJSNullOrUndefined :: JSVal -> Maybe JSVal
maybeJSNullOrUndefined r | isNull r || isUndefined r = Nothing
maybeJSNullOrUndefined r = Just r

class InstanceOf ty where
  instanceOf :: (PToJSVal a) => a -> Bool

instance InstanceOf JSElement where
  instanceOf a = js_instanceOfJSElement (pToJSVal a)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return (a1 instanceof Element); })($1)"
#else
foreign import javascript unsafe "((a1) => { return (a1 instanceof Element); })"
#endif
  js_instanceOfJSElement :: JSVal -> Bool

{-
fromJSValUnchecked :: (FromJSVal a) => JSVal a -> IO a
fromJSValUnchecked j =
    do x <- fromJSVal j
       case x of
         Nothing -> error "failed."
         (Just a) -> return a
-}

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => alert(a1))($1)"
#else
foreign import javascript unsafe "((a1) => alert(a1))"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return (a1 instanceof Node); })($1)"
#else
foreign import javascript unsafe "((a1) => { return (a1 instanceof Node); })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"append\"](a2))($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"append\"](a2))"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"children\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"children\"]; })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return (new EventTarget()); })()"
#else
foreign import javascript unsafe "(() => { return (new EventTarget()); })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"length\"]; })($1)" js_nodeListLength ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"length\"]; })" js_nodeListLength ::
#endif
        JSNodeList -> IO Int

nodeListLength :: (MonadIO m) => JSNodeList -> m Int
nodeListLength nodeList = liftIO $ js_nodeListLength nodeList

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"item\"](a2))($1,$2)" js_item ::
#else
foreign import javascript unsafe "((a1,a2) => a1[\"item\"](a2))" js_item ::
#endif
        JSNodeList -> Word -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/NodeList.item Mozilla NodeList.item documentation>
item ::
     (MonadIO m) => JSNodeList -> Word -> m (Maybe JSNode)
item self index
  = liftIO
      ((js_item (self) index) >>= fromJSVal)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"length\"]; })($1)" js_length ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"length\"]; })" js_length ::
#endif
        JSNode -> IO Word

-- | <https://developer.mozilla.org/en-US/docs/Web/API/NodeList.item Mozilla NodeList.item documentation>
getLength :: (MonadIO m, IsJSNode self) => self -> m Word
getLength self
  = liftIO (js_length ( (toJSNode self))) -- >>= fromJSValUnchecked)

-- foreign import javascript unsafe "$1[\"length\"]" js_getLength ::
--         JSVal NodeList -> IO Word

-- * contenteditable

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"contentEditable\"] = a2)($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"contentEditable\"] = a2)"
#endif
  js_setContentEditable :: JSElement -> Bool -> IO ()

setContentEditable :: (MonadIO m) => JSElement -> Bool -> m ()
setContentEditable e b = liftIO $ js_setContentEditable e b

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"contentEditable\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"contentEditable\"]; })"
#endif
  js_getContentEditable :: JSElement -> IO Bool

getContentEditable :: (MonadIO m) => JSElement -> m Bool
getContentEditable e = liftIO $ js_getContentEditable e

data DocumentPosition = DocumentPosition
  { dpDisconnected :: Bool
  , dpPreceding    :: Bool
  , dpFollowing    :: Bool
  , dpContains     :: Bool
  , dpContainedBy  :: Bool
  , dpImplementationSpecific :: Bool
  }
  deriving (Eq, Ord, Read, Show)

maskToDocumentPosition :: Int -> DocumentPosition
maskToDocumentPosition m = DocumentPosition
  { dpDisconnected = (m .&. 1) == 1
  , dpPreceding    = (m .&. 2) == 2
  , dpFollowing    = (m .&. 4) == 4
  , dpContains     = (m .&. 8) == 8
  , dpContainedBy  = (m .&. 16) == 16
  , dpImplementationSpecific = (m .&. 32) == 32
  }

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"compareDocumentPosition\"](a2); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"compareDocumentPosition\"](a2); })"
#endif
   js_compareDocumentPosition :: JSNode -> JSNode -> IO Int

compareDocumentPosition :: (IsJSNode node, IsJSNode otherNode, MonadIO m) => node -> otherNode -> m DocumentPosition
compareDocumentPosition node otherNode =
  liftIO $ maskToDocumentPosition <$> js_compareDocumentPosition (toJSNode node) (toJSNode otherNode)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"contains\"](a2); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"contains\"](a2); })"
#endif
   js_contains :: JSNode -> JSNode -> IO Bool

contains :: (IsJSNode node, IsJSNode otherNode, MonadIO m) => node -> otherNode -> m Bool
contains node otherNode = liftIO $ js_contains (toJSNode node) (toJSNode otherNode)

-- * cloneNode

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"cloneNode\"](a2); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"cloneNode\"](a2); })"
#endif
  js_cloneNode :: JSNode -> Bool -> IO JSNode

cloneNode :: (MonadIO m, IsJSNode self) => self -> Bool -> m JSNode
cloneNode self deep =
  liftIO (js_cloneNode (toJSNode self) deep)

-- * parentNode

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"parentNode\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"parentNode\"]; })"
#endif
        js_parentNode :: JSNode -> IO JSVal

parentNode :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
parentNode self =
    liftIO (fromJSVal =<< js_parentNode (toJSNode self))

-- * parentElement

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"parentElement\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"parentElement\"]; })"
#endif
        js_parentElement :: JSNode -> IO JSVal

parentElement :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSElement)
parentElement self =
    liftIO (fromJSVal =<< js_parentElement (toJSNode self))

-- * nodeType

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"nodeType\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"nodeType\"]; })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"nodeName\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"nodeName\"]; })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return (a1 instanceof DocumentFragment); })($1)"
#else
foreign import javascript unsafe "((a1) => { return (a1 instanceof DocumentFragment); })"
#endif
  js_instanceOfJSDocumentFragment :: JSVal -> Bool

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"firstElementChild\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"firstElementChild\"]; })"
#endif
        js_firstElementChild :: JSVal -> IO JSVal

firstElementChild :: (MonadIO m, ToJSVal parent, IsParentNode parent) => parent -> m (Maybe JSElement)
firstElementChild p
  = liftIO (fromJSVal =<< js_firstElementChild =<< toJSVal p)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"lastElementChild\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"lastElementChild\"]; })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return (a1 instanceof Document); })($1)"
#else
foreign import javascript unsafe "((a1) => { return (a1 instanceof Document); })"
#endif
  js_instanceOfJSDocument :: JSVal -> Bool

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return (new window[\"Document\"]()); })()"
#else
foreign import javascript unsafe "(() => { return (new window[\"Document\"]()); })"
#endif
        js_newDocument :: IO JSDocument

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document Mozilla Document documentation>
newJSDocument :: (MonadIO m) => m JSDocument
newJSDocument = liftIO js_newDocument

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return (a1[\"implementation\"][\"createHTMLDocument\"]()); })($1)"
#else
foreign import javascript unsafe "((a1) => { return (a1[\"implementation\"][\"createHTMLDocument\"]()); })"
#endif
       js_createHTMLDocument :: JSDocument -> IO JSDocument

-- foreign import javascript unsafe "document.implementation.createHTMLDocument()"
--        js_createHTMLDocument :: JSDocument -> IO JSDocument

-- | FIXME: actually use title when provided
createHTMLDocument :: JSDocument -> Maybe JSString -> IO JSDocument
createHTMLDocument d mTitle =
  js_createHTMLDocument d

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return ((typeof document === 'undefined') ? null : document); })()"
#else
foreign import javascript unsafe "(() => { return ((typeof document === 'undefined') ? null : document); })"
#endif
  ghcjs_currentDocument :: IO JSVal

currentDocument :: (MonadIO m) => m (Maybe JSDocument)
currentDocument = liftIO $ fromJSVal =<< ghcjs_currentDocument

#if defined(wasm32_HOST_ARCH)
-- Reflect.set: sloppy-mode assignment semantics (defines the global under
-- node/jsdom, silently does nothing in a browser); the wasm glue is strict.
foreign import javascript unsafe "Reflect.set(globalThis, 'document', $1)"
#else
foreign import javascript unsafe "((a1) => document = a1)"
#endif
   js_setCurrentDocument :: JSDocument -> IO ()

setCurrentDocument :: (MonadIO m) => JSDocument -> m ()
setCurrentDocument doc = liftIO $ js_setCurrentDocument doc

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"document\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"document\"]; })"
#endif
        js_document :: JSWindow -> IO JSVal

document :: (MonadIO m) => JSWindow -> m (Maybe JSDocument)
document w = liftIO $ fromJSVal =<< js_document w

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"querySelector\"](a2); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"querySelector\"](a2); })"
#endif
        js_querySelector :: JSVal -> JSString -> IO (Nullable JSElement)

querySelector :: (MonadIO m, IsParentNode obj, PToJSVal obj) => obj -> JSString -> m (Maybe JSElement)
querySelector o sel = liftIO (nullableToMaybe <$> js_querySelector (pToJSVal o) sel)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"body\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"body\"]; })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3,a4) => { return a1[\"execCommand\"](a2,a3,a4); })($1,$2,$3,$4)"
#else
foreign import javascript unsafe "((a1,a2,a3,a4) => { return a1[\"execCommand\"](a2,a3,a4); })"
#endif
        js_execCommand :: JSDocument -> JSString -> Bool -> JSVal -> IO Bool

-- | TODO: many commands not implemented
execCommand :: (MonadIO m) => JSDocument -> Command -> Bool -> Maybe JSString -> m Bool
execCommand doc aCommand aShowDefaultUI aValueArgument  =
  liftIO $ js_execCommand doc (commandStr aCommand) aShowDefaultUI (maybe jsNull pToJSVal aValueArgument)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"queryCommandState\"](a2); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"queryCommandState\"](a2); })"
#endif
        js_queryCommandState :: JSDocument -> JSString -> IO Bool

queryCommandState :: (MonadIO m) => JSDocument -> Command -> m Bool
queryCommandState doc aCommand = liftIO (js_queryCommandState doc (commandStr aCommand))


#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"queryCommandValue\"](a2); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"queryCommandValue\"](a2); })"
#endif
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

instance PFromJSVal JSWindow where
  pFromJSVal = JSWindow
  {-# INLINE pFromJSVal #-}

instance PToJSVal JSWindow where
  pToJSVal (JSWindow jsval) = jsval
  {-# INLINE pToJSVal #-}

instance IsEventTarget JSWindow where
    toEventTarget = EventTarget . unJSWindow

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return (a1 instanceof Window); })($1)"
#else
foreign import javascript unsafe "((a1) => { return (a1 instanceof Window); })"
#endif
  js_instanceOfJSWindow :: JSVal -> Bool

instance InstanceOf JSWindow where
  instanceOf a = js_instanceOfJSWindow (pToJSVal a)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return ((typeof window === 'undefined') ? null : window); })()"
#else
foreign import javascript unsafe "(() => { return ((typeof window === 'undefined') ? null : window); })"
#endif
  js_window :: IO JSVal

window :: (MonadIO m) => m (Maybe JSWindow)
window = liftIO $ fromJSVal =<< js_window

#if defined(wasm32_HOST_ARCH)
-- (see js_setCurrentDocument)
foreign import javascript unsafe "Reflect.set(globalThis, 'window', $1)"
#else
foreign import javascript unsafe "((a1) => window = a1)"
#endif
   js_setWindow :: JSWindow -> IO ()

setWindow :: (MonadIO m) => JSWindow -> m ()
setWindow w = liftIO $ js_setWindow w

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"devicePixelRatio\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"devicePixelRatio\"]; })"
#endif
  js_devicePixelRatio :: JSWindow -> IO JSVal

devicePixelRatio :: (MonadIO m) => JSWindow -> m (Maybe Double)
devicePixelRatio w = liftIO (fromJSVal =<< js_devicePixelRatio w)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return (a1[\"getSelection\"]()); })($1)"
#else
foreign import javascript unsafe "((a1) => { return (a1[\"getSelection\"]()); })"
#endif
  js_getSelection :: JSWindow -> IO Selection

getSelection :: (MonadIO m) => JSWindow -> m Selection
getSelection w = liftIO (js_getSelection w)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"scrollX\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"scrollX\"]; })"
#endif
  js_scrollX :: JSWindow -> IO Double

scrollX :: (MonadIO m) => JSWindow -> m Double
scrollX w = liftIO (js_scrollX w)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"scrollY\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"scrollY\"]; })"
#endif
  js_scrollY :: JSWindow -> IO Double

scrollY :: (MonadIO m) => JSWindow -> m Double
scrollY w = liftIO (js_scrollY w)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"innerHeight\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"innerHeight\"]; })"
#endif
  js_innerHeight :: JSWindow -> IO Double

innerHeight :: (MonadIO m) => JSWindow -> m Double
innerHeight w = liftIO (js_innerHeight w)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"innerWidth\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"innerWidth\"]; })"
#endif
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

instance PToJSVal JSElement where
  pToJSVal (JSElement jsval) = jsval
  {-# INLINE pToJSVal #-}

instance IsJSNode JSElement where
    toJSNode = JSNode . unJSElement

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"clientLeft\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"clientLeft\"]; })"
#endif
        js_getClientLeft :: JSElement -> IO Double

getClientLeft :: (MonadIO m) => JSElement -> m Double
getClientLeft = liftIO . js_getClientLeft

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"clientTop\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"clientTop\"]; })"
#endif
        js_getClientTop :: JSElement -> IO Double

getClientTop :: (MonadIO m) => JSElement -> m Double
getClientTop = liftIO . js_getClientTop

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"clientWidth\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"clientWidth\"]; })"
#endif
        js_getClientWidth :: JSElement -> IO Double

getClientWidth :: (MonadIO m) => JSElement -> m Double
getClientWidth = liftIO . js_getClientWidth

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"clientHeight\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"clientHeight\"]; })"
#endif
        js_getClientHeight :: JSElement -> IO Double

getClientHeight :: (MonadIO m) => JSElement -> m Double
getClientHeight = liftIO . js_getClientHeight

-- * createJSElement

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return (a1[\"createElement\"](a2)); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return (a1[\"createElement\"](a2)); })"
#endif
        js_createJSElement ::
        JSDocument -> JSString -> IO JSElement

-- | <https://developer.mozilla.org/en-US/docs/Web/API/JSDocument.createJSElement Mozilla JSDocument.createJSElement documentation>
-- FIXME: can this actually return Nothing?
createJSElement :: (MonadIO m) => JSDocument -> Text -> m (Maybe JSElement)
createJSElement document tagName
  = liftIO ((js_createJSElement document (textToJSString tagName))
            >>= return . Just)

-- * innerHTML

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"innerHTML\"] = a2)($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"innerHTML\"] = a2)"
#endif
        js_setInnerHTML :: JSElement -> JSString -> IO ()

setInnerHTML :: (MonadIO m) => JSElement -> JSString -> m ()
setInnerHTML elm content = liftIO $ js_setInnerHTML elm content

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => a1[\"innerHTML\"])($1)"
#else
foreign import javascript unsafe "((a1) => a1[\"innerHTML\"])"
#endif
        js_getInnerHTML :: JSElement -> IO JSString

getInnerHTML :: (MonadIO m) => JSElement -> m JSString
getInnerHTML element = liftIO $ js_getInnerHTML element

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"outerHTML\"] = a2)($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"outerHTML\"] = a2)"
#endif
        js_setOuterHTML :: JSElement -> JSString -> IO ()

setOuterHTML :: (MonadIO m) => JSElement -> JSString -> m ()
setOuterHTML elm content = liftIO $ js_setOuterHTML elm content

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"outerHTML\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"outerHTML\"]; })"
#endif
        js_getOuterHTML :: JSElement -> IO JSString

getOuterHTML :: (MonadIO m) => JSElement -> m JSString
getOuterHTML element = liftIO $ js_getOuterHTML element

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"tagName\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"tagName\"]; })"
#endif
  js_tagName :: JSElement -> IO JSString

tagName :: (MonadIO m) => JSElement -> m Text
tagName e =
  do v <- liftIO $ js_tagName e
     pure (textFromJSString v)

-- * childNodes

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"childNodes\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"childNodes\"]; })"
#endif
        js_childNodes :: JSNode -> IO JSNodeList

childNodes :: (MonadIO m, IsJSNode self) => self -> m JSNodeList
childNodes self
    = liftIO (js_childNodes (toJSNode self))

class (IsJSNode obj) => DocumentOrElement obj
instance DocumentOrElement JSDocument
instance DocumentOrElement JSElement

-- * getElementsByName

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"getElementsByName\"](a2); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"getElementsByName\"](a2); })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"getElementsByClassName\"](a2); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"getElementsByClassName\"](a2); })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"getElementsByTagName\"](a2); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"getElementsByTagName\"](a2); })"
#endif
        js_getElementsByTagName ::
        JSNode -> JSString -> IO JSNodeList

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Document.getElementsByTagName Mozilla Document.getElementsByTagName documentation>
getElementsByTagName :: (DocumentOrElement obj, MonadIO m) =>
                        obj
                     -> JSString
                     -> m (Maybe JSNodeList)
getElementsByTagName self tagname
  = liftIO ((js_getElementsByTagName (toJSNode self) tagname) >>= return . Just)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return (a1[\"getElementById\"](a2)); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return (a1[\"getElementById\"](a2)); })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => { return (a1[\"insertAdjacentElement\"](a2, a3)); })($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => { return (a1[\"insertAdjacentElement\"](a2, a3)); })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => { return (a1[\"insertBefore\"](a2, a3)); })($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => { return (a1[\"insertBefore\"](a2, a3)); })"
#endif
        js_insertBefore :: JSNode -> JSNode -> JSNode -> IO JSNode

insertBefore :: (MonadIO m, IsJSNode parentNode, IsJSNode newNode, IsJSNode referenceNode) =>
               parentNode
             -> newNode
            -> referenceNode
            -> m JSNode
insertBefore parentNode newNode referenceNode =
  liftIO $ (js_insertBefore (toJSNode parentNode) (toJSNode newNode) (toJSNode referenceNode))

-- * appendChild

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return (a1[\"appendChild\"](a2)); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return (a1[\"appendChild\"](a2)); })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"focus\"](); })($1)" js_focus :: JSElement -> IO ()
#else
foreign import javascript unsafe "((a1) => { return a1[\"focus\"](); })" js_focus :: JSElement -> IO ()
#endif

focus :: (MonadIO m) => JSElement -> m ()
focus e = liftIO (js_focus e)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => a1[\"blur\"]())($1)" js_blur :: JSElement -> IO ()
#else
foreign import javascript unsafe "((a1) => a1[\"blur\"]())" js_blur :: JSElement -> IO ()
#endif

blur :: (MonadIO m) => JSElement -> m ()
blur e = liftIO (js_blur e)

-- * textContent

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"textContent\"] = a2)($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"textContent\"] = a2)"
#endif
        js_setTextContent :: JSVal -> JSString -> IO ()

setTextContent :: (MonadIO m, IsJSNode self) =>
                  self
               -> Text
               -> m ()
setTextContent self content =
    liftIO $ (js_setTextContent (unJSNode (toJSNode self)) (textToJSString content))

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"textContent\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"textContent\"]; })"
#endif
        js_getTextContent :: JSVal -> IO JSString

getTextContent :: (MonadIO m, IsJSNode self) =>
                  self
               -> m Text
getTextContent self =
  liftIO $ (fmap textFromJSString $ js_getTextContent (unJSNode (toJSNode self)))


-- * replaceData

-- FIMXE: perhaps only a TextNode?
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3,a4) => a1[\"replaceData\"](a2, a3, a4))($1,$2,$3,$4)" js_replaceData
#else
foreign import javascript unsafe "((a1,a2,a3,a4) => a1[\"replaceData\"](a2, a3, a4))" js_replaceData
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => a1[\"remove\"]())($1)"
#else
foreign import javascript unsafe "((a1) => a1[\"remove\"]())"
#endif
        js_remove :: JSNode -> IO ()

remove :: (MonadIO m, IsJSNode self) => self -> m ()
remove self
  = liftIO (js_remove (toJSNode self))

-- * removeChild

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return (a1[\"removeChild\"](a2)); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return (a1[\"removeChild\"](a2)); })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => { return (a1[\"replaceChild\"](a2, a3)); })($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => { return (a1[\"replaceChild\"](a2, a3)); })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"replaceWith\"](a2))($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"replaceWith\"](a2))"
#endif
        js_replaceWith :: JSNode -> JSNode -> IO ()

replaceWith :: (IsJSNode oldNode, IsJSNode newNode, MonadIO m) => oldNode -> newNode -> m ()
replaceWith old new = liftIO $ js_replaceWith (toJSNode old) (toJSNode new)

-- * firstChild

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"firstChild\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"firstChild\"]; })"
#endif
        js_getFirstChild :: JSNode -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.firstChild Mozilla Node.firstChild documentation>
getFirstChild :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
getFirstChild self
  = liftIO ((js_getFirstChild ((toJSNode self))) >>= fromJSVal)

firstChild :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
firstChild = getFirstChild

-- * lastChild

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return (a1[\"lastChild\"]); })($1)"
#else
foreign import javascript unsafe "((a1) => { return (a1[\"lastChild\"]); })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"nextSibling\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"nextSibling\"]; })"
#endif
        js_nextSibling :: JSNode -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.nextSibling Mozilla Node.nextSibling documentation>
nextSibling :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
nextSibling self
  = liftIO ((js_nextSibling ((toJSNode self))) >>= fromJSVal)

-- * nextElementSibling

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"nextElementSibling\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"nextElementSibling\"]; })"
#endif
        js_nextElementSibling :: JSNode -> IO JSVal

-- | <https://developer.mozilla.org/en-us/docs/Web/API/NonDocumentTypeChildNode/nextElementSibling Mozilla nextElementSibling documentation>
nextElementSibling :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSElement)
nextElementSibling self
  = liftIO ((js_nextElementSibling ((toJSNode self))) >>= fromJSVal)


#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"previousSibling\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"previousSibling\"]; })"
#endif
        js_previousSibling :: JSNode -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.previousSibling Mozilla Node.previousSibling documentation>
previousSibling :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
previousSibling self
  = liftIO ((js_previousSibling ((toJSNode self))) >>= fromJSVal)

-- * previousElementSibling

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"previousElementSibling\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"previousElementSibling\"]; })"
#endif
        js_previousElementSibling :: JSNode -> IO JSVal

-- | <https://developer.mozilla.org/en-us/docs/Web/API/NonDocumentTypeChildNode/previousElementSibling Mozilla previousElementSibling documentation>
previousElementSibling :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSElement)
previousElementSibling self
  = liftIO ((js_previousElementSibling ((toJSNode self))) >>= fromJSVal)


#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => a1[\"setAttribute\"](a2, a3))($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => a1[\"setAttribute\"](a2, a3))"
#endif
        js_setAttribute :: JSElement -> JSString -> JSString -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.setAttribute Mozilla Element.setAttribute documentation>
setAttribute ::
             (MonadIO m) =>
               JSElement -> Text -> Text -> m ()
setAttribute self name value
  = liftIO
      (js_setAttribute self (textToJSString name) (textToJSString value))


#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return (a1[\"getAttribute\"](a2)); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return (a1[\"getAttribute\"](a2)); })"
#endif
        js_getAttribute :: JSElement -> JSString -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.setAttribute Mozilla Element.setAttribute documentation>
getAttribute :: (MonadIO m) =>
                JSElement
             -> JSString
             -> m (Maybe JSString)
getAttribute self name = liftIO (pFromJSVal <$> js_getAttribute self name)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"removeAttribute\"](a2))($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"removeAttribute\"](a2))"
#endif
        js_removeAttribute :: JSElement -> JSString -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.removeAttribute Mozilla Element.removeAttribute documentation>
removeAttribute :: (MonadIO m) =>
                JSElement -> Text -> m ()
removeAttribute self name = liftIO (js_removeAttribute self (textToJSString name))

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => a1[\"style\"][a2] = a3)($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => a1[\"style\"][a2] = a3)"
#endif
        js_setStyle :: JSElement -> JSString -> JSVal -> IO ()

setStyle :: (MonadIO m, PToJSVal v) => JSElement -> JSString -> v -> m ()
setStyle self name value
  = liftIO
      (js_setStyle self name (pToJSVal value))

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => a1[a2] = a3)($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => a1[a2] = a3)"
#endif
        js_setProperty :: JSElement -> JSString -> JSVal -> IO ()

setProperty :: (MonadIO m, PToJSVal v) => JSElement -> Text -> v -> m ()
setProperty self name value
  = liftIO
      (js_setProperty self (textToJSString name) (pToJSVal value))

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => delete a1[a2])($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => delete a1[a2])"
#endif
    js_delete :: JSVal -> JSString -> IO ()

deleteProperty :: (MonadIO m) => JSElement -> Text -> m ()
deleteProperty e n = liftIO (js_delete (unJSElement e) (textToJSString n))

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"checked\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"checked\"]; })"
#endif
  js_getChecked :: JSElement -> IO Bool

getChecked :: (MonadIO m) => JSElement -> m Bool
getChecked e = liftIO $ js_getChecked e

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"checked\"] = a2)($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"checked\"] = a2)"
#endif
        js_setChecked :: JSElement -> Bool -> IO ()

setChecked :: (MonadIO m) => JSElement -> Bool -> m ()
setChecked e b = liftIO $ js_setChecked e b


-- * Window/Element scroll position

class (PToJSVal a) => Scrollable a
instance Scrollable JSWindow
instance Scrollable JSElement

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"scrollTop\"]; })($1)" js_scrollTop ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"scrollTop\"]; })" js_scrollTop ::
#endif
        JSElement -> IO Double

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"scrollLeft\"]; })($1)" js_scrollLeft ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"scrollLeft\"]; })" js_scrollLeft ::
#endif
        JSElement -> IO Double

scrollTop :: (MonadIO m) => JSElement -> m Double
scrollTop e = liftIO $ js_scrollTop e

scrollLeft :: (MonadIO m) => JSElement -> m Double
scrollLeft e = liftIO $ js_scrollLeft e

data ScrollBehavior
     = ScrollInstant
     | ScrollSmooth
     | ScrollAuto
       deriving (Eq, Ord, Read, Show)

jstrScrollBehavior :: ScrollBehavior -> JSString
jstrScrollBehavior behavior =
  case behavior of
    ScrollInstant -> "instant"
    ScrollSmooth  -> "smooth"
    ScrollAuto    -> "auto"

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3,a4) => a1[\"scrollTo\"]({top: a2, left: a3, behavior: a4}))($1,$2,$3,$4)" js_scrollTo ::
#else
foreign import javascript unsafe "((a1,a2,a3,a4) => a1[\"scrollTo\"]({top: a2, left: a3, behavior: a4}))" js_scrollTo ::
#endif
    JSVal -> Double -> Double -> JSString -> IO ()

scrollTo :: (MonadIO m, Scrollable obj) => obj -> Double -> Double -> ScrollBehavior -> m ()
scrollTo obj top left behavior =
  liftIO $ js_scrollTo (pToJSVal obj) top left (jstrScrollBehavior behavior)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3,a4) => a1[\"scrollBy\"]({top: a2, left: a3, behavior: a4}))($1,$2,$3,$4)" js_scrollBy ::
#else
foreign import javascript unsafe "((a1,a2,a3,a4) => a1[\"scrollBy\"]({top: a2, left: a3, behavior: a4}))" js_scrollBy ::
#endif
    JSVal -> Double -> Double -> JSString -> IO ()

scrollBy :: (MonadIO m, Scrollable obj) => obj -> Double -> Double -> ScrollBehavior -> m ()
scrollBy obj top left behavior =
  liftIO $ js_scrollBy (pToJSVal obj) top left (jstrScrollBehavior behavior)


#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"scrollWidth\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"scrollWidth\"]; })"
#endif
  js_scrollWidth :: JSElement -> IO Double

scrollWidth :: (MonadIO m) => JSElement -> m Double
scrollWidth e = liftIO (js_scrollWidth e)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"scrollHeight\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"scrollHeight\"]; })"
#endif
  js_scrollHeight :: JSElement -> IO Double

scrollHeight :: (MonadIO m) => JSElement -> m Double
scrollHeight e = liftIO (js_scrollHeight e)

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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"value\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"value\"]; })"
#endif
        js_getValue :: JSNode -> IO JSString

getValue :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSString)
getValue self
  = liftIO ((js_getValue (toJSNode self)) >>= pure . Just)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"value\"] = a2)($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"value\"] = a2)"
#endif
        js_setValue :: JSNode -> JSString -> IO ()

setValue :: (MonadIO m, IsJSNode self) => self -> Text -> m ()
setValue self str =
    liftIO (js_setValue (toJSNode self) (textToJSString str))

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"hasFocus\"](); })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"hasFocus\"](); })"
#endif
        js_hasFocus :: JSDocument -> IO Bool

hasFocus :: (MonadIO m) => JSDocument -> m Bool
hasFocus doc = liftIO (js_hasFocus doc)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"matches\"](a2); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"matches\"](a2); })"
#endif
        js_matches :: JSElement -> JSString -> IO Bool

matches :: (MonadIO m) => JSElement -> Text -> m Bool
matches e selectorStr = liftIO $ (js_matches e (textToJSString selectorStr))

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"activeElement\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"activeElement\"]; })"
#endif
        js_getActiveElement :: JSDocument -> IO JSElement

getActiveElement :: (MonadIO m) => JSDocument -> m JSElement
getActiveElement d = liftIO (js_getActiveElement d)

-- * dataset

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return (a1[\"dataset\"][a2]); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return (a1[\"dataset\"][a2]); })"
#endif
        js_getData :: JSNode -> JSString -> IO (Nullable JSString)

getData :: (MonadIO m, IsJSNode self) => self -> JSString -> m (Maybe JSString)
getData self name = liftIO (nullableToMaybe <$> js_getData (toJSNode self) name)
--getData self name = liftIO (fmap fromJSVal <$> maybeJSNullOrUndefined <$> (js_getData (toJSNode self) name))

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => a1[\"dataset\"][a2] = a3)($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => a1[\"dataset\"][a2] = a3)"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => { return (a1[\"attachShadow\"]({mode: a2, delegatesFocus: a3})); })($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => { return (a1[\"attachShadow\"]({mode: a2, delegatesFocus: a3})); })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return (a1 instanceof Text); })($1)"
#else
foreign import javascript unsafe "((a1) => { return (a1 instanceof Text); })"
#endif
  js_instanceOfText :: JSVal -> Bool

instance InstanceOf JSTextNode where
  instanceOf a = js_instanceOfText (pToJSVal a)

-- * isEqualNode

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"isEqualNode\"](a2); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"isEqualNode\"](a2); })"
#endif
  js_isEqualNode :: JSNode -> JSNode -> IO Bool

isEqualNode :: (MonadIO m) => (IsJSNode obj1, IsJSNode obj2) => obj1 -> obj2 -> m Bool
isEqualNode obj1 obj2 = liftIO $ js_isEqualNode (toJSNode obj1) (toJSNode obj2)

-- * createTextNode

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"createTextNode\"](a2); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"createTextNode\"](a2); })"
#endif
        js_createTextNode :: JSDocument -> JSString -> IO JSTextNode

-- * TextNode length

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"length\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"length\"]; })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"nodeValue\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"nodeValue\"]; })"
#endif
  js_nodeValue :: JSNode -> IO JSString

nodeValue :: (MonadIO m) => JSNode -> m JSString
nodeValue node = liftIO $ js_nodeValue node


#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"nodeValue\"] = a2)($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"nodeValue\"] = a2)"
#endif
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
  = Change
  | Invalid
  | LanguageChange
  | Open
  | ReadyStateChange
  | Reset
  | Submit
  deriving (Eq, Show, Read)

instance IsEvent Event where
  eventToJSString Change            = JS.pack "change"
  eventToJSString Invalid           = JS.pack "invalid"
  eventToJSString LanguageChange    = JS.pack "languagechange"
  eventToJSString Open              = JS.pack "open"
  eventToJSString ReadyStateChange  = JS.pack "readystatechange"
  eventToJSString Reset             = JS.pack "reset"
  eventToJSString Submit            = JS.pack "submit"


type instance UniqEventName Change           = "change"
type instance UniqEventName Invalid          = "invalid"
type instance UniqEventName LanguageChange   = "languagechange"
type instance UniqEventName Open             = "open"
type instance UniqEventName ReadyStateChange = "readystatechange"
type instance UniqEventName Reset            = "reset"
type instance UniqEventName Submit           = "submit"

-- * MouseEvent

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

-- * PointerEvent

data PointerEvent
  = PointerDown
  | PointerUp
  | PointerMove
  | PointerOver
  | PointerOut
  | PointerEnter
  | PointerLeave
  | PointerCancel
  | GotPointerCapture
  | LostPointerCapture
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

type instance UniqEventName PointerDown        = "pointerdown"
type instance UniqEventName PointerUp          = "pointerup"
type instance UniqEventName PointerMove        = "pointermove"
type instance UniqEventName PointerOver        = "pointerover"
type instance UniqEventName PointerOut         = "pointerout"
type instance UniqEventName PointerEnter       = "pointerenter"
type instance UniqEventName PointerLeave       = "pointerleave"
type instance UniqEventName PointerCancel      = "pointercancel"
type instance UniqEventName GotPointerCapture  = "gotpointercapture"
type instance UniqEventName LostPointerCapture = "lostpointercapture"

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
  | BeforeInput
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance IsEvent InputEvent where
  eventToJSString Input       = JS.pack "input"
  eventToJSString BeforeInput = JS.pack "beforeinput"

type instance UniqEventName Input       = "input"
type instance UniqEventName BeforeInput = "beforeinput"

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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"data\"]; })($1)" js_inputData ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"data\"]; })" js_inputData ::
#endif
        InputEventObject ev -> Nullable JSString

inputData :: InputEventObject ev -> Maybe JSString
inputData ev = nullableToMaybe (js_inputData ev)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"dataTransfer\"]; })($1)" js_inputDataTransfer ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"dataTransfer\"]; })" js_inputDataTransfer ::
#endif
        InputEventObject ev -> Nullable DataTransfer

inputDataTransfer :: InputEventObject ev -> Maybe DataTransfer
inputDataTransfer ev = nullableToMaybe (js_inputDataTransfer ev)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"inputType\"]; })($1)" inputType ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"inputType\"]; })" inputType ::
#endif
        InputEventObject ev -> JSString

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"isComposing\"]; })($1)" isComposing ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"isComposing\"]; })" isComposing ::
#endif
        InputEventObject ev -> Bool

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return (a1[\"getTargetRanges\"]()); })($1)" js_getTargetRanges ::
#else
foreign import javascript unsafe "((a1) => { return (a1[\"getTargetRanges\"]()); })" js_getTargetRanges ::
#endif
        InputEventObject ev -> IO JSVal

getTargetRanges :: (MonadIO m) => InputEventObject ev -> m [Range]
getTargetRanges ieo = liftIO ((js_getTargetRanges ieo) >>= fromJSValUnchecked)
{-
-- * FormDataEvent

data FormDataEvent
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
-}
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


#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"key\"]; })($1)" js_key ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"key\"]; })" js_key ::
#endif
  StorageEventObject ev -> Nullable JSString

key :: StorageEventObject ev -> Maybe JSString
key e = nullableToMaybe (js_key e)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"newValue\"]; })($1)" js_newValue ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"newValue\"]; })" js_newValue ::
#endif
  StorageEventObject ev -> Nullable JSString

newValue :: StorageEventObject ev -> Maybe JSString
newValue e = nullableToMaybe (js_newValue e)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"oldValue\"]; })($1)" js_oldValue ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"oldValue\"]; })" js_oldValue ::
#endif
  StorageEventObject ev -> Nullable JSString

oldValue :: StorageEventObject ev -> Maybe JSString
oldValue e = nullableToMaybe (js_oldValue e)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"url\"]; })($1)" js_url ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"url\"]; })" js_url ::
#endif
  StorageEventObject ev -> JSString

url :: StorageEventObject ev -> JSString
url e = js_url e


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

-- * methods

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"defaultPrevented\"]; })($1)" js_defaultPrevented ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"defaultPrevented\"]; })" js_defaultPrevented ::
#endif
        EventObject ev -> IO Bool

defaultPrevented :: (IsEventObject obj, MonadIO m) => obj -> m Bool
defaultPrevented obj = liftIO (js_defaultPrevented (asEventObject obj))

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"currentTarget\"]; })($1)" js_currentTarget ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"currentTarget\"]; })" js_currentTarget ::
#endif
        EventObject ev -> EventTarget

currentTarget :: (IsEventObject obj) => obj -> EventTarget
currentTarget obj = js_currentTarget (asEventObject obj)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"target\"]})($1)" js_target ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"target\"]})" js_target ::
#endif
        EventObject ev -> EventTarget

target :: (IsEventObject obj) => obj -> EventTarget
target obj = js_target (asEventObject obj)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => a1[\"preventDefault\"]())($1)" js_preventDefault ::
#else
foreign import javascript unsafe "((a1) => a1[\"preventDefault\"]())" js_preventDefault ::
#endif
        EventObject ev -> IO ()

preventDefault :: (IsEventObject obj) => obj -> IO ()
preventDefault obj = (js_preventDefault (asEventObject obj))

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => a1[\"stopPropagation\"]())($1)" js_stopPropagation ::
#else
foreign import javascript unsafe "((a1) => a1[\"stopPropagation\"]())" js_stopPropagation ::
#endif
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

class IsMouseEventObject obj where
  asMouseEventObject :: obj -> MouseEventObject ev

instance IsMouseEventObject (MouseEventObject ev) where
  asMouseEventObject (MouseEventObject jsval) = (MouseEventObject jsval)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"clientX\"]; })($1)" clientX ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"clientX\"]; })" clientX ::
#endif
        MouseEventObject ev -> Double

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"clientY\"]; })($1)" clientY ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"clientY\"]; })" clientY ::
#endif
        MouseEventObject ev -> Double

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"button\"]; })($1)" button ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"button\"]; })" button ::
#endif
        MouseEventObject ev -> Int

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"shiftKey\"]; })($1)" mouse_shiftKey ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"shiftKey\"]; })" mouse_shiftKey ::
#endif
        MouseEventObject ev -> Bool

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"ctrlKey\"]; })($1)" mouse_ctrlKey ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"ctrlKey\"]; })" mouse_ctrlKey ::
#endif
        MouseEventObject ev -> Bool

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"altKey\"]; })($1)" mouse_altKey ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"altKey\"]; })" mouse_altKey ::
#endif
        MouseEventObject ev -> Bool

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"metaKey\"]; })($1)" mouse_metaKey ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"metaKey\"]; })" mouse_metaKey ::
#endif
        MouseEventObject ev -> Bool

instance HasModifierKeys (MouseEventObject ev) where
  shiftKey = mouse_shiftKey
  ctrlKey  = mouse_ctrlKey
  altKey   = mouse_altKey
  metaKey  = mouse_metaKey

-- * PointerEventObject

newtype PointerEventObject (ev :: PointerEvent) = PointerEventObject { unPointerEventObject :: JSVal }

instance Show (PointerEventObject ev) where
  show _ = "PointerEventObject"

instance ToJSVal (PointerEventObject ev) where
  toJSVal = return . unPointerEventObject
  {-# INLINE toJSVal #-}

instance FromJSVal (PointerEventObject ev) where
  fromJSVal = return . fmap PointerEventObject . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsEventObject (PointerEventObject ev) where
  type Ev (PointerEventObject ev) = ev
  asEventObject (PointerEventObject jsval) = EventObject jsval

instance IsMouseEventObject (PointerEventObject ev) where
  asMouseEventObject (PointerEventObject jsval) = (MouseEventObject jsval)


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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"charCode\"]; })($1)" charCode ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"charCode\"]; })" charCode ::
#endif
        (KeyboardEventObject ev) -> Int

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"keyCode\"]; })($1)" keyCode ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"keyCode\"]; })" keyCode ::
#endif
        (KeyboardEventObject ev) -> Int

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"which\"]; })($1)" which ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"which\"]; })" which ::
#endif
        (KeyboardEventObject ev) -> Int

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"repeat\"]; })($1)" repeat ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"repeat\"]; })" repeat ::
#endif
        (KeyboardEventObject ev) -> Bool

class HasModifierKeys obj where
  shiftKey :: obj -> Bool
  ctrlKey  :: obj -> Bool
  altKey   :: obj -> Bool
  metaKey  :: obj -> Bool

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"shiftKey\"]; })($1)" keyboard_shiftKey ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"shiftKey\"]; })" keyboard_shiftKey ::
#endif
        (KeyboardEventObject ev) -> Bool

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"ctrlKey\"]; })($1)" keyboard_ctrlKey ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"ctrlKey\"]; })" keyboard_ctrlKey ::
#endif
        (KeyboardEventObject ev) -> Bool

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"altKey\"]; })($1)" keyboard_altKey ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"altKey\"]; })" keyboard_altKey ::
#endif
        (KeyboardEventObject ev) -> Bool

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"metaKey\"]; })($1)" keyboard_metaKey ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"metaKey\"]; })" keyboard_metaKey ::
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"relatedTarget\"]; })($1)" js_relatedTarget ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"relatedTarget\"]; })" js_relatedTarget ::
#endif
        FocusEventObject ev -> Nullable JSElement

relatedTarget :: FocusEventObject ev -> Maybe JSElement
relatedTarget = nullableToMaybe . js_relatedTarget

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

-- * EventObjectOf

type family EventObjectOf (event :: k) :: o where
  EventObjectOf (ev :: Event)                 = EventObject ev
  EventObjectOf (ev :: AnimationEvent)        = AnimationEventObject ev
  EventObjectOf (ev :: ClipboardEvent)        = ClipboardEventObject ev
  EventObjectOf (ev :: CloseEvent)            = CloseEventObject ev
  EventObjectOf (ev :: DragEvent)             = DragEventObject ev
  EventObjectOf (ev :: FocusEvent)            = FocusEventObject ev
--  EventObject (ev :: FormEvent)             = EventObject ev
  EventObjectOf (ev :: HashChangeEvent)       = HashChangeEventObject ev
  EventObjectOf (ev :: InputEvent)            = InputEventObject ev
  EventObjectOf (ev :: KeyboardEvent)         = KeyboardEventObject ev
  EventObjectOf (ev :: MediaEvent)            = EventObject ev
  EventObjectOf (ev :: MessageEvent)          = MessageEventObject ev
  EventObjectOf (ev :: MouseEvent)            = MouseEventObject ev
  EventObjectOf (ev :: NetworkEvent)          = EventObject ev
  EventObjectOf (ev :: PageTransitionEvent)   = PageTransitionEventObject ev
  EventObjectOf (ev :: PointerEvent)          = PointerEventObject ev
  EventObjectOf (ev :: PopStateEvent)         = PopStateEventObject ev
  EventObjectOf (ev :: PrintingEvent)         = EventObject ev
  EventObjectOf (ev :: ProgressEvent)         = ProgressEventObject ev
  EventObjectOf (ev :: PromiseRejectionEvent) = PromiseRejectionEventObject ev
  EventObjectOf (ev :: ResourceEvent)         = EventObject ev
  EventObjectOf (ev :: SelectionEvent)        = EventObject ev
  EventObjectOf (ev :: StorageEvent)          = StorageEventObject ev
  EventObjectOf (ev :: TouchEvent)            = TouchEventObject ev
  EventObjectOf (ev :: TransitionEvent)       = TransitionEventObject ev
  EventObjectOf (ev :: ViewEvent)             = EventObject ev
  EventObjectOf (ev :: VDOMEvent)             = VDOMEventObject ev
  EventObjectOf (ev :: WheelEvent)            = WheelEventObject ev
  EventObjectOf e                             = CustomEventObject e (CustomEventDetail e)

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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3,a4) => { return (new CustomEvent(a1, { 'detail': a2, 'bubbles' : a3, 'cancelable' : a4})); })($1,$2,$3,$4)"
#else
foreign import javascript unsafe "((a1,a2,a3,a4) => { return (new CustomEvent(a1, { 'detail': a2, 'bubbles' : a3, 'cancelable' : a4})); })"
#endif
        js_newCustomEvent :: JSString -> JSVal -> Bool -> Bool -> IO JSVal

newCustomEventWithDetail :: (KnownSymbol (UniqEventName ev), FromJSVal (CustomEventDetail ev), ToJSVal (CustomEventDetail ev)) => EventName ev -> (CustomEventDetail ev) -> Bool -> Bool -> IO (CustomEventObject ev detail)
newCustomEventWithDetail ev detail bubbles cancelable =
  do let evStr = JS.pack $ eventName ev
     d <- toJSVal detail
     jsval <- js_newCustomEvent evStr d bubbles cancelable
     pure $ CustomEventObject jsval

newCustomEventNoDetail :: (KnownSymbol (UniqEventName ev), (CustomEventDetail ev) ~ ()) => EventName ev -> Bool -> Bool -> IO (CustomEventObject ev detail)
newCustomEventNoDetail ev bubbles cancelable =
  do let evStr = JS.pack $ eventName ev
     jsval <- js_newCustomEvent evStr jsNull bubbles cancelable
     pure $ CustomEventObject jsval

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"detail\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"detail\"]; })"
#endif
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
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3,a4) => a1[\"addEventListener\"](a2,a3,a4))($1,$2,$3,$4)"
#else
foreign import javascript unsafe "((a1,a2,a3,a4) => a1[\"addEventListener\"](a2,a3,a4))"
#endif
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
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3,a4,a5,a6) => a1['addEventListener'](a2, a3,{'capture':a4,'once':a5,'passive':a6}))($1,$2,$3,$4,$5,$6)"
#else
foreign import javascript unsafe "((a1,a2,a3,a4,a5,a6) => a1['addEventListener'](a2, a3,{'capture':a4,'once':a5,'passive':a6}))"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"dispatchEvent\"](a2); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"dispatchEvent\"](a2); })"
#endif
  js_dispatchEvent :: EventTarget -> EventObject ev -> IO ()

dispatchEvent :: (MonadIO m, IsEventTarget eventTarget, IsEventObject eventObj) => eventTarget -> eventObj -> m ()
dispatchEvent et ev = liftIO $ js_dispatchEvent (toEventTarget et) (asEventObject ev)

-- * DOMRect

newtype DOMClientRect = DOMClientRect { unDomClientRect :: JSVal }

instance PFromJSVal DOMClientRect where
  pFromJSVal = DOMClientRect
  {-# INLINE pFromJSVal #-}

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"width\"]; })($1)" width ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"width\"]; })" width ::
#endif
         DOMClientRect -> Double

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"top\"]; })($1)" rectTop ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"top\"]; })" rectTop ::
#endif
         DOMClientRect -> Double

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"left\"]; })($1)" rectLeft ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"left\"]; })" rectLeft ::
#endif
         DOMClientRect -> Double

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"right\"]; })($1)" rectRight ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"right\"]; })" rectRight ::
#endif
         DOMClientRect -> Double

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"bottom\"]; })($1)" rectBottom ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"bottom\"]; })" rectBottom ::
#endif
         DOMClientRect -> Double

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"height\"]; })($1)" height ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"height\"]; })" height ::
#endif
         DOMClientRect -> Double

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"getBoundingClientRect\"](); })($1)" js_getBoundingClientRect ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"getBoundingClientRect\"](); })" js_getBoundingClientRect ::
#endif
  JSElement -> IO DOMClientRect

getBoundingClientRect :: (MonadIO m) => JSElement -> m DOMClientRect
getBoundingClientRect = liftIO . js_getBoundingClientRect

showDOMClientRect :: DOMClientRect -> String
showDOMClientRect rect = "{ rectLeft = " ++ show (rectLeft rect) ++ " , rectTop = " ++ show (rectTop rect) ++ " , rectRight = " ++ show (rectRight rect) ++ " , rectBottom = " ++ show (rectBottom rect)  ++ " , height = " ++ show (height rect) ++ " , width = " ++ show (width rect) ++ " }"

instance Show DOMClientRect where
  show = showDOMClientRect

-- * offsetWidth

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/offsetWidth
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"offsetWidth\"]; })($1)" js_offsetWidth ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"offsetWidth\"]; })" js_offsetWidth ::
#endif
  JSElement -> IO Int

offsetWidth :: (MonadIO m) => JSElement -> m Int
offsetWidth = liftIO . js_offsetWidth

-- * ArrayBuffer <=> ByteString

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => { return a3.slice(a1, a1 + a2); })($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => { return a3.slice(a1, a1 + a2); })"
#endif
  js_bufferSlice :: Int -> Int -> ArrayBuffer -> ArrayBuffer

byteStringToArrayBuffer :: BS.ByteString -> ArrayBuffer
byteStringToArrayBuffer bs =
  let (buffer, offset, len) = Buffer.fromByteString bs
  in js_bufferSlice offset len $ Buffer.getArrayBuffer buffer

byteStringFromArrayBuffer :: ArrayBuffer -> BS.ByteString
byteStringFromArrayBuffer =
  Buffer.toByteString 0 Nothing . Buffer.createFromArrayBuffer

-- * XMLHttpRequest
newtype XMLHttpRequest = XMLHttpRequest { unXMLHttpRequest :: JSVal }

instance Eq (XMLHttpRequest) where
  (XMLHttpRequest a) == (XMLHttpRequest b) = js_eq a b

instance IsEventTarget XMLHttpRequest where
    toEventTarget = EventTarget . unXMLHttpRequest

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return (a1 instanceof XMLHttpRequest); })($1)"
#else
foreign import javascript unsafe "((a1) => { return (a1 instanceof XMLHttpRequest); })"
#endif
  js_instanceOfXMLHttpRequest :: JSVal -> Bool

instance InstanceOf XMLHttpRequest where
  instanceOf a = js_instanceOfXMLHttpRequest (pToJSVal a)

instance ToJSVal XMLHttpRequest where
  toJSVal = return . unXMLHttpRequest
  {-# INLINE toJSVal #-}

instance FromJSVal XMLHttpRequest where
  fromJSVal = return . fmap XMLHttpRequest . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return (new window[\"XMLHttpRequest\"]()); })()"
#else
foreign import javascript unsafe "(() => { return (new window[\"XMLHttpRequest\"]()); })"
#endif
        js_newXMLHttpRequest :: IO XMLHttpRequest

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest Mozilla XMLHttpRequest documentation>
newXMLHttpRequest :: (MonadIO m) => m XMLHttpRequest
newXMLHttpRequest
  = liftIO js_newXMLHttpRequest

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3,a4) => a1[\"open\"](a2, a3, a4))($1,$2,$3,$4)"
#else
foreign import javascript unsafe "((a1,a2,a3,a4) => a1[\"open\"](a2, a3, a4))"
#endif
        js_open ::
        XMLHttpRequest ->
          JSString -> JSString -> Bool -> {- JSString -> JSString -> -} IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.open Mozilla XMLHttpRequest.open documentation>
open ::
     (MonadIO m) =>
       XMLHttpRequest -> Text -> Text -> Bool -> m ()
open self method url async
  = liftIO (js_open self (textToJSString method) (textToJSString url) async)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => a1[\"setRequestHeader\"](a2,a3))($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => a1[\"setRequestHeader\"](a2,a3))"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((r) => r[\"send\"]())($1)" js_send ::
#else
foreign import javascript unsafe "((r) => r[\"send\"]())" js_send ::
#endif
        XMLHttpRequest -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest#send() Mozilla XMLHttpRequest.send documentation>
send :: (MonadIO m) => XMLHttpRequest -> m ()
send self =
    liftIO $ js_send self >> return () -- >>= throwXHRError

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((r,v) => r[\"send\"](v))($1,$2)" js_sendString ::
#else
foreign import javascript unsafe "((r,v) => r[\"send\"](v))" js_sendString ::
#endif
        XMLHttpRequest -> JSString -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest#send() Mozilla XMLHttpRequest.send documentation>
sendString :: (MonadIO m) => XMLHttpRequest -> JSString -> m ()
sendString self str =
    liftIO $ js_sendString self str >> return () -- >>= throwXHRError

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((r,v) => r[\"send\"](v))($1,$2)" js_sendArrayBuffer ::
#else
foreign import javascript unsafe "((r,v) => r[\"send\"](v))" js_sendArrayBuffer ::
#endif
        XMLHttpRequest -> ArrayBuffer -> IO ()

sendArrayBuffer :: (MonadIO m) => XMLHttpRequest -> ArrayBuffer -> m ()
sendArrayBuffer xhr buf =
  liftIO $ js_sendArrayBuffer xhr buf
{-
    liftIO $ do ref <- fmap (pToJSVal . getArrayBuffer) (ArrayBuffer.thaw buf)
                js_sendArrayBuffer xhr ref
-}
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((r,v) => r[\"send\"](v))($1,$2)" js_sendData ::
#else
foreign import javascript unsafe "((r,v) => r[\"send\"](v))" js_sendData ::
#endif
        XMLHttpRequest
    -> JSVal
    -> IO ()

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"readyState\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"readyState\"]; })"
#endif
        js_getReadyState :: XMLHttpRequest -> IO Word

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.readyState Mozilla XMLHttpRequest.readyState documentation>
getReadyState :: (MonadIO m) => XMLHttpRequest -> m Word
getReadyState self
  = liftIO (js_getReadyState self)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"responseType\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"responseType\"]; })"
#endif
        js_getResponseType ::
        XMLHttpRequest -> IO JSString -- XMLHttpRequestResponseType

-- | <Https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.responseType Mozilla XMLHttpRequest.responseType documentation>
getResponseType ::
                (MonadIO m) => XMLHttpRequest -> m Text
getResponseType self
  = liftIO (textFromJSString <$> js_getResponseType self)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"responseType\"] = a2)($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"responseType\"] = a2)"
#endif
        js_setResponseType ::
        XMLHttpRequest -> JSString -> IO () -- XMLHttpRequestResponseType

setResponseType :: (MonadIO m) =>
                   XMLHttpRequest
                -> Text
                -> m ()
setResponseType self typ =
    liftIO $ js_setResponseType self (textToJSString typ)

data XMLHttpRequestResponseType = XMLHttpRequestResponseType
                                | XMLHttpRequestResponseTypeArrayBuffer
                                | XMLHttpRequestResponseTypeBlob
                                | XMLHttpRequestResponseTypeDocument
                                | XMLHttpRequestResponseTypeJson
                                | XMLHttpRequestResponseTypeText
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return \"\"; })()"
#else
foreign import javascript unsafe "(() => { return \"\"; })"
#endif
        js_XMLHttpRequestResponseType :: JSVal -- XMLHttpRequestResponseType

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return \"arraybuffer\"; })()"
#else
foreign import javascript unsafe "(() => { return \"arraybuffer\"; })"
#endif
        js_XMLHttpRequestResponseTypeArraybuffer ::
        JSVal -- XMLHttpRequestResponseType

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return \"blob\"; })()"
#else
foreign import javascript unsafe "(() => { return \"blob\"; })"
#endif
        js_XMLHttpRequestResponseTypeBlob ::
        JSVal -- XMLHttpRequestResponseType

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return \"document\"; })()"
#else
foreign import javascript unsafe "(() => { return \"document\"; })"
#endif
        js_XMLHttpRequestResponseTypeDocument ::
        JSVal -- XMLHttpRequestResponseType

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return \"json\"; })()"
#else
foreign import javascript unsafe "(() => { return \"json\"; })"
#endif
        js_XMLHttpRequestResponseTypeJson ::
        JSVal -- XMLHttpRequestResponseType

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return \"text\"; })()"
#else
foreign import javascript unsafe "(() => { return \"text\"; })"
#endif
        js_XMLHttpRequestResponseTypeText ::
        JSVal -- XMLHttpRequestResponseType


instance ToJSVal XMLHttpRequestResponseType where
        toJSVal XMLHttpRequestResponseType
          = return js_XMLHttpRequestResponseType
        toJSVal XMLHttpRequestResponseTypeArrayBuffer
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
                return (Just XMLHttpRequestResponseTypeArrayBuffer)
            | x == js_XMLHttpRequestResponseTypeBlob =
                return (Just XMLHttpRequestResponseTypeBlob)
            | x == js_XMLHttpRequestResponseTypeDocument =
                return (Just XMLHttpRequestResponseTypeDocument)
            | x == js_XMLHttpRequestResponseTypeJson =
                return (Just XMLHttpRequestResponseTypeJson)
            | x == js_XMLHttpRequestResponseTypeText =
                return (Just XMLHttpRequestResponseTypeText)
            | otherwise = error "instance FromJSVal XMLHttpRequestResponseType"

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"response\"]; })($1)" js_getResponse
#else
foreign import javascript unsafe "((a1) => { return a1[\"response\"]; })" js_getResponse
#endif
        :: XMLHttpRequest
        -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.response Mozilla XMLHttpRequest.response documentation>
getResponse :: (MonadIO m) =>
               XMLHttpRequest
            -> m JSVal
getResponse self =
    liftIO (js_getResponse self)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"responseText\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"responseText\"]; })"
#endif
        js_getResponseText :: XMLHttpRequest -> IO JSString

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.responseText Mozilla XMLHttpRequest.responseText documentation>
getResponseText ::
                (MonadIO m) => XMLHttpRequest -> m Text
getResponseText self
  = liftIO
      (textFromJSString <$> js_getResponseText self)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.responseText Mozilla XMLHttpRequest.responseText documentation>
getResponseByteString ::
                (MonadIO m) => XMLHttpRequest -> m (Maybe BS.ByteString)
getResponseByteString self
  = liftIO $ do
     rt <- getResponseType self
     if rt == "arraybuffer"
       then do r <- js_getResponse self
               ab <- ArrayBuffer.freeze ((pFromJSVal r) :: MutableArrayBuffer)
               pure $ Just $ byteStringFromArrayBuffer ab
       else do pure Nothing

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"status\"]; })($1)" js_getStatus ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"status\"]; })" js_getStatus ::
#endif
        XMLHttpRequest -> IO Word

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.status Mozilla XMLHttpRequest.status documentation>
getStatus :: (MonadIO m) => XMLHttpRequest -> m Word
getStatus self = liftIO (js_getStatus self)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"statusText\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"statusText\"]; })"
#endif
        js_getStatusText :: XMLHttpRequest -> IO JSString

-- | <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest.statusText Mozilla XMLHttpRequest.statusText documentation>
getStatusText ::
              (MonadIO m) => XMLHttpRequest -> m Text
getStatusText self
  = liftIO
      (textFromJSString <$> js_getStatusText self)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"responseURL\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"responseURL\"]; })"
#endif
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
    do let request = WebSocketRequest { JavaScript.Web.WebSocket.url       = url'
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

instance IsMouseEventObject (DragEventObject ev) where
  asMouseEventObject (DragEventObject jsval) = (MouseEventObject jsval)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"dataTransfer\"]; })($1)" js_dataTransfer ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"dataTransfer\"]; })" js_dataTransfer ::
#endif
        DragEventObject ev -> Nullable DataTransfer

dataTransfer :: DragEventObject ev -> Maybe DataTransfer
dataTransfer ev = nullableToMaybe (js_dataTransfer ev)

-- * Selection

newtype Selection = Selection { unSelection ::  JSVal }

instance ToJSVal Selection where
  toJSVal = pure . unSelection
  {-# INLINE toJSVal #-}

instance FromJSVal Selection where
  fromJSVal = pure . fmap Selection . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

-- ** Properties

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"rangeCount\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"rangeCount\"]; })"
#endif
        js_getRangeCount :: Selection -> IO Int

getRangeCount :: (MonadIO m) => Selection -> m Int
getRangeCount selection = liftIO (js_getRangeCount selection)

rangeCount :: (MonadIO m) => Selection -> m Int
rangeCount = getRangeCount

-- ** methods

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"addRange\"](a2))($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"addRange\"](a2))"
#endif
  js_addRange :: Selection -> Range -> IO ()

addRange :: (MonadIO m) => Selection -> Range -> m ()
addRange selection range = liftIO $ (js_addRange selection range)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"anchorNode\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"anchorNode\"]; })"
#endif
  js_anchorNode :: Selection -> IO JSNode

anchorNode :: (MonadIO m) => Selection -> m JSNode
anchorNode s = liftIO (js_anchorNode s)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"anchorOffset\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"anchorOffset\"]; })"
#endif
  js_anchorOffset :: Selection -> IO Int

anchorOffset :: (MonadIO m) => Selection -> m Int
anchorOffset s = liftIO (js_anchorOffset s)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"focusNode\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"focusNode\"]; })"
#endif
  js_focusNode :: Selection -> IO JSNode

focusNode :: (MonadIO m) => Selection -> m JSNode
focusNode s = liftIO (js_focusNode s)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"focusOffset\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"focusOffset\"]; })"
#endif
  js_focusOffset :: Selection -> IO Int

focusOffset :: (MonadIO m) => Selection -> m Int
focusOffset s = liftIO (js_focusOffset s)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => a1[\"collapse\"](a2, a3))($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => a1[\"collapse\"](a2, a3))"
#endif
  js_collapse :: Selection -> JSNode -> Int -> IO ()

collapse :: (MonadIO m) => Selection -> Maybe JSNode -> Maybe Int -> m ()
collapse sel mNode mOffset = liftIO $ js_collapse sel (fromMaybe (JSNode jsNull) mNode) (fromMaybe 0 mOffset)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => a1[\"collapseToStart\"]())($1)"
#else
foreign import javascript unsafe "((a1) => a1[\"collapseToStart\"]())"
#endif
  js_collapseToStart :: Selection -> IO ()

collapseToStart :: (MonadIO m) => Selection -> m ()
collapseToStart s = liftIO (js_collapseToStart s)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => a1[\"collapseToEnd\"]())($1)"
#else
foreign import javascript unsafe "((a1) => a1[\"collapseToEnd\"]())"
#endif
  js_collapseToEnd :: Selection -> IO ()

collapseToEnd :: (MonadIO m) => Selection -> m ()
collapseToEnd s = liftIO (js_collapseToEnd s)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => a1[\"deleteFromDocument\"]())($1)"
#else
foreign import javascript unsafe "((a1) => a1[\"deleteFromDocument\"]())"
#endif
  js_deleteFromDocument :: Selection -> IO ()

deleteFromDocument :: (MonadIO m) => Selection -> m ()
deleteFromDocument sel = liftIO $ js_deleteFromDocument sel

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"isCollapsed\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"isCollapsed\"]; })"
#endif
  js_isCollapsed :: Selection -> IO Bool

isCollapsed :: (MonadIO m) => Selection -> m Bool
isCollapsed sel = liftIO $ js_isCollapsed sel

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"getRangeAt\"](a2); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"getRangeAt\"](a2); })"
#endif
        js_getRangeAt :: Selection -> Int -> IO Range

getRangeAt :: (MonadIO m) => Selection -> Int -> m Range
getRangeAt selection index = liftIO (js_getRangeAt selection index)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => a1[\"removeAllRanges\"]())($1)"
#else
foreign import javascript unsafe "((a1) => a1[\"removeAllRanges\"]())"
#endif
  js_removeAllRanges :: Selection -> IO ()

removeAllRanges :: (MonadIO m) => Selection -> m ()
removeAllRanges s = liftIO $ js_removeAllRanges s


#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2, a3, a4, a5) => a1[\"setBaseAndExtent\"](a2, a3, a4, a5))($1,$2,$3,$4,$5)"
#else
foreign import javascript unsafe "((a1,a2, a3, a4, a5) => a1[\"setBaseAndExtent\"](a2, a3, a4, a5))"
#endif
  js_setBaseAndExtent :: Selection -> JSNode -> Int -> JSNode -> Int -> IO ()

setBaseAndExtent :: (MonadIO m, IsJSNode an, IsJSNode fn) => Selection -> an -> Int -> fn -> Int -> m ()
setBaseAndExtent s an ao fn fo = liftIO $ js_setBaseAndExtent s (toJSNode an) ao (toJSNode fn) fo

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"toString\"](); })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"toString\"](); })"
#endif
 js_selectionToString :: Selection -> IO JSString

selectionToString :: (MonadIO m) => Selection -> m JSString
selectionToString s = liftIO $ js_selectionToString s

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => { return a1[\"containsNode\"](a2,a3); })($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => { return a1[\"containsNode\"](a2,a3); })"
#endif
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

instance PFromJSVal Range where
  pFromJSVal = Range
  {-# INLINE pFromJSVal #-}

instance PToJSVal Range where
  pToJSVal (Range jsval) = jsval
  {-# INLINE pToJSVal #-}

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return (new Range()); })()"
#else
foreign import javascript unsafe "(() => { return (new Range()); })"
#endif
  js_newRange :: IO Range

newRange :: (MonadIO m) => m Range
newRange = liftIO js_newRange

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"cloneContents\"](); })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"cloneContents\"](); })"
#endif
  js_cloneContents :: Range -> IO JSVal

cloneContents :: (MonadIO m) => Range -> m JSDocumentFragment
cloneContents r = liftIO $ pFromJSVal <$> js_cloneContents r

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"commonAncestorContainer\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"commonAncestorContainer\"]; })"
#endif
  js_commonAncestorContainer :: Range -> IO JSNode

commonAncestorContainer :: (MonadIO m) => Range -> m JSNode
commonAncestorContainer r = liftIO $ js_commonAncestorContainer r

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => a1[\"deleteContents\"]())($1)"
#else
foreign import javascript unsafe "((a1) => a1[\"deleteContents\"]())"
#endif
  js_deleteContents :: Range -> IO ()

deleteContents :: (MonadIO m) => Range -> m ()
deleteContents r = liftIO $ js_deleteContents r

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => a1[\"getBoundingClientRect\"]())($1)" js_getRangeBoundingClientRect ::
#else
foreign import javascript unsafe "((a1) => a1[\"getBoundingClientRect\"]())" js_getRangeBoundingClientRect ::
#endif
  Range -> IO DOMClientRect

getRangeBoundingClientRect :: (MonadIO m) => Range -> m DOMClientRect
getRangeBoundingClientRect = liftIO . js_getRangeBoundingClientRect


#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"startContainer\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"startContainer\"]; })"
#endif
        js_startContainer :: Range -> IO JSNode
{-
foreign import javascript unsafe "$[\"createRange\"]()"
  js_createRange :: JSDocument -> IO Range

createRange :: JSDocument -> IO Range
createRange d = js_createRange d
-}
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"selectNode\"](a2))($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"selectNode\"](a2))"
#endif
  js_selectNode :: Range -> JSNode -> IO ()

selectNode :: (MonadIO m, IsJSNode node) => Range -> node -> m ()
selectNode r n = liftIO (js_selectNode r (toJSNode n))

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"selectNodeContents\"](a2))($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"selectNodeContents\"](a2))"
#endif
  js_selectNodeContents :: Range -> JSNode -> IO ()

selectNodeContents :: (MonadIO m, IsJSNode node) => Range -> node -> m ()
selectNodeContents r n = liftIO (js_selectNodeContents r (toJSNode n))

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"insertNode\"](a2))($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"insertNode\"](a2))"
#endif
  js_insertNode :: Range -> JSNode -> IO ()

insertNode :: (MonadIO m, IsJSNode node) => Range -> node -> m ()
insertNode r n = liftIO (js_insertNode r (toJSNode n))

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"toString\"](); })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"toString\"](); })"
#endif
  js_rangeToString :: Range -> IO JSString

rangeToJSString :: Range -> IO JSString
rangeToJSString r = liftIO (js_rangeToString r)

startContainer :: (MonadIO m) => Range -> m JSNode
startContainer r = liftIO (js_startContainer r)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"startOffset\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"startOffset\"]; })"
#endif
        js_startOffset :: Range -> IO Int

startOffset :: (MonadIO m) => Range -> m Int
startOffset r = liftIO (js_startOffset r)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"endContainer\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"endContainer\"]; })"
#endif
        js_endContainer :: Range -> IO JSNode

endContainer :: (MonadIO m) => Range -> m JSNode
endContainer r = liftIO (js_endContainer r)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"endOffset\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"endOffset\"]; })"
#endif
        js_endOffset :: Range -> IO Int

endOffset :: (MonadIO m) => Range -> m Int
endOffset r = liftIO (js_endOffset r)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => a1[\"setStart\"](a2,a3))($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => a1[\"setStart\"](a2,a3))"
#endif
  js_setStart :: Range -> JSNode -> Int -> IO ()

setStart :: (MonadIO m, IsJSNode node) => Range -> node -> Int -> m ()
setStart r n i = liftIO $ js_setStart r (toJSNode n) i

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"setStartBefore\"](a2))($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"setStartBefore\"](a2))"
#endif
  js_setStartBefore :: Range -> JSNode -> IO ()

setStartBefore :: (MonadIO m, IsJSNode node) => Range -> node -> m ()
setStartBefore r n = liftIO $ js_setStartBefore r (toJSNode n)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"setStartAfter\"](a2))($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"setStartAfter\"](a2))"
#endif
  js_setStartAfter :: Range -> JSNode -> IO ()

setStartAfter :: (MonadIO m, IsJSNode node) => Range -> node -> m ()
setStartAfter r n = liftIO $ js_setStartAfter r (toJSNode n)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => a1[\"setEnd\"](a2,a3))($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => a1[\"setEnd\"](a2,a3))"
#endif
  js_setEnd :: Range -> JSNode -> Int -> IO ()

setEnd :: (MonadIO m, IsJSNode node) => Range -> node -> Int -> m ()
setEnd r n i = liftIO $ js_setEnd r (toJSNode n) i

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"setEndBefore\"](a2))($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"setEndBefore\"](a2))"
#endif
  js_setEndBefore :: Range -> JSNode -> IO ()

setEndBefore :: (MonadIO m, IsJSNode node) => Range -> node -> m ()
setEndBefore r n = liftIO $ js_setEndBefore r (toJSNode n)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"setEndAfter\"](a2))($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"setEndAfter\"](a2))"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"item\"](a2); })($1,$2)" js_collectionItem ::
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"item\"](a2); })" js_collectionItem ::
#endif
        HTMLCollection a -> Int -> IO (Nullable a)

collectionItem :: (MonadIO m, PFromJSVal a) => HTMLCollection a -> Int -> m (Maybe a)
collectionItem col n =
  liftIO (nullableToMaybe <$> js_collectionItem col n)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"length\"]; })($1)" js_collectionLength ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"length\"]; })" js_collectionLength ::
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"getClientRects\"](); })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"getClientRects\"](); })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[a2]})($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[a2]})"
#endif
 js_clientRectIx :: ClientRects -> Int -> IO ClientRect

clientRectIx :: (MonadIO m) => ClientRects -> Int -> m ClientRect
clientRectIx crs i = liftIO $ js_clientRectIx crs i

foreign import javascript unsafe "a1[\"length\"]"
  clientRectsLength :: ClientRects -> Int

foreign import javascript unsafe "a1[\"left\"]"
  crLeft :: ClientRect -> Int

-- crLeft :: (MonadIO m) => ClientRect -> m Int
-- crLeft cr = liftIO $ js_crLeft cr

foreign import javascript unsafe "a1[\"top\"]"
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => window[\"setTimeout\"](a1, a2))($1,$2)" js_setTimeout ::
#else
foreign import javascript unsafe "((a1,a2) => window[\"setTimeout\"](a1, a2))" js_setTimeout ::
#endif
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



#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"getData\"](a2); })($1,$2)" js_getDataTransferData ::
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"getData\"](a2); })" js_getDataTransferData ::
#endif
        DataTransfer -> JSString -> IO JSString

getDataTransferData :: -- (MonadIO m) =>
           DataTransfer
        -> JSString -- ^ format
        -> IO JSString
getDataTransferData dt format = (js_getDataTransferData dt format)

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => a1[\"setData\"](a2, a3))($1,$2,$3)" js_setDataTransferData ::
#else
foreign import javascript unsafe "((a1,a2,a3) => a1[\"setData\"](a2, a3))" js_setDataTransferData ::
#endif
        DataTransfer -> JSString -> JSString -> IO ()

setDataTransferData :: DataTransfer
                    -> JSString -- ^ format
                    -> JSString -- ^ data
                    -> IO ()
setDataTransferData dataTransfer format data_ = (js_setDataTransferData dataTransfer format data_)
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"types\"]; })($1)" js_getTypes ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"types\"]; })" js_getTypes ::
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"kind\"](); })($1)" js_dataTransferItemKind ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"kind\"](); })" js_dataTransferItemKind ::
#endif
        DataTransferItem -> JSString

dataTransferItemKind :: DataTransferItem -> Text
dataTransferItemKind dti = textFromJSString $ js_dataTransferItemKind dti

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"type\"](); })($1)" js_dataTransferItemType ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"type\"](); })" js_dataTransferItemType ::
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"clipboardData\"]; })($1)" clipboardData ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"clipboardData\"]; })" clipboardData ::
#endif
        ClipboardEventObject ev -> IO DataTransfer

-- * Event

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => { return (new Event(a1, { 'bubbles' : a2, 'cancelable' : a3})); })($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => { return (new Event(a1, { 'bubbles' : a2, 'cancelable' : a3})); })"
#endif
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

instance MkEvent Change where
  mkEvent _ jsval = EventObject jsval

instance MkEvent Input where
  mkEvent _ jsval = InputEventObject jsval

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


#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return require('jsdom'); })()"
#else
foreign import javascript unsafe "(() => { return require('jsdom'); })"
#endif
   js_requireJSDOM :: IO JSVal

requireJSDOM :: (MonadIO m) => m (Maybe JSDOM)
requireJSDOM = liftIO $ fromJSVal =<< js_requireJSDOM

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return (new a1.JSDOM(a2).window); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return (new a1.JSDOM(a2).window); })"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"srcObject\"] = a2)($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"srcObject\"] = a2)"
#endif
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

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"add\"](a2))($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"add\"](a2))"
#endif
        js_addToken1 :: DOMTokenList -> JSString -> IO ()

addToken1 :: (MonadIO m) => DOMTokenList -> JSString -> m ()
addToken1 dtl t = liftIO $ js_addToken1 dtl t

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => a1[\"remove\"](a2))($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => a1[\"remove\"](a2))"
#endif
        js_removeToken1 :: DOMTokenList -> JSString -> IO ()

removeToken1 :: (MonadIO m) => DOMTokenList -> JSString -> m ()
removeToken1 dtl t = liftIO $ js_removeToken1 dtl t

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"replace\"](a2); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"replace\"](a2); })"
#endif
        js_replaceToken :: DOMTokenList -> JSString -> JSString -> IO Bool

replaceToken :: (MonadIO m) => DOMTokenList -> JSString -> JSString -> m Bool
replaceToken dtl old new = liftIO $ js_replaceToken dtl old new

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[\"contains\"](a2); })($1,$2)"
#else
foreign import javascript unsafe "((a1,a2) => { return a1[\"contains\"](a2); })"
#endif
        js_containsToken :: DOMTokenList -> JSString -> IO Bool

containsToken :: (MonadIO m) => DOMTokenList -> JSString -> m Bool
containsToken lst tkn = liftIO $ js_containsToken lst tkn

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"classList\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"classList\"]; })"
#endif
        js_classList :: JSElement -> IO DOMTokenList

classList :: (MonadIO m) => JSElement -> m DOMTokenList
classList e = liftIO $ js_classList e

-- * CharacterData

-- | https:\/\/developer.mozilla.org\/en-US\/docs\/Web\/API\/CharacterData
class (IsJSNode obj) => CharacterData obj
instance CharacterData JSTextNode


#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"data\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"data\"]; })"
#endif
    js_data :: JSNode -> IO JSString


-- | object.data
getCharacterData :: (CharacterData obj) => obj -> IO JSString
getCharacterData o = js_data (toJSNode o)


-- * currentScript

-- FIXME: could be a more specific JSHTMLScriptElement if we had bothered to create such a thing
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"currentScript\"]; })($1)" js_currentScript ::
#else
foreign import javascript unsafe "((a1) => { return a1[\"currentScript\"]; })" js_currentScript ::
#endif
  JSDocument -> IO JSVal

currentScript :: (MonadIO m) => JSDocument -> m (Maybe JSElement)
currentScript d =
  liftIO (fromJSVal =<< js_currentScript d)

-- Instead of using (!!) in the diff/patch code, use (@@) which throws
-- custom exception PatchIndexTooLarge.

data PatchIndexTooLarge = PatchIndexTooLarge deriving Show
instance Exception PatchIndexTooLarge

(@@) :: [a] -> Int -> a
xs @@ i = maybe (throw PatchIndexTooLarge) id (atMay xs i)

-- * caretPositionFromPoint and friends

newtype CaretPos = CaretPos { unCaretPos :: JSVal } deriving Eq

instance ToJSVal CaretPos where
  toJSVal = toJSVal . unCaretPos
  {-# INLINE toJSVal #-}

instance FromJSVal CaretPos where
  fromJSVal = return . fmap CaretPos . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance PFromJSVal CaretPos where
  pFromJSVal = CaretPos
  {-# INLINE pFromJSVal #-}

instance PToJSVal CaretPos where
  pToJSVal (CaretPos jsval) = jsval
  {-# INLINE pToJSVal #-}

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return document.caretPositionFromPoint; })()"
#else
foreign import javascript unsafe "(() => { return document.caretPositionFromPoint; })"
#endif
  hasCaretPositionFromPoint :: Bool

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => { return a1[\"caretPositionFromPoint\"](a2,a3); })($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => { return a1[\"caretPositionFromPoint\"](a2,a3); })"
#endif
  caretPositionFromPoint :: JSDocument -> Double -> Double -> IO CaretPos

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"offsetNode\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"offsetNode\"]; })"
#endif
  offsetNode :: CaretPos -> JSNode

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"offset\"]; })($1)"
#else
foreign import javascript unsafe "((a1) => { return a1[\"offset\"]; })"
#endif
  offset :: CaretPos -> Int


#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return document.caretRangeFromPoint; })()"
#else
foreign import javascript unsafe "(() => { return document.caretRangeFromPoint; })"
#endif
  hasCaretRangeFromPoint :: Bool

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => { return a1[\"caretRangeFromPoint\"](a2,a3); })($1,$2,$3)"
#else
foreign import javascript unsafe "((a1,a2,a3) => { return a1[\"caretRangeFromPoint\"](a2,a3); })"
#endif
  js_caretRangeFromPoint :: JSDocument -> Double -> Double -> IO (Nullable Range)

caretRangeFromPoint :: (MonadIO m) => JSDocument -> Double -> Double -> m (Maybe Range)
caretRangeFromPoint doc x y = liftIO (nullableToMaybe <$> js_caretRangeFromPoint doc x y)


-- | uses `caretPositionFromPoint` or `caretRangeFromPoint` depending on availability.
caretFromPoint :: JSDocument -> Double -> Double -> IO (Maybe (JSNode, Int))
caretFromPoint doc x y
  | hasCaretPositionFromPoint =
      do cp <- caretPositionFromPoint doc x y
         pure (Just (offsetNode cp, offset cp))
  | hasCaretRangeFromPoint =
      do mr <- caretRangeFromPoint doc x y
         case mr of
           Nothing  -> pure Nothing
           (Just r) ->
             do sc <- startContainer r
                so <- startOffset r
                pure (Just (sc, so))
  | otherwise =
      pure Nothing
