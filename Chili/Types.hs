{-# LANGUAGE ConstrainedClassMethods, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GADTs, JavaScriptFFI, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RankNTypes #-}
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
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.JSString as JS
import Data.JSString.Text (textToJSString, textFromJSString)
import qualified Data.Text as Text
-- import GHCJS.Prim (ToJSString(..), FromJSString(..))
-- import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer)
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

newtype EIO a = EIO { eioToIO :: IO a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

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

newtype JSNode = JSNode JSVal

unJSNode (JSNode o) = o

instance ToJSVal JSNode where
  toJSVal = toJSVal . unJSNode
  {-# INLINE toJSVal #-}

instance FromJSVal JSNode where
  fromJSVal = return . fmap JSNode . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

-- * IsJSNode

class IsJSNode obj where
    toJSNode :: (IsJSNode obj) => obj -> JSNode

instance IsJSNode JSNode where
    toJSNode = id

-- * IsEventTarget

class IsEventTarget o where
    toEventTarget :: o -> EventTarget
--    toEventTarget = EventTarget


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

-- * JSDocument

newtype JSDocument = JSDocument JSVal

unJSDocument (JSDocument o) = o

instance ToJSVal JSDocument where
  toJSVal = pure . unJSDocument
  {-# INLINE toJSVal #-}

instance FromJSVal JSDocument where
  fromJSVal = pure . fmap JSDocument . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

instance IsJSNode JSDocument where
    toJSNode = JSNode . unJSDocument

instance IsEventTarget JSDocument where
    toEventTarget = EventTarget . unJSDocument

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

foreign import javascript unsafe "$1[\"focus\"]()" focus :: JSElement -> IO ()

{-
probably broken on IE9

-- * textContent

foreign import javascript unsafe "$1[\"textContent\"] = $2"
        js_setTextContent :: JSVal JSNode -> JSString -> IO ()

setTextContent :: (MonadIO m, IsJSNode self, ToJSString content) =>
                  self
               -> content
               -> m ()
setTextContent self content =
    liftIO $ (js_setTextContent (unJSNode (toJSNode self)) (toJSString content))
-}

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

-- * firstChild

foreign import javascript unsafe "$1[\"firstChild\"]"
        js_getFirstChild :: JSNode -> IO JSVal

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Node.firstChild Mozilla Node.firstChild documentation>
getFirstChild :: (MonadIO m, IsJSNode self) => self -> m (Maybe JSNode)
getFirstChild self
  = liftIO ((js_getFirstChild ((toJSNode self))) >>= fromJSVal)

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
getAttribute self name = liftIO (js_getAttribute self name >>= fromJSVal)

foreign import javascript unsafe "$1[\"removeAttribute\"]($2)"
        js_removeAttribute :: JSElement -> JSString -> IO ()

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element.removeAttribute Mozilla Element.removeAttribute documentation>
removeAttribute :: (MonadIO m) =>
                JSElement -> Text -> m ()
removeAttribute self name = liftIO (js_removeAttribute self (textToJSString name))

foreign import javascript unsafe "$1[\"style\"][$2] = $3"
        setStyle :: JSElement -> JSString -> JSString -> IO ()

foreign import javascript unsafe "$1[$2] = $3"
        js_setProperty :: JSElement -> JSString -> JSString -> IO ()

setProperty :: (MonadIO m) => JSElement -> Text -> Text -> m ()
setProperty self name value
  = liftIO
      (js_setProperty self (textToJSString name) (textToJSString value))

foreign import javascript unsafe "delete $1[$2]"
    js_delete :: JSVal -> JSString -> IO ()

deleteProperty :: (MonadIO m) => JSElement -> Text -> m ()
deleteProperty e n = liftIO (js_delete (unJSElement e) (textToJSString n))

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
  = liftIO ((js_getValue (toJSNode self)) >>= return . Just)

foreign import javascript unsafe "$1[\"value\"] = $2"
        js_setValue :: JSNode -> JSString -> IO ()

setValue :: (MonadIO m, IsJSNode self) => self -> Text -> m ()
setValue self str =
    liftIO (js_setValue (toJSNode self) (textToJSString str))

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

instance IsJSNode JSTextNode where
    toJSNode = JSNode . unJSTextNode

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

newtype EventTarget = EventTarget { unEventTarget :: JSVal }

instance Eq (EventTarget) where
  (EventTarget a) == (EventTarget b) = js_eq a b

instance ToJSVal EventTarget where
  toJSVal = return . unEventTarget
  {-# INLINE toJSVal #-}

instance FromJSVal EventTarget where
  fromJSVal = return . fmap EventTarget . maybeJSNullOrUndefined
  {-# INLINE fromJSVal #-}

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
    deriving (Eq, Ord, Show, Read)

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
    deriving (Eq, Ord, Show, Read)

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
    deriving (Eq, Ord, Show, Read)

data FocusEvent
  = Blur
  | Focus
  | FocusIn  -- bubbles
  | FocusOut -- bubbles
    deriving (Eq, Ord, Show, Read)

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
  deriving (Eq, Ord, Show, Read)

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
  deriving (Eq, Ord, Show, Read)

data PrintEvent
  = AfterPrint
  | BeforePrint
  deriving (Eq, Ord, Show, Read)

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
  deriving (Eq, Ord, Show, Read)

data ProgressEvent
  = LoadStart
  | Progress
  | ProgressAbort
  | ProgressError
  | ProgressLoad
  | Timeout
  | LoadEnd
  deriving (Eq, Ord, Show, Read)

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
  deriving (Eq, Ord, Show, Read)

data TransitionEvent
  = TransitionEnd
  deriving (Eq, Ord, Show, Read)

data ServerSentEvent
  = ServerError
  | ServerMessage
  | Open
    deriving (Eq, Ord, Show, Read)

data MiscEvent
  = MiscMessage
  | Online
  | Offline
  | PopState
  | MiscShow
  | Storage
  | Toggle
  | Wheel
  deriving (Eq, Ord, Show, Read)

data TouchEvent
  = TouchCancel
  | TouchEnd
  | TouchMove
  | TouchStart
  deriving (Eq, Ord, Show, Read)

data SelectionEvent
  = SelectionStart
  | SelectionChange
  deriving (Eq, Ord, Show, Read)

instance IsEvent SelectionEvent where
  eventToJSString SelectionStart  = JS.pack "selectionstart"
  eventToJSString SelectionChange = JS.pack "selectionchange"

-- https://developer.mozilla.org/en-US/docs/Web/API/ProgressEvent
-- data ProgressEvent =

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

foreign import javascript unsafe "$1[\"target\"]" js_target ::
        EventObject -> JSVal

target :: (IsEventObject obj) => obj -> JSElement
target obj = JSElement (js_target (asEventObject obj))


foreign import javascript unsafe "$1[\"currentTarget\"]" js_currentTarget ::
        EventObject -> JSVal

currentTarget :: (IsEventObject obj) => obj -> JSElement
currentTarget obj = JSElement (js_currentTarget (asEventObject obj))

-- target :: (IsEventObject obj, MonadIO m) => obj -> m JSElement
-- target obj = liftIO (fromJSValUnchecked =<< (js_target (asEventObject obj)))

foreign import javascript unsafe "$1[\"preventDefault\"]()" js_preventDefault ::
        EventObject -> IO ()

-- preventDefault :: (IsEventObject obj, MonadIO m) => obj -> m ()
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
-- * XMLHttpRequest
newtype XMLHttpRequest = XMLHttpRequest { unXMLHttpRequest :: JSVal }

instance Eq (XMLHttpRequest) where
  (XMLHttpRequest a) == (XMLHttpRequest b) = js_eq a b

instance IsEventTarget XMLHttpRequest where
    toEventTarget = EventTarget . unXMLHttpRequest

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


-- ** methods

foreign import javascript unsafe "$1[\"addRange\"]($2)"
  js_addRange :: Selection -> Range -> IO ()

addRange :: (MonadIO m) => Selection -> Range -> m ()
addRange selection range = liftIO $ (js_addRange selection range)

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

foreign import javascript unsafe "$1[\"toString\"]()"
  js_rangeToString :: Range -> IO JSString

selectNode :: Range -> JSNode -> IO ()
selectNode r n = js_selectNode r n

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

setStart :: (MonadIO m) => Range -> JSNode -> Int -> m ()
setStart r n i = liftIO $ js_setStart r n i

foreign import javascript unsafe "$1[\"setEnd\"]($2,$3)"
  js_setEnd :: Range -> JSNode -> Int -> IO ()

setEnd :: (MonadIO m) => Range -> JSNode -> Int -> m ()
setEnd r n i = liftIO $ js_setEnd r n i

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

-- | is this legit?
instance IsEventTarget JSNode where
  toEventTarget = EventTarget . unJSNode

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

-- * Clipboard

data ClipboardEvent
  = Copy
  | Cut
  | Paste
    deriving (Eq, Ord, Show, Read)

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
