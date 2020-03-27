module Chili.Canvas where

import Chili.Types as Chili
import Chili.Canvas.Types as Canvas
import Chili.Canvas.Image as Canvas
import Chili.Canvas.ImageData (ImageData, putImageData)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO(..))
import Data.Maybe (fromJust, fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.JSString as JS
import Data.JSString.Text (textToJSString, textFromJSString)
import qualified Data.Text as Text
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Types (IsJSVal(..), JSVal(..), JSString(..))


foreign import javascript unsafe "$1[\"getContext\"](\"2d\")"
        js_getContext2d ::
        JSElement -> IO JSVal

getContext2D :: (MonadIO m) => JSElement -> m (Maybe JSContext2D)
getContext2D elem = liftIO $ fromJSVal =<< js_getContext2d elem

-- * Canvas
foreign import javascript unsafe "$1[\"fillRect\"]($2, $3, $4, $5)"
        js_fillRect ::
        JSContext2D -> Double -> Double -> Double -> Double -> IO ()

fillRect :: JSContext2D -> Double -> Double -> Double -> Double -> IO ()
fillRect = js_fillRect


foreign import javascript unsafe "$1[\"clearRect\"]($2, $3, $4, $5)"
        js_clearRect ::
        JSContext2D -> Double -> Double -> Double -> Double -> IO ()

clearRect :: JSContext2D -> Double -> Double -> Double -> Double -> IO ()
clearRect = js_clearRect

renderColor :: Color -> JSString
renderColor (ColorName c) = c
renderColor (RGBA _ _ _ _) = error "Chili.Canvas.renderColor (RGBA _ _ _ _)"

renderStyle :: Style -> JSString
renderStyle (StyleColor color) = renderColor color
renderStyle (StyleGradient _) = error "Chili.Canvas.renderStyle (StyleGradient _)"
renderStyle (StylePattern _) = error "Chili.Canvas.renderStyle (StylePattern _)"

foreign import javascript unsafe "$1[\"fillStyle\"] = $2"
        js_fillStyle ::
        JSContext2D -> JSString -> IO ()

setFillStyle :: JSContext2D -> Style -> IO ()
setFillStyle ctx style = js_fillStyle ctx (renderStyle style)

foreign import javascript unsafe "$1[\"strokeStyle\"] = $2"
        js_strokeStyle ::
        JSContext2D -> JSString -> IO ()

setStrokeStyle :: JSContext2D -> Style -> IO ()
setStrokeStyle ctx style = js_strokeStyle ctx (renderStyle style)

foreign import javascript unsafe "$1[\"save\"]()"
        js_save ::
        JSContext2D -> IO ()

save :: (MonadIO m) => JSContext2D -> m ()
save = liftIO . js_save

foreign import javascript unsafe "$1[\"restore\"]()"
        js_restore ::
        JSContext2D -> IO ()

restore :: (MonadIO m) => JSContext2D -> m ()
restore = liftIO . js_restore

foreign import javascript unsafe "$1[\"moveTo\"]($2, $3)"
        js_moveTo ::
        JSContext2D -> Double -> Double -> IO ()

moveTo :: (MonadIO m) => JSContext2D -> Double -> Double -> m ()
moveTo ctx x y = liftIO $ js_moveTo ctx x y

foreign import javascript unsafe "$1[\"lineTo\"]($2, $3)"
        js_lineTo ::
        JSContext2D -> Double -> Double -> IO ()

lineTo :: (MonadIO m) => JSContext2D -> Double -> Double -> m ()
lineTo ctx x y = liftIO $ js_lineTo ctx x y


foreign import javascript unsafe "$1[\"arc\"]($2, $3, $4, $5, $6, $7)"
        js_arc ::
        JSContext2D -> Double -> Double -> Double -> Double -> Double -> Bool -> IO ()

arc :: (MonadIO m) => JSContext2D -> Double -> Double -> Double -> Double -> Double -> Bool -> m ()
arc ctx x y radius startAngle endAngle counterClockwise = liftIO $ js_arc ctx x y radius startAngle endAngle counterClockwise

foreign import javascript unsafe "$1[\"beginPath\"]()"
        js_beginPath ::
        JSContext2D -> IO ()

beginPath :: (MonadIO m) => JSContext2D -> m ()
beginPath = liftIO . js_beginPath

foreign import javascript unsafe "$1[\"stroke\"]()"
        js_stroke ::
        JSContext2D -> IO ()

stroke :: (MonadIO m) => JSContext2D -> m ()
stroke = liftIO . js_stroke

foreign import javascript unsafe "$1[\"fill\"]()"
        js_fill ::
        JSContext2D -> IO ()

fill :: (MonadIO m) => JSContext2D -> m ()
fill = liftIO . js_fill

foreign import javascript unsafe "$1[\"lineWidth\"] = $2"
        js_setLineWidth ::
        JSContext2D -> Double -> IO ()

setLineWidth :: (MonadIO m) => JSContext2D -> Double -> m ()
setLineWidth ctx w = liftIO $ js_setLineWidth ctx w


-- * Font/Text

foreign import javascript unsafe "$1[\"font\"] = $2"
        js_font ::
        JSContext2D -> JSString -> IO ()

setFont :: (MonadIO m) => JSContext2D -> JSString -> m ()
setFont ctx font = liftIO $ js_font ctx font

foreign import javascript unsafe "$1[\"textAlign\"] = $2"
        js_textAlign ::
        JSContext2D -> JSString -> IO ()

data TextAlign
  = AlignStart
  | AlignEnd
  | AlignLeft
  | AlignCenter
  | AlignRight
    deriving (Eq, Ord, Show, Read)

textAlignToJSString :: TextAlign -> JSString
textAlignToJSString AlignStart  = JS.pack "start"
textAlignToJSString AlignEnd    = JS.pack "end"
textAlignToJSString AlignLeft   = JS.pack "left"
textAlignToJSString AlignCenter = JS.pack "center"
textAlignToJSString AlignRight  = JS.pack "right"

setTextAlign :: (MonadIO m) => JSContext2D -> TextAlign -> m ()
setTextAlign ctx align = liftIO $ js_textAlign ctx (textAlignToJSString align)

foreign import javascript unsafe "$1[\"fillText\"]($2, $3, $4)"
  js_fillText :: JSContext2D -> JSString -> Double -> Double -> IO ()

foreign import javascript unsafe "$1[\"fillText\"]($2, $3, $4, $5)"
        js_fillTextMaxWidth ::
        JSContext2D -> JSString -> Double -> Double -> Double -> IO ()

fillText :: (MonadIO m) =>
            JSContext2D
         -> JSString
         -> Double
         -> Double
         -> Maybe Double
         -> m ()
fillText ctx txt x y Nothing = liftIO $ js_fillText ctx txt x y
fillText ctx txt x y (Just maxWidth) = liftIO $ js_fillTextMaxWidth ctx txt x y maxWidth


-- * Various Transformations

foreign import javascript unsafe "$1[\"scale\"]($2, $3)"
  js_scale :: JSContext2D -> Double -> Double -> IO ()

scale :: (MonadIO m) => JSContext2D -> Double -> Double -> m ()
scale ctx x y = liftIO $ js_scale ctx x y

foreign import javascript unsafe "$1[\"rotate\"]($2)"
  js_rotate :: JSContext2D -> Double -> IO ()

-- | apply rotation to commands that draw on the canvas
rotate :: (MonadIO m) =>
         JSContext2D -- ^ canvas to affect
      -> Double -- ^ rotation in radians
      -> m ()
rotate ctx r = liftIO $ js_rotate ctx r

foreign import javascript unsafe "$1[\"translate\"]($2, $3)"
  js_translate :: JSContext2D -> Double -> Double -> IO ()

-- | apply translation to commands that draw on the canvas
translate :: (MonadIO m) =>
             JSContext2D -- ^ canvas
          -> Double -- ^ x translation
          -> Double -- ^ y translation
          -> m ()
translate ctx x y = liftIO $ js_translate ctx x y

data Gradient = Gradient
    deriving (Eq, Ord, Show, Read)
data Pattern = Pattern
    deriving (Eq, Ord, Show, Read)

type Percentage = Double
type Alpha = Double

data Color
  = ColorName JSString
  | RGBA Percentage Percentage Percentage Alpha
    deriving (Eq, Ord, Show, Read)

data Style
  = StyleColor Color
  | StyleGradient Gradient
  | StylePattern Pattern
    deriving (Eq, Ord, Show, Read)

data Rect
  = Rect { _rectX      :: Double
         , _rectY      :: Double
         , _rectWidth  :: Double
         , _rectHeight :: Double
         }
  deriving (Eq, Ord, Show, Read)

-- https://developer.mozilla.org/en-US/docs/Web/API/Path2D
data Path2D
  = MoveTo Double Double
  | LineTo Double Double
  | PathRect Rect
  | Arc Double Double Double Double Double Bool
    deriving (Eq, Ord, Show, Read)

data Draw
  = FillRect Rect
  | ClearRect Rect
  | Stroke [Path2D]
  | Fill [Path2D]
  | FillText JSString Double Double (Maybe Double)
  | DrawImage Image Int Int
  | PutImageData ImageData Int Int
--    deriving (Eq, Ord, Show, Read)

data Context2D
  = FillStyle Style
  | StrokeStyle Style
  | LineWidth Double
  | Font JSString
  | TextAlign TextAlign
  | Scale Double Double
  | Translate Double Double
  | Rotate Double
    deriving (Eq, Read, Show)

-- | this is not sustainable. A Set of attributes is probably a better choice

data Canvas = Canvas
  { _canvasId :: Text
  , _canvas :: Canvas2D
  }
--  deriving (Eq, Show, Read)

data Canvas2D
  = WithContext2D [Context2D] [ Canvas2D ]
  | Draw Draw
--  deriving (Eq, Show, Read)

mkPath :: (MonadIO m) => JSContext2D -> [Path2D] -> m ()
mkPath ctx segments =
  do beginPath ctx
     mapM_ (mkSegment ctx) segments
  where
    mkSegment ctx segment =
      case segment of
       (MoveTo x y) -> moveTo ctx x y
       (LineTo x y) -> lineTo ctx x y
       (Arc x y radius startAngle endAngle counterClockwise) -> arc ctx x y radius startAngle endAngle counterClockwise
       (PathRect (Rect x y w h)) -> error "mkPath"


-- https://gist.github.com/joubertnel/870190
drawCanvas :: Canvas -> IO ()
drawCanvas (Canvas cid content) =
  do (Just document) <- currentDocument
     mCanvasElem <- getElementById document (textToJSString cid)
     case mCanvasElem of
      Nothing       -> pure ()
      (Just canvasElem) ->
           -- http://www.html5rocks.com/en/tutorials/canvas/hidpi/
           -- NOTE: backingStorePixelRatio is deprecated, we just ignore it
        do rescaleCanvas <- do ms <- Chili.getData canvasElem (JS.pack "rescale")
                               case ms of
                                Nothing -> pure True
                                (Just s) ->
                                  case (JS.unpack s) of
                                    "true" -> pure True
                                    _ -> pure False
           ratio <- fmap (fromMaybe 1) (devicePixelRatio . fromJust =<< window)
           (w, h) <-
              if rescaleCanvas
                 then do (Just oldWidth)  <- fmap (read . JS.unpack) <$> getAttribute canvasElem (JS.pack "width")
                         (Just oldHeight) <- fmap (read . JS.unpack) <$> getAttribute canvasElem (JS.pack "height")
                         js_setAttribute canvasElem (JS.pack "width")  (JS.pack $ show $ oldWidth * ratio)
                         js_setAttribute canvasElem (JS.pack "height") (JS.pack $ show $ oldHeight * ratio)
                         setStyle canvasElem (JS.pack "width")  (JS.pack  $ (show oldWidth) ++ "px")
                         setStyle canvasElem (JS.pack "height")  (JS.pack $ (show oldHeight) ++ "px")
                         setData canvasElem (JS.pack "rescale") (JS.pack "false")
                         pure (oldWidth * ratio, oldHeight * ratio)
                 else do (Just width)  <- fmap (read . JS.unpack) <$> getAttribute canvasElem (JS.pack "width")
                         (Just height) <- fmap (read . JS.unpack) <$> getAttribute canvasElem (JS.pack "height")
                         pure (width, height)
           mctx <- getContext2D canvasElem
           case mctx of
            Nothing -> pure ()
            (Just ctx) -> do
              when (rescaleCanvas) (scale ctx ratio ratio)
              clearRect ctx 0 0 w h
              drawCanvas' ctx content
  where
    drawCanvas' ctx (Draw (FillRect (Rect x y w h))) =
      fillRect ctx x y w h
    drawCanvas' ctx  (Draw (ClearRect (Rect x y w h))) =
      clearRect ctx x y w h
    drawCanvas' ctx  (Draw (Stroke path2D)) =
      do mkPath ctx path2D
         stroke ctx
    drawCanvas' ctx  (Draw (Fill path2D)) =
      do mkPath ctx path2D
         fill ctx
    drawCanvas' ctx  (Draw (FillText text x y maxWidth)) =
      do fillText ctx text x y maxWidth
    drawCanvas' ctx  (Draw (DrawImage img dx dy)) =
      do drawImage ctx img dx dy
    drawCanvas' ctx  (Draw (PutImageData imgData dx dy)) =
      do putImageData ctx imgData dx dy
    drawCanvas' ctx  (WithContext2D ctx2d content) =
     do save ctx
        mapM_ (setContext2D ctx) ctx2d
        mapM_ (drawCanvas' ctx) content
        restore ctx
        where
          setContext2D ctx op =
            case op of
             (FillStyle style)   -> setFillStyle ctx style
             (StrokeStyle style) -> setStrokeStyle ctx style
             (LineWidth w)       -> setLineWidth ctx w
             (Font font)         -> setFont ctx font
             (TextAlign a)       -> setTextAlign ctx a
             (Scale x y)         -> scale ctx x y
             (Translate x y)     -> translate ctx x y
             (Rotate r )         -> rotate ctx r
