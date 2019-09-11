{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where

import Chili.Types (MouseEvent(Click), JSDocument, JSElement(..), JSNode(..), unJSNode, currentDocument, createHTMLDocument, document, getInnerHTML, getElementById, getElementsByTagName, getLength, item, setCurrentDocument, newJSDOM, requireJSDOM, setWindow, window, newJSDocument)
import Control.Applicative (liftA2)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Trans (MonadIO(liftIO))

import Data.JSString (JSString)
import qualified Data.JSString as JS
import Data.JSString.Text (textToJSString, textFromJSString)
import Data.Text (Text)
import Data.Maybe (catMaybes)

import Dominator
import Dominator.Patch
import Dominator.Types (Attr(..), Html(..), isEqualNode)
import Dominator.HSX

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Hedgehog.Main

import Language.Haskell.HSX.QQ (hsx)


html :: (MonadGen m) => m Html
html =
  Gen.resize (Size 40) $ recursive Gen.frequency
     -- non-recursive generators
     [ (1, CData <$> Gen.text (Range.linear 5 10) Gen.ascii)
     ]
     -- recursive generators
     [ (4, do c     <- Gen.list (Range.linear 1 4) html
              tag   <- Gen.element [ "div", "span", "p"]
              attrs <- genAttrs
              pure $ Element tag attrs c)
     ]
  where
    genAttrs :: (MonadGen m) => m [Attr]
    genAttrs =
      do c <- Gen.maybe $ Gen.element [ Attr "class" "some", Attr "class" "none" ]
         s <- Gen.maybe $ Gen.element [ Attr "style" "some", Attr "style" "none" ]
         pure $ catMaybes [c,s]

recursive :: MonadGen m => ([(Int, m a)] -> m a) -> [(Int, m a)] -> [(Int, m a)] -> m a
recursive f nonrec rec =
  Gen.sized $ \n ->
    if n <= 1 then
      f nonrec
    else
      f $ nonrec ++ fmap (\(f, g) -> (f, Gen.small g)) rec

prop_Random :: Property
prop_Random =
  withTests 100 $ property $ do
   (b, a) <- forAll $ (liftA2 (,) html html)
   prop_diffPatch' b a

prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs


assertDOM :: (MonadTest m, MonadIO m) => m ()
assertDOM =
  do mDoc <- currentDocument
     case mDoc of
         Nothing -> do mJSDOM <- requireJSDOM
                       case mJSDOM of
                         Nothing -> do annotate "unable to require jsdom"
                                       failure
                         (Just jsdom) ->
                           do mWindow <- newJSDOM jsdom "<html><head></head><body></body></html>"
                              case mWindow of
                                Nothing -> do annotate "unable to require jsdom"
                                              failure
                                (Just w) ->
                                  do setWindow w
         (Just _) -> pure ()
     mWindow <- window
     case mWindow of
         Nothing -> do annotate "still no window"
                       failure
         (Just w) -> do md <- document w
                        case md of
                          Nothing -> do annotate "window does not have a document"
                                        failure
                          (Just d) -> setCurrentDocument d

{-
prop_attachByTagName :: Property
prop_attachByTagName =
  withTests 1 $ property $
    do assertDOM
       r <- liftIO $ do mdomH <- attachByTagName "body"
                        case mdomH of
                          Nothing -> pure failure
                          (Just domH) -> pure success
       r
-}

getElementByTagName :: JSDocument -> JSString -> IO (Maybe JSElement)
getElementByTagName d tagName =
  do mElems <- getElementsByTagName d tagName
     case mElems of
       Nothing -> pure Nothing
       (Just elems) ->
         do l <- getLength elems
            case l of
              0 -> pure Nothing
              _ -> do (Just e) <- item elems 0
                      pure (Just (JSElement (unJSNode e)))

prop_diffPatch :: Html -> Html -> Property
prop_diffPatch orig new =
  withTests 1 $ property $ test $
    prop_diffPatch' orig new

prop_diffPatch' :: (MonadFail m, MonadIO m, MonadTest m) => Html -> Html -> m ()
prop_diffPatch' orig new =
   do assertDOM
      (Just d) <- liftIO $ currentDocument
      d_orig <- liftIO $ createHTMLDocument d Nothing
      mdomH_orig <- liftIO $ attachByTagName d_orig "body"
      case mdomH_orig of
        Nothing     -> failure
        (Just domH_orig) ->
          do liftIO $ initView domH_orig orig
             liftIO $ updateView domH_orig new
             me <- liftIO $ getElementByTagName d_orig "body"
             case me of
                 Nothing -> do annotate "Could not find <body> element"
                               failure
                 (Just origE) ->
                   do h_orig <- liftIO $ getInnerHTML origE
--                      print h_orig
                      d_new <- liftIO $ createHTMLDocument d Nothing
                      mdomH_new <- liftIO $ attachByTagName d_new "body"
                      case mdomH_new of
                        Nothing     -> failure
                        (Just domH_new) ->
                          do liftIO $ initView domH_new new
                             me <- liftIO $ getElementByTagName d_new "body"
                             case me of
                               Nothing -> do annotate "Could not find <body> element"
                                             failure
                               (Just newE) ->
                                 do h_new <- liftIO $ getInnerHTML newE
                                    same <- isEqualNode origE newE
--                                    print h_new
                                    if same
                                      then success
                                      else do annotate $ "orig: " ++ show orig
                                              annotate $ "new: " ++ show new
                                              annotate $ "orig DOM: " ++ show h_orig
                                              annotate $ "new DOM: " ++ show h_new
                                              failure


prop_domTest :: Property
prop_domTest =
  property $ do
    do assertDOM
       success

tests :: IO Bool
tests =
  checkSequential $ Group "Dominator Diff/Patch"
        [ ("prop_DiffPatch - change text in paragraph", prop_diffPatch [hsx| <p>hello world</p> |] [hsx| <p>goodbye world</p> |])
        , ("prop_DiffPatch - change 'class' attribute", prop_diffPatch [hsx| <p class="hello">hello world</p> |] [hsx| <p class="goodbye">goodbye world</p> |])
        , ("prop_DiffPatch - delete 'class' attribute", prop_diffPatch [hsx| <p class="hello">hello world</p> |] [hsx| <p>goodbye world</p> |])
        , ("prop_DiffPatch - mix it up 1", prop_diffPatch [hsx| <div><p>a</p><p>b</p><p>c</p></div> |] [hsx| <div><p>c</p><p>b</p><p>a</p></div> |])
        , ("prop_DiffPatch - mix it up 2", prop_diffPatch [hsx| <div><p>a</p><p>b</p><p>c</p></div> |] [hsx| <div><p>a</p><p>c</p><p>b</p></div> |])
        , ("prop_DiffPatch - mix it up 3", prop_diffPatch [hsx| <div><p>a</p><p>b</p><p>c</p></div> |] [hsx| <div><p>a</p><p>bbb</p><p>c</p></div> |])
        , ("prop_Random", prop_Random)
        ]

main :: IO ()
main =
  do defaultMain [tests]
     -- v <- Gen.sample html
     -- print v
