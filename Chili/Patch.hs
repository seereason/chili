{-# LANGUAGE DataKinds, ScopedTypeVariables #-}
{-# language RankNTypes #-}
{- Apply some patches -}
module Chili.Patch where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar)
import Control.Monad (when)
import Control.Monad.Fail (MonadFail)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadIO(..))
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (unpack)
import qualified Data.Text as Text
import Chili.Diff (Patch(..), diff)
import Chili.Internal (debugStrLn, debugPrint)
import Chili.Types (Control(..), EventName(..), Html(..), Attr(..), JSDocument, JSElement(..), JSNode, Loop, VDOMEvent(..), WithModel, addEventListener, childNodes, createJSElement, createJSTextNode, item, js_setTimeout, eventName, getFirstChild, getLength, replaceData, setAttribute, setProperty, unJSNode, setValue, parentNode, removeChild, replaceChild, toJSNode, appendChild, descendants, nodeType, currentDocument, newEvent, dispatchEvent)
import Chili.TDVar (TDVar, readTDVar, cleanTDVar, isDirtyTDVar)
import GHCJS.Foreign.Callback (OnBlocked(..), Callback, asyncCallback, asyncCallback1, syncCallback1)

renderHtml :: (MonadIO m, MonadFail m) => Loop -> TDVar model -> (remote -> IO ()) -> TMVar (Html model) -> JSDocument -> JSNode -> Html model -> ((remote -> IO ()) -> model -> Html model) -> m (Maybe JSNode)
renderHtml loop model sendWS htmlV doc body (Cntl (Control cmodel cinit cview) eventType eventHandler) view =
  do (Just cBody) <- fmap toJSNode <$> createJSElement doc (Text.pack "span")
     tid <- liftIO $ forkIO (loop doc cBody cmodel cinit Nothing (\_ _ _ -> pure ()) cview >> pure ())
     addEventListener cBody eventType (\e -> handleAndUpdate eventHandler e model) False
     pure (Just cBody)
     where
       handleAndUpdate eventHandler e model =
        do eventHandler e model
           updateView loop model sendWS htmlV doc body view -- or should this be cview?

renderHtml _ _ _ _ doc _ (CData t) _ = fmap (fmap toJSNode) $ createJSTextNode doc t
renderHtml loop model sendWS htmlV doc body (Element tag attrs children) view =
    do me <- createJSElement doc tag
       case me of
         Nothing -> return Nothing
         (Just e) ->
             do mapM_ (\c -> appendChild e =<< renderHtml loop model sendWS htmlV doc body c view) children
                mapM_ (doAttr e) attrs
                return (Just $ toJSNode e)
    where
      doAttr elem (Attr k v)   = setAttribute elem k v
      doAttr elem (Prop k v)   = setProperty elem k v
      doAttr elem (OnCreate f) = liftIO $ do cb <- asyncCallback $ f elem model
                                             js_setTimeout cb 0
      doAttr elem (EL eventType eventHandler) = do
        liftIO $ debugStrLn $ "Adding event listener for " ++ eventName eventType
        addEventListener elem eventType (\e -> {- putStrLn "eventHandler start" >> -} (handleAndUpdate eventHandler e model) {- >> putStrLn "eventHandler end"-}) False

      handleAndUpdate eventHandler e model =
        do -- putStrLn "before eventHandler"
           eventHandler e model
           -- putStrLn "after eventHandler"
           updateView loop model sendWS htmlV doc body view
{-
           dirty <- atomically $ isDirtyTDVar model
           if not dirty
             then pure ()
             else do -- putStrLn "handleAndUpdate"
                     atomically $ cleanTDVar model
                     oldHtml <- atomically $ takeTMVar htmlV
                     model' <- atomically $ readTDVar model
                     let newHtml = view sendWS model'
                         patches = diff oldHtml (Just newHtml)
                     apply loop model sendWS htmlV doc body view body oldHtml patches
                     atomically $ putTMVar htmlV newHtml
                     pure ()
-}
updateView :: Loop -> TDVar model -> (remote -> IO ()) -> TMVar (Html model) -> JSDocument -> JSNode -> ((remote -> IO ()) -> model -> Html model) -> IO ()
updateView loop model sendWS htmlV doc body view = do
           dirty <- atomically $ isDirtyTDVar model
           if not dirty
             then do debugStrLn "not dirty"
                     pure ()
             else do -- putStrLn "handleAndUpdate"
                     debugStrLn "dirty -- update view"
                     (oldHtml, model') <- atomically $
                       do cleanTDVar model
                          oldHtml <- takeTMVar htmlV
                          model' <- readTDVar model
                          pure (oldHtml, model')
                     let newHtml = view sendWS model'
                         patches = diff oldHtml (Just newHtml)
                     debugStrLn $ "oldHtml = " ++ show oldHtml
                     debugStrLn $ "newHtml = " ++ show newHtml
                     debugStrLn $ "patches = " ++ show patches -- (oldHtml, newHtml, patches)
--                     debugPrint (oldHtml, newHtml, patches)
                     apply loop model sendWS htmlV doc body view body oldHtml patches
                     atomically $ putTMVar htmlV newHtml
                     (Just node) <- getFirstChild body
                     vdomEventObject <- newEvent (EventName :: EventName Redrawn) True True
                     dispatchEvent node vdomEventObject
                     pure ()

apply :: Loop
      -> TDVar model -- ((model -> IO model) -> IO ())
      -> (remote -> IO ())
      -> TMVar (Html model)
      -> JSDocument
      -> JSNode
      -> ((remote -> IO ()) -> model -> Html model)
      -> JSNode
      -> Html model
      -> Map Int [Patch model]
      -> IO JSNode
apply loop model sendWS htmlV document body view rootNode vdom patches =
    do let indices = Map.keys patches
       case indices of
        [] -> pure rootNode
        _ -> do -- debugStrLn $ "indices (keys) = " ++ show indices
                -- debugStrLn $ "apply = " ++ show patches
                -- putStrLn $ "apply = " ++ show patches
                (Just first) <- getFirstChild rootNode -- FIXME: handle Nothing
                nodeList <- getNodes first vdom indices
                -- debugStrLn $ "nodeList length = " ++ show (length nodeList)
                mapM_ (apply' loop model sendWS htmlV document body view patches) nodeList
                return rootNode

apply' :: Loop
       -> TDVar model -- ((model -> IO model) -> IO ())
       -> (remote -> IO ())
       -> TMVar (Html model)
       -> JSDocument
       -> JSNode
       -> ((remote -> IO ()) -> model -> Html model)
       -> Map Int [Patch model]
       -> (Int, JSNode)
       -> IO ()
apply' loop model sendWS htmlV document body view patchMap (index, node) = do
    debugStrLn $ "apply' with index = " ++ show index
    case Map.lookup index patchMap of
      (Just patches) ->
          mapM_ (apply'' loop model sendWS htmlV document body view node) patches
      Nothing -> error $ "Y NO PATCH? " ++ show index

apply'' :: Loop
        -> TDVar model -- ((model -> IO model) -> IO ())
        -> (remote -> IO ())
        -> TMVar (Html model)
        -> JSDocument
        -> JSNode
        -> ((remote -> IO ()) -> model -> Html model)
        -> JSNode
        -> Patch model
        -> IO ()
apply'' loop model sendWS htmlV document body view node patch =
    case patch of
      (VText t) -> do nt <- nodeType node
                      case nt of
                        -- if the existing node is text we can just replace the text content
                        3 -> do oldLength <- getLength node
                                -- debugStrLn $  "replaceData(0" ++ ", " ++ show oldLength ++ ", " ++ unpack t ++ ")"
                                replaceData node 0 oldLength t -- (escape b t)
                        -- otherwise we need to replace the node entirely
                        _ -> do mparent <- parentNode node
                                case mparent of
                                  Nothing -> debugStrLn $ "Can't replaceChild because there is no parentNode"
                                  (Just parent) ->
                                    do debugStrLn "replacing old node with new text"
                                       -- (Just newChild) <- renderHtml loop model sendWS htmlV document body newElem view
                                       (Just doc) <- currentDocument
                                       (Just newChild) <- createJSTextNode doc t
                                       replaceChild parent newChild node
                                       return ()

      (Props newProps) -> -- FIXME: doesn't handle changes to events.
          do let e = JSElement $ unJSNode node
             debugStrLn $ "set Attr: " ++ show [ (k,v) | Attr k v <- newProps ]
             debugStrLn $ "set Prop: " ++ show [ (k,v) | Prop k v <- newProps ]
             mapM_ (\(k, v) ->
                        case (unpack k) of
--                          "value" -> setValue e v -- FIXME: this causes issues with the cursor position
                          _ -> do setAttribute e k v) [ (k,v) | Attr k v <- newProps ]
             mapM_ (\(k, v) ->
                        case (unpack k) of
--                          "value" -> setValue e v -- FIXME: this causes issues with the cursor position
                          _ -> setProperty e k v) [ (k,v) | Prop k v <- newProps ]
      (Insert elem) ->
          -- FIXME: don't get parent?
          do debugStrLn "apply'': Insert"
             mparent <- parentNode node
             case mparent of
               Nothing -> pure () -- debugStrLn $ "Can't appendChild because there is no parentNode"
               (Just parent) ->
                   do -- debugStrLn $  "Insert --> " ++ show elem
                      child <- renderHtml loop model sendWS htmlV document body elem view
                      appendChild node child
                      return ()
      Remove ->
          do mparent <- parentNode node
             case mparent of
               Nothing -> pure () -- debugStrLn $ "Can't removeChild because there is no parentNode"
               (Just parent) ->
                   do removeChild parent (Just node)
                      return ()
      VNode newElem ->
          do debugStrLn "VNode"
             mparent <- parentNode node
             case mparent of
               Nothing -> debugStrLn $ "Can't replaceChild because there is no parentNode"
               (Just parent) ->
                   do debugStrLn "replacing old node with new"
                      (Just newChild) <- renderHtml loop model sendWS htmlV document body newElem view
                      replaceChild parent newChild node
                      return ()

escape _ t = t

{-
How this works:

We want to use in-order numbering. This basically means you start at
the top of the document and work your way towards the bottom numbering
each element/cdata/ctrl as you go.

We start with:

 - a reference to the root node
 - a virtual dom that matches the current DOM
 - a list of the node indices we want

We return an assoc list of the (indices, node)



-- FIXME: do not walk down DOM trees that contain no nodes if interest
-}
getNodes :: JSNode      -- ^ root node of DOM
         -> Html model -- ^ virtual DOM that matches the current DOM
         -> [Int]       -- ^ nodes indices we want (using in-order numbering)
         -> IO [(Int, JSNode)]
getNodes currNode vdom nodeIndices = do
    debugStrLn $ "getNodes = " ++ show nodeIndices
    evalStateT (getNodes' currNode vdom nodeIndices) 0
    where
      inc :: (MonadIO m) => StateT Int m Int
      inc =
          do i <- get
             let i' = i + 1
             put i'
             return i'

      getNodes' :: JSNode
                -> Html model
                -> [Int]
                -> StateT Int IO [(Int, JSNode)]
      -- if we are not looking for any more indices then we are done
      getNodes' _ _ [] = return []
      getNodes' currNode node@(Cntl {}) is@(i:_) = liftIO (debugPrint is) >> pure [] -- FIXME: surely not right
      -- if the current vdom node is CData, then it can match at most 1 index
      getNodes' currNode node@(CData _) (i:is) =
          do index <- get
--             liftIO $ debugStrLn $ "CDATA index = " ++ show index
             liftIO $ debugStrLn $ "CData index = " ++ show index ++ " looking for i = " ++ show i
             case () of
               () | i < index -> getNodes' currNode node is
                  | i == index ->
                      do liftIO $ debugStrLn $ "match CDATA on index = " ++ show index
                         return [(i, currNode)]
                  | otherwise ->
                     return []

      getNodes' currNode vdom@(Element _tag _attrs children) is'' =
          do liftIO $ debugStrLn $ "getNodes _tag = " ++ show _tag ++ " is'' = " ++ show is''
             let count = descendants children
             index <- get
             -- get the subset of indices which match this node or children of this node
             case getInRange index count is'' of
               [] -> do liftIO $ debugStrLn $ "getInRange index = " ++ show index ++ " count = " ++ show count ++ " is''= " ++ show is'' --
                        return []
               (i:is) ->
                   do liftIO $ debugStrLn $ "inRange = " ++ show (i:is)
                      liftIO $ debugStrLn $ "Element index = " ++ show index ++ " looking for i = " ++ show i
                      when (i == index) $ liftIO $ debugStrLn $ "match Element on index = " ++ show index
                      -- get the direct children of this parent node
                      cs <- childNodes currNode
                      -- get the number of direct children
                      l <- fromIntegral <$> getLength cs
                      liftIO $ debugStrLn $ "l = "    ++ show l
                      liftIO $ debugStrLn $ "vdom = " ++ show vdom
                      let is' = if (i == index) then is else (i:is)
                      liftIO $ debugStrLn $ "is' = " ++ show is'
                      childNodes' <- mapM (\i' -> do (Just c) <- item cs (fromIntegral i')
                                                     liftIO $ debugStrLn $ "l = " ++ show l ++ ", length children = " ++ show (length children) ++ ", i' = " ++ show i'
                                                     inc
                                                     getNodes' c (children!!i') is'
                                          ) [0..(l-1)]
                      if (i == index)
                      then do return $ (i, currNode) : (concat childNodes')
                      else return (concat childNodes')
      getInRange index count indexes =
          {- takeWhile (\i -> i <= index + count) $ -} dropWhile (\i -> i < index) indexes
