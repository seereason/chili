{-# LANGUAGE ScopedTypeVariables #-}
{-# language RankNTypes #-}
{- Apply some patches -}
module Chili.Patch where

import Control.Monad (when)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadIO(..))
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (unpack)
import Chili.Diff (Patch(..))
import Chili.Types (Html(..), Attr(..), JSDocument, JSElement(..), JSNode, Loop, WithModel, childNodes, item, getFirstChild, getLength, renderHtml, replaceData, setAttribute, setProperty, unJSNode, setValue, parentNode, removeChild, replaceChild, appendChild, descendants)

-- | change implementation to 'putStrLn s' to enable debug output
debugStrLn s = pure ()

apply :: Loop
      -> WithModel model -- ((model -> IO model) -> IO ())
      -> JSDocument
      -> JSNode
      -> Html model
      -> Map Int [Patch model]
      -> IO JSNode
apply loop withModel document rootNode vdom patches =
    do let indices = Map.keys patches
       case indices of
        [] -> pure rootNode
        _ -> do -- debugStrLn $ "indices (keys) = " ++ show indices
                -- debugStrLn $ "apply = " ++ show patches
                (Just first) <- getFirstChild rootNode -- FIXME: handle Nothing
                nodeList <- getNodes first vdom indices
                -- debugStrLn $ "nodeList length = " ++ show (length nodeList)
                mapM_ (apply' loop withModel document patches) nodeList
                return rootNode

apply' :: Loop
       -> WithModel model -- ((model -> IO model) -> IO ())
       -> JSDocument
       -> Map Int [Patch model]
       -> (Int, JSNode)
       -> IO ()
apply' loop withModel document patchMap (index, node) = do
    debugStrLn $ "apply' with index = " ++ show index
    case Map.lookup index patchMap of
      (Just patches) ->
          mapM_ (apply'' loop withModel document node) patches
      Nothing -> error $ "Y NO PATCH? " ++ show index

apply'' :: Loop
        -> WithModel model -- ((model -> IO model) -> IO ())
        -> JSDocument
        -> JSNode
        -> Patch model
        -> IO ()
apply'' loop withModel document node patch =
    case patch of
      (VText t) -> do oldLength <- getLength node
                      -- debugStrLn $  "replaceData(0" ++ ", " ++ show oldLength ++ ", " ++ unpack t ++ ")"
                      replaceData node 0 oldLength t -- (escape b t)
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
                      child <- renderHtml loop withModel document elem
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
                      (Just newChild) <- renderHtml loop withModel document newElem
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
      getNodes' currNode node@(Cntl {}) is@(i:_) = liftIO (print is) >> pure [] -- FIXME: surely not right
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
