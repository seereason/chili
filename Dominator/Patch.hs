{-# LANGUAGE ScopedTypeVariables #-}
{-# language RankNTypes #-}
{- Apply some patches -}
module Dominator.Patch where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (takeMVar, putMVar)
import Control.Monad (when)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadIO(..))
import Chili.Debug (Debug)
import Chili.Types ((@@), JSDocument, JSElement(..), JSNode, JSNodeList, PatchIndexTooLarge(..), unJSNode, addEventListener, addEventListenerOpt, currentDocument, childNodes, deleteProperty, eventName, getFirstChild, getLength, item, parentNode, nodeType, item, toJSNode, removeAttribute, removeChild, replaceData, replaceChild, setAttribute, setProperty, insertBefore)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Dominator.Diff (Move(..), Moves, Patch(..), diff)
import Dominator.Types (DHandle(..), Html(..), Attr(..), appendChild, createJSElement, createJSTextNode, descendants, debugStrLn)
{-
import Chili.Internal (debugStrLn, debugPrint)
import Chili.Types (Control(..), Html(..), Attr(..), JSDocument, JSElement(..), JSNode, Loop, VDOMEvent(..), WithModel, addEventListener, childNodes, createJSElement, createJSTextNode, item, js_setTimeout, getFirstChild, getLength, replaceData, setAttribute, setProperty, unJSNode, setValue, parentNode, removeChild, replaceChild, toJSNode, appendChild, descendants, nodeType, currentDocument, newEvent, dispatchEvent)
import Chili.TDVar (TDVar, readTDVar, cleanTDVar, isDirtyTDVar)
-}
import GHCJS.Foreign.Callback (OnBlocked(..), Callback, asyncCallback, asyncCallback1, syncCallback1)

-- should we attach the DOM nodes to the VDOM? would that simplify diff/patch?
renderHtml :: Debug => JSDocument -> Html -> IO JSNode
renderHtml doc (CData t) =
  toJSNode <$> createJSTextNode doc t
renderHtml doc (Element tag mKey attrs children) =
  do e <- createJSElement doc tag
     mapM_ (\c -> appendChild e =<< renderHtml doc c) children
     mapM_ (doAttr e) attrs
     pure (toJSNode e)
    where
      doAttr elem (Attr k v)   = setAttribute elem k v
      doAttr elem (Prop k v)   = setProperty elem k v
      doAttr elem (EL eventType eventHandler) = do
        liftIO $ debugStrLn $ "Adding event listener for " ++ eventName eventType
        addEventListener elem eventType (\e -> {- putStrLn "eventHandler start" >> -} (eventHandler e) {- >> putStrLn "eventHandler end"-}) False
      doAttr elem (ELO eventType opts eventHandler) = do
        liftIO $ debugStrLn $ "Adding event listener for " ++ eventName eventType
        addEventListenerOpt elem eventType (\e -> {- putStrLn "eventHandler start" >> -} (eventHandler e) {- >> putStrLn "eventHandler end"-}) opts
      doAttr _ (OnCreate _) = error "Dominator.Patch.renderHtml"

updateView :: Debug => DHandle -> Html -> IO ()
updateView (DHandle root vdom doc) newHtml =
  do oldHtml <- takeMVar vdom
     let patches = diff (const False) oldHtml (Just newHtml)
     apply doc (toJSNode root) oldHtml patches
     putMVar vdom newHtml
     pure ()

apply :: Debug => JSDocument -> JSNode -> Html -> Map Int [Patch] -> IO ()
apply document rootNode vdom patches =
  do let indices = Map.keys patches
     case indices of
        [] -> pure ()
        _ -> do -- debugStrLn $ "indices (keys) = " ++ show indices
                -- debugStrLn $ "apply = " ++ show patches
                (Just first) <- getFirstChild rootNode -- FIXME: handle Nothing
                nodeList <- getNodes first vdom indices
                -- debugStrLn $ "nodeList length = " ++ show (length nodeList)
                mapM_ (apply' document rootNode patches) nodeList
                return ()

apply' :: Debug => JSDocument
       -> JSNode
       -> Map Int [Patch]
       -> (Int, JSNode)
       -> IO ()
apply' document rootNode patchMap (index, node) = do
    debugStrLn $ "apply' with index = " ++ show index
    case Map.lookup index patchMap of
      (Just patches) ->
          mapM_ (apply'' document rootNode node) patches
      Nothing -> error $ "Y NO PATCH? " ++ show index

apply'' :: Debug => JSDocument
        -> JSNode
        -> JSNode
        -> Patch
        -> IO ()
apply'' document body node patch =
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
                                       newChild <- createJSTextNode doc t
                                       replaceChild parent newChild node
                                       return ()

      (Props newProps removeAttrs removeProps) -> -- FIXME: doesn't handle changes to events.
          do let e = JSElement $ unJSNode node
             debugStrLn $ "set Attr: " ++ show [ (k,v) | Attr k v <- newProps ]
             debugStrLn $ "set Prop: " ++ show [ (k,v) | Prop k v <- newProps ]
             mapM_ (\(k, v) ->
                        case (unpack k) of
--                          "value" -> setValue e v -- FIXME: this causes issues with the cursor position
                          _ -> do debugStrLn $ "setAttribute " ++ show (k,v)
                                  setAttribute e k v) [ (k,v) | Attr k v <- newProps ]
             mapM_ (\(k, v) ->
                        case (unpack k) of
--                          "value" -> setValue e v -- FIXME: this causes issues with the cursor position
                          _ -> setProperty e k v) [ (k,v) | Prop k v <- newProps ]
             mapM_ (\k -> removeAttribute e k) removeAttrs
             mapM_ (\k -> deleteProperty e k)  removeProps

      (Insert elem) ->
          -- FIXME: don't get parent?
          do debugStrLn "apply'': Insert"
             mparent <- parentNode node
             case mparent of
               Nothing -> pure () -- debugStrLn $ "Can't appendChild because there is no parentNode"
               (Just parent) ->
                   do -- debugStrLn $  "Insert --> " ++ show elem
                      child <- renderHtml document elem -- loop model sendWS htmlV document body elem view
                      appendChild node child
                      return ()

      Remove ->
          do mparent <- parentNode node
             case mparent of
               Nothing -> pure () -- debugStrLn $ "Can't removeChild because there is no parentNode"
               (Just parent) ->
                   do removeChild parent (Just node)
                      pure ()

      VNode newElem ->
          do debugStrLn "VNode"
             mparent <- parentNode node
             case mparent of
               Nothing -> debugStrLn $ "Can't replaceChild because there is no parentNode"
               (Just parent) ->
                   do debugStrLn "replacing old node with new"
                      newChild <- renderHtml document newElem
                      replaceChild parent newChild node
                      return ()
      Reorder moves ->
        do putStrLn $ "Reorder: " ++ show moves
           cs <- childNodes node
           l <- getLength cs
           debugStrLn $ "Reorder: " ++ show moves
           -- we need to remove from the end of the list to the beginning or the indexes
           -- will shift as you remove things
           let removes = reverse $ sort $ [ RemoveKey i mKey| RemoveKey i mKey <- moves ]
               inserts = [ InsertKey i key | InsertKey i key <- moves ]
           keyMap <- applyRemoves node cs Map.empty removes
           print (Map.keys keyMap)
           applyInserts node cs keyMap inserts
           pure ()
      None -> error "Dominator.Patch.apply''"

applyRemoves :: Debug => JSNode -> JSNodeList -> Map Text JSNode -> Moves -> IO (Map Text JSNode)
applyRemoves parent children keyMap [] = pure keyMap
applyRemoves parent children keyMap (move:moves) =
  case move of
    (RemoveKey i mKey) ->
      do (Just child) <- item children (fromIntegral i)
         let keyMap' = case mKey of
               Nothing -> keyMap
               (Just key) -> Map.insert key child keyMap
         l <- getLength  children
         putStrLn ("remove " ++ show i ++ " from list of length " ++ show l)
         if (i < fromIntegral l)
           then removeChild parent (Just child)
           else pure Nothing
         applyRemoves parent children keyMap' moves
    (InsertKey _ _) -> error "Dominator.Patch.applyRemoves"

applyInserts :: Debug => JSNode -> JSNodeList -> Map Text JSNode -> Moves -> IO ()
applyInserts parent children keyMap [] = pure ()
applyInserts parent children keyMap (move:moves) =
  case move of
    (InsertKey i key) ->
      case Map.lookup key keyMap of
        Nothing -> error $  "applyInserts: the missing key - " ++ Text.unpack key ++ " keys=" ++ show (Map.keys keyMap)
        (Just n) ->
          do (Just before) <- item children (fromIntegral i)
             insertBefore parent n before
             applyInserts parent children keyMap moves
    (RemoveKey _ _) -> error "Dominator.Patch.applyInserts"

getNodes :: Debug => JSNode      -- ^ root node of DOM
         -> Html -- ^ virtual DOM that matches the current DOM
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
                -> Html
                -> [Int]
                -> StateT Int IO [(Int, JSNode)]
      -- if we are not looking for any more indices then we are done
      getNodes' _ _ [] = return []
--      getNodes' currNode node@(Cntl {}) is@(i:_) = liftIO (debugPrint is) >> pure [] -- FIXME: surely not right
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

      getNodes' currNode vdom@(Element _tag _key _attrs children) is'' =
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
                                                     getNodes' c (children@@i') is'
                                          ) [0..(l-1)]
                      if (i == index)
                      then do return $ (i, currNode) : (concat childNodes')
                      else return (concat childNodes')
      getInRange index count indexes =
          {- takeWhile (\i -> i <= index + count) $ -} dropWhile (\i -> i < index) indexes
