{-# LANGUAGE ScopedTypeVariables #-}
module Chili.Diff where

import Control.Monad.State (State(..), evalState, get, put)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Chili.Types (Html(..), Attr(..), descendants, flattenCData)

data Patch model
    = Remove
    | Insert (Html model)
    | VText Text
    | VNode (Html model)
    | Props [Attr model] -- FIXME: add list of attributes to remove
--      deriving Eq

instance Show (Patch model) where
    show Remove        = "Remove"
    show (Insert node) = "Insert " <> show node
    show (VText t)     = "VText " <> Text.unpack t
    show (VNode e)     = "VNode " <> show e
    show (Props attrs) = "Props " <> show attrs

diff :: forall model. Html model -> Maybe (Html model) -> Map Int [Patch model]
diff a b = Map.fromListWith (flip (++)) (walk a b 0)

-- FIXME: does not handle changes to Events or Properties
-- FIXME: we should be able to add and remove single attributes
diffAttrs :: [Attr model] -> [Attr model] -> Int -> [(Int, [Patch model])]
diffAttrs attrsA attrsB index =
          let attrsA' = [(k,v) | Attr k v <- attrsA]
              attrsB' = [(k,v) | Attr k v <- attrsB]
              propsA' = [(k,v) | Prop k v <- attrsA]
              propsB' = [(k,v) | Prop k v <- attrsB]
          in if (attrsA' == attrsB') && (propsA' == propsB')
             then []
             else [(index, [Props attrsB])]
--  where
--    isAttrProp (Attr k v) = (k, v)

{-
          let attrsA' = [(k,v) | Attr k v <- attrsA]
              attrsB' = [(k,v) | Attr k v <- attrsB]
          in if attrsA' == attrsB'
             then []
             else [(index, [Props attrsB])]
-}
walk :: Html model -> Maybe (Html model) -> Int -> [(Int, [Patch model])]
walk a mb index =
  case mb of
   Nothing -> [(index, [Remove])]
   (Just b@(Element tagNameB attrsB {- keyB _ -} childrenB)) ->
     case a of
      (Element tagNameA attrsA childrenA)
        | tagNameA == tagNameB {- && keyA == keyB -} ->
            let propsPatches    = diffAttrs attrsA attrsB index
                childrenPatches = diffChildren index childrenA childrenB index
            in propsPatches ++ childrenPatches
      _ -> [(index, [VNode b])]
   (Just (CData  txtB)) ->
     case a of
      (CData  txtA)
        | txtA == txtB -> []
      _ -> [(index, [VText txtB])]
   (Just b@cntl) ->
     case a of
       -- FIXME: add something to allow us to distinguish between two different types of components
       (Cntl {}) -> []
       _ -> [(index, [VNode b])]

diffChildren :: Int -> [Html model] -> [Html model] -> Int -> [(Int, [Patch model])]
diffChildren parentIndex childrenA childrenB index =
  case (childrenA, childrenB) of
   ([], []) -> []
   ([], (b:bs)) ->
     (parentIndex, [Insert b]) : diffChildren parentIndex [] bs (index + 1)
   ((a:as), []) ->
     (walk a Nothing (index + 1)) ++ (diffChildren parentIndex as [] (index + 1 + (elementDescendants' a)))
   ((a:as), (b:bs)) ->
     (walk a (Just b) (index + 1)) ++ (diffChildren parentIndex as bs (index + 1 + (elementDescendants' a)))
  where
       elementDescendants' (Element _ _ c) = descendants c
       elementDescendants' _ = 0

{-
diffChildren [] cs index = [(pid, map Insert cs)]
diffChildren (a:as) (b:bs) index =
   let d = walk a b (index + 1)
       diffs = diffChildren pid as bs
       return $ d ++ diffs
diffChildren (a:as) [] index =
  do diffs <- diffChildren pid as []
     return $ (index, [Remove]) : diffs
-}
{-
      walk (Element tagNameA attrsA keyA descendantsA childrenA) b@(Element tagNameB attrsB keyB _ childrenB)

      // two cdata that are the same
      walk (CData escapeA txtA) b@(CData escapeB txtB)
            | escapeA == escapeB && txtA == txtB =
                return []
            | otherwise =
                do index <- get
                   return [(index, [VText escapeB txtB])]
      walk (Element _tagNameA _attrsA descendantsA _childrenA) b =
          do index <- get
             put (index + descendantsA)
             return [(index, [VNode b])] -- FIXME: this does not work correctly if the node is not the last in the list of children
                                                 -- FIXME: maybe we want VNode?
      walk (CData{}) b =
          do index <- get
             return [(index, [VNode b])] -- FIXME: this does not work correctly if the node is not the last in the list of children
                                                 -- FIXME: maybe we want VNode?
      -- FIXME: handle reordered children
      diffChildren :: Int -> [Html model] -> [Html model] -> State Int [(Int, [Patch model])]
      diffChildren _ [] [] = return []
      diffChildren pid (a:as) (b:bs) =
          do index <- inc
             d <- walk a b
             diffs <- diffChildren pid as bs
             return $ d ++ diffs
      diffChildren pid (a:as) [] =
          do index <- inc
             put (index + descendants [a])
             diffs <- diffChildren pid as []
             return $ (index, [Remove]) : diffs
      diffChildren pid [] cs =
          do index <- get
             return $ [(pid, map Insert cs)]
-}
{-
          where
            findAttrVal :: Text -> [Attr model] -> Maybe Text
            findAttrVal _ [] = Nothing
            findAttrVal n' (Attr n v : _)
                | n' == n = Just v
            findAttrVal n (_ : as) = findAttrVal n as

            diffIds :: [Attr model] -> [Attr model] -> Bool
            diffIds attrsA attsrB = (findAttrVal (pack "id") attrsA) /= (findAttrVal (pack "id") attrsB)
-}
