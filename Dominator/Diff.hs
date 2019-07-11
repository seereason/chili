module Dominator.Diff where

import Control.Monad.State (State(..), evalState, get, put)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Dominator.Types (Html(..), Attr(..), descendants, flattenCData)

data Patch
    = Remove
    | Insert Html
    | VText Text
    | VNode Html
    | Props [Attr] -- FIXME: add list of attributes to remove
--      deriving Eq

instance Show Patch where
    show Remove        = "Remove"
    show (Insert node) = "Insert " <> show node
    show (VText t)     = "VText "  <> Text.unpack t
    show (VNode e)     = "VNode "  <> show e
    show (Props attrs) = "Props "  <> show attrs


diff :: Html -> Maybe Html -> Map Int [Patch]
diff a b = Map.fromListWith (flip (++)) (walk a b 0)

-- FIXME: does not handle changes to Events or Properties
-- FIXME: we should be able to add and remove single attributes
diffAttrs :: [Attr] -> [Attr] -> Int -> [(Int, [Patch])]
diffAttrs attrsA attrsB index =
          let attrsA' = [(k,v) | Attr k v <- attrsA]
              attrsB' = [(k,v) | Attr k v <- attrsB]
              propsA' = [(k,v) | Prop k v <- attrsA]
              propsB' = [(k,v) | Prop k v <- attrsB]
          in if (attrsA' == attrsB') && (propsA' == propsB')
             then []
             else [(index, [Props attrsB])]

walk :: Html -> Maybe Html -> Int -> [(Int, [Patch])]
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
   (Just (CData txtB)) ->
     case a of
      (CData txtA)
        | txtA == txtB -> []
      _ -> [(index, [VText txtB])]
{-
   (Just b@cntl) ->
     case a of
       -- FIXME: add something to allow us to distinguish between two different types of components
       (Cntl {}) -> []
       _ -> [(index, [VNode b])]
-}

diffChildren :: Int -> [Html] -> [Html] -> Int -> [(Int, [Patch])]
diffChildren parentIndex childrenA childrenB index =
  case (childrenA, childrenB) of
   ([], []) -> []
   ([], (b:bs)) ->
     (parentIndex, [Insert b]) : diffChildren parentIndex [] bs (index + 1)
   ((a:as), []) ->
     (walk a Nothing  (index + 1)) ++ (diffChildren parentIndex as [] (index + 1 + (elementDescendants' a)))
   ((a:as), (b:bs)) ->
     (walk a (Just b) (index + 1)) ++ (diffChildren parentIndex as bs (index + 1 + (elementDescendants' a)))
  where
       elementDescendants' (Element _ _ c) = descendants c
       elementDescendants' _ = 0