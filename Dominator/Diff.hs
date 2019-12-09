module Dominator.Diff where

import Control.Monad.State (State(..), evalState, get, put)
import Data.List ((\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, isNothing)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Debug.Trace (trace)
import Dominator.Types (Html(..), Attr(..), descendants, flattenCData)

data Patch
    = None
    | VText Text
    | VNode Html
    | Props [Attr] [Text] [Text] -- ^ Attr/Prop to add/modify, Attr to remove, Prop to remove
    | Reorder Moves
    | Insert Html
    | Remove
--      deriving Eq

instance Show Patch where
  show None               = "None"
  show (VText t)          = "VText "  <> Text.unpack t
  show (VNode e)          = "VNode "  <> show e
  show (Props add removeAttrs removeProps) = "Props "  <> show add <> " " <> show removeAttrs <> " " <> show removeProps
  show (Reorder moves)    = "Order "  <> show moves
  show (Insert node)      = "Insert " <> show node
  show Remove             = "Remove"

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
              removeAttrs = (map fst attrsA') \\ (map fst attrsB')
              addAttrs = attrsB' \\ attrsA'
              removeProps = (map fst propsA') \\ (map fst propsB')
              addProps = propsB' \\ propsA'
          in if (addProps == []) && (removeProps == []) && (addAttrs == []) && (removeAttrs == [])
             then []
             else [(index, [Props ((map (\(k,v) -> Prop k v) addProps) ++ (map (\(k,v) -> Attr k v) addAttrs))
                                  removeAttrs removeProps
                           ])
                  ]
{-
          in if {- (attrsA' == attrsB') && -} (propsA' == propsB')
             then []
             else [(index, [Props attrsB])]
-}
walk :: Html -> Maybe Html -> Int -> [(Int, [Patch])]
walk a mb index =
  case mb of
   Nothing -> [(index, [Remove])]
   (Just b@(Element tagNameB mKeyB attrsB childrenB)) ->
     case a of
      (Element tagNameA mKeyA attrsA childrenA)
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

{-
Reordering:

When we reorder nodes we might also need to apply additional patches to those nodes.

So we can either patch the nodes and then reorder them, or reorder them and then patch them.

It is easiest to diff the nodes if the keys are in sorted order.

What happens if some of the nodes do not have keys?


 [ 1 2 a b 3 ] -> [ 2 a b 1 3 ]

-}

diffChildren :: Int -> [Html] -> [Html] -> Int -> [(Int, [Patch])]
diffChildren parentIndex childrenA childrenB' index =
  let -- FIXME: disabled reorder because it is broken. Fix it!
      -- (childrenB, moves) = trace ("childrenA = " ++ show childrenA ++ " childrenB = " ++ show childrenB') (reorder childrenA childrenB')
      (childrenB, moves) = (childrenB', [])
      patches = case (childrenA, childrenB) of
        ([], []) -> []
        ([], (b:bs)) ->
          (parentIndex, [Insert b]) : diffChildren parentIndex [] bs (index + 1)
        ((a:as), []) ->
          (walk a Nothing  (index + 1)) ++ (diffChildren parentIndex as [] (index + 1 + (elementDescendants' a)))
        ((a:as), (b:bs)) ->
          (walk a (Just b) (index + 1)) ++ (diffChildren parentIndex as bs (index + 1 + (elementDescendants' a)))
  in if (null moves)
     then patches
     else (patches ++ [(index, [Reorder moves])])
  where
       elementDescendants' (Element _ _ _ c) = descendants c
       elementDescendants' _ = 0


{-
-- | create a assoc list of keys to index and well as a list of indexes that have no keys
keyIndex :: [Html] -> (Map Text Int, [Int]) -- ^ (keys, free)
keyIndex elems =
  let (keys, free) = keyIndex' elems 0 ([], [])
  in (Map.fromList keys, free)
  where
    keyIndex' [] _ acc = acc
    -- would it make sense for CData to have a key as well?
    keyIndex' ((CData {}):es) i (key, free) =
      keyIndex' es (succ i) (key, i:free)
    keyIndex' ((Element _tagName mKey _attrs _children):es) i (key, free) =
      case mKey of
        Nothing -> keyIndex' es (succ i) (key, i:free)
        (Just k) -> keyIndex' es (succ i) ((k, i):key, free)
-}

-- | create a Map from keys to elements and well as a list of elements that have no keys
keyIndex :: [Html] -> (Map Text (Int, Html), [(Int, Html)]) -- ^ (keys, free)
keyIndex elems =
  let (keys, free) = keyIndex' elems 0 ([], [])
  in (Map.fromList keys, free)
  where
    keyIndex' [] _ acc = acc
    -- would it make sense for CData to have a key as well?
    keyIndex' (e@(CData {}):es) i (key, free) =
      keyIndex' es (succ i) (key, (i, e):free)
    keyIndex' (e@(Element _tagName mKey _attrs _children):es) i (key, free) =
      case mKey of
        Nothing  -> keyIndex' es (succ i) (key, (i, e) : free)
        (Just k) -> keyIndex' es (succ i) ((k, (i, e)) : key, free)

type Moves = [Move]
data Move
  = InsertKey Int Text
  | RemoveKey Int (Maybe Text)
    deriving (Eq, Ord, Read, Show)

reorder :: [Html] -> [Html] -> ([Html], Moves)
reorder aChildren bChildren =
  let (aKeys, aFree) = keyIndex aChildren
      (bKeys, bFree) = keyIndex bChildren
  in
    if (null aKeys || null bKeys)
    -- if there were no keys in aChildren or no keys in bChildren, then do not reorder
    then (bChildren, [])
    else let (newChildren', bFree') = matchChildren aChildren bFree bKeys []
             newChildren = (appendNewKeys aKeys newChildren' bChildren) ++ (map (Just . snd) bFree')
             moves = calcMoves bKeys (zip [0..] bChildren) (zip [0..] newChildren)
         in (catMaybes newChildren, moves)
  where
    key :: Html -> Maybe Text
    key (Element _ k _ _ ) = k
    key _ = Nothing

    -- actually prepends
    appendNewKeys aKeys newChildren [] = newChildren
    appendNewKeys aKeys newChildren (b:bs) =
      case key b of
        (Just k) ->
          case Map.lookup k aKeys of
            -- a new key which was not in the old children
            Nothing   -> appendNewKeys aKeys (Just b : newChildren) bs
            -- key already matched in old children
            (Just {}) -> appendNewKeys aKeys newChildren bs
        -- no key
        Nothing -> appendNewKeys aKeys newChildren bs

    matchChildren :: [Html] -> [(Int, Html)] -> Map Text (Int, Html) -> [Maybe Html] -> ([Maybe Html], [(Int, Html)])
    -- if aItem has a key try to match it with an element in bKeys
    -- if no match is found, remove aItem
    matchChildren aItem@((Element _tag (Just aKey) _attrs _children):as) bFree bKeys newChildren =
      case Map.lookup aKey bKeys of
        Nothing -> matchChildren as bFree bKeys (Nothing : newChildren)
        (Just (_, bChild)) -> matchChildren as bFree bKeys (Just bChild : newChildren)
    -- if aItem does not have a key, match it with the first free element in bFree
    matchChildren aItem@((Element _tag Nothing _attrs _children):as) bFree bKeys newChildren =
      case bFree of
        ((_, b):bFree) ->
          matchChildren as bFree bKeys (Just b : newChildren)
        -- no more free times in bFree, so remaining free items in 'a' are deleted
        [] -> matchChildren as bFree bKeys (Nothing : newChildren)
    matchChildren [] bFree _ newChildren = (reverse newChildren, bFree)

    calcMoves :: Map Text (Int, Html) -> [(Int, Html)] -> [(Int, Maybe Html)] -> Moves
    calcMoves _ [] [] = []
    calcMoves bKeys ((i, b):bChildren) [] =
      case key b of
        -- no key, so no move required
        Nothing -> calcMoves bKeys bChildren []
        -- key is now an insert
        (Just k) -> (InsertKey i k) : calcMoves bKeys bChildren []
    --  out of wanted, so remove remaining simulated
    calcMoves bKeys [] simulated =
      let removeK (simulateIndex, mSimulateItem) =
            case mSimulateItem of
              (Just (Element _ mKeySimulate _ _)) -> RemoveKey simulateIndex mKeySimulate
              _ -> RemoveKey simulateIndex Nothing
      in
        map removeK simulated
    calcMoves bKeys b@((wantedIndex, wantedItem):bChildren) ((simulateIndex, mSimulateItem) : newChildren) =
      case mSimulateItem of
        Nothing -> RemoveKey simulateIndex Nothing : calcMoves bKeys b newChildren
        (Just simulateItem) ->
          case (wantedItem, simulateItem) of
            (Element _ mKeyWanted _ _, Element _ mKeySimulate _ _)
              -- if the keys are the same (or both Nothing) then no move is required, so continue to the next pair
              | mKeyWanted == mKeySimulate -> calcMoves bKeys bChildren newChildren
              | otherwise ->
                case mKeyWanted of
                  -- we need a key here, just not the want that simulate would like to provide
                  (Just keyWanted) ->
                    case mKeySimulate of
                      (Just simulateKey) ->
                        case Map.lookup simulateKey bKeys of
                            -- if an insert is not going to put the key in place
                          (Just (k, _))

                            | (k /= simulateIndex + 1) ->
                              let remove = RemoveKey simulateIndex (Just simulateKey) in
                              case newChildren of
                                ((simulateIndex, simulateItem) : newChildren) ->
                                  if (isNothing simulateItem || (key (fromJust simulateItem) /= key wantedItem))
                                   -- removing an item did not happen to get the right item in place so we need an explicit insert
                                  then remove : (InsertKey wantedIndex keyWanted) : calcMoves bKeys bChildren newChildren
                                     -- items match so move on
                                  else remove : calcMoves bKeys bChildren newChildren
                                _ -> remove : calcMoves bKeys bChildren newChildren

                            | otherwise -> (InsertKey wantedIndex keyWanted) : calcMoves bKeys bChildren newChildren

                          _ -> (InsertKey wantedIndex keyWanted) : calcMoves bKeys bChildren newChildren

                      _ -> (InsertKey wantedIndex keyWanted) : calcMoves bKeys bChildren newChildren

                  -- no keyWanted
                  Nothing ->
                    case mKeySimulate of
                        -- key in simulate is not wanted, so just remove it
                        -- we call calcMoves without the unwanted simulateItem, but with the same wanted
                      (Just keySimulate) ->
                        RemoveKey simulateIndex (Just keySimulate) : calcMoves bKeys b newChildren
                      -- in theory the == above should make this case impossible
                      Nothing -> error "Diff -> reorder -> calcMoves: can this happen?" -- calcMoves bChildren newChildren (succ simulateIndex)

slice :: [(Int, a)] -> [(Int, a)]
slice cs = map (\(i,c) -> (pred i, c)) cs
