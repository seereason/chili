{-# language QuasiQuotes, TemplateHaskell, DeriveLift #-}
module Dominator.DOMC where

import Chili.Types (removeAttribute)
import Dominator.Types (JSDocument, JSNode, JSElement(..), appendChild, createJSElement, createJSTextNode, getFirstChild, toJSNode, nextSibling, setAttribute, setNodeValue, setProperty)
import Control.Monad.Trans (MonadIO)
import qualified Data.Text.Lazy as L
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tree
import Data.List (unzip)
import Data.Maybe
import qualified Data.JSString as JS
-- import Dominator.Types
import Text.HTML.Parser (Token(..), canonicalizeTokens, parseTokensLazy)
import qualified Text.HTML.Parser as P
import Text.HTML.Tree
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib (tupleT)
import Language.Haskell.Meta.Parse (parseExp)

data Attr
  = Attr String String
  | PropS String String
  | PropB String Bool
  deriving (Eq, Ord, Show, Lift)

data Html
  = Element String [Attr] [Html]
  | CData String
  | Noop
  deriving (Eq, Ord, Show, Lift)

data SpliceType
  = Str
  | AttrList
  deriving (Eq, Ord, Show, Lift)

data SpliceVal
  = StrV      { unStrV :: String }
  | AttrListV { unAttrListV :: [Attr] }
  deriving (Eq, Ord, Show, Lift)

domc :: QuasiQuoter
domc = QuasiQuoter
  { quoteExp  = domcExpr
  , quotePat  = error "domc does not yet define an pattern quoter"
  , quoteType = error "domc does not yet define an type quoter"
  , quoteDec  = domcDec
  }

forestToHtml :: Forest Token -> [Html]
forestToHtml f = map (treeToHtml . normalizeTree) f

normalizeTree :: Tree Token -> Tree Token
normalizeTree (Node token subForest) = (Node token (stripLeadingWhite subForest))
  where
    stripLeadingWhite (Node (ContentText txt) []:ts) | Text.all isSpace txt = ts
    stripLeadingWhite ts = ts

treeToHtml :: Tree Token -> Html
treeToHtml (Node token subForest) =
  case token of
    (TagOpen tagName attrs)      -> Element (Text.unpack tagName) (map toAttr attrs) (forestToHtml subForest)
    (TagSelfClose tagName attrs) -> Element (Text.unpack tagName) (map toAttr attrs) (forestToHtml subForest)
    (ContentText txt)            -> CData (Text.unpack txt)
    _ -> Noop
  where
    toAttr :: P.Attr -> Attr
    toAttr (P.Attr a v) = Attr (Text.unpack a) (Text.unpack v)

htmlToString :: Html -> String
htmlToString Noop = ""
htmlToString (CData t) = t
htmlToString (Element tagName attrs children) =
  concat [ "<", tagName, pAttrs attrs, ">", pChildren children, "</", tagName, ">" ]
  where
    pAttrs attrs = ""
    pChildren children = concatMap htmlToString children

renderHtml :: (MonadIO m) => JSDocument -> Html -> m JSNode
renderHtml doc (CData t) =
    toJSNode <$> createJSTextNode doc (Text.pack t)
renderHtml doc (Element tag attrs children) =
  do e <- createJSElement doc (Text.pack tag)
     mapM_ (\c -> appendChild e =<< renderHtml doc c) children
     mapM_ (doAttr e) attrs
     pure (toJSNode e)
    where
      -- fixme: strip out all expressions?
      doAttr elem (Attr k v)
         | k == "expr" = pure ()
         | otherwise   = setAttribute elem (Text.pack k) (Text.pack v)
      doAttr elem (PropS k v)   = setProperty elem (Text.pack k) (Text.pack v)
      doAttr elem (PropB k v)   = setProperty elem (Text.pack k) v
--      doAttr elem (OnCreate f) = liftIO $ do cb <- asyncCallback $ f elem model
--                                             js_setTimeout cb 0
{-      doAttr elem (EL eventType eventHandler) = do
        liftIO $ debugStrLn $ "Adding event listener for " ++ show eventType
        addEventListener elem eventType (\e -> {- putStrLn "eventHandler start" >> -} (eventHandler e) {- >> putStrLn "eventHandler end"-}) False
-}

-- FIXME: this should not strip whitespace instead of <pre> and <code> tags
stripWhitespace :: [Token] -> [Token]
stripWhitespace = filter notWS
  where
    notWS :: Token -> Bool
    notWS (ContentText txt) = not (Text.all isSpace txt)
    notWS _ = True

domcDec :: String -> Q [Dec]
domcDec template =
  case tokensToForest $ {- canonicalizeTokens $ stripWhitespace $ -} parseTokensLazy (L.pack template) of
    (Left e)  -> error $ show e
    (Right f) ->
      let html = forestToHtml f in
      [d| template d = mapM (renderHtml d) (html :: [Html])  |]

domcExpr :: String -> Q Exp
domcExpr template =
  case tokensToForest $ {- canonicalizeTokens $ stripWhitespace $ -} parseTokensLazy (L.pack template) of
    (Left e)  -> error $ show e
    (Right f) ->
      do let html = forestToHtml f
             update = mkUpdater html
--             v = $([| someVar |])
             init = [| \d e ->
                         do h <- mapM (renderHtml d) (html :: [Html])
--                          mk <- mkUpdater html
--                          u <- mk d
                            removeChildren e
                            appendChild e (head h)
                            u <- $(mkUpdater html) (toJSNode e)
                            pure u
                      |]
         init

{-

This version attempts to replace the entire document including the <html>. But that might be leading to an error:

 "Node cannot be inserted at the specified point in the hierarchy"

It seems to work when loading an index.html from disk, but not when the index.html comes from a server.

domcExpr :: String -> Q Exp
domcExpr template =
  case tokensToForest $ parseTokensLazy (L.pack template) of
    (Left e)  -> error $ show e
    (Right f) ->
      do let html = forestToHtml f
             update = mkUpdater html
--             v = $([| someVar |])
             init = [| \d ->
                         do h <- mapM (renderHtml d) (html :: [Html])
--                          mk <- mkUpdater html
--                          u <- mk d
                            removeChildren d
                            appendChild d (head h)
                            u <- $(mkUpdater html) (toJSNode d)
                            pure u
                      |]
         init
-}
mkUpdate :: (JSNode, String) -> ExpQ
mkUpdate (node, val) =
  [| \textNode -> do setNodeValue textNode (JS.pack "foo") |]

pExp :: (Path, String, SpliceType) -> (Path, ExpQ, SpliceType)
pExp (path, str, spliceType) =
  case parseExp str of
    (Left e) -> error e
    (Right e) ->
      case spliceType of
        Str      -> (path, [| StrV $(pure e) |] , spliceType)
        AttrList ->  (path, [| AttrListV $(pure e) |] , spliceType)

selectorName :: Path -> Name
selectorName = mkName . selectorName'

selectorName' :: Path -> String
selectorName' Start = ""
selectorName' (A name p) = "a_"++name++"_" ++ selectorName' p
selectorName' (F Start) = "f"
selectorName' (N Start) = "f"
selectorName' (F p) = "f_" ++ selectorName' p
selectorName' (N p) = "n_" ++ selectorName' p

data UpdateNode
  = UpdateAttribute String
  | UpdateNodeValue
  | AppendAttributes

-- | The path is inside out -- the Start node is at the end.
mkSelector :: JSNode -> Path -> IO (UpdateNode, JSNode)
mkSelector root Start = pure (UpdateNodeValue, root)
mkSelector n (A name p) = do
  do (kind, n') <- mkSelector n p
     pure (UpdateAttribute name, n')
mkSelector n (E p) = do
  do (kind, n') <- mkSelector n p
     pure (AppendAttributes, n')
mkSelector n (F p) = do (kind, n') <- mkSelector n p
                        (Just n) <- getFirstChild n'
                        pure (kind, n)
mkSelector n (N p) = do (kind, n') <- mkSelector n p
                        (Just n) <- nextSibling n'
                        pure (kind, n)
{- compiles

mkUpdater :: [Html] -> ExpQ
mkUpdater html =
  do let exps = findExpressions Start html
--         pExps' = map pExp exps
--     pExp <- snd (head pExps')
--     [| \rootNode -> $( [| pure $ \model -> pure () |]) |]
     [| \rootNode ->
           let (path, expStr) = head exps
           in
             $( let exps = findExpressions Start html
                    (path, expStr) = head exps
                    exp = case parseExp expStr of
                            Left e -> error e
                            (Right e) -> pure e
                in

                  [| pure $ \model -> do print $(exp) ; pure () |])
      |]
-}

{- compiles
mkUpdater :: [Html] -> ExpQ
mkUpdater html =
  do let exps = findExpressions Start html
--         pExps' = map pExp exps
--     pExp <- snd (head pExps')
--     [| \rootNode -> $( [| pure $ \model -> pure () |]) |]
     [| \rootNode ->
           let (path, expStr) = head exps
           in
             $( do let exps = findExpressions Start html
                       (path, expStr) = head exps
                       exp = case parseExp expStr of
                               Left e -> error e
                               (Right e) -> pure e
                   [| pure $ \model -> do print path
                                          print html
                                          n <- mkSelector path  rootNode
                                          setNodeValue n (JS.pack $(exp))
                                          pure () |])
-}
-- toJSElement = JSElement . unJSNode

setAttr :: JSElement -> Attr -> IO ()
setAttr elem attr =
  case attr of
    (Attr k v) -> setAttribute elem (Text.pack k) (Text.pack v)
    (PropS k v) -> setProperty elem (Text.pack k) (Text.pack v)
    (PropB k v) -> setProperty elem (Text.pack k) v

-- walks the template, finds the expressions, and constructs a function which takes the model and updates the elements
mkUpdater :: [Html] -> ExpQ
mkUpdater html =
  do -- let exps = findExpressions Start html
--         pExps' = map pExp exps
--     pExp <- snd (head pExps')
--     [| \rootNode -> $( [| pure $ \model -> pure () |]) |]
     [| \rootNode ->
--           let (path, expStr) = head exps
--           in
             $( do let exps = findExpressions Start html
{-
                       (path, expStr) = head exps
                       exp = case parseExp expStr of
                               Left e -> error e
                               (Right e) -> pure e
-}
                       pExps = map pExp exps
--                       (path, exp) = head pExps
                       (allPaths, allExps, allTypes) = unzip3 pExps
                   [| do -- putStrLn $ show html
                         -- putStrLn $ show allPaths
                         -- putStrLn $ "exps = " ++ show exps
                         nodes <- mapM (mkSelector rootNode) allPaths
                         pure $ \model -> do -- print path
                                             -- print html
--                                             let (path, exp') = head pExps
--                                             n <- mkSelector rootNode path  
--                                             setNodeValue n (JS.pack $(exp))
                                             mapM_ (\((kind, n), e) ->
                                                     case kind of
                                                       UpdateNodeValue    ->
                                                         do -- putStrLn $  "setNodeValue = " ++ e
                                                            e `seq` setNodeValue n (JS.pack $ unStrV e)
                                                       UpdateAttribute nm ->
                                                         do -- set attribute and property?
                                                            e `seq` setAttribute (JSElement (unJSNode n)) (Text.pack nm) (Text.pack $ unStrV e)
                                                            setProperty (JSElement (unJSNode n)) (Text.pack nm) (Text.pack $ unStrV e)
                                                       AppendAttributes ->
                                                         do -- FIXME: we need to keep a list of attributes which are added so they can also be removed
                                                            mapM_ (setAttr (JSElement (unJSNode n))) (unAttrListV e)
--                                                            removeAttribute (JSElement (unJSNode n)) (Text.pack "expr")
                                                            pure ()
                                                   ) (zip nodes $(listE allExps))
--                                            mapM (\e -> setNodeValue n (JS.pack $(e))) [exp]
                                             pure () |])
      |]
{-
     [| \rootNode ->
         do $([| do print (exps :: [(Path, String)]) -- print $(pExps)
                    let (p, _) = head exps
--                    node <- mkSelector p rootNode
                    pure $ \model -> do print p
                                        let (_, exp) = head exps
                                        snd (head pExps)
--                                        print $(map snd pExps)
--                                        print (exps :: [(Path, String)]) -- print $(pExps)
--                                        setNodeValue node (JS.pack $(exp))
                                        putStrLn "done."
                                        |])
     |]
-}
{-
  [| \rootNode -> $( [| do exps <- findExpressions rootNode html ;
                           pure $ \model -> pure ()
                      |]) |]
-}
{-
  [| \rootNode ->
       do exps <- findExpressions rootNode html
          let updates = map mkUpdate exps
          pure $ \model -> $( updates )
--          pure $(lamE [ varP (mkName "model") ] (doE updates) )
--          pure $(lamE [ varP (mkName "model") ] [| print $ () |])

   |]
-}
{-
  case parseExp "message model" of
    (Left e) -> error e
    (Right exp) -> 
-}

data Path
  = F Path -- first child
  | N Path -- next sibling
  | A String Path -- attribute-name
  | E Path -- expression which returns a list of attributes to add (how do you remove them?)
  | Start -- top-level
    deriving (Eq, Ord, Show, Lift)

-- | find all the expressions in the tree. aka '{{ expr }}'
findExpressions :: Path -> [Html] -> [(Path, String, SpliceType)]
findExpressions p [] = []
findExpressions p (h:hs) =
  (findExpressions' (F p) h) ++ findSiblingExpressions (F p) hs

findSiblingExpressions :: Path -> [Html] -> [(Path, String, SpliceType)]
findSiblingExpressions _  [] = []
findSiblingExpressions p (h:hs) =
  (findExpressions' (N p) h) ++ (findSiblingExpressions (N p) hs)

findExpressions' :: Path -> Html -> [(Path, String, SpliceType)]
findExpressions' p Noop = []
findExpressions' p (Element tag attrs c) = (catMaybes (map (findAttrExpr p) attrs)) ++ findExpressions p c
findExpressions' p (CData str) =
  case str of
    '{':'{':rest ->
      [(p, reverse $ drop 2 $ reverse $ rest, Str )]
    _ -> []


-- FIXME: allow expressions to be less than the entire attribute value
findAttrExpr :: Path -> Attr -> Maybe (Path, String, SpliceType)
findAttrExpr p (Attr name val) =
  case val of
    '{':'{':rest ->
      case name of
        "expr" -> Just (E p, reverse $ drop 2 $ reverse $ rest, AttrList)
        _ -> Just (A name p, reverse $ drop 2 $ reverse $ rest, Str)
    _ -> Nothing

{-
findExpressions :: JSNode -> [Html] -> IO [(JSNode, String)]
findExpressions _ [] = pure []
findExpressions root (h:hs) =
  do print h
     f <- unsafeFirstChild root
     e <- findExpressions' f h
     es <- findSiblingExpressions f hs
     pure (e++es)

findSiblingExpressions :: JSNode -> [Html] -> IO [(JSNode, String)]
findSiblingExpressions _ [] = pure []
findSiblingExpressions f (h:hs) =
  do f_n <- unsafeNextSibling f
     e <- findExpressions' f_n h
     es <- findSiblingExpressions f_n hs
     pure (e ++ es)

findExpressions' :: JSNode -> Html -> IO [(JSNode, String)]
findExpressions' _ Noop = pure []
findExpressions' node (Element tag _ c) =
  do print tag
     findExpressions node c
findExpressions' node (CData str) =
  case str of
    '{':'{':rest ->
      do print str
         pure [(node, "message model")]
    _ -> pure []

unsafeFirstChild :: JSNode -> IO JSNode
unsafeFirstChild = fmap fromJust . getFirstChild

unsafeNextSibling :: JSNode -> IO JSNode
unsafeNextSibling = fmap fromJust . nextSibling
-}
