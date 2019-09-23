{-# language QuasiQuotes, TemplateHaskell, DeriveLift #-}
module Dominator.DOMC where

import Dominator.Types (JSDocument, JSNode, appendChild, createJSElement, createJSTextNode, getFirstChild, toJSNode, nextSibling, setAttribute, setNodeValue, setProperty)
import Control.Monad.Trans (MonadIO)
import qualified Data.Text.Lazy as L
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tree
import Data.List (unzip)
import Data.Maybe
import qualified Data.JSString as JS
-- import Dominator.Types
import Text.HTML.Parser (Token(..), parseTokensLazy)
import qualified Text.HTML.Parser as P
import Text.HTML.Tree
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib (tupleT)
import Language.Haskell.Meta.Parse (parseExp)

data Attr
  = Attr String String
  | Prop String String
  deriving (Eq, Ord, Show, Lift)

data Html
  = Element String [Attr] [Html]
  | CData String
  | Noop
  deriving (Eq, Ord, Show, Lift)


domc :: QuasiQuoter
domc = QuasiQuoter
  { quoteExp  = domcExpr
  , quotePat  = error "domc does not yet define an pattern quoter"
  , quoteType = error "domc does not yet define an type quoter"
  , quoteDec  = domcDec
  }

forestToHtml :: Forest Token -> [Html]
forestToHtml f = map treeToHtml f

treeToHtml :: Tree Token -> Html
treeToHtml (Node token subForest) =
  case token of
    (TagOpen tagName attrs) -> Element (Text.unpack tagName) (map toAttr attrs) (forestToHtml subForest)
    (TagSelfClose tagName attrs) ->  Element (Text.unpack tagName) (map toAttr attrs) (forestToHtml subForest)
    (ContentText txt) -> CData (Text.unpack txt)
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
      doAttr elem (Attr k v)   = setAttribute elem (Text.pack k) (Text.pack v)
      doAttr elem (Prop k v)   = setProperty elem (Text.pack k) (Text.pack v)
--      doAttr elem (OnCreate f) = liftIO $ do cb <- asyncCallback $ f elem model
--                                             js_setTimeout cb 0
{-      doAttr elem (EL eventType eventHandler) = do
        liftIO $ debugStrLn $ "Adding event listener for " ++ show eventType
        addEventListener elem eventType (\e -> {- putStrLn "eventHandler start" >> -} (eventHandler e) {- >> putStrLn "eventHandler end"-}) False
-}

domcDec :: String -> Q [Dec]
domcDec template =
  case tokensToForest $ parseTokensLazy (L.pack template) of
    (Left e)  -> error $ show e
    (Right f) ->
      let html = forestToHtml f in
      [d| template d = mapM (renderHtml d) (html :: [Html])  |]

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

mkUpdate :: (JSNode, String) -> ExpQ
mkUpdate (node, val) =
  [| \textNode -> do setNodeValue textNode (JS.pack "foo") |]


pExp :: (Path, String) -> (Path, ExpQ)
pExp (path, str) =
  case parseExp str of
    (Left e) -> error e
    (Right e) -> (path, pure e)

selectorName :: Path -> Name
selectorName = mkName . selectorName'

selectorName' :: Path -> String
selectorName' Start = ""
selectorName' (F Start) = "f"
selectorName' (N Start) = "f"
selectorName' (F p) = "f_" ++ selectorName' p
selectorName' (N p) = "n_" ++ selectorName' p

mkSelector :: JSNode ->  Path -> IO JSNode
mkSelector root Start = pure root
mkSelector n (F p) = do n' <- mkSelector n p
                        (Just n) <- getFirstChild n'
                        pure n
mkSelector n (N p) = do n' <- mkSelector n p
                        (Just n) <- nextSibling n'
                        pure n
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
{-
                       (path, expStr) = head exps
                       exp = case parseExp expStr of
                               Left e -> error e
                               (Right e) -> pure e
-}
                       pExps = map pExp exps
                       (path, exp) = head pExps
                       (allPaths, allExps) = unzip pExps
                   [| do nodes <- mapM (mkSelector rootNode) allPaths
                         pure $ \model -> do print path
                                             print html
--                                             let (path, exp') = head pExps
--                                             n <- mkSelector rootNode path  
--                                             setNodeValue n (JS.pack $(exp))
                                             mapM_ (\(n, e) -> setNodeValue n (JS.pack e)) (zip nodes $(listE allExps))
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
  = F Path
  | N Path
  | Start
    deriving (Eq, Ord, Show, Lift)

findExpressions :: Path -> [Html] -> [(Path, String)]
findExpressions p [] = []
findExpressions p (h:hs) =
  (findExpressions' (F p) h) ++ findSiblingExpressions (F p) hs

findSiblingExpressions :: Path -> [Html] -> [(Path, String)]
findSiblingExpressions _  [] = []
findSiblingExpressions p (h:hs) =
  (findExpressions' (N p) h) ++ (findSiblingExpressions (N p) hs)

findExpressions' :: Path -> Html -> [(Path, String)]
findExpressions' p Noop = []
findExpressions' p (Element tag _ c) = findExpressions p c
findExpressions' p (CData str) =
  case str of
    '{':'{':rest ->
      [(p, reverse $ drop 2 $ reverse $ rest )]
    _ -> []


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
