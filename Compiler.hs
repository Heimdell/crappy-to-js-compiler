
{-# language LambdaCase #-}
{-# language FlexibleInstances #-}

import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.String.Utils

import Control.Monad
import Control.Applicative

import Text.PrettyPrint hiding (empty, (<$>), (<*>), (<>))

import System.Environment

data AST5 info var lit
  = Let5 info [Binding5 info var lit] (AST5 info var lit)
  | App5 info [AST5 info var lit]
  | Lam5 info [var] (AST5 info var lit)
  | Var5 info var
  | Lit5 info lit
  | Rec5 info var var (AST5 info var lit)

noInfo = ((-1, -1), "nowhere")

type AST = AST5 ((Int, Int), String) String String

js ast = text "var it = " <+> jsAST ast $+$ text "console.log" <+> parens (text "it")

jsAST :: AST -> Doc
jsAST = \case
    Let5 _ bs body
      ->  text "((() => {"
      $+$ nest 4 (vcat (map jsBinding bs ++ [text "return" <+> jsAST body]))
      $+$ text "}) ())"

    App5 _ pile -> fsep (map (parens . jsAST) pile)

    Lam5 _ vars body -> foldr param (jsAST body) vars

    Var5 _ name -> jsName name
    Lit5 _ lit -> text lit
    Rec5 _ name arg body
        -> text "(function "
        <+> jsName name <+> parens (jsName arg) <+> text " { return ("
        $+$ nest 4 (jsAST body)
        $+$ text ")})"

param arg body = parens (jsName arg) <+> text "=>" <+> body

jsBinding (Binding5 _ name (arg : args) body) =
    text "function" <+> jsName name <+> parens (jsName arg) <+> text "{ return ("
    $+$ nest 4 (foldr param (jsAST body) args)
    $+$ text ")}"

jsBinding (Binding5 _ name [] body) =
    text "var " <+> jsName name <+> text "="
    <+> nest 4 (jsAST body)

value (Lam5 _ _ _) = True
value (Lit5 _ _)   = True
value (Rec5 _ _ _ _) = True
value  _           = False

color code str = "\27[" ++ show code ++ "m" ++ str ++ "\27[0m"

red    = color 31
green  = color 32
yellow = color 33
blue   = color 34
pink   = color 35
lblue  = color 36

class Pretty p where
    p :: p -> Doc

instance Pretty (AST5 ((Int, Int), String) String String) where
    p = \case
        Let5 _ bs body ->
          text (blue "let") $+$ vcat (map p bs)
          $+$ text (blue "in") <+> p body

        App5 _ [f] -> p f
        App5 _ fxs -> hsep (map p1 fxs)

        Lam5 _ args body -> text (lblue "->") <+> hsep (map text args) <+> text (lblue ":")
          <+> p body

        Var5 _ name -> text name

        Lit5 _ lit -> text (yellow lit)

        Rec5 _ name arg body -> text (lblue "@") <+> text name <+> text "->" <+> text arg <+> p body
      where
        p1 ast | atomic ast = p ast
               | otherwise  = text (lblue "(") <> p ast <> text (lblue ")")

atomic = \case
   Var5 _ _   -> True
   Lit5 _ _   -> True
   App5 _ [x] -> atomic x
   _          -> False

instance Pretty (Binding5 ((Int, Int), String) String String) where
    p = \case
      Binding5 _ f xs body ->
        nest 4 (text (lblue ";") <+> text (green f) <+> hsep (map text xs) <+> text (lblue "=") $+$
          nest 4 (p body))

jsName = text . (>>= decorate_char)
  where
    decorate_char c
        | c `elem` (['A' .. 'Z'] ++ ['a' .. 'z'] ++ "_.")
            = return c
        | otherwise
            = "_" ++ show (ord c) ++ "_"

instance Show (AST5 ((Int, Int), String) String String) where
    show = show . p

data Binding5 info var lit = Binding5 info var [var] (AST5 info var lit)

data Located a = At { item :: a, loc :: ((Int, Int), String) }
    deriving (Eq)

instance Show a => Show (Located a) where
    show (x `At` ((l, p), file)) = show x ++ " @ " ++ file ++ ":" ++ show l ++ ":" ++ show p

countChars :: (Int, Int) -> Char -> (Int, Int)
countChars (l, p) '\n' = (l + 1, 1)
countChars (l, p)   c  = (l, p + 1)

pinpoint file str = zipWith At str (zip (scanl countChars (1, 1) str) (repeat file))

type Tokenizer s = s -> (Maybe ([s], s))

wither :: [Located Char] -> Located String
wither (c `At` loc : cs) = (c : map item cs) `At` loc

firstMatch :: a -> [a -> Maybe b] -> Maybe b
firstMatch a fs = firstJust (map ($ a) fs)

firstJust = headOr Nothing . filter isJust

headOr def []       = def
headOr _   (x : xs) = x

tokenize :: [Tokenizer [Located Char]] -> [Located Char] -> [Located String]
tokenize rules stream
    | Just (token, other) <- firstMatch stream rules
        = map wither token ++ tokenize rules other
    | otherwise
        = []

singleChar :: Tokenizer [a]
singleChar []        = Nothing
singleChar (c : cs) = Just ([[c]], cs)

dropAllThat pred (c `At` _ : rest) | pred c = Just ([], dropWhile (pred . item) rest)
dropAllThat _     _                         = Nothing

notAName = (`elem` " \n()[]{}")

name :: Tokenizer [Located Char]
name [] = Nothing
name stream @ (c `At` _ : _)
    | notAName c
        = Nothing

    | otherwise
        = let (tok, rest) = break (notAName . item) stream
          in Just ([tok], rest)

lineCommentStartingFrom :: String -> Tokenizer [Located Char]
lineCommentStartingFrom start stream
    | startswith start (map item stream)
        = let (_, rest) = break (('\n' ==) . item) (drop (length start) stream)
          in Just ([], if null rest then rest else tail rest)

    | otherwise
        = Nothing

(~>) :: String -> String -> Tokenizer [Located Char]
(l ~> r) str
    | startswith l chars =
        let (steps, rest) = break (startswith r) (tails (tail chars))
            len = length steps + length r + length l
        in Just ([take len str], drop len str)

    | otherwise =
        Nothing
  where
    chars = map item str

tokenizeFile :: String -> IO [Located String]
tokenizeFile file = do
    text <- readFile file

    let tokenizer = tokenize
            [ dropAllThat (`elem` " \t\n")
            , lineCommentStartingFrom "//"
            , "`" ~> "`"
            , "'" ~> "'"
            , ['"'] ~> ['"']
            , name
            , singleChar
            ]

    return (tokenizer (pinpoint file text))

data Parses a = Parses { runParse :: [Located String] -> ([Located String], Either [String] a) }

instance Functor Parses where
    fmap = liftM

instance Applicative Parses where
    pure  = return
    (<*>) = ap

instance Monad Parses where
    return x = Parses $ \s -> (s, Right x)
    fail   m = Parses $ \s -> (s, Left [m])

    p >>= amb = Parses $ \s ->
        case p `runParse` s of
            (s1, Left x) -> (s1, Left x)
            (s1, Right a) -> amb a `runParse` s1

instance Alternative Parses where
    empty = fail "no wai"

    l <|> r = Parses $ \s ->
        case l `runParse` s of
            (s1, Left x) | s1 == s ->
                case r `runParse` s of
                    (s2, Left y) ->
                        case compareStreams s1 s2 of
                            EQ -> (s2, Left (x ++ y))
                            GT -> (s1, Left x)
                            LT -> (s2, Left y)
                    other ->
                        other
            (s1, res)              -> (s1, res)

compareStreams [] [] = EQ
compareStreams [] _  = GT
compareStreams _  [] = LT
compareStreams (x `At` ((l, p), f) : _) (y `At` ((l1, p1), _) : _) =
    compare l l1 <> compare p p1

instance MonadPlus Parses where
    mzero = empty
    mplus = (<|>)

try :: Parses a -> Parses a
try p = Parses $ \s ->
    case p `runParse` s of
        (_, Left x) -> (s, Left x)
        other       -> other

token str = satisfying str (== str)

satisfying msg pred = Parses $ \s ->
    case s of
        x `At` _ : xs
            | pred x ->
                (xs, Right x)

        _ ->
            (s, Left [msg])

point = Parses $ \s ->
    case s of
        _ `At` loc : _ ->
            (s, Right loc)

        _ ->
            (s, Left ["EOF"])

literal = Lit5 <$> point <*> (intLiteral <|> stringLiteral <|> spliced')
  where
    intLiteral = satisfying "int" (\s -> head s `elem` "0123456789")
    stringLiteral = satisfying "string" (\s -> head s `elem` "\"\'")
    spliced = satisfying "spliced" (\s -> head s == '`')
    spliced' = (init . tail) <$> spliced

reserved = flip elem $ words "let in ; -> : = rec ( ) [ ] { }"

var = do
    Var5 <$> point <*> identifier

identifier = satisfying "name" (not . reserved)

lam = do
    pt <- point
    token "->"
    args <- many identifier
    token ":"
    body <- program

    return $ Lam5 pt args body

app = do
    App5 <$> point <*> some terminal

p `sepBy` sep = do
    x <- p
    xs <- many (sep *> p)
    return (x : xs)

letExpr = do
    pt <- point
    token "let"
    optional (token ";")
    bs <- binding `sepBy` token ";"
    token "in"
    body <- program

    return $ Let5 pt bs body

binding = do
    pt <- point
    f <- identifier
    xs <- many identifier
    token "="
    body <- program

    return $ Binding5 pt f xs body

terminal = literal <|> var <|> lam <|> letExpr <|> rec <|> (token "(" *> program <* token ")")

rec = do
    pt <- point
    token "rec"
    self <- identifier
    Lam5 info (arg : args) body <- lam
    if null args
    then do
        return (Rec5 pt self arg body)
    else do
        return (Rec5 pt self arg (Lam5 info args body))

program = app

parseFile p f = do
    toks <- tokenizeFile f
    -- mapM_ print toks
    case p `runParse` toks of
        ([], Right prog) -> do
            putStrLn "done."
            return prog

        (r, Right prog) -> do
            putStrLn "=== remains: ===="
            mapM_ print r
            return prog

        (pt : _, Left x) -> error $ "expected one of " ++ show x ++ ", but found " ++ show pt
        ([],     Left x) -> error $ "expected one of " ++ show x ++ ", but found EOF"

compile file = do
    ast <- parseFile program file

    let jsd = js ast

    writeFile (file ++ ".js") (show jsd)

main = do
   mapM compile =<< getArgs
