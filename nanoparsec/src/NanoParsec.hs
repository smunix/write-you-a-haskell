{-# LANGUAGE OverloadedStrings #-}

module NanoParsec
  ( Parser
  , unParser
  , choice
  , char
  , digit
  , natural
  , number
  , float
  , double
  , string
  , reserved
  , token
  , spaces
  , some
  , many
  , satisfy
  , someSatisfy
  , oneOf
  , chainl
  , chainl1
  , parens
  , empty
  )
where

import Data.Char (isDigit)
import Control.Arrow (second, (&&&), (>>>))
import Control.Applicative hiding (some, many)
import Control.Monad

type Str = String -- use Text ?
newtype Parser a = Parser {unParser :: Str -> [(Str, a)]}

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (second f) . p

instance Applicative Parser where
  pure f = Parser $ \str -> [(str, f)]
  (Parser p1) <*> (Parser p2) = Parser $ \str -> [(s2, f v2) | (s1, f) <- (p1 str), (s2, v2) <- (p2 s1)]

instance Monad Parser where
  (Parser m) >>= f = Parser $ \str -> [(s', v') | (s, v) <- (m str), (s', v') <- unParser (f v) s]

instance Alternative Parser where
  empty = Parser $ const []
  (Parser p1) <|> (Parser p2) = Parser aux
    where
      aux str
        | null (p1 str) = p2 str
        | otherwise = p1 str

instance MonadPlus Parser where
  mzero = empty
  mplus (Parser p1) (Parser p2) = Parser $ (p1 &&& p2) >>> (\(as, bs) -> as++bs)

{- parses a single Char -}
aChar :: Parser Char
aChar = Parser aux
  where
    aux [] = []
    aux (c:cs) = [(cs, c)]

{- check current character -}
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- aChar
  if f c then return c else empty

{- parses a single Digit -}
digit :: Parser Int
digit = flip (-) (fromEnum '0') . fromEnum <$> satisfy (isDigit)

oneOf :: [Char] -> Parser Char
oneOf chars = satisfy (flip elem chars)

{- chaining effectful Parser actions -}
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p pOp =
  do
    v1 <- p
    rest v1
  where
    rest v1 = (do
      v2 <- p
      op <- pOp
      rest (op v1 v2)) <|> return v1

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl pP pOp v = (chainl1 pP pOp) <|> return v

{- one or more -}
some :: (Alternative f) => f a -> f [a]
some v = auxSome
  where
    auxSome = (:) <$> v <*> auxTail
    auxTail = auxSome <|> pure []

many :: (Alternative f) => f a -> f [a]
many v = auxMany
  where
    auxMany = auxTail <|> pure []
    auxTail = (:) <$> v <*> auxMany

someSatisfy :: (Char -> Bool) -> Parser [Char]
someSatisfy = some . satisfy

char :: Char -> Parser Char
char c = satisfy (c ==)

num :: (Read nm) => Parser nm
num = ((.).(.)) read (:) <$> (char '-' <|> return ' ') <*> someSatisfy isDigit

natural :: Parser Integer
natural = num

number :: Parser Int
number = num

floating :: (Read ft) => Parser ft
floating = do
  s <- char '-' <|> return ' '
  significant <- someSatisfy isDigit
  dot <- char '.' <|> return '.'
  decimal <- someSatisfy isDigit <|> return "0"
  read <$> return ((s:significant) ++ [dot] ++ decimal)

float :: Parser Float
float = floating

double :: Parser Double
double = floating

string :: String -> Parser String
string [] = return []
string (c:cs) = (:) <$> char c <*> string cs

spaces :: Parser String
spaces = many $ oneOf " \r\n"

token :: Parser a -> Parser a
token p = ((.).(.)) fst (,) <$> p <*> spaces

reserved :: String -> Parser String
reserved str = token (string str)

parens :: Parser a -> Parser a
parens m = do
  _ <- reserved "("
  v <- m
  _ <- reserved ")"
  return v

choice :: Parser a -> Parser a -> Parser a
choice = (<|>)
