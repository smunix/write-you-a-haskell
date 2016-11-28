module Main where

import qualified NanoParsec as NP

str :: String
str = unlines ["Providence has 1.89 percent"
              , "Tito has 1.89 percent"
              , "Begin has 1.99 percent"]

data Sentence = Sentence { subject :: String,
                           verb    :: String,
                           double  :: Double,
                           percent :: String
                         } deriving (Show)

parser :: [String] -> NP.Parser Sentence
parser (s:sxs) = do
  p   <- foldl (\a -> \e -> a `NP.choice` NP.reserved e) (NP.reserved s) sxs
  h   <- NP.reserved "has"
  v   <- NP.token NP.double
  pct <- NP.reserved "percent"
  return $ Sentence p h v pct
parser _ = NP.empty

main :: IO ()
main = do
  print $ "Parsing \"" ++ str ++ "\" ..."
  print $ NP.unParser (NP.many $ parser ["Providence", "Tito", "Begin"]) str
