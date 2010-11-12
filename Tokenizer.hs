module Tokenizer
       ( Token(..), 
         Tok,
         toTok,
         tokenize
       )
where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Data.List (intersperse)
import Data.List.Split (splitOn)

import Markup hiding (Header)

data Token = SText TaggedMarkup
           | Indent Int
           | Newline
           | Header Int
           | UnOrderedListE
           | OrderedListE
             deriving (Show, Eq)

type Tok = (SourcePos, Token)

toTok :: [Token] -> [Tok]
toTok ts = map (\(n,m,t) -> (newPos "" n m,t)) $ f ts
    where 
      f :: [Token] -> [(Int,Int,Token)]
      f ts = f' 1 1 ts
      f' :: Int -> Int -> [Token] -> [(Int,Int,Token)]
      f' _ _ [] = []
      f' n m (t:ts) = (n,m,t):
                      if t == Newline
                      then f' (n+1) 1 ts
                      else f' n (m+1) ts

tokenize :: String -> [Token]
tokenize = tokenizeText . preprocess

-- This catches all the markups, since we don't want to tokenize
-- what's inside them
preprocess :: String -> Text
preprocess s = 
    case parse (many $ getSTextTill "\\" <|> markup) "" s of
      Right r -> r
      Left e -> []
--      Left e -> error $ "Couldn\'t tokenize: " ++ show e


-- This tokenizes all the Text values, by tokenizing one by one
-- for indents, headers, and list element signs, there probably is
-- a better performing solution, but performance isn't an issue here
tokenizeText :: Text -> [Token]
tokenizeText ts = 
    filter (not . emptySText) $ tail 
    $ mapAfter tokenOrdered 
    $ mapAfter tokenUnOrdered 
    $ mapAfter tokenHeaders 
    $ mapAfter tokenIndents ts'
        where 
          ts' = Newline : (concatMap tokenNewlines $ map SText ts)

-- These functions divide a token into two or more other tokens
-- For example tokenNewlines gets a SText Text value, finds
-- newlines and inserts a Newline value in their respective places

tokenNewlines :: Token -> [Token]
tokenNewlines (SText (Text s)) = 
    intersperse Newline $ map (SText . Text) $ splitOn "\n" s
tokenNewlines r = [r]


tokenIndents :: Token -> [Token]
tokenIndents (SText (Text s)) =
    case parse parseSpaces "" s of
      Right (ss, others) -> [Indent ss, SText $ Text others]
      Left _ -> [SText $ Text s]
tokenIndents r = [r]

tokenHeaders :: Token -> [Token]
tokenHeaders (SText (Text s)) = 
    case parse parseStars "" s of 
      Right (stars, others) -> [Header stars, SText $ Text others]
      Left _ -> [SText $ Text s]
tokenHeaders r = [r]

tokenUnOrdered :: Token -> [Token]
tokenUnOrdered (SText (Text s)) =
    case parse parseUnordered "" s of
      Right r -> [UnOrderedListE, SText $ Text r]
      Left _ -> [SText $ Text s]
tokenUnOrdered r = [r]

tokenOrdered :: Token -> [Token]
tokenOrdered (SText (Text s)) = 
    case parse parseOrdered "" s of
      Right r -> [OrderedListE, SText $ Text r]
      Left _ -> [SText $ Text s]
tokenOrdered r = [r]

-- Some tokens make sense only after the newline like for example
-- the indents, this makes sure to apply the token function only
-- to those tokens
mapAfter :: (Token -> [Token]) -> [Token] -> [Token]
mapAfter _ [] = []
mapAfter _ [x] = [x]
mapAfter f (SText s:xs) = SText s : mapAfter f xs
mapAfter f (y:x:xs) = if f x == [x]
                      then y : mapAfter f (x:xs)
                      else [y] ++ f x ++ mapAfter f xs

emptySText :: Token -> Bool
emptySText (SText (Text "")) = True
emptySText _ = False

-- Some very simple parsers

getSTextTill :: String -> Parser TaggedMarkup
getSTextTill s = do 
  str <- many1 $ noneOf s
  return $ Text str

markup :: Parser TaggedMarkup
markup = do
  char '\\'
  name <- many1 $ noneOf "{"
  char '{'
  text <- manyTill (getSTextTill "}\\" <|> markup) (char '}')
  return $ Tag name text

parseSpaces :: Parser (Int, String)
parseSpaces = do
  ss <- many1 $ char ' '
  others <- many anyChar
  return (length ss, others)

parseUnordered :: Parser String
parseUnordered = char '-' >> char ' ' >> many anyChar 

parseOrdered :: Parser String
parseOrdered = char '#' >> char ' ' >> many anyChar

parseStars :: Parser (Int, String)
parseStars = do
  stars <- many1 $ char '*'
  char ' ' -- eat the space
  others <- many anyChar
  return (length stars, others)
