import Text.ParserCombinators.Parsec hiding (newline)
import qualified Data.Set as S
import qualified Tokenizer as T
import Markup

type MarkupParser a = GenParser T.Tok () a

-- testMarkup

testMarkup :: MarkupParser a -> String -> Either ParseError a
testMarkup p = runParser p () "" . T.toTok . T.tokenize 

paragraph :: MarkupParser MarkupAST
paragraph = do 
  xs <- many1 stext
  paragraphEnd
  return $ Node Paragraph [Leaf xs]

-- A paragraph ends because of two new lines, with spaces
-- in between, or one newline and eof or just eof.
paragraphEnd :: MarkupParser ()
paragraphEnd = newline >> 
               (newline <|> (blanks >> newline) <|> eof)

blanks :: MarkupParser ()
blanks = do 
  s <- stext
  case s of 
    Text s' -> if s' `only` " \t" then return () else pzero
    otherwise -> pzero
    where s `only` p = S.null $ 
                       S.fromList s `S.difference` S.fromList p



-- The simple parsers

mytoken :: (T.Token -> Maybe a) -> MarkupParser a
mytoken test
  = token showToken posToken testToken
  where
    showToken (pos,tok)   = show tok
    posToken  (pos,tok)   = pos
    testToken (pos,tok)   = test tok

newline :: MarkupParser ()
newline 
  = mytoken (\tok -> case tok of 
                       T.Newline -> Just ()
                       other   -> Nothing)
stext :: MarkupParser TaggedMarkup
stext 
  = mytoken (\tok -> case tok of
                      T.SText t -> Just t
                      other   -> Nothing)

indent :: MarkupParser Int
indent 
  = mytoken (\tok -> case tok of
                      T.Indent i -> Just i
                      other    -> Nothing)

sheader :: MarkupParser Int
sheader 
  = mytoken (\tok -> case tok of
                      T.Header i -> Just i
                      other    -> Nothing)

unorderedliste :: MarkupParser ()
unorderedliste 
  = mytoken (\tok -> case tok of
                      T.UnOrderedListE -> Just ()
                      other          -> Nothing)
orderedliste :: MarkupParser ()
orderedliste 
  = mytoken (\tok -> case tok of 
                      T.OrderedListE -> Just ()
                      other        -> Nothing)
