module Markup where 

data TaggedMarkup = 
    Text String
    | Tag String Text
      deriving (Show, Eq)

type Text = [TaggedMarkup] 

data ListType = 
    Ordered 
    | Unordered
      deriving (Show)

data Node = 
    Body
    | Paragraph
    | Header Int
    | BlockQuote
    | List ListType
    | ListItem 
    | Verbatim
      deriving (Show) 

data MarkupAST = 
    Node Node [MarkupAST] 
    | Leaf Text
      deriving (Show) 
