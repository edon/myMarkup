module Markup where 

data TaggedMarkup = 
    Text String
    | Tag String TaggedMarkup
      deriving (Show) 

data ListType = Ordered | Unordered
                deriving (Show)
data Node = 
    Paragraph
    | Header Int
    | BlockQuote
    | List ListType
    | ListItem 
    | Verbatim
      deriving (Show) 
data MarkupAST = Node Node [MarkupAST] | Leaf [TaggedMarkup]
                 deriving (Show) 

