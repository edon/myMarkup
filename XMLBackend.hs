import Markup

taggedToXML :: TaggedMarkup -> String
taggedToXML (Text s) = s
taggedToXML (Tag t x) = "<" ++ t ++ ">" 
                      ++ taggedToXML x ++
                      "</" ++ t ++ ">"

markupToXML :: MarkupAST -> String
markupToXML (Leaf xs) = concatMap taggedToXML xs
markupToXML (Node Paragraph xs) = 
    "\n<p>\n" ++ concatMap markupToXML xs ++ "\n</p>\n"
markupToXML (Node (Header i) xs) = 
    let otag = "\n<h" ++ show i ++ ">\n"
        ctag = "\n</h" ++ show i ++ ">\n"
    in otag ++ concatMap markupToXML xs ++ ctag
markupToXML (Node BlockQuote xs) = 
    "\n<blockquote>\n" ++ concatMap  markupToXML xs
    ++ "\n</blockquote>\n"
markupToXML (Node Verbatim xs) = 
    "\n<verbatim>\n" ++ concatMap markupToXML xs
    ++ "\n</verbatim>\n"
markupToXML (Node (List Ordered) xs) = 
    "\n<ol>\n" ++ concatMap markupToXML xs ++ "\n</ol>\n"
markupToXML (Node (List Unordered) xs) = 
    "\n<ul>\n" ++ concatMap markupToXML xs ++ "\n</ul>\n"
markupToXML (Node ListItem xs) = 
    "\n<li>\n" ++ concatMap markupToXML xs ++ "\n</li>\n" 
