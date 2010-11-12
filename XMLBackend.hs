module XMLBackend (markupToXML) where
import Markup

taggedToXML :: TaggedMarkup -> String
taggedToXML (Text s) = s
taggedToXML (Tag t x) = 
    "<" ++ t ++ ">" 
    ++ concatMap  taggedToXML x
    ++ "</" ++ t ++ ">"

markupToXML :: MarkupAST -> String
markupToXML s = 
    let markupToXML' _ (Leaf xs) = concatMap taggedToXML xs
        markupToXML' i (Node Paragraph xs) = 
            replicate i ' ' 
            ++ "<p>"
            ++ concatMap (markupToXML' i) xs 
            ++ "</p>\n"
        markupToXML' i (Node (Header j) xs) = 
            replicate i ' ' 
            ++ "<h"
            ++ show j
            ++ ">"
            ++ concatMap (markupToXML' i) xs
            ++ "</h"
            ++ show j
            ++ ">\n"
        markupToXML' i (Node BlockQuote xs) = 
            replicate i ' '
            ++ "<blockquote>\n"
            ++ concatMap (markupToXML' (i+4)) xs
            ++ replicate i ' '
            ++ "</blockquote>\n"
        markupToXML' i (Node Verbatim xs) = 
            replicate i ' '
            ++ "<pre>"
            ++ concatMap (markupToXML' i) xs
            ++ "</pre>\n"
        markupToXML' i (Node (List Ordered) xs) = 
            replicate i ' '
            ++ "<ol>\n"
            ++ concatMap (markupToXML' (i+4)) xs
            ++ replicate i ' '
            ++ "</ol>\n"
        markupToXML' i (Node (List Unordered) xs) = 
            replicate i ' '
            ++ "<ul>\n"
            ++ concatMap (markupToXML' (i+4)) xs
            ++ replicate i ' '
            ++ "</ul>\n"
        markupToXML' i (Node ListItem xs) = 
            replicate i ' '
            ++ "<li>\n"
            ++ concatMap (markupToXML' (i+4)) xs
            ++ replicate i ' '
            ++ "</li>\n"
    in "<body>\n" ++
       markupToXML' 4 s ++
       "</body>\n"
