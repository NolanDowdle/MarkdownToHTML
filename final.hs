import Data.Char
import System.IO

--Created for an undergraduate university project
--this project converts a markdown file to an HTML file

--data to hold the tags in html and markdown
data Tag = Bold | Italics | BoldItalics | Blockquote |
           Header | UnorderedList | OrderedList | Paragraph |
           P String | Err | NewLine | Header2 | Header3 | Header4 |
           Header5 | Header6 | ListBegin | ULListBegin | LinkBegin | LinkEnd |
           OpenParenthesis | CloseParenthesis | ExclamationPoint | TabC
    deriving Show

--classifys the individual words as tokens (tags)
classify :: String -> Tag
classify "######" = Header6
classify "#####" = Header5
classify "####" = Header4
classify "###" = Header3
classify "##" = Header2
classify "***" = BoldItalics
classify "___" = BoldItalics
classify "**" = Bold
classify "__" = Bold
classify "*" = Italics
classify "_" = Italics
classify "#" = Header
classify "-" = UnorderedList
classify "+" = UnorderedList
classify ">" = Blockquote
classify "[" = LinkBegin
classify "]" = LinkEnd
classify "(" = OpenParenthesis
classify ")" = CloseParenthesis
classify "!" = ExclamationPoint
classify "$NL" = NewLine
classify "$TB" = TabC
classify (x:'.':xs) = if isDigit' x then OrderedList else classify xs
classify x = P x
--need to figure out how to differentiate between * for italics or unordered list

--checks if input char is a digit (used in classify for ordered list)
isDigit' :: Char -> Bool
isDigit' x = '0' <= x && x <= '9'

--parses the input array of tags
parse :: [Tag] -> String
parse x = sr [] x

--breaks the individual words into tags
lexer :: String -> [Tag]
lexer x = map classify (words (addSpaces (prep x)))

--adds spaces to each "token" to make sure the classify method can recognize them (preprocessor)
addSpaces :: String -> String
addSpaces "" = ""
addSpaces ('#':'#':'#':'#':'#':'#':s) = " ###### " ++ addSpaces s
addSpaces ('#':'#':'#':'#':'#':s) = " ##### " ++ addSpaces s
addSpaces ('#':'#':'#':'#':s) = " #### " ++ addSpaces s
addSpaces ('#':'#':'#':s) = " ### " ++ addSpaces s
addSpaces ('#':'#':s) = " ## " ++ addSpaces s
addSpaces ('*':'*':'*':s) = " *** " ++ addSpaces s
addSpaces ('_':'_':'_':s) = " ___ " ++ addSpaces s
addSpaces ('*':'*':s) = " ** " ++ addSpaces s
addSpaces ('_':'_':s) = " __ " ++ addSpaces s
addSpaces ('*':s) = " * " ++ addSpaces s
addSpaces ('_':s) = " _ " ++ addSpaces s
addSpaces ('#':s) = " # " ++ addSpaces s
addSpaces ('+':s) = " + " ++ addSpaces s
addSpaces ('>':s) = " > " ++ addSpaces s
addSpaces ('<':s) = " < " ++ addSpaces s
addSpaces ('[':s) = " [ " ++ addSpaces s
addSpaces (']':s) = " ] " ++ addSpaces s
addSpaces ('(':s) = " ( " ++ addSpaces s
addSpaces (')':s) = " ) " ++ addSpaces s
addSpaces ('!':s) = " ! " ++ addSpaces s
addSpaces ('$':'N':'L':s) = " $NL " ++ addSpaces s
addSpaces ('$':'T':'B':s) = " $TB " ++ addSpaces s
addSpaces (x : s) = x : addSpaces s

--preprocessor to identify \n as $NEWLINE, so they aren't removed when running
--words in the lexer
prep :: String -> String
prep "" = ""
prep ('\n':xs) = "$NL" ++ prep xs
prep (' ':' ':' ':' ':xs) = "$TB" ++ prep xs
prep (x:xs) = x : prep xs

--shift reduce function
sr :: [Tag] -> [Tag] -> String
sr (P x : P xs : ts) [] = sr (P (xs++x) : ts) []
sr (P x : P xs : ts) i = sr (P (xs++x) : ts) i
sr (P x : ts) (P xi:i) = sr (P (" " ++ xi) : P x : ts) i
sr (P x : r : ts) (P xi:i) = sr (P xi : P x : r : ts) i

sr (CloseParenthesis : P xi : OpenParenthesis : LinkEnd : P x : LinkBegin : ExclamationPoint : ts) i = sr (P ("<img src=\"" ++ xi ++ "\" alt=\"" ++ x ++ "\">") : ts) i
sr (CloseParenthesis : P xi : OpenParenthesis : LinkEnd : P x : LinkBegin : ts) i = sr (P ("<a href=\"" ++ xi ++ "\"> " ++ x ++ " </a>") : ts) i
sr (CloseParenthesis : P x : OpenParenthesis : ts) i = sr (P ("(" ++ x ++ ")") : ts) i
sr (BoldItalics : P x : BoldItalics : ts) i = sr (P ("<strong><em> " ++ x ++ " </em></strong>") : ts) i
sr (Bold : P x : Bold : ts) i = sr (P ("<strong> " ++ x ++ " </strong>") : ts) i
sr (Italics : P x : Italics : ts) i = sr (P ("<em> " ++ x ++ " </em>") : ts) i
sr (NewLine : ts) i = sr (P "<br>" : ts) i
sr (P x : UnorderedList : ULListBegin : ts) (NewLine:i) = sr (NewLine : P ("<li>" ++ x ++ "</li>") : ULListBegin : ts) i
sr (P x : UnorderedList : P xi : ULListBegin : ts) (NewLine:i) = sr (NewLine : P ("<li>" ++ x ++ "</li>") : P xi : ULListBegin : ts) i
sr (P x : UnorderedList : ts) (NewLine:i) = sr (NewLine : P ("<li>" ++ x ++ "</li>") : ULListBegin : ts) i
sr (UnorderedList : P x : ULListBegin : ts) (P r:i) = sr (P r : UnorderedList : P x : ULListBegin : ts) i
sr (P x : ULListBegin : ts) [] = sr (P ("<ul>" ++ x ++ "</ul>") : ts) []
sr (P x : OrderedList : ListBegin : ts) (NewLine:i) = sr (NewLine : P ("<li>" ++ x ++ "</li>") : ListBegin : ts) i
sr (P x : OrderedList : P xi : ListBegin : ts) (NewLine:i) = sr (NewLine : P ("<li>" ++ x ++ "</li>") : P xi : ListBegin : ts) i
sr (P x : OrderedList : ts) (NewLine:i) = sr (NewLine : P ("<li>" ++ x ++ "</li>") : ListBegin : ts) i
sr (P x : UnorderedList : TabC : ts) i = sr (P ("    ") : P x : UnorderedList : ts) i
sr (P x : ULListBegin : TabC : ts) i = sr (P ("    ") : P x : ULListBegin :ts) i
sr (P x : UnorderedList : TabC : ts) i = sr (P ("    ") : P x : UnorderedList : ULListBegin : ts) i
sr (OrderedList : P x : ListBegin : ts) (P r:i) = sr (P r : OrderedList : P x : ListBegin : ts) i
sr (P x : ListBegin : ts) [] = sr (P ("<ol>" ++ x ++ "</ol>") : ts) []
sr (P x : Header : ts) (ExclamationPoint:i) = sr (P (x ++ "!") : Header : ts) i
sr (P x : Header2 : ts) (ExclamationPoint:i) = sr (P (x ++ "!") : Header2 : ts) i
sr (P x : Header3 : ts) (ExclamationPoint:i) = sr (P (x ++ "!") : Header3 : ts) i
sr (P x : Header4 : ts) (ExclamationPoint:i) = sr (P (x ++ "!") : Header4 : ts) i
sr (P x : Header5 : ts) (ExclamationPoint:i) = sr (P (x ++ "!") : Header5 : ts) i
sr (P x : Header6 : ts) (ExclamationPoint:i) = sr (P (x ++ "!") : Header6 : ts) i
sr (ExclamationPoint : P x : ts) (LinkBegin:i) = sr (LinkBegin : ExclamationPoint : P x : ts) i
sr (ExclamationPoint : P x : ts) i = sr (P (x ++ "!") : ts) i
sr (P x : Header : ts) i = sr (P ("<h1>" ++ x ++ "</h1>") : ts) i
sr (P x : Header2 : ts) i = sr (P ("<h2>" ++ x ++ "</h2>") : ts) i
sr (P x : Header3 : ts) i = sr (P ("<h3>" ++ x ++ "</h3>") : ts) i
sr (P x : Header4 : ts) i = sr (P ("<h4>" ++ x ++ "</h4>") : ts) i
sr (P x : Header5 : ts) i = sr (P ("<h5>" ++ x ++ "</h5>") : ts) i
sr (P x : Header6 : ts) i = sr (P ("<h6>" ++ x ++ "</h6>") : ts) i
sr (P x : Blockquote : ts) (NewLine:i) = sr (NewLine : P ("<blockquote>" ++ x ++ "</blockquote>") : ts) i
sr (l : P x : ULListBegin : ts) i = sr (l : P ("<ul>" ++ x ++ "</ul>") : ts) i
sr (l : P x : ListBegin : ts) i =  sr (l : P ("<ol>" ++ x ++ "</ol>") : ts) i
sr [P p] [] = p
sr xs (i:is) = sr (i:xs) is
sr (Err : _) _ = error "Lexical error!"
sr x [] = error (show x)

--basically our execute or run method
createOutputString :: String -> IO String
createOutputString x = return (parse (lexer x))

--main function that takes input (markdown file name) and creates output (a new .html file)
main :: IO()
main = do 
    putStr "Which Markdown file would you like to convert to HTML? \n"
    fileName <- getLine
    file <- openFile fileName ReadMode
    content <- hGetContents file
    output <- createOutputString content
    putStr "What would you like your .html file to be called? (include .html file extension on the end) \n"
    outputFileName <- getLine
    outputFile <- openFile outputFileName WriteMode
    hPutStr outputFile output
    hClose file
    hClose outputFile
    putStrLn "Finished creating HTML file!"

