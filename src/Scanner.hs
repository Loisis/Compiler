module Scanner where

import           Data.Char  (isAlpha, isDigit)
import           Text.Regex 

-- MD: Markdown
data MDToken = T_Newline     -- '\n' 
             | T_H Int       -- ein Header mit der Anzahl der Hashes
             | T_Text String -- Text, aber immer nur bis zum Zeilenende, Text über mehrere Zeilen muss vom Parser zusammengesetzt werden
             | T_ULI Int     -- ein ungeordnetes Listenelement-Marker mit der (Einrückungs-)Ebene
             | T_LI Int      -- ein  Listenelemnt-Marker mit der (Einrückungs-) Ebene
             | T_B           -- Fettschrift
             | T_K           -- Kursiv
             | T_SpKlA       -- Spitze Klammer auf
             | T_SpKlZ       -- Spitze Klammer zu
             | T_EcKlA       -- Eckige Klammer auf
             | T_EcKlZ       -- Eckige Klammer zu
             | T_RuKlA       -- Runde Klammer auf
             | T_RuKlZ       -- Runde Klammer zu
             | T_ExMark      -- Ausrufezeichen
             | T_Backquote   -- `
             | T_ZU          -- Zeilenumbruch, durch 2 (TODO: oder auch mehr) Leerzeichen am Zeilenende
             | T_CB Int          -- Codeblock
    deriving (Show, Eq)

scan :: String -> Int -> Maybe [MDToken]

-- Rekursionsende
scan "" _          = Just []

-- eine Überschrift
scan str@('#':xs) level =
        -- String aufteilen in Hashes und Rest
    let (hashes, rest) = span (=='#') str
        -- Anzahl der Hashes ergibt das Level, aber höchstens 6 werden gezählt, der Rest ignoriert
        level = min (length hashes) 6
    in maybe Nothing (\tokens -> Just (T_H level:tokens))      $ scanline rest ""

-- Wennn ein Tab gelesen wird muss das Level erhöht werden
scan (' ':' ':' ':' ':xs) level   = maybe Nothing (\tokens -> Just (tokens)) $ scan xs (level +1)

-- Zeilenumbrüche aufheben um im Parser Leerzeilen zu erkennen -> Level auf 1 setzen
scan ('\n':xs) level   = maybe Nothing (\tokens -> Just (T_Newline:tokens)) $ scan xs 1

-- wenn das '-' am Zeilenanfang gelesen wird, ist es Level 1
-- TODO: noch sind wir sicher am Zeilenanfang, aber nicht mehr unbedingt, wenn wir weitere Fälle einbauen (Links etc.)
scan ('-':' ':xs) level    = maybe Nothing (\tokens -> Just (T_ULI level:tokens))    $ scanline xs ""
scan ('*':' ':xs) level   = maybe Nothing (\tokens -> Just (T_ULI level:tokens))    $ scanline xs ""
scan ('+':' ':xs) level   = maybe Nothing (\tokens -> Just (T_ULI level:tokens))    $ scanline xs ""


-- wenn wir eine Zahl mit einem Punkt lesen, dann ist es eine geordnete Liste
scan str level
      | isOList str =   let (number,rest) = span isDigit str
                            (dot, afterdot) = span (=='.') rest
                        in maybe Nothing (\tokens -> Just (T_LI level:tokens))    $ scanline afterdot ""
      | level /= 1 = maybe Nothing (\tokens -> Just (T_CB level:tokens))    $ scanCodeBlockLine str ""
      | otherwise = scanline str ""
-- Alte Version vom Braun
--      | otherwise =     let (restOfLine, restOfStr) = span (/='\n') str                                      
--                        in maybe Nothing (\tokens -> Just (T_Text restOfLine:tokens)) $ scan restOfStr text level

scanline :: String -> String -> Maybe [MDToken]

-- Explizieten Zeilenumbruch erkennen
scanline (' ':' ':'\n':xs) text
                | text == "" = maybe Nothing (\tokens -> Just (T_ZU:T_Newline:tokens))    $ scan xs 1
                | otherwise = maybe Nothing (\tokens -> Just (T_Text text:T_ZU:T_Newline:tokens))    $ scan xs 1

-- Zeilenende erkennen
scanline ('\n':xs) text
            | text == "" = maybe Nothing (\tokens -> Just (T_Newline:tokens))    $ scan xs 1
            | otherwise = maybe Nothing (\tokens -> Just (T_Text text:T_Newline:tokens))    $ scan xs 1

-- Fettschrift erkennen 
scanline ('*':'*':xs) text = maybe Nothing (\tokens -> Just (T_Text text:T_B:tokens))    $ scanline xs ""
scanline ('_':'_':xs) text = maybe Nothing (\tokens -> Just (T_Text text:T_B:tokens))    $ scanline xs ""
-- Kursivschrift erkennen
scanline ('*':xs) text = maybe Nothing (\tokens -> Just (T_Text text:T_K:tokens))    $ scanline xs ""
scanline ('_':xs) text = maybe Nothing (\tokens -> Just (T_Text text:T_K:tokens))    $ scanline xs ""

-- Backquote erkennen
scanline ('`':xs) text = maybe Nothing (\tokens -> Just (T_Text text:T_Backquote:tokens))    $ scanline xs "" 

-- Klammern erkennen
scanline ('<':xs) text 
            | text == "" = maybe Nothing (\tokens -> Just (T_SpKlA:tokens))    $ scanline xs ""
            | otherwise = maybe Nothing (\tokens -> Just (T_Text text:T_SpKlA:tokens))    $ scanline xs ""
         
scanline ('>':xs) text 
            | text == "" = maybe Nothing (\tokens -> Just (T_SpKlZ:tokens))    $ scanline xs ""
            | otherwise = maybe Nothing (\tokens -> Just (T_Text text:T_SpKlZ:tokens))    $ scanline xs ""
    
scanline ('[':xs) text 
            | text == "" = maybe Nothing (\tokens -> Just (T_EcKlA:tokens))    $ scanline xs ""
            | otherwise = maybe Nothing (\tokens -> Just (T_Text text:T_EcKlA:tokens))    $ scanline xs ""
            
scanline (']':xs) text 
            | text == "" = maybe Nothing (\tokens -> Just (T_EcKlZ:tokens))    $ scanline xs ""
            | otherwise = maybe Nothing (\tokens -> Just (T_Text text:T_EcKlZ:tokens))    $ scanline xs ""
            
scanline ('(':xs) text 
            | text == "" = maybe Nothing (\tokens -> Just (T_RuKlA:tokens))    $ scanline xs ""
            | otherwise = maybe Nothing (\tokens -> Just (T_Text text:T_RuKlA:tokens))    $ scanline xs ""
            
scanline (')':xs) text 
            | text == "" = maybe Nothing (\tokens -> Just (T_RuKlZ:tokens))    $ scanline xs ""
            | otherwise = maybe Nothing (\tokens -> Just (T_Text text:T_RuKlZ:tokens))    $ scanline xs ""
            
scanline ('!':xs) text 
            | text == "" = maybe Nothing (\tokens -> Just (T_ExMark:tokens))    $ scanline xs ""
            | otherwise = maybe Nothing (\tokens -> Just (T_Text text:T_ExMark:tokens))    $ scanline xs ""
            
-- Escape Sequenzen: 
scanline ('\\':x:xs) text 
                        | x == '\\'= maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x])
                        | x == '`' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x])
                        | x == '*' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x])
                        | x == '_' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x])
                        | x == '{' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x])
                        | x == '}' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x])
                        | x == '[' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x])
                        | x == ']' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x])
                        | x == '(' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x])
                        | x == ')' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x])
                        | x == '#' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x])
                        | x == '+' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x])
                        | x == '-' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x])
                        | x == '.' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x])
                        | x == '!' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x])
                        

-- Jedes andere Zeichen hinzufügen
scanline (x:xs) text = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x])

-- Ende der Rekursion
scanline "" _   = Just []  


scanCodeBlockLine :: String -> String -> Maybe [MDToken]

-- Ende der Rekursionen 
scanCodeBlockLine "" _  = Just []

scanCodeBlockLine (x:xs) text  
                     | x == '\n' = maybe Nothing(\tokens -> Just (T_Text text:T_Newline:tokens)) $ scan xs 1
                     | otherwise =  maybe Nothing (\tokens -> Just (tokens)) $ scanCodeBlockLine xs (text ++ [x]) 


-- Entscheidet, ob es der Anfang einer geordneten Liste ist, also eine Zahl gefolgt von einem Punkt
isOList :: String -> Bool
isOList str = let (number,rest) = span isDigit str
              in (take 1 rest) == ['.']

