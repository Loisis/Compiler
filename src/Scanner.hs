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
    in maybe Nothing (\tokens -> Just (T_H level:tokens))      $ scan rest level

-- Wennn ein Tab gelesen wird muss das Level erhöht werden
scan (' ':' ':' ':' ':xs) level   = maybe Nothing (\tokens -> Just (tokens)) $ scan xs (level +1)

-- Zeilenumbrüche aufheben um im Parser Leerzeilen zu erkennen -> Level auf 1 setzen
scan ('\n':xs) level   = maybe Nothing (\tokens -> Just (T_Newline:tokens)) $ scan xs 1

-- wenn das '-' am Zeilenanfang gelesen wird, ist es Level 1
-- TODO: noch sind wir sicher am Zeilenanfang, aber nicht mehr unbedingt, wenn wir weitere Fälle einbauen (Links etc.)
scan ('-':' ':xs) level    = maybe Nothing (\tokens -> Just (T_ULI level:tokens))    $ scan xs level
scan ('*':' ':xs) level   = maybe Nothing (\tokens -> Just (T_ULI level:tokens))    $ scan xs level
scan ('+':' ':xs) level   = maybe Nothing (\tokens -> Just (T_ULI level:tokens))    $ scan xs level

-- Fettschrift erkennen
scan ('*':'*':xs) level   = maybe Nothing (\tokens -> Just (T_B:tokens))    $ scan xs level
scan ('_':'_':xs) level   = maybe Nothing (\tokens -> Just (T_B:tokens))    $ scan xs level

-- Kursiv erkennen
scan ('*':xs) level   = maybe Nothing (\tokens -> Just (T_K:tokens))    $ scan xs level
scan ('_':xs)level   = maybe Nothing (\tokens -> Just (T_K:tokens))    $ scan xs level

-- Backquote erkennen
scan ('`':xs)level   = maybe Nothing (\tokens -> Just (T_Backquote:tokens))    $ scan xs level

-- Klammern erkennen
scan ('<':xs) level   = maybe Nothing (\tokens -> Just (T_SpKlA:tokens))    $ scan xs level
scan ('>':xs)level   = maybe Nothing (\tokens -> Just (T_SpKlZ:tokens))    $ scan xs level
scan ('[':xs) level   = maybe Nothing (\tokens -> Just (T_EcKlA:tokens))    $ scan xs level
scan (']':xs)level   = maybe Nothing (\tokens -> Just (T_EcKlZ:tokens))    $ scan xs level
scan ('(':xs) level   = maybe Nothing (\tokens -> Just (T_RuKlA:tokens))    $ scan xs level
scan (')':xs)level   = maybe Nothing (\tokens -> Just (T_RuKlZ:tokens))    $ scan xs level

scan ('!':xs)level   = maybe Nothing (\tokens -> Just (T_ExMark:tokens))    $ scan xs level

-- wenn wir eine Zahl mit einem Punkt lesen, dann ist es eine geordnete Liste
-- TODO: noch sind wir sicher am Zeilenanfang, aber nicht mehr unbedingt, wenn wir weitere Fälle einbauen (Links etc.)
scan str level
      | isOList str =   let (number,rest) = span isDigit str
                            (dot, afterdot) = span (=='.') rest
                        in maybe Nothing (\tokens -> Just (T_LI level:tokens))    $ scanline afterdot "" 1
      | otherwise = scanline str "" 1
-- Alte Version vom Braun
--      | otherwise =     let (restOfLine, restOfStr) = span (/='\n') str                                      
--                        in maybe Nothing (\tokens -> Just (T_Text restOfLine:tokens)) $ scan restOfStr text level

scanline :: String -> String -> Int -> Maybe [MDToken]

-- Explizieten Zeilenumbruch erkennen
scanline (' ':' ':'\n':xs) text level = maybe Nothing (\tokens -> Just (T_Text text:T_ZU:T_Newline:tokens))    $ scan xs  level

-- Zeilenende erkennen
scanline ('\n':xs) text level = maybe Nothing (\tokens -> Just (T_Text text:T_Newline:tokens))    $ scan xs  level

-- Fettschrift erkennen 
scanline ('*':'*':xs) text level = maybe Nothing (\tokens -> Just (T_Text text:T_B:tokens))    $ scanline xs "" level
scanline ('_':'_':xs) text level = maybe Nothing (\tokens -> Just (T_Text text:T_B:tokens))    $ scanline xs "" level
-- Kursivschrift erkennen
scanline ('*':xs) text level = maybe Nothing (\tokens -> Just (T_Text text:T_K:tokens))    $ scanline xs "" level
scanline ('_':xs) text level = maybe Nothing (\tokens -> Just (T_Text text:T_K:tokens))    $ scanline xs "" level

-- Backquote erkennen
scanline ('`':xs) text level = maybe Nothing (\tokens -> Just (T_Text text:T_Backquote:tokens))    $ scan xs level

-- Klammern erkennen
scanline ('<':xs) text level   = maybe Nothing (\tokens -> Just (T_Text text:T_SpKlA:tokens))    $ scan xs level
scanline ('>':xs) text level   = maybe Nothing (\tokens -> Just (T_Text text:T_SpKlZ:tokens))    $ scan xs level
scanline ('[':xs) text level   = maybe Nothing (\tokens -> Just (T_Text text:T_EcKlA:tokens))    $ scan xs level
scanline (']':xs) text level   = maybe Nothing (\tokens -> Just (T_Text text:T_EcKlZ:tokens))    $ scan xs level
scanline ('(':xs) text level   = maybe Nothing (\tokens -> Just (T_Text text:T_RuKlA:tokens))    $ scan xs level
scanline (')':xs) text level   = maybe Nothing (\tokens -> Just (T_Text text:T_RuKlZ:tokens))    $ scan xs level

scanline ('!':xs) text level   = maybe Nothing (\tokens -> Just (T_Text text:T_ExMark:tokens))    $ scan xs level

-- Escape Sequenzen: 
scanline ('\\':x:xs) text level 
                        | x == '\\'= maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x]) level
                        | x == '`' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x]) level
                        | x == '*' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x]) level
                        | x == '_' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x]) level
                        | x == '{' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x]) level
                        | x == '}' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x]) level
                        | x == '[' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x]) level
                        | x == ']' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x]) level
                        | x == '(' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x]) level
                        | x == ')' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x]) level
                        | x == '#' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x]) level
                        | x == '+' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x]) level
                        | x == '-' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x]) level
                        | x == '.' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x]) level
                        | x == '!' = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x]) level
                        

-- Jedes andere Zeichen hinzufügen
scanline (x:xs) text level = maybe Nothing (\tokens -> Just (tokens)) $ scanline xs (text ++ [x]) level

-- Ende der Rekursion
scanline "" _ _          = Just []  

-- Entscheidet, ob es der Anfang einer geordneten Liste ist, also eine Zahl gefolgt von einem Punkt
isOList :: String -> Bool
isOList str = let (number,rest) = span isDigit str
              in (take 1 rest) == ['.']

