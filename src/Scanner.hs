module Scanner where

import           Data.Char  (isAlpha, isDigit)
import           Text.Regex 

-- MD: Markdown
data MDToken = T_Newline     -- '\n' 
             | T_H Int       -- ein Header mit der Anzahl der Hashes
             | T_Text String -- Text, aber immer nur bis zum Zeilenende, Text über mehrere Zeilen muss vom Parser zusammengesetzt werden
             | T_ULI Int     -- ein ungeordnetes Listenelement-Marker mit der (Einrückungs-)Ebene
             | T_LI Int   -- ein  Listenelemnt-Marker mit der (Einrückungs-) Ebene
             | T_B         --  Fettschrift
             | T_K         --- Kursiv
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

-- Zeilenumbrüche aufheben um im Parser Leerzeilen zu erkennen -> Level auf 0 setzen
scan ('\n':xs) level   = maybe Nothing (\tokens -> Just (T_Newline:tokens)) $ scan xs 0

-- wenn das '-' am Zeilenanfang gelesen wird, ist es Level 0
-- TODO: noch sind wir sicher am Zeilenanfang, aber nicht mehr unbedingt, wenn wir weitere Fälle einbauen (Links etc.)
scan ('-':' ':xs) level    = maybe Nothing (\tokens -> Just (T_ULI level:tokens))    $ scan xs level
scan ('*':' ':xs) level   = maybe Nothing (\tokens -> Just (T_ULI level:tokens))    $ scan xs level
scan ('+':' ':xs) level   = maybe Nothing (\tokens -> Just (T_ULI level:tokens))    $ scan xs level

-- Fettschrift erkennen
scan ('*':'*':xs) level   = maybe Nothing (\tokens -> Just (T_B:tokens))    $ scan xs level
scan ('_':'_':xs) level   = maybe Nothing (\tokens -> Just (T_B:tokens))    $ scan xs level

-- Kursiv erkennen
scan ('*':xs) level   = maybe Nothing (\tokens -> Just (T_K:tokens))    $ scan xs level
scan ('_':xs) level   = maybe Nothing (\tokens -> Just (T_K:tokens))    $ scan xs level

-- wenn wir eine Zahl mit einem Punkt lesen, dann ist es eine geordnete Liste
-- TODO: noch sind wir sicher am Zeilenanfang, aber nicht mehr unbedingt, wenn wir weitere Fälle einbauen (Links etc.)
scan str level
      | isOList str =   let (number,rest) = span isDigit str
                            (dot, text) = span (=='.') rest
                        in maybe Nothing (\tokens -> Just (T_LI level:tokens))    $ scan text level
      | otherwise =     let (restOfLine, restOfStr) = span (/='\n') str                                      
                        in maybe Nothing (\tokens -> Just (T_Text restOfLine:tokens)) $ scan restOfStr level


-- Entscheidet, ob es der Anfang einer geordneten Liste ist, also eine Zahl gefolgt von einem Punkt
isOList :: String -> Bool
isOList str = let (number,rest) = span isDigit str
              in (take 1 rest) == ['.']

