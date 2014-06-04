module Parser ( parse {- nur parse exportieren -} )
    where

import           IR
import           Scanner

-- Der Parser versucht aus einer Liste von MDToken einen AST zu erzeugen 
parse :: [MDToken] -> Bool -> Bool -> Maybe AST
-- Die leere Liste ergibt eine leere Sequenz
parse [] b k                      = Just $ Sequence []

-- 2 Leerzeichen hintereinander vor dem Zeilenumbruch erzeugen expliziten Zeilenumbruch
parse(T_ZU:xs) b k = maybe Nothing (\(Sequence ast) -> Just $ Sequence (EZU : ast)) $ parse xs b k
-- Zwei Zeilenumbrüche hintereinander sind eine leere Zeile, die in eine Sequenz eingeführt wird (wirklich immer?)
parse (T_Newline:T_Newline:xs) b k = maybe Nothing (\(Sequence ast) -> Just $ Sequence (EmptyLine : ast)) $ parse xs b k
-- ein einzelnes Leerzeichen ignorieren wir (vorerst?)
parse (T_Newline:xs) b k         = parse xs b k
-- einem Header muss ein Text folgen. Das ergibt zusammen einen Header im AST, er wird einer Sequenz hinzugefügt
parse (T_H i : T_Text str: xs) b k  = maybe Nothing (\(Sequence ast) -> Just $ Sequence (H i str:ast)) $ parse xs b k


-- Fett erkennen
parse(T_B:xs) b k  
                  | b = maybe Nothing (\(Sequence ast) -> Just $ Sequence (BE : ast)) $ parse xs False k
                  | otherwise = maybe Nothing (\(Sequence ast) -> Just $ Sequence (BS : ast)) $ parse xs True k
-- Kursiv erkennen
parse(T_K:xs) b k  
                  | k = maybe Nothing (\(Sequence ast) -> Just $ Sequence (KE : ast)) $ parse xs b False
                  | otherwise = maybe Nothing (\(Sequence ast) -> Just $ Sequence (KS : ast)) $ parse xs b True

                  
-- unsortierte Liste:
-- einem listitem-Marker muss auch ein Text/Kursiv/Bolt folgen. Das gibt zusammen ein Listitem im AST.
-- es wird mit der Hilfsfunktion addULI eingefügt
-- parse (T_ULI level: T_Text str: xs) b k = maybe Nothing (\ast -> Just $ addULI level (LI str) ast) $ parse xs b k
parse (T_ULI level:xs) b k = maybe Nothing (\(Sequence ast) -> Just $ Sequence (ULS:LIS: ast)) $ parseUList xs b k
                        

-- sortierte Liste:
-- einem listitem-Marker muss auch ein Text folgen. Das gibt zusammen ein Listitem im AST.
-- es wird mit der Hilfsfunktion addLI eingefügt
-- parse (T_LI i: T_Text str: xs) b k = maybe Nothing (\ast -> Just $ addLI (LI str) ast) $ parse xs b k
parse (T_LI level:xs) b k = maybe Nothing (\(Sequence ast) -> Just $ Sequence (OLS:LIS:ast)) $ parseOList xs b k
   

-- ein Text am Anfang gehört in einen Absatz. Damit direkt auf einander folgende Texte in einem gemeinsamen
-- Absatz landen, wird die Hilfsfunktion addP genutzt um den Text einzufügen
parse (T_Text str: xs) b k        = maybe Nothing (\ast -> Just $ addP (P str) ast) $ parse xs b k
-- Der gesamte Rest wird für den Moment ignoriert. Achtung: Der Parser schlägt, in der momentanen Implementierung, nie fehl.
-- Das kann in der Endfassung natürlich nicht so bleiben!
parse _ _ _ = Just $ Sequence []


-- Parsen von Listeneinträgen 
parseOList :: [MDToken] -> Bool -> Bool -> Maybe AST
parseOList (T_B:xs) b k 
                              | b = maybe Nothing (\(Sequence ast) -> Just $ Sequence (BE : ast)) $ parseOList xs False k
                              | otherwise = maybe Nothing (\(Sequence ast) -> Just $ Sequence (BS : ast)) $ parseOList xs True k
parseOList (T_K:xs) b k 
                             | k = maybe Nothing (\(Sequence ast) -> Just $ Sequence (KE : ast)) $ parseOList xs b False
                             | otherwise = maybe Nothing (\(Sequence ast) -> Just $ Sequence (KS : ast)) $ parseOList xs b True
parseOList (T_Text str:xs) b k =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (P str:ast)) $ parseOList xs b k 
parseOList (T_Newline:T_LI level:xs) b k =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:LIS:ast)) $ parseOList xs b k
parseOList (T_Newline:xs) b k =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:OLE:ast)) $ parse xs b k 
parseOList _ _ _ = Just $ Sequence []

parseUList :: [MDToken] -> Bool -> Bool -> Maybe AST
parseUList (T_B:xs) b k 
                              | b = maybe Nothing (\(Sequence ast) -> Just $ Sequence (BE : ast)) $ parseUList xs False k
                              | otherwise = maybe Nothing (\(Sequence ast) -> Just $ Sequence (BS : ast)) $ parseUList xs True k
parseUList (T_K:xs) b k 
                             | k = maybe Nothing (\(Sequence ast) -> Just $ Sequence (KE : ast)) $ parseUList xs b False
                             | otherwise = maybe Nothing (\(Sequence ast) -> Just $ Sequence (KS : ast)) $ parseUList xs b True
parseUList (T_Text str:xs) b k =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (P str:ast)) $ parseUList xs b k 
parseUList (T_Newline:T_ULI level:xs) b k =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:LIS:ast)) $ parseUList xs b k
parseUList (T_Newline:xs) b k =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:ULE:ast)) $ parse xs b k
parseUList _ _ _ = Just $ Sequence []

-- Hilfsfunktionen für den Parser

{-
-- Einfügen eines Listenelements in eine geordnete Liste
addULI :: Int -> AST ->  AST -> AST
-- Wenn wir ein Listenelement einfügen wollen und im Rest schon eine UL haben, fügen wir das Element in die UL ein
addULI level li (Sequence (UL lis : ast)) = Sequence (UL (li:lis) : ast)
-- Andernfalls erzeugen wir eine neue UL.
addULI level li (Sequence ast) = Sequence (UL [li] : ast)

-- Einfügen eines Listenelements in eine eordnete Liste
addLI :: AST -> AST -> AST
-- Wenn wir ein Listenelement einfügen wollen und im Rest schon eine UL haben, fügen wir das Element in die UL ein
addLI li (Sequence (L lis : ast)) = Sequence (L (li:lis) : ast)
-- Andernfalls erzeugen wir eine neue UL.
addLI li (Sequence ast) = Sequence (L [li] : ast)
-}

-- Mehrere aufeinander folgende Texte werden zu einem Absatz zusammengefügt.
addP :: AST -> AST -> AST
-- Wenn wir zwei Absätze hintereinander finden, fassen wir diese zusammen 
addP (P str1) (Sequence (P str2 : ast)) = Sequence (P (str1 ++ "\n" ++ str2) : ast)
-- Andernfalls bleibt der Absatz alleine
addP p (Sequence ast) = Sequence (p : ast)


