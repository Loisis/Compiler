module Parser ( parse {- nur parse exportieren -} )
    where

import           IR
import           Scanner

data List = OL | UL

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
parse (T_ULI level:xs) b k = maybe Nothing (\(Sequence ast) -> Just $ Sequence (ULS:LIS: ast)) $ parseUList xs [ULE] 1 b k
                        

-- sortierte Liste:
-- einem listitem-Marker muss auch ein Text folgen. Das gibt zusammen ein Listitem im AST.
-- es wird mit der Hilfsfunktion addLI eingefügt
-- parse (T_LI i: T_Text str: xs) b k = maybe Nothing (\ast -> Just $ addLI (LI str) ast) $ parse xs b k
parse (T_LI level:xs) b k = maybe Nothing (\(Sequence ast) -> Just $ Sequence (OLS:LIS:ast)) $ parseOList xs [OLE] 1 b k
   

-- ein Text am Anfang gehört in einen Absatz. Damit direkt auf einander folgende Texte in einem gemeinsamen
-- Absatz landen, wird die Hilfsfunktion addP genutzt um den Text einzufügen
parse (T_Text str: xs) b k        = maybe Nothing (\ast -> Just $ addP (P str) ast) $ parse xs b k
-- Der gesamte Rest wird für den Moment ignoriert. Achtung: Der Parser schlägt, in der momentanen Implementierung, nie fehl.
-- Das kann in der Endfassung natürlich nicht so bleiben!
parse _ _ _ = Just $ Sequence []


-- Parsen von Listeneinträgen 
parseOList :: [MDToken] -> [AST] -> Int -> Bool -> Bool -> Maybe AST

-- Zum besseren Verständnis: lh = Listenhistorie, l = level, b = Bold, k = kursiv
parseOList (T_B:xs) lh l b k 
                              | b = maybe Nothing (\(Sequence ast) -> Just $ Sequence (BE : ast)) $ parseOList xs lh l False k
                              | otherwise = maybe Nothing (\(Sequence ast) -> Just $ Sequence (BS : ast)) $ parseOList xs lh l True k
parseOList (T_K:xs) lh l b k 
                             | k = maybe Nothing (\(Sequence ast) -> Just $ Sequence (KE : ast)) $ parseOList xs lh l b False
                             | otherwise = maybe Nothing (\(Sequence ast) -> Just $ Sequence (KS : ast)) $ parseOList xs lh l b True
parseOList (T_Text str:xs) lh l b k =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (P str:ast)) $ parseOList xs lh l b k 
-- Neuer Listeneintrag: Entweder in die gleiche Liste oder in eine neue oder die Liste beenden und in die alte Liste
parseOList (T_Newline:T_LI level:xs) lh l b k 
                                        | l == level =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:LIS:ast)) $ parseOList xs lh level b k
                                        | l < level =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:OLS:LIS:ast)) $ parseOList xs (lh ++ [OLE]) level b k
                                        | l > level =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:ast)) $ closeLists xs lh l level b k False 
parseOList (T_Newline:T_ULI level:xs) lh l b k 
                                        | l == level =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:LIS:ast)) $ parseUList xs lh level b k
                                        | l < level =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:ULS:LIS:ast)) $ parseUList xs (lh ++ [ULE]) level b k
                                        | l > level =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:ast)) $ closeLists xs lh l level b k True                                        
                                     
-- Bei allen anderen: Ende der Liste. Falls Level != 0 müssen noch die alten Listen geschlossen werden
parseOList (T_Newline:xs) lh l b k =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:ast)) $ closeLists xs lh l 0 b k False
parseOList _ _ _ _ _ = Just $ Sequence []

parseUList :: [MDToken] -> [AST] -> Int -> Bool -> Bool -> Maybe AST
parseUList (T_B:xs) lh l b k 
                              | b = maybe Nothing (\(Sequence ast) -> Just $ Sequence (BE : ast)) $ parseUList xs lh l False k
                              | otherwise = maybe Nothing (\(Sequence ast) -> Just $ Sequence (BS : ast)) $ parseUList xs lh l True k
parseUList (T_K:xs) lh l b k 
                             | k = maybe Nothing (\(Sequence ast) -> Just $ Sequence (KE : ast)) $ parseUList xs lh l b False
                             | otherwise = maybe Nothing (\(Sequence ast) -> Just $ Sequence (KS : ast)) $ parseUList xs lh l b True
                             
parseUList (T_Text str:xs) lh l b k =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (P str:ast)) $ parseUList xs lh l b k 

-- Neuer Listeneintrag: Entweder in die gleiche Liste oder in eine neue oder die Liste beenden und in die alte Liste
parseUList (T_Newline:T_LI level:xs) lh l b k 
                                        | l == level =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:LIS:ast)) $ parseOList xs lh level b k
                                        | l < level =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:OLS:LIS:ast)) $ parseOList xs (lh ++ [OLE]) level b k
                                        | l > level =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:ast)) $ closeLists xs lh l level b k False
parseUList (T_Newline:T_ULI level:xs) lh l b k 
                                        | l == level =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:LIS:ast)) $ parseUList xs lh level b k
                                        | l < level =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:ULS:LIS:ast)) $ parseUList xs (lh ++ [ULE]) level b k
                                        | l > level =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:ast)) $ closeLists xs lh l level b k True

-- Ende der Liste -> für alle Level die Listen schließen                                        
parseUList (T_Newline:xs) lh l b k =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:ast)) $ closeLists xs lh l 0 b k False
parseUList (T_ZU:xs) lh l b k =   maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIE:ast)) $ closeLists xs lh l 0 b k False

parseUList _ _ _ _ _ = Just $ Sequence []

-- Hilfsfunktionen für den Parser

-- Wenn man feststellt, dass eine Liste bzw. ein Listenbaum zu ende ist oder ein neues Listenelement mit einem kleineren level folgt,
-- dann müssen alle vorherigen LIsten mit höherem Level geschlossen werden 
closeLists :: [MDToken] -> [AST] -> Int -> Int -> Bool -> Bool -> Bool -> Maybe AST
-- lh = Listenhistorie, l1 = level zuvor, l2 = neues level (0 bei Ende der Liste), b= bold, k= kursiv, ulist gibt an was das nächste Listenelemt ist (True = Ungeordnet, false = geordnet)
closeLists xs lh l1 l2 b k ulist 
                        | l1 == 0 = maybe Nothing (\(Sequence ast) -> Just $ Sequence ast) $ parse xs b k -- Liste zuende ->  und normal weiter parsen
                        | l1 == l2 && ulist = maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIS:ast)) $ parseUList xs lh l2 b k -- Level gleich -> Ungeordnetes Listenelemt einfügen
                        | l1 == l2 && not ulist = maybe Nothing (\(Sequence ast) -> Just $ Sequence (LIS:ast)) $ parseOList xs lh l2 b k -- Level gleich -> Geordnetes Listenelement einfügen
                        | l1 > l2 = maybe Nothing (\(Sequence ast) -> Just $ Sequence ((last lh):ast)) $ closeLists xs (init lh) (l1-1) l2 b k ulist  -- Level anders -> Letzte gemerkte Liste beenden und vom Stack nehmen
                        
closeLists _ _ _ _ _ _ _ = Just $ Sequence []


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



