module Referencer where

import IR

-- Ruft Methode mit doppelten Parametern auf
reference :: AST -> Maybe AST
reference ast = reference' ast ast


-- Fuehrt fuer jeden AST 'manipulate' Methode aus
reference' :: AST -> AST -> Maybe AST
reference' ast (Sequence (x:xs)) = maybe Nothing (\(Sequence rest) -> Just $ Sequence ((manipulate ast x) : rest)) $ reference' ast (Sequence xs)
reference' _ _ = Just $ Sequence []


-- Wenn AST ein Link oder Bild ist, dann checke, ob es eine Referenz gibt
manipulate :: AST -> AST -> AST
manipulate ast (A url str) = A (check ast url) str
manipulate ast (Img url str) = Img (check ast url) str
manipulate ast x = x


-- Durchlaeuft den kompletten AST und sucht nach Referenzen, die zur ID passen
check :: AST -> String -> String
check (Sequence (x:xs)) id =
    let ret = isMyReference x id in
    if fst ret
    then snd ret
    else check (Sequence xs) id
check (Sequence []) id = id


-- Prueft, ob AST eine Referenz mit der ID ist
-- Gibt True und dazugehoerige URL oder False zurueck
isMyReference :: AST -> String -> (Bool, String)
isMyReference (R url id) i =
    if id == i
    then (True, url)
    else (False, "")
isMyReference _ i = (False, "")
