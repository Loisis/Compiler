-- Modul zum Generieren von HTML-Code der als String repräsentiert wird aus einem AST.
module CodeGen where

import IR

-- HTML generieren
-- zuerst das äußere Gerüst
generateHTML :: AST -> String
generateHTML ast = "<html>\n<head></head>\n<body>\n" ++ generateHTML' ast ++ "</body>\n</html>"

-- dann Elemente für jeden AST-Knoten
generateHTML' :: AST -> String

-- eine Sequenz
generateHTML' (Sequence (a:as)) = generateHTML' a ++ "\n" ++ generateHTML' (Sequence as)

-- eine Überschrift
generateHTML' (H i str) = "<h" ++ show i ++ ">" ++ str ++ "</h" ++ show i ++ ">\n"

-- eine ungeordnete Liste
generateHTML' (ULS) = "<ul>\n"
generateHTML' (ULE) = "</ul>\n"

-- eine geordnete Liste
generateHTML' (OLS) = "<ol>\n"
generateHTML' (OLE) = "</ol>\n"

-- Listenelemente
generateHTML' (LIS) = "<li>\n"
generateHTML' (LIE) = "</li>\n"

-- ein Absatz
--generateHTML' (P str)  = "<p>" ++ str ++ "</p>\n"
generateHTML' (P str)  =  str

-- Explizieter Zeilenumbruch
generateHTML' (EZU) = "<br>"

-- Fett + kursiv
generateHTML' (BS)  = "<b>"
generateHTML' (BE)  = "</b>"
generateHTML' (KS)  = "<i>"
generateHTML' (KE)  = "</i>"

-- alles andere (?) wird für den Moment ignoriert
generateHTML' _ = ""
