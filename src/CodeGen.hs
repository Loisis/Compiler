-- Modul zum Generieren von HTML-Code der als String repräsentiert wird aus einem AST.
module CodeGen where

import IR

-- HTML generieren
-- zuerst das äußere Gerüst
generateHTML :: AST -> String
generateHTML ast = "<html>\n<head></head>\n<body>\n" ++ generateHTML' ast ++ "\n</body>\n</html>"

-- dann Elemente für jeden AST-Knoten
generateHTML' :: AST -> String

-- eine Sequenz
generateHTML' (Sequence (a:as)) = generateHTML' a ++ generateHTML' (Sequence as)

-- eine Überschrift
generateHTML' (H i str) = "<h" ++ show i ++ ">" ++ str ++ "</h" ++ show i ++ ">\n"

-- eine ungeordnete Liste
generateHTML' (ULS) = "<ul>\n"
generateHTML' (ULE) = "</ul>\n"

-- eine geordnete Liste
generateHTML' (OLS) = "<ol>\n"
generateHTML' (OLE) = "</ol>\n"

-- Listenelemente
generateHTML' (LIS) = "<li>"
generateHTML' (LIE) = "</li>\n"

-- ein Absatz
--generateHTML' (P str)  = "<p>" ++ str ++ "</p>\n"
generateHTML' (P str)  =  str

-- Explizieter Zeilenumbruch
generateHTML' (EZU) = "<br>"

-- ein Link
generateHTML' (A url str) = "<a href=" ++ show url ++ ">" ++ str ++ "</a>"

-- ein Inline Code
generateHTML' (CS) = "<code>"
generateHTML' (CE) = "<code>"
generateHTML' (Code str) = "<code>" ++ str ++ "</code>"

-- ein Code Block
generateHTML' (SCB) = "\n<pre><code>\n"
generateHTML' (ECB) = "\n</code></pre>\n"

-- ein Bild
generateHTML' (Img url str) = "<img src=" ++ show url ++ " alt =" ++ show str ++ " />\n"

-- Fett + kursiv
generateHTML' (BS)  = "<b>"
generateHTML' (BE)  = "</b>"
generateHTML' (KS)  = "<i>"
generateHTML' (KE)  = "</i>"

-- alles andere (?) wird für den Moment ignoriert
generateHTML' _ = ""
