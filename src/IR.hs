module IR where

-- Abstract Syntax Tree für HTML-Generierung. Daher schon nahe an HTML angelehnt.
data AST = Sequence [AST]      -- eine Sequenz von HTML-Elementen
         | H Int String        -- eine Überschrift, de Int ist das Level (6 für H6) und der String der Text  
         | ULS                 -- Anfang einer ungeordnete Liste    
         | OLS                 -- Anfang einer geordnete Liste 
         | ULE                 -- Ende einer ungeordnete Liste 
         | OLE                 -- Ende einer geordnete Liste   
         | LIS                 -- Start eines Listenelementes
         | LIE                 -- Ende eines Listenelementes
         | P String            -- ein Absatz mit dem Inhalt
         | EmptyLine           -- eine leere Zeile
         | BS                  -- Start der fettschrift
         | BE                  -- Ende der Fettschrift
         | KS                  -- Start von kursiv
         | KE                  -- ende von kursi
         | SCB            -- Start of Code Block
         | ECB            -- Ende vom Code Block
         | Code String         -- Inline Code
         | A String String     -- Link mit (1)Url und (2)Text
         | Img String String   -- Bild mit (1)Url und (2)Alt
         | R String String     -- Referenz mit (1)Url und (2)Id
         | EZU                 -- Expliziter Zeilenumbruch (2 Leerzeichen vor zeilenumbruch)
    deriving (Show,Eq)