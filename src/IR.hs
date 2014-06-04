module IR where

-- Abstract Syntax Tree für HTML-Generierung. Daher schon nahe an HTML angelehnt.
data AST = Sequence [AST] -- eine Sequenz von HTML-Elementen
         | H Int String   -- eine Überschrift, de Int ist das Level (6 für H6) und der String der Text  
         | UL [AST]       -- eine ungeordnete Liste, in der Liste müssen dann die Listenelemente stehen
         | LI String      -- ein Listenelement mit dem Inhalt     
         | L [AST]        -- eine geordnete Liste, in der Liste müssen dann die Listenelemente stehen  
         | P String       -- ein Absatz mit dem Inhalt
         | EmptyLine      -- eine leere Zeile
    deriving (Show)