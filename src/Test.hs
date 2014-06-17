module Test where

import Main
import Test.HUnit
import IR
import Scanner
import Parser
import Referencer
import CodeGen

scannerTest :: Test
scannerTest =
    let expr = "##########Titel  \n1.*List*\n    + Sublist\n[](!\n        Codeblock\n<http://www.google.de/>\n"
        expectedValue = Just [ T_H 6,T_Text "Titel",T_ZU,T_Newline,T_LI 1,T_K,T_Text "List",T_K,T_Newline,T_ULI 2, 
                          T_Text "Sublist",T_Newline,T_EcKlA,T_EcKlZ,T_RuKlA,T_ExMark,T_Newline,T_CB 3,T_Text "Codeblock",
                          T_Newline,T_SpKlA,T_Text "http://www.google.de/",T_SpKlZ,T_Newline
                             ]
    in TestCase (assertEqual "ScannerTest" expectedValue $ scan expr 1)

parserTest :: Test
parserTest =
    let expr =  [ T_H 6,  T_Text "Titel", T_ZU,T_Newline,T_LI 1,T_K,T_Text "List",T_K,T_Newline,T_ULI 2,T_Text "Sublist",T_Newline,
                          T_Newline,T_CB 3,T_Text "Codeblock",T_Newline,T_SpKlA,T_Text "http://www.google.de/",
                          T_SpKlZ,T_Newline
                          ]
        expectedValue = Just $ Sequence [ H 6 "Titel",EZU,OLS,LIS,KS,P "List",KE,LIE,ULS,LIS,P "Sublist",LIE,ULE,OLE,
                          SCB,P "            Codeblock",ECB,A "http://www.google.de/" "http://www.google.de/"
                          ]                       
    in TestCase (assertEqual "ParserTest" expectedValue $ parse expr False False)    

referencerTest :: Test
referencerTest =
    let expr =  Sequence [ H 6 "Titel", EmptyLine ,Img "bild" "Das ist ein Bild",EmptyLine,P "Text",EmptyLine, R " /path/image.png" "bild" 
                          ]
        expectedValue = Just $ Sequence [ H 6 "Titel",EmptyLine,Img " /path/image.png" "Das ist ein Bild",EmptyLine,P "Text",EmptyLine,R " /path/image.png" "bild"
                          ]                       
    in TestCase (assertEqual "RefTest" expectedValue $ reference expr)

codeGenTest :: Test
codeGenTest =
    let expr =  Sequence [  H 6 "Titel",EmptyLine,KS,P "Haus",BS,P "Dickes Haus",KE,BE,Img " /path/image.png" "Das ist ein Bild",EmptyLine,P "Text",EmptyLine,R " /path/image.png" "bild" 
                          ]
        expectedValue = "<html>\n<head></head>\n<body>\n<h6>Titel</h6>\n<i>Haus<b>Dickes Haus</i></b><img src=\" /path/image.png\" alt =\"Das ist ein Bild\" />\nText\n</body>\n</html>"
                                                 
    in TestCase (assertEqual "RefTest" expectedValue $ generateHTML expr)
    
tests :: Test
tests = TestList [ TestLabel "Scanner" scannerTest,
                   TestLabel "Parser" parserTest,
                   TestLabel "RefTest" referencerTest,
                   TestLabel "CodeTest" codeGenTest
                 ]

main :: IO ()
main = do
        runTestTT tests
        return ()