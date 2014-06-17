module Test where

import Main
import Test.HUnit
import IR
import Scanner


scannerTest :: Test
scannerTest =
    let expr = "##########Titel  \n1. *List*\n    + Sublist\n[](!\n        Codeblock\n<http://www.google.de/>\n"
        expectedValue = Just [ T_H 6,
                          T_Text "Titel",
                          T_ZU,
                          T_Newline,
                          T_LI 1,
                          T_K,
                          T_Text " List",
                          T_K,
                          T_Newline,
                          T_ULI 2, 
                          T_Text "Sublist",
                          T_Newline,
                          T_EcKlA,
                          T_EcKlZ,
                          T_RuKlA,
                          T_ExMark,
                          T_Newline,
                          T_CB 3,
                          T_Text "Codeblock",
                          T_Newline,
                          T_SpKlA,
                          T_Text "http://www.google.de/",
                          T_SpKlZ,
                          T_Newline
                             ]
    in TestCase (assertEqual expr expectedValue $ scan expr 1)

parserTest :: Test
parserTest =
    let expr = "##########Titel  \n1. List\n    + Sublist\n[](>!\n        Codeblock\n"
        expectedValue = Just [ T_H 6,
                          T_Text "Titel",
                          T_ZU,
                          T_Newline,
                          T_LI 1,
                          T_Text " List",
                          T_Newline,
                          T_ULI 2, 
                          T_Text "Sublist",
                          T_Newline,
                          T_EcKlA,
                          T_EcKlZ,
                          T_RuKlA,
                          T_SpKlZ,
                          T_ExMark,
                          T_Newline,
                          T_CB 3,
                          T_Text "Codeblock",
                          T_Newline
                             ]
    in TestCase (assertEqual expr expectedValue $ scan expr 1)    

tests :: Test
tests = TestList [ TestLabel "Scanner" scannerTest,
                   TestLabel "Parser" parserTest
                 ]

main :: IO ()
main = do
        runTestTT tests
        return ()