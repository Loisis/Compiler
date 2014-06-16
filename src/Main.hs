module Main where

import           CodeGen
import           IR
import           Parser
import           Scanner
import           Referencer

main :: IO ()
main = do
    -- lese den Inhalt der Datei "test.md" als einen kompletten String ein
    input <- readFile "test.md"
    -- versuche den String zu scannen
    let maybeTokens = scan input 0 
    putStrLn "Scanner output\n=============="
    print maybeTokens
    -- the parse
    case maybeTokens of
        Nothing -> putStrLn "scanner failed"
        Just tokens -> do -- der Scanner war erfolgreich
            -- versuche die Tokens zu parsen
            let maybeAst = parse tokens False False
            putStrLn "\nParser output\n============="
            print maybeAst
            case maybeAst of
                Nothing -> putStrLn "parser failed"
                Just ast -> do -- der Parser war erfolgreich
                    -- versuche Referenzen zu finden und anzupassen
                    let maybeAst = reference ast
                    putStrLn "\nReferencer output\n============="
                    print maybeAst
                    case maybeAst of
                        Nothing -> putStrLn "referencer failed"
                        Just ast -> do -- der Referencer war erfolgreich
                        putStrLn "\nGenerated HTML\n=============="
                        -- generiere HTML und gebe es aus
                        putStrLn $ generateHTML ast
