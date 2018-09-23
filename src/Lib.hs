module Lib
    (
     showGrid
    , grid
    , languages
    , findInLine
    , findWord
    , findWords
    , skew
    ) where

import           Data.List  (isInfixOf, transpose)
import           Data.Maybe (catMaybes)


showGrid :: [String] ->  IO()
showGrid grid = putStrLn (unlines grid)


findInLine :: String -> String -> Bool
findInLine = isInfixOf

skew :: [String] -> [String]
skew [] = []
skew (l:ls) = l : skew (map indent ls)
        where indent line = '_' : line


gridLines :: [String] -> [String]
gridLines grid =
            let horizontal = grid
                vertical = transpose grid
                diagonal1 = diagonalize grid
                diagonal2 = diagonalize (map reverse grid)
                lines =  horizontal ++ vertical ++ diagonal1 ++ diagonal2
             in lines ++ map reverse lines

diagonalize = transpose . skew

findWord :: [String] -> String -> Maybe String
findWord grid word =
        let lines = gridLines grid
            found = or $ map (findInLine word) lines
         in   if found then Just word else Nothing

findWords grid words =
    let foundWords = map (findWord grid) words
    in catMaybes foundWords

-- onlyJusts = catMaybes findWords

-- containedInGrid

grid = [ "__C______R____M"
        ,"_________U____L"
        ,"_________B____E"
        ,"_HASKELL_Y____O"
        ,"____PHP______C_"
        ,"E___________A_J"
        ,"RL_________M__A"
        ,"L_I___LREP_P__V"
        ,"A__X________S_A"
        ,"N___I________I_"
        ,"G____R________L"]

languages = ["OCAM","ELIXIR","LISP","CSHARP", "PERL", "PYTHON", "CPLUSPLUS", "NODE", "HASKELL", "RUBY", "JAVA", "PHP", "ERLANG", "JAVA", "ELM"]
