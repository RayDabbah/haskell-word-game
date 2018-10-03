module Lib
    (
     showGrid
    , findInLine
    , findWord
    , findWords
    , skew
    ,diagonalize
    ) where

import           Data.List  (isInfixOf, transpose)
import           Data.Maybe (catMaybes)

type Grid = [String]

showGrid :: [String] ->  IO()
showGrid grid = putStrLn (unlines grid)


findInLine :: String -> String -> Bool
findInLine = isInfixOf

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (map indent ls)
        where indent line = '_' : line


gridLines :: Grid -> Grid
gridLines grid =
            let horizontal = grid
                vertical = transpose grid
                diagonal1 = diagonalize grid
                diagonal2 = diagonalize (map reverse grid)
                lines =  horizontal ++ vertical ++ diagonal1 ++ diagonal2
             in lines ++ map reverse lines

diagonalize :: Grid -> Grid
diagonalize = transpose . skew

findWord :: Grid -> String -> Maybe String
findWord grid word =
        let lines = gridLines grid
            found = any (findInLine word) lines
         in   if found then Just word else Nothing

findWords grid words =
    let foundWords = map (findWord grid) words
    in catMaybes foundWords
