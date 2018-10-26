module Data (
      grid
    , languages
    , coords
    , og
) where

import System.IO
import Control.Monad

coords = [ [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7)]
        , [(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7)]
        , [(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7)]
        , [(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7)]
        , [(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7)]
        , [(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7)]
        , [(6,0),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7)]
        , [(7,0),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7)]
        ]

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

languages = ["OCAM"
            ,"ELIXIR"
            ,"LISP"
            , "PERL"
            , "HASKELL"
            , "RUBY"
            , "JAVA"
            , "PHP"
            , "ERLANG"
            , "JAVA"
            , "ELM"]

og :: Show a => [a] ->  IO ()
og  = putStrLn . unlines . map show

div2 x = x `mod` 2 == 0

mapped = do
        i <- [1, 73..1000]
        return (i * 3)

-- anotherMapped =

printConfig  = do
  contents <- readFile "stack.yaml"
  putStrLn contents

reading = do
  putStrLn "What is your name?"
  line <- getLine
  putStrLn $ "Hello " ++ line
  return  line


filtered = do
        i <- [1..]
        guard (div2 i)
        return (i + 1)

cols = repeat [1..]

rows = map repeat [1..]

coordsInf = zipOverGrid cols rows

repeat8 = take 8 . repeat
cols8 = repeat8 [0..7]
rows8 = map repeat8 [0..7]

zipOverGrid = zipWith zip
