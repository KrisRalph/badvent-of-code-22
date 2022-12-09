{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Control.Monad
import DayFour (main)
import DayOne (main)
import DayThree (main)
import DayTwo (main)
import ImportList (adventOfCodeMains)

$(adventOfCodeMains)

main :: IO ()
main = sequence_ adventOfCode >> putStrLn "done!"
