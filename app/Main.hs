{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Main where

import DayFive (main)
import DayFour (main)
import DayOne (main)
import DaySeven (main)
import DaySix (main)
import DayThree (main)
import DayTwo (main)
import DayEight (main)
import DayNine (main)
import DayTen (main)
import ImportList (adventOfCodeMains)

$(adventOfCodeMains)

main :: IO ()
main = sequence_ adventOfCode >> putStrLn (replicate 80 '-' <> "\ndone!")
