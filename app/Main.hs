{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import DayFive (main)
import DayFour (main)
import DayOne (main)
import DaySeven (main)
import DaySix (main)
import DayThree (main)
import DayTwo (main)
import DayEight (main)
import ImportList (adventOfCodeMains)

$(adventOfCodeMains)

main :: IO ()
main = sequence_ adventOfCode >> putStrLn "done!"
