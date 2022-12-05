{-# LANGUAGE TemplateHaskellQuotes #-}

module ImportList where

import Control.Monad
import Data.Functor ((<&>))
import Data.List (intersperse)
import GHC.IO.Unsafe
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- utility library to not have to write so many lines in main
-- but it's more lines, so, uh...
-- whatever it's worth it

importList :: Q [Module]
importList = do
  (ModuleInfo ms) <- thisModule >>= reifyModule
  return ms

localPkgName :: Q PkgName
localPkgName = do
  (Module p _) <- thisModule
  return p

-- uhh, don't look at these
isModuleLocal, isModuleInLib, isModuleAdventOfCodeDay :: Module -> Bool
isModuleLocal (Module (PkgName p) _) = take 14 p == "advent-of-code"
isModuleInLib (Module _ (ModName m)) = take 3 m == "Day"
isModuleAdventOfCodeDay m = isModuleLocal m && isModuleInLib m

adventOfCodeDays :: Q [Module]
adventOfCodeDays = importList >>= filterM (return . isModuleAdventOfCodeDay)

getModuleMain :: Module -> Q Name
getModuleMain (Module _ (ModName m)) = do
  Just n <- lookupValueName (m ++ ".main")
  pure n

showModName :: Module -> String
showModName (Module _ (ModName m)) = m

funcRef :: Module -> Q Exp
funcRef mod' = getModuleMain mod' <&> VarE

makeHeading :: Module -> Q Exp
makeHeading mod' =
  let modNameLen = length (showModName mod')
      spacing = 78 - modNameLen -- 2 spaces either side
      formattedString = replicate (spacing `div` 2) '-' <> " " <> showModName mod' <> " " <> replicate (spacing `div` 2) '-'
   in [|putStrLn formattedString|]

doFuncRefs :: [Module] -> Q [Exp]
doFuncRefs modules = do
  headingRefs <- mapM makeHeading modules
  funcRefs <- mapM funcRef modules
  dividerRef <- repeat <$> [|putStrLn $ replicate 80 '-'|]
  let interleave3 xs ys zs = concatMap (\(a, b, c) -> [a, b, c]) $ zip3 xs ys zs
  return $ interleave3 headingRefs funcRefs dividerRef

-- let listRef = map (zip modules)

adventOfCodeMains :: Q [Dec]
adventOfCodeMains = do
  days <- adventOfCodeDays
  listRef <- doFuncRefs days
  let ioType = AppT ListT (AppT (ConT ''IO) (ConT ''()))
  let newName' = mkName "adventOfCode"
  return
    [ SigD newName' ioType, -- adventOfCode :: [IO ()]
      ValD (VarP newName') (NormalB (ListE listRef)) [] -- adventOfCode = [DayOneMain, DayTwoMain, ...]
    ]
