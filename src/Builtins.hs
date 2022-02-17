module Builtins
  ( checkBuiltin
  , runBuiltin
  ) where

import Tree

checkBuiltin :: Term -> Maybe Term
checkBuiltin term
  = case term of
      (Atom w) -> runBuiltin w []
      (Compound w args) -> runBuiltin w args
      _ -> Nothing


runBuiltin :: String -> [Term] -> Maybe Term
runBuiltin name args
  = let fullname = name ++ "/" ++ (show (length args)) in
    let list = filter (\(n, _) -> n == fullname) builtins in
    if length list > 0 then
      (snd (head list)) args
    else
      Nothing


builtins = [
    ("true/0", builtinTrue)
  , ("</2", builtinLessThan)
  , (">=/2", builtinGreaterThanEqual)
  ]


builtinTrue :: [Term] -> Maybe Term
builtinTrue args = Just (Atom "true")

builtinLessThan :: [Term] -> Maybe Term
builtinLessThan args
  = case (head args, head (tail args)) of
      (Number a, Number b) -> if a < b then Just (Atom "true") else Nothing
      _ -> Nothing

builtinGreaterThanEqual :: [Term] -> Maybe Term
builtinGreaterThanEqual args
  = case (head args, head (tail args)) of
      (Number a, Number b) -> if a >= b then Just (Atom "true") else Nothing
      _ -> Nothing
