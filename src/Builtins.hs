module Builtins
  ( checkBuiltin
  , runBuiltin
  ) where

import Tree

checkBuiltin :: Term -> Maybe (Maybe Term)
checkBuiltin term
  = case term of
      (Atom w) -> runBuiltin w []
      (Compound w args) -> runBuiltin w args
      _ -> Nothing


runBuiltin :: String -> [Term] -> Maybe (Maybe Term)
runBuiltin name args
  = let fullname = name ++ "/" ++ (show (length args)) in
    let list = filter (\(n, _) -> n == fullname) builtins in
    if length list > 0 then
      Just $ (snd (head list)) args
    else
      Nothing


builtins = [
    ("true/0", builtinTrue)
  , ("</2", builtinLessThan)
  , (">/2", builtinGreaterThan)
  , ("<=/2", builtinLessThanEqual)
  , (">=/2", builtinGreaterThanEqual)
  ]


builtinTrue :: [Term] -> Maybe Term
builtinTrue args = Just (Atom "true")

builtinLessThan :: [Term] -> Maybe Term
builtinLessThan args
  = case (head args, head (tail args)) of
      (Number a, Number b) -> if a < b then Just (Atom "true") else Nothing
      _ -> Nothing

builtinGreaterThan :: [Term] -> Maybe Term
builtinGreaterThan args
  = case (head args, head (tail args)) of
      (Number a, Number b) -> if a > b then Just (Atom "true") else Nothing
      _ -> Nothing

builtinLessThanEqual :: [Term] -> Maybe Term
builtinLessThanEqual args
  = case (head args, head (tail args)) of
      (Number a, Number b) -> if a <= b then Just (Atom "true") else Nothing
      _ -> Nothing

builtinGreaterThanEqual :: [Term] -> Maybe Term
builtinGreaterThanEqual args
  = case (head args, head (tail args)) of
      (Number a, Number b) -> if a >= b then Just (Atom "true") else Nothing
      _ -> Nothing


