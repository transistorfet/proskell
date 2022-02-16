module Lib
  ( mainFunc
  ) where


data Term =
    Atom String
  | Compound String [Term]
  | Variable String
  | Number Int

data Expr =
    Term Term
  | Conjunct Term Term

data Clause =
    Fact Term
  | Rule Term Expr
  | Query Expr

data Binding = Binding {
  name :: String
, value :: Term
}

type Bindings = [Binding]

data Database = Database {
  clauses :: [Clause]
}


data Result = Result {
  term :: Term
, bindings :: Bindings
}

bindTerm :: String -> Term -> Binding
bindTerm name term
  = Binding {
      name = name
    , value = term
  }

makeResult :: (Term, Bindings) -> Result
makeResult (term, bindings)
  = Result {
      term = term
    , bindings = bindings
  }


instance Show Term where
  show (Atom n) = "Atom " ++ n
  show (Compound n terms) = "Compound(" ++ n ++ ", " ++ (show terms) ++ ")"
  show (Variable n) = "Variable " ++ n
  show (Number n) = "Number " ++ (show n)

instance Show Expr where
  show (Term term) = show term
  show (Conjunct t1 t2) = (show t1) ++ ", " ++ (show t2)

instance Show Clause where
  show (Fact term) = show term ++ "."
  show (Rule lhs rhs) = (show lhs) ++ " :- " ++ (show rhs) ++ "."
  show (Query expr) = "?- " ++ (show expr) ++ "."

--instance Show Database where
--  show db = "[" ++ (intercalate ", " (map (\x -> (show x)) (clauses db))) ++ "]"


exampleProgram = [
    Fact (Atom "person")
  , Fact (Atom "thing")
  , Rule (Compound "has" [(Atom "person"), (Atom "thing")]) (Term (Atom "person"))
  ]

-- TODO this is temporary, needs to be unified
mergeBindings :: Bindings -> Bindings -> Maybe Bindings
mergeBindings b1 b2
  = Just $ concat [b1, b2]

unify :: Term -> Term -> Maybe (Term, Bindings)
unify (Atom n1) (Atom n2) | n1 == n2
  = Just (Atom n1, [])
unify (Number n1) (Number n2) | n1 == n2
  = Just (Number n1, [])
unify (Variable n1) (Variable n2) | n1 == n2
  = Just (Variable n1, [])
unify (Compound n1 subs1) (Compound n2 subs2) | n1 == n2
  = unifyList subs1 subs2 >>= (\(list, bindings) -> Just (Compound n1 list, bindings))
unify (Variable name) term
  = Just (term, [bindTerm name term])
unify term (Variable name)
  = Just (term, [bindTerm name term])
unify _ _
  = Nothing

unifyList :: [Term] -> [Term] -> Maybe ([Term], Bindings)
unifyList [] []
  = Just ([], [])
unifyList [] _
  = Nothing
unifyList _ []
  = Nothing
unifyList (t1 : ts1) (t2 : ts2)
  = do
      (t, b1) <- unify t1 t2
      (ts, b2) <- unifyList ts1 ts2
      bindings <- mergeBindings b1 b2
      return (t : ts, bindings)


solve :: [Clause] -> Term -> Maybe Result
solve db query
  = searchClauses db db query

searchClauses :: [Clause] -> [Clause] -> Term -> Maybe Result
searchClauses db [] query
  = Nothing
searchClauses db (c : cs) query
  = case solveClause db c query of
      Just result -> Just result
      Nothing -> searchClauses db cs query

solveClause :: [Clause] -> Clause -> Term -> Maybe Result
solveClause db (Fact term) query
  = unify term query >>= (\r -> Just $ makeResult r)
solveClause db (Rule lhs rhs) query
  = do
      (t1, b1) <- unify lhs query
      b2 <- solveExpr db rhs
      b <- mergeBindings b1 b2
      return $ makeResult (t1, b)

solveExpr :: [Clause] -> Expr -> Maybe Bindings
solveExpr db (Term t)
  = do
    r <- solve db t
    return $ bindings r
solveExpr db (Conjunct t1 t2)
  = do
      r1 <- solve db t1
      r2 <- solve db t2
      mergeBindings (bindings r1) (bindings r2)



mainFunc :: IO ()
mainFunc = do
  putStrLn (show exampleProgram)
  let result = solve exampleProgram (Compound "has" [(Atom "person"), (Variable "X")]) in
    case result of
      Just result -> putStrLn (show (term result))
      _ -> putStrLn "not found"


