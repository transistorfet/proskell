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
  | Conjunct Expr Expr

data Clause =
    Fact Term
  | Rule Term Expr
  | Query Expr

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
, atRule :: Int
}

bindTerm :: String -> Term -> Binding
bindTerm name term
  = Binding {
      name = name
    , value = term
  }

makeResult :: (Term, Bindings) -> Int -> Result
makeResult (term, bindings) atRule
  = Result {
      term = term
    , bindings = bindings
    , atRule = atRule
    }

--instance Show Database where
--  show db = "[" ++ (intercalate ", " (map (\x -> (show x)) (clauses db))) ++ "]"

instance Show Binding where
  show b = "{ " ++ (show (name b)) ++ " = " ++ (show (value b)) ++ " }"




mergeBindings :: Bindings -> Bindings -> Maybe Bindings
mergeBindings [] bs
  = Just bs
mergeBindings (a:as) bs
  = do
      unified <- searchBinding a bs
      remaining <- mergeBindings as (filter (\x -> (name x) /= (name a)) bs)
      return $ unified ++ remaining
  where
    searchBinding :: Binding -> Bindings -> Maybe Bindings
    searchBinding a []
      = Just [a]
    searchBinding a (b:bs) | (name a) == (name b)
      = do
          (term, bindings) <- unify (value a) (value b)
          return $ (bindTerm (name a) term : bindings)
    searchBinding a (b:bs)
      = searchBinding a bs


substituteTerm :: Bindings -> Term -> Term
substituteTerm bs (Compound n terms)
  = Compound n (map (\t -> substituteTerm bs t) terms)
substituteTerm bs (Variable n)
  = findBinding bs n
  where
    findBinding [] n = Variable n
    findBinding (b:bs) n | (name b) == n = value b
    findBinding (b:bs) n = findBinding bs n
substituteTerm bs t
  = t

substituteExpr :: Bindings -> Expr -> Expr
substituteExpr bs (Term t)
  = Term (substituteTerm bs t)
substituteExpr bs (Conjunct e1 e2)
  = Conjunct (substituteExpr bs e1) (substituteExpr bs e2)


renameTerm :: String -> Term -> Term
renameTerm tag (Compound n terms) = Compound n (map (\t -> renameTerm tag t) terms)
renameTerm tag (Variable n) = Variable (n ++ tag)
renameTerm tag t = t

renameExpr :: String -> Expr -> Expr
renameExpr tag (Conjunct e1 e2) = Conjunct (renameExpr tag e1) (renameExpr tag e2)
renameExpr tag (Term t) = Term (renameTerm tag t)


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


solve :: Database -> Term -> Maybe Result
solve db query
  = solveFrom db 0 query


solveFrom :: Database -> Int -> Term -> Maybe Result
solveFrom db i query
  = searchClauses db i (drop i (clauses db)) query


searchClauses :: Database -> Int -> [Clause] -> Term -> Maybe Result
searchClauses db i [] query = Nothing
searchClauses db i (c : cs) query
  = case solveClause db i c query of
      Just result -> Just result
      Nothing -> searchClauses db (i + 1) cs query


solveClause :: Database -> Int -> Clause -> Term -> Maybe Result
solveClause db i (Fact term) query
  = do
      r <- unify term query
      return $ makeResult r i
solveClause db i (Rule lhs rhs) query
  = do
      (t1, b1) <- unify lhs query
      r2 <- solveExpr db 0 rhs
      nb <- mergeBindings b1 (bindings r2)
      return $ makeResult ((substituteTerm nb t1), nb) i


solveExpr :: Database -> Int -> Expr -> Maybe Result
solveExpr db i (Term t) = solveFrom db i t
solveExpr db i (Conjunct t1 t2) = solveConjunct db i t1 t2

solveConjunct :: Database -> Int -> Expr -> Expr -> Maybe Result
solveConjunct db i t1 t2
  = do
      r1 <- solveExpr db i t1
      case solveExpr db 0 (substituteExpr (bindings r1) t2) of
        Just r2 ->
          do
            nb <- mergeBindings (bindings r1) (bindings r2)
            return $ makeResult (term r2, nb) (atRule r2)
        Nothing ->
          solveConjunct db ((atRule r1) + 1) t1 t2



exampleProgram = [
    Fact (Compound "female" [(Atom "marge")])
  , Fact (Compound "female" [(Atom "lisa")])
  , Fact (Compound "male" [(Atom "homer")])
  , Fact (Compound "male" [(Atom "bart")])
  , Fact (Compound "parent" [(Atom "marge"), (Atom "bart")])
  , Fact (Compound "parent" [(Atom "marge"), (Atom "lisa")])
  , Fact (Compound "parent" [(Atom "homer"), (Atom "bart")])
  , Fact (Compound "parent" [(Atom "homer"), (Atom "lisa")])
  , Rule (Compound "father" [(Variable "X"), (Variable "Y")]) (Conjunct (Term (Compound "parent" [(Variable "X"), (Variable "Y")])) (Term (Compound "male" [(Variable "X")])))
  ]

mainFunc :: IO ()
mainFunc
  = do
    -- putStrLn (show exampleProgram)
    -- putStrLn (show $ mergeBindings [bindTerm "X" (Atom "person")] [bindTerm "Y" (Atom "thing")])
    putStrLn $ show $ substituteTerm ([bindTerm "X" (Atom "person")]) (Compound "parent" [(Variable "X")])
    let db = Database { clauses = exampleProgram } in
      let query = (Compound "father" [(Variable "X"), (Atom "bart")]) in
      case solve db query of
        Just result -> do
          putStrLn (show (term result))
          putStrLn (show (bindings result))
        _ -> putStrLn "not found"


