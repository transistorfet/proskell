module Solver where

import Debug.Trace

import Tree

data Database = Database {
  clauses :: [Clause]
, iteration :: Int
}

data Result = Result {
  term :: Term
, bindings :: Bindings
, atRule :: Int
}

data Binding = Binding {
  name :: String
, value :: Term
}

type Bindings = [Binding]


instance Show Binding where
  show b = "{ " ++ (show (name b)) ++ " = " ++ (show (value b)) ++ " }"


makeDatabase :: [Clause] -> Database
makeDatabase clauses
  = Database {
      clauses = clauses
    , iteration = 0
    }

incDatabase :: Database -> Database
incDatabase db
  = db {
      iteration = (iteration db) + 1
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
substituteTerm bs (Cons t1 t2)
  = Cons (substituteTerm bs t1) (substituteTerm bs t2)
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
renameTerm tag (Cons t1 t2) = Cons (renameTerm tag t1) (renameTerm tag t2)
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
unify (Variable name) term
  = Just (term, [bindTerm name term])
unify term (Variable name)
  = Just (term, [bindTerm name term])
unify (Compound n1 subs1) (Compound n2 subs2) | n1 == n2
  = unifyList subs1 subs2 >>= (\(list, bindings) -> Just (Compound n1 list, bindings))
unify EmptyList EmptyList
  = Just (EmptyList, [])
unify (Cons a1 a2) (Cons b1 b2)
  = do
      (t1, r1) <- unify a1 b1
      (t2, r2) <- unify a2 b2
      nb <- mergeBindings r1 r2
      return $ (Cons t1 t2, nb)
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
  = trace ("Solve " ++ (show query)) $ do
      r <- searchClauses (incDatabase db) i (drop i (clauses db)) query
      trace ("Result " ++ (show (term r)) ++ " with " ++ (show (bindings r))) $ return r


searchClauses :: Database -> Int -> [Clause] -> Term -> Maybe Result
searchClauses db i [] query = Nothing
searchClauses db i (c : cs) query
  = case solveClause db i c query of
      Just result -> Just result
      Nothing -> searchClauses db (i + 1) cs query


solveClause :: Database -> Int -> Clause -> Term -> Maybe Result
solveClause db i (Fact term) query
  = trace ("Try fact " ++ (show (Fact term))) $ do
      r <- unify term query
      return $ makeResult r i
solveClause db i (Rule lhs rhs) query
  = trace ("Try rule " ++ (show (Rule lhs rhs))) $ do
      (t1, b1) <- unify (renameTerm (show (iteration db)) lhs) query
      r2 <- solveExpr db 0 (substituteExpr b1 (renameExpr (show (iteration db)) rhs))
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


