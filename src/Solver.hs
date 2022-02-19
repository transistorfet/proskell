module Solver where

import Debug.Trace

import Tree
import Builtins

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
  show b = "\n{ " ++ (show (name b)) ++ " = " ++ (show (value b)) ++ " }"


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
      term = substituteTerm bindings term
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
substituteTerm bindings (Compound n terms)
  = Compound n (map (\t -> substituteTerm bindings t) terms)
substituteTerm bindings (Cons t1 t2)
  = Cons (substituteTerm bindings t1) (substituteTerm bindings t2)
substituteTerm bindings (Variable n)
  = findBinding bindings n
  where
    findBinding [] n = Variable n
    findBinding (b:bs) n | (name b) == n = substituteTerm bindings (value b)
    findBinding (b:bs) n = findBinding bs n
substituteTerm bindings t
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

renameClause :: String -> Clause -> Clause
renameClause tag (Fact t) = Fact (renameTerm tag t)
renameClause tag (Rule lhs rhs) = Rule (renameTerm tag lhs) (renameExpr tag rhs)


unify :: Term -> Term -> Maybe (Term, Bindings)
unify (Atom "_") t
  = Just (t, [])
unify t (Atom "_")
  = Just (t, [])
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
      return (Cons t1 t2, nb)
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


debugNothing (Just r) = Just r
debugNothing Nothing = trace "Fail!" Nothing


solve :: Database -> Term -> Maybe Result
solve db query
  = solveFrom db 0 query


solveFrom :: Database -> Int -> Term -> Maybe Result
solveFrom db i query
  = trace ("Solve " ++ (show query)) $
      case checkBuiltin query of
        -- Builtin found and returned a result
        Just (Just t) -> printResult $ Just $ makeResult (t, []) i
        -- Builtin found but returned Nothing
        Just Nothing -> Nothing
        -- Builtin not found so try unifying
        Nothing -> printResult $ searchClauses (incDatabase db) i (drop i (clauses db)) query
  where
    printResult result
      = case result of
          Just r -> trace ("Result " ++ (show (term r)) ++ " with " ++ (show (bindings r))) $ Just r
          Nothing -> trace "Failed" Nothing


searchClauses :: Database -> Int -> [Clause] -> Term -> Maybe Result
searchClauses db i [] query = Nothing
searchClauses db i (c : cs) query
  = let clause = renameClause (show (iteration db)) c in
    trace ("Try " ++ (show clause)) $ case solveClause db i clause query of
      Just result -> Just result
      Nothing -> searchClauses (incDatabase db) (i + 1) cs query


solveClause :: Database -> Int -> Clause -> Term -> Maybe Result
solveClause db i (Fact term) query
  = do
      r <- unify term query
      return $ makeResult r i
solveClause db i (Rule lhs rhs) query
  = do
      (t1, b1) <- unify lhs query
      r2 <- solveExpr db 0 (substituteExpr b1 rhs)
      nb <- mergeBindings b1 (bindings r2)
      return $ makeResult (t1, nb) i


solveExpr :: Database -> Int -> Expr -> Maybe Result
solveExpr db i (Term t) = solveFrom db i t
solveExpr db i (Conjunct t1 t2) = solveConjunct db i t1 t2

solveConjunct :: Database -> Int -> Expr -> Expr -> Maybe Result
solveConjunct db i e1 e2
  = trace ("Do conjunct " ++ (show e1) ++ " and " ++ (show e2)) $ do
      r1 <- trace ("Solving first " ++ (show e1)) $ solveExpr db i e1
      case solveExpr (incDatabase db) 0 (substituteExpr (bindings r1) e2) of
        Just r2 ->
          do
            nb <- mergeBindings (bindings r1) (bindings r2)
            return $ makeResult (term r2, nb) (atRule r2)
        Nothing ->
          -- Backtrack by recursing if e2 didn't unify
          trace ("Backtracking from " ++ (show (substituteExpr (bindings r1) e2)) ++ " to " ++ (show e1)) $ solveConjunct db ((atRule r1) + 1) e1 e2

