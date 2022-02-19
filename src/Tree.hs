module Tree where

data Term =
    Atom String
  | Compound String [Term]
  | Variable String
  | Number Int
  | EmptyList
  | Cons Term Term
  deriving (Eq)

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
  show EmptyList = "EmptyList"
  show (Cons t1 t2) = "Cons(" ++ (show t1) ++ ", " ++ (show t2) ++ ")"

instance Show Expr where
  show (Term term) = show term
  show (Conjunct t1 t2) = (show t1) ++ ", " ++ (show t2)

instance Show Clause where
  show (Fact term) = show term ++ "."
  show (Rule lhs rhs) = (show lhs) ++ " :- " ++ (show rhs) ++ "."
  show (Query expr) = "?- " ++ (show expr) ++ "."

