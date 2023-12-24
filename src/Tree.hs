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
  show (Compound n terms) = "Compound(" ++ n ++ ", " ++ show terms ++ ")"
  show (Variable n) = "Variable " ++ n
  show (Number n) = "Number " ++ show n
  show EmptyList = "EmptyList"
  show (Cons t1 t2) = "Cons(" ++ show t1 ++ ", " ++ show t2 ++ ")"

instance Show Expr where
  show (Term term) = show term
  show (Conjunct t1 t2) = show t1 ++ ", " ++ show t2

instance Show Clause where
  show (Fact term) = show term ++ "."
  show (Rule lhs rhs) = show lhs ++ " :- " ++ show rhs ++ "."
  show (Query expr) = "?- " ++ show expr ++ "."

class Emit a where
  emit :: a -> String 

emitList :: [Term] -> String
emitList [x] = emit x
emitList (x:xs) =
  emit x ++ ", " ++ emitList xs

emitCons :: Term -> String
emitCons (Cons t1 EmptyList) = emit t1
emitCons (Cons t1 t2) =
  emit t1 ++ ", " ++ emitCons t2

instance Emit Term where
  emit (Atom n) = n
  emit (Compound n terms) = n ++ "(" ++ emitList terms ++ ")"
  emit (Variable n) = n
  emit (Number n) = show n
  emit EmptyList = "[]"
  emit (Cons t1 t2) = "[" ++ emit t1 ++ ", " ++ emitCons t2 ++ "]"

instance Emit Expr where
  emit (Term term) = emit term
  emit (Conjunct t1 t2) = emit t1 ++ ", " ++ emit t2

instance Emit Clause where
  emit (Fact term) = emit term ++ "."
  emit (Rule lhs rhs) = emit lhs ++ " :- " ++ emit rhs ++ "."
  emit (Query expr) = "?- " ++ emit expr ++ "."
