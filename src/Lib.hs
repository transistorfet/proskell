module Lib
  ( mainFunc
  , parsePair
  , parseTerm
  , parseAndSolve
  , solve
  , makeDatabase
  , Result
  , term
  , bindings
  ) where

import Tree
import Solver
import Parser

exampleProgram1 = [
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
exampleProgramQ1 = (Compound "father" [(Variable "X"), (Atom "bart")])

exampleProgram2 = [
    Fact (Compound "test" [EmptyList])
  , Rule (Compound "test" [Cons (Variable "X") (Variable "Xs")]) (Term (Compound "test" [(Variable "Xs")]))
  ]
exampleProgramQ2 = (Compound "test" [Cons (Atom "cat") (Cons (Atom "thing") (Cons (Atom "stuff") EmptyList))])

exampleProgram3 = [
    Rule (Compound "reverse" [Variable "X", Variable "Y"]) (Term (Compound "reverse" [Variable "X", Variable "Y", EmptyList]))
  , Fact (Compound "reverse" [EmptyList, Variable "Z", Variable "Z"])
  , Rule (Compound "reverse" [Cons (Variable "H") (Variable "T"), Variable "Z", Variable "Acc"]) (Term (Compound "reverse" [Variable "T", Variable "Z", Cons (Variable "H") (Variable "Acc")]))
  ]
exampleProgramQ3 = Compound "reverse" [(Cons (Atom "cat") (Cons (Atom "dog") (Cons (Atom "bird") EmptyList))), Variable "X"]


exampleProgram4 = "\
\       member(X, [X|Xs]).\n\
\       member(X, [Y|Xs]) :- member(X, Xs).\n"
exampleProgramQ4 = "member(cat, [thing, cat, stuff])."

exampleProgram5 = "\
\ % an example\n\
\       female(marge).\n\
\       female(lise).\n\
\       male(homer).\n\
\       male(bart).\n\
\       parent(marge, bart).\n\
\       parent(marge, lisa).\n\
\       parent(homer, bart).\n\
\       parent(homer, lisa).\n\
\       father(X, Y) :- parent(X, Y), male(X).\n"
exampleProgramQ5 = "father(X, bart)."


exampleProgram6 = "\
\       highest(X, [X|[]]).\n\
\       highest(X, [X|Xs]) :- highest(Y, Xs), X >= Y.\n\
\       highest(Y, [X|Xs]) :- highest(Y, Xs), X < Y.\n"

exampleProgramQ6 = "highest(X, [1, 8, 904, 234, 42])."

exampleProgram7 = "\
\    append([], Ys, Ys).\n\
\    append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).\n\
\    pivot(_, [], [], []).\n\
\    pivot(Pivot, [Head|Tail], [Head|LessOrEqualThan], GreaterThan) :- Pivot >= Head, pivot(Pivot, Tail, LessOrEqualThan, GreaterThan).\n\
\    pivot(Pivot, [Head|Tail], LessOrEqualThan, [Head|GreaterThan]) :- pivot(Pivot, Tail, LessOrEqualThan, GreaterThan).\n\
\    quicksort([], []).\n\
\    quicksort([Head|Tail], Sorted) :- pivot(Head, Tail, List1, List2), quicksort(List1, SortedList1), quicksort(List2, SortedList2), append(SortedList1, [Head|SortedList2], Sorted).\n"

exampleProgramQ7 = "quicksort([1, 8, 904, 234, 42], Sorted)."
--exampleProgramQ7 = "quicksort([1, 8, 904, 234, 42], Sorted)."

exampleProgram8 = "\
\        append([], Ys, Ys).\n\
\        append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).\n"

exampleProgramQ8 = "append([thing, stuff, cat], [more, cat, stuff], Zs)."


printResult :: Maybe Result -> IO ()
printResult result
  = case result of
      Just result -> do
        putStrLn $ "\nFound result " ++ (emit (term result))
      Nothing -> putStrLn "\nNot Found"


parsePair :: String -> String -> Maybe ([Clause], Term)
parsePair clausesStr queryStr
  = do
      clauses <- parseClauseList clausesStr
      (query, remain) <- parseTerm queryStr
      return (clauses, query)


parseAndSolve :: String -> String -> IO ()
parseAndSolve clausesStr queryStr
  = case parsePair clausesStr queryStr of
      Just (clauses, query) -> printResult $ solve (makeDatabase clauses) query
      Nothing -> putStrLn "Parse error"


mainFunc :: IO ()
mainFunc
  = do
      parseAndSolve exampleProgram8 exampleProgramQ8


