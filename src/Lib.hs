module Lib
  ( mainFunc
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


parseAndSolve :: IO ()
parseAndSolve
  = case parseClauseList exampleProgram6 of
      Nothing -> putStrLn "Parse Error"
      Just clauses ->
        let db = makeDatabase clauses in
          case parseTerm exampleProgramQ6 of
            Nothing -> putStrLn "Query Parse Error"
            Just (query, _) -> do
              putStrLn (show clauses)
              case solve db query of
                Just result -> do
                  putStrLn (show (term result))
                  putStrLn (show (bindings result))
                _ -> putStrLn "not found"


getAndSolve :: IO ()
getAndSolve
  = let db = makeDatabase exampleProgram1 in
      let query = exampleProgramQ1 in
      case solve db query of
        Just result -> do
          putStrLn (show (term result))
          putStrLn (show (bindings result))
        _ -> putStrLn "not found"

mainFunc :: IO ()
mainFunc
  = do
    -- putStrLn (show exampleProgram)
    -- putStrLn (show $ mergeBindings [bindTerm "X" (Atom "person")] [bindTerm "Y" (Atom "thing")])
    -- putStrLn $ show $ substituteTerm ([bindTerm "X" (Atom "person")]) (Compound "parent" [(Variable "X")])

    getAndSolve
    parseAndSolve


