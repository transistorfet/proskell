module Lib
  ( mainFunc
  ) where

import Tree
import Solver


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
exampleProgramQ = (Compound "father" [(Variable "X"), (Atom "bart")])

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


mainFunc :: IO ()
mainFunc
  = do
    -- putStrLn (show exampleProgram)
    -- putStrLn (show $ mergeBindings [bindTerm "X" (Atom "person")] [bindTerm "Y" (Atom "thing")])
    -- putStrLn $ show $ substituteTerm ([bindTerm "X" (Atom "person")]) (Compound "parent" [(Variable "X")])
    let db = makeDatabase exampleProgram3 in
      let query = exampleProgramQ3 in
      case solve db query of
        Just result -> do
          putStrLn (show (term result))
          putStrLn (show (bindings result))
        _ -> putStrLn "not found"


