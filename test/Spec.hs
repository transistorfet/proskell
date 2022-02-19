
import Lib
import Tree


runQuery :: String -> String -> String -> Maybe (Term, Term)
runQuery clausesStr queryStr expectedTermStr
  = do
      (clauses, query) <- parsePair clausesStr queryStr
      (expectedResult, _) <- parseTerm expectedTermStr
      result <- solve (makeDatabase clauses) query
      return (term result, expectedResult)


runTest :: String -> String -> String -> IO ()
runTest clauseStr queryStr expectedTermStr
  = case runQuery clauseStr queryStr expectedTermStr of
      Just (result, expected) | result /= expected -> 
        putStrLn ("Expected " ++ (show expected) ++ "\nBut got " ++ (show result))
      _ ->
        putStrLn "."


main :: IO ()
main = do
  runTest
    " % an example\n\
    \  female(marge).\n\
    \  female(lise).\n\
    \  male(homer).\n\
    \  male(bart).\n\
    \  parent(marge, bart).\n\
    \  parent(marge, lisa).\n\
    \  parent(homer, bart).\n\
    \  parent(homer, lisa).\n\
    \  father(X, Y) :- parent(X, Y), male(X).\n"
    "father(X, bart)."
    "father(homer, bart)."

  runTest
    "  highest(X, [X|[]]).\n\
    \  highest(X, [X|Xs]) :- highest(Y, Xs), X >= Y.\n\
    \  highest(Y, [X|Xs]) :- highest(Y, Xs), X < Y.\n"
    "highest(X, [1, 8, 904, 234, 42])."
    "highest(904, [1, 8, 904, 234, 42])."


  runTest
    "  member(X, [X|Xs]).\n\
    \  member(X, [Y|Xs]) :- member(X, Xs).\n"
    "member(cat, [thing, cat, stuff])."
    "member(cat, [thing, cat, stuff])."


  runTest
    "  append([], Ys, Ys).\n\
    \  append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).\n\
    \  pivot(_, [], [], []).\n\
    \  pivot(Pivot, [Head|Tail], [Head|LessOrEqualThan], GreaterThan) :- Pivot >= Head, pivot(Pivot, Tail, LessOrEqualThan, GreaterThan).\n\
    \  pivot(Pivot, [Head|Tail], LessOrEqualThan, [Head|GreaterThan]) :- pivot(Pivot, Tail, LessOrEqualThan, GreaterThan).\n\
    \  quicksort([], []).\n\
    \  quicksort([Head|Tail], Sorted) :- pivot(Head, Tail, List1, List2), quicksort(List1, SortedList1), quicksort(List2, SortedList2), append(SortedList1, [Head|SortedList2], Sorted).\n"
    "quicksort([1, 8, 904, 234, 42], Sorted)."
    "quicksort([1, 8, 904, 234, 42], [1, 8, 42, 234, 904])."

  runTest
    "  append([], Ys, Ys).\n\
    \  append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).\n"
    "append([thing, stuff, cat], [more, cat, stuff], Zs)."
    "append([thing, stuff, cat], [more, cat, stuff], [thing, stuff, cat, more, cat, stuff])."

  putStrLn "Complete"

