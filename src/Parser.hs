module Parser where

import Debug.Trace

import Tree

data Token
  = Word String
  | OpenBracket
  | CloseBracket
  | OpenSquare
  | CloseSquare
  | VerticalBar
  | Comma
  | Horn
  | Period
  | Eof
  deriving (Eq)


isNumber :: Char -> Bool
isNumber c
  = c >= '0' && c <= '9'

isUpperCase :: Char -> Bool
isUpperCase c
  = c >= 'A' && c <= 'Z'

isWord :: Char -> Bool
isWord c
  = c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9' || c == '_'

isOperator :: Char -> Bool
isOperator c
  = elem c [';', ':', '=', '>', '<', '+', '-', '*', '\\', '/', '!', '#', '$', '?', '@', '^']

operators = [",", "=", "\\=", ">", ">=", "<", "<=", "+", "-", "is"]


takeUntil :: (Char -> Bool) -> String -> (String, String)
takeUntil f [] = ([], [])
takeUntil f (c:cs)
  = if f c
    then let (word, remain) = takeUntil f cs
      in (c:word, remain)
    else ([], c:cs)


getToken :: String -> Maybe (Token, String)
getToken [] = Just (Eof, [])
getToken (' ':cs) = getToken cs
getToken ('\n':cs) = getToken cs
getToken ('\t':cs) = getToken cs
getToken (c:cs)
  = case c of
      '(' -> Just (OpenBracket, cs)
      ')' -> Just (CloseBracket, cs)
      '[' -> Just (OpenSquare, cs)
      ']' -> Just (CloseSquare, cs)
      '|' -> Just (VerticalBar, cs)
      ',' -> Just (Comma, cs)
      '.' -> Just (Period, cs)
      ':' -> if (head cs) == '-' then Just (Horn, (tail cs)) else Nothing
      '%' ->
        let (_, remain) = takeUntil (\c -> c /= '\n') cs
        in getToken remain
      c ->
        if isWord c then
          let (word, remain) = takeUntil isWord (c:cs)
          in Just (Word word, remain)
        else if isOperator c then
          let (word, remain) = takeUntil isOperator (c:cs)
          in Just (Word word, remain)
        else
          Nothing -- TODO this is temporary, infix operators and such aren't parsed


expectToken :: Token -> String -> Maybe String
expectToken token input
  = checkToken $ getToken input
  where
    checkToken :: Maybe (Token, String) -> Maybe String
    checkToken (Just (next, remain)) | next == token = Just remain
    checkToken _ = Nothing


parseList :: String -> Maybe (Term, String)
parseList input
  = case getToken input of
      Just (Comma, remain) -> parseList remain
      Just (CloseSquare, remain) -> Just (EmptyList, remain)
      Just (VerticalBar, remain) -> do
        (term, remain) <- parseTerm remain
        remain <- expectToken CloseSquare remain
        return (term, remain)
      Just _ -> do
        (term, remain) <- parseTerm input
        (list, remain) <- parseList remain
        return (Cons term list, remain)
      Nothing -> Nothing


parseTerm :: String -> Maybe (Term, String)
parseTerm input
  = do
      (token, remain) <- getToken input
      (term, remain) <- parseToken token remain
      case checkOperator remain of
        Just (operator, remain) -> do
          (rhs, remain) <- parseTerm remain
          return (Compound operator [term, rhs], remain)
        Nothing -> return (term, remain)
  where

    parseToken token remain =
      case token of
        Word w -> do
          (next, next_rem) <- getToken remain
          if next == OpenBracket then
            do
              (list, remain) <- parseTermList next_rem
              remain <- expectToken CloseBracket remain
              return $ (Compound w list, remain)
          else if isNumber (head w) then
            Just (Number (read w :: Int), remain)
          else if isUpperCase (head w) then
            Just (Variable w, remain)
          else
            Just (Atom w, remain)
        OpenSquare -> parseList remain
        _ -> Nothing

    checkOperator remain =
      case getToken remain of
        Just (Word w, next_rem) | elem w operators -> Just (w, next_rem)
        _ -> Nothing


parseTermList :: String -> Maybe ([Term], String)
parseTermList input
  = do
      (term, remain) <- parseTerm input
      case expectToken Comma remain of
        Just remain -> do
          (list, remain) <- parseTermList remain
          return $ (term:list, remain)
        Nothing ->
          Just ([term], remain)



parseExpr :: String -> Maybe (Expr, String)
parseExpr input
  = do
      (term, remain) <- parseTerm input
      case expectToken Comma remain of
        Just remain -> do
          (rhs, next_remain) <- parseExpr remain
          return $ (Conjunct (Term term) rhs, next_remain)
        Nothing ->
          return $ (Term term, remain)


parseClause :: String -> Maybe (Clause, String)
parseClause input
  = do
      (term, remain1) <- parseTerm input
      case getToken remain1 of
        Just (Period, remain2) ->
          return $ (Fact term, remain2)
        Just (Horn, remain2) -> do
          (rhs, remain3) <- parseExpr remain2
          remain4 <- expectToken Period remain3
          return $ (Rule term rhs, remain4)
        _ ->
          Nothing


parseClauseList :: String -> Maybe [Clause]
parseClauseList input
  = case expectToken Eof input of
      Just _ -> Just []
      Nothing -> do
        (clause, remain) <- parseClause input
        list <- parseClauseList remain
        return $ clause:list

