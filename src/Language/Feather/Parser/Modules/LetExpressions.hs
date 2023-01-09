module Language.Feather.Parser.Modules.LetExpressions where
  import Language.Feather.CST.Expression ( Expression(..), LetExpression(..), Pattern(..) )
  import Control.Applicative ( Alternative(some, (<|>)) )
  import Language.Feather.CST.Literal ( Located((:>:)) )
  import Language.Feather.Parser.Modules.Pattern ( patternExpression )
  import Language.Feather.Parser.Modules.Operators ( operators )

  import qualified Text.Parsec as P
  import qualified Language.Feather.Parser.Lexer as L

  letExpression :: Monad m => L.Feather m Expression -> L.Parser m LetExpression
  letExpression expression =  P.try (letPatternExpression expression)
                          <|> P.try (letInfixAbsExpression expression)
                          <|> P.try (letAbsExpression expression)
                          <|> P.try (letInfixAbsClauseExpression expression)
                          <|> P.try (letAbsClauseExpression expression)
                          <|> P.try (letExpression' expression)

  letExpression' :: Monad m => L.Feather m Expression -> L.Parser m LetExpression
  letExpression' expr = LetExpression <$> (L.identifier <|> L.parens operators) <*> (L.reservedOp "=" *> expr)

  letInfixAbsExpression :: Monad m => L.Feather m Expression -> L.Parser m LetExpression
  letInfixAbsExpression expr = do
    (arg1, name) <- P.try $ (,) <$> L.identifier <*> operators
    arg2 <- L.identifier
    L.reservedOp "="
    body <- expr
    whereClause <- P.option [] (L.reserved "where" *> some (L.locate $ letExpression' expr))
    return $ LetAbsExpression name [arg1, arg2] body whereClause

  letInfixAbsClauseExpression :: Monad m => L.Feather m Expression -> L.Parser m LetExpression
  letInfixAbsClauseExpression expr = do
    (arg1, name) <- P.try $ (,) <$> L.identifier <*> operators
    arg2 <- L.identifier
    clauses <- some (do
      s <- P.getPosition
      L.reservedOp "|"
      e <- expr
      L.reservedOp "="
      e' <- expr
      end <- P.getPosition
      return $ (e, e') :>: (s, end))
    whereClause <- P.option [] (L.reserved "where" *> some (L.locate $ letExpression' expr))
    return $ LetAbsClauseExpression name [arg1, arg2] clauses whereClause
  
  letAbsExpression :: Monad m => L.Feather m Expression -> L.Parser m LetExpression
  letAbsExpression expr = do
    name <- L.identifier <|> L.parens operators
    args <- some (L.identifier <|> L.parens operators)
    L.reservedOp "="
    body <- expr
    whereClause <- P.option [] (L.reserved "where" *> some (L.locate $ letExpression' expr))
    return $ LetAbsExpression name args body whereClause
    
  letAbsClauseExpression :: Monad m => L.Feather m Expression -> L.Parser m LetExpression
  letAbsClauseExpression expr = do
    name <- L.identifier <|> L.parens operators
    args <- some (L.identifier <|> L.parens operators)
    clauses <- some (do
      s <- P.getPosition
      L.reservedOp "|"
      e <- expr
      L.reservedOp "="
      e' <- expr
      end <- P.getPosition
      return $ (e, e') :>: (s, end))
    whereClause <- P.option [] (L.reserved "where" *> some (L.locate $ letExpression' expr))
    return $ LetAbsClauseExpression name args clauses whereClause

  letPatternExpression :: Monad m => L.Feather m Expression -> L.Parser m LetExpression
  letPatternExpression expr = do
    name <- L.parens patternExpression <|> L.locate (L.reservedOp "_" *> return PWildcard)
    L.reservedOp "="
    body <- expr
    return $ LetPatternExpression name body