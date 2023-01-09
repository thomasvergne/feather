module Language.Feather.Parser.Parser where
  import Language.Feather.CST.Literal ( Located((:>:)) )
  import Language.Feather.CST.Expression ( Expression(..) )
  import Control.Applicative ( Alternative((<|>), some) )
  import Control.Monad.State ( void, modify )

  import qualified Text.Parsec as P
  import qualified Language.Feather.Parser.Lexer as L
  import qualified Text.Parsec.Expr as E

  import Language.Feather.Parser.Modules.LetExpressions
    ( letExpression )
  import Language.Feather.Parser.Modules.Operators
    ( operators, operatorsTable )
  import Language.Feather.Parser.Modules.Literal
    ( literal )
  import Language.Feather.Parser.Modules.Pattern
    ( patternExpression )
  import Language.Feather.Parser.Modules.Declaration
    ( declaration )

  parseFeather :: String -> String -> Either P.ParseError [Located Expression]
  parseFeather file x = L.runAssoc (P.runParserT (P.sepEndBy parser (P.optionMaybe L.semi) <* P.eof) () file x)

  parser :: Monad m => L.Feather m Expression
  parser = (L.whiteSpace *> expression)

  -- Expression parsing

  expression :: Monad m => L.Feather m Expression
  expression = do
    operators' <- operatorsTable
    E.buildExpressionParser operators' term P.<?> "expression"

  term :: Monad m => L.Feather m Expression
  term =  L.parens expression
      <|> infixExpression
      <|> structure
      <|> matchWith
      <|> literal
      <|> variable
      <|> abstraction
      <|> condition
      <|> P.try letInExpression'
      <|> letExpression'

  -- Structure parsing

  structure :: Monad m => L.Feather m Expression
  structure = do
    s <- P.getPosition
    L.reserved "struct"
    name <- L.identifier
    tyArgs <- P.many (P.char '\'' *> L.identifier)
    fields <- some $ do
      L.reservedOp "|"
      field <- L.identifier <|> L.parens operators
      L.reservedOp ":"
      ty <- declaration
      return (field, ty)
    e <- P.getPosition
    return $ EStructure name tyArgs fields :>: (s, e)

  -- Condition parsing

  condition :: Monad m => L.Feather m Expression
  condition = do
    s <- P.getPosition
    L.reserved "if"
    c <- expression
    L.reserved "then"
    t <- expression
    L.reserved "else"
    el <- expression
    e <- P.getPosition
    return $ EIf c t el :>: (s, e)

  -- Pattern matching parsing

  matchWith :: Monad m => L.Feather m Expression
  matchWith = do
    s <- P.getPosition
    L.reserved "match"
    e <- expression
    L.reserved "with"
    clauses <- some (do
      L.reservedOp "|"
      p <- patternExpression
      L.reservedOp "="
      expr <- expression
      return $ (p, expr))
    end <- P.getPosition
    return $ ECase e clauses :>: (s, end)

  -- Abstraction parsing

  abstraction :: Monad m => L.Feather m Expression
  abstraction = do
    s <- P.getPosition
    L.reserved "fun"
    args <- some L.identifier
    L.reserved "in"
    e <- P.getPosition
    body <- expression
    return $ EAbstraction args body :>: (s, e)
  
  -- Infix expression parsing
  
  infixExpression :: Monad m => L.Feather m Expression
  infixExpression = do
    s <- P.getPosition
    L.reserved "infix"
    assoc <- (L.reserved "left" >> return E.AssocLeft)
         <|> (L.reserved "right" >> return E.AssocRight)
         <|> (L.reserved "non" >> return E.AssocNone)
    op <- L.parens operators
    e <- P.getPosition
    modify $ \x -> x { L.infixOperators = (op, assoc) : L.infixOperators x }
    return $ EInfixDeclaration assoc op :>: (s, e)

  -- Variable parsing

  variable :: Monad m => L.Feather m Expression
  variable = L.locate $ EVariable <$> L.identifier

  -- Let expression parsing

  letInExpression' :: Monad m => L.Feather m Expression
  letInExpression' = do
    s <- P.getPosition
    L.reserved "let"
    e <- letExpression expression
    L.reserved "in"
    e' <- expression <* P.optionMaybe (void P.endOfLine <|> P.eof)
    return $ ELetIn e e' :>: (s, snd (L.getLoc e'))
  
  letExpression' :: Monad m => L.Feather m Expression
  letExpression' = do
    s <- P.getPosition
    L.reserved "let"
    le <- letExpression expression
    e <- P.getPosition 
    return $ ELet le :>: (s, e)
  