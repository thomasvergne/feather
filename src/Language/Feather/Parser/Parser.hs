module Language.Feather.Parser.Parser where
  import Language.Feather.CST.Literal ( Located((:>:)) )
  import Language.Feather.CST.Expression ( Expression(..) )
  import Control.Applicative ( Alternative((<|>), some) )
  import Control.Monad.State ( void, modify )

  import qualified Text.Parsec as P
  import qualified Language.Feather.Parser.Lexer as L
  import qualified Text.Parsec.Expr as E
  import qualified Text.Parsec.Token as Token

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
  parseFeather file x = L.runAssoc $ (P.runParserT (P.sepEndBy parser (P.optionMaybe L.semi) <* P.eof) () file x)

  parser :: Monad m => L.Feather m Expression
  parser = (L.whiteSpace *> topLevel)

  -- Expression parsing

  topLevel :: Monad m => L.Feather m Expression
  topLevel =  infixExpression
          <|> structure
          <|> import'
          <|> class'
          <|> inherit
          <|> letExpression'
          <|> expression

  expression :: Monad m => L.Feather m Expression
  expression = do
    operators' <- operatorsTable
    E.buildExpressionParser operators' term P.<?> "expression"

  term :: Monad m => L.Feather m Expression
  term =  L.parens expression
      <|> matchWith
      <|> literal
      <|> variable
      <|> abstraction
      <|> condition
      <|> P.try letInExpression'

  -- Import opening parsing

  import' :: Monad m => L.Feather m Expression
  import' = do
    s <- P.getPosition
    L.reserved "open"
    name <- Token.stringLiteral L.lexer
    e <- P.getPosition
    return $ EOpen name :>: (s, e)

  -- Class parsing

  class' :: Monad m => L.Feather m Expression
  class' = do
    s <- P.getPosition
    L.reserved "class"
    name <- L.identifier
    decl <- P.char '\'' *> L.identifier
    decls <- some $ L.reservedOp "|" *> do
      name' <- L.identifier <|> L.parens operators
      tys <- P.many (P.char '\'' *> L.identifier)
      L.reservedOp ":"
      ty <- declaration
      return (name', tys, ty)
    e <- P.getPosition
    return $ EClass name decl decls :>: (s, e)

  -- Type inheritance parsing

  inherit :: Monad m => L.Feather m Expression
  inherit = do
    s <- P.getPosition
    L.reserved "inherit"
    name <- L.identifier
    decl <- declaration
    supers <- P.option [] $ L.reserved "with" *> some ((,) <$> L.identifier <*> (P.char '\'' *> L.identifier))
    exprs <- some $ L.reservedOp "|" *> L.locate (letExpression expression)
    e' <- P.getPosition
    return $ EInherit supers name decl exprs :>: (s, e')

  -- Structure parsing

  structure :: Monad m => L.Feather m Expression
  structure = do
    s <- P.getPosition
    L.reserved "struct"
    name <- L.identifier <|> L.parens operators
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
  