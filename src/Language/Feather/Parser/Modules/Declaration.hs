module Language.Feather.Parser.Modules.Declaration where
  import Language.Feather.CST.Declaration ( Declaration(..) )
  import Control.Applicative ( Alternative((<|>)) )
  import Data.Functor ( ($>), (<&>) )
  import Control.Monad.State ( gets )
  import Language.Feather.Parser.Modules.Operators ( operators )

  import qualified Text.Parsec as P
  import qualified Language.Feather.Parser.Lexer as L
  import qualified Text.Parsec.Expr as E

  declaration :: Monad m => L.Parser m Declaration
  declaration = do
    table' <- table
    E.buildExpressionParser table' term
    where table = do 
                  infixOps <- gets L.infixOperators
                  return $ [
                   [ E.Infix (return DApp) E.AssocLeft ], 
                   [ E.Infix (L.reservedOp "->" >> return (DApp . DApp (DId "->"))) E.AssocRight ],
                   map (\(op, assoc) -> E.Infix (L.reservedOp op >> return (DApp . DApp (DId op))) assoc) infixOps]

  term :: Monad m => L.Parser m Declaration
  term =  ((P.char '\'' *> L.identifier) <&> DGeneric)
      <|> P.try ((L.identifier <|> L.parens operators) <&> DId)
      <|> (L.reserved "float"  $> DFloat)
      <|> (L.reserved "int"    $> DInt)
      <|> (L.reserved "string" $> DString)
      <|> (L.reserved "char"   $> DChar)
      <|> (L.reserved "bool"   $> DBool)
      <|> L.parens declaration
