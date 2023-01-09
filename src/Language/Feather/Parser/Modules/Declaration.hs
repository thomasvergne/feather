module Language.Feather.Parser.Modules.Declaration where
  import Language.Feather.CST.Declaration ( Declaration(..) )
  import Control.Applicative ( Alternative((<|>)) )
  import Data.Functor ( ($>), (<&>) )

  import qualified Text.Parsec as P
  import qualified Language.Feather.Parser.Lexer as L
  import qualified Text.Parsec.Expr as E

  declaration :: Monad m => L.Parser m Declaration
  declaration = E.buildExpressionParser table term
    where table = [[ E.Infix (return DApp) E.AssocLeft ], 
                   [ E.Infix (L.reservedOp "->" >> return (DApp . DApp (DId "->"))) E.AssocRight ]]

  term :: Monad m => L.Parser m Declaration
  term =  (L.identifier <&> DId)
      <|> ((P.char '\'' *> L.identifier) <&> DGeneric)
      <|> (L.reserved "float"  $> DFloat)
      <|> (L.reserved "int"    $> DInt)
      <|> (L.reserved "string" $> DString)
      <|> (L.reserved "char"   $> DChar)
      <|> (L.reserved "bool"   $> DBool)
      <|> declarationPair
      <|> L.parens declaration

  declarationPair :: Monad m => L.Parser m Declaration
  declarationPair = do
    ds <- L.parens $ L.commaSep declaration
    case ds of
      [d] -> return d
      (x:xs) -> return $ foldl (\a b -> DPair a b) x xs
      [] -> return $ DVoid 
