module Language.Feather.Parser.Modules.Pattern where
  import Language.Feather.CST.Expression ( Pattern(..) )
  import Control.Applicative ( Alternative((<|>)) )
  import Control.Monad.State ( gets )
  import Language.Feather.CST.Literal ( Located((:>:)) )
  import Language.Feather.Parser.Modules.Literal ( literal' )
  import Data.Functor ( (<&>) )

  import qualified Text.Parsec.Expr as E  
  import qualified Language.Feather.Parser.Lexer as L

  patternExpression :: Monad m => L.Parser m (Located Pattern)
  patternExpression = do
    table' <- table
    E.buildExpressionParser table' term
    where table = do
            ops <- gets L.infixOperators
            return $ [ map (\(op, assoc) -> E.Infix (L.reservedOp op 
              >> return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> 
                PApp (PApp (PVariable op :>: (s, e)) x :>: (s, e)) y :>: (s, e))) assoc) ops,
                       [ E.Infix 
                          (return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> PApp x y :>: (s, e)))
                          E.AssocLeft ] ]

  term :: Monad m => L.Parser m (Located Pattern)
  term =  L.locate (L.identifier <&> PVariable)
      <|> L.locate (L.reservedOp "_" *> return PWildcard)
      <|> L.locate (literal' <&> PLiteral)