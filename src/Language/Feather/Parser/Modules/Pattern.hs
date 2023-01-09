module Language.Feather.Parser.Modules.Pattern where
  import Language.Feather.CST.Expression ( Pattern(..) )
  import Control.Applicative ( Alternative((<|>)) )
  import Language.Feather.CST.Literal ( Located((:>:)) )
  import Language.Feather.Parser.Modules.Literal ( literal' )
  import Data.Functor ( (<&>) )

  import qualified Language.Feather.Parser.Lexer as L
  import qualified Text.Parsec as P

  patternExpression :: Monad m => L.Parser m (Located Pattern)
  patternExpression =  L.locate (L.identifier <&> PVariable)
                   <|> patternPair
                   <|> L.locate (L.reservedOp "_" *> return PWildcard)
                   <|> L.locate (literal' <&> PLiteral)

    
  patternPair :: Monad m => L.Feather m Pattern
  patternPair = do
    s <- P.getPosition
    ps <- L.parens $ L.commaSep patternExpression
    e <- P.getPosition
    case ps of
      [p] -> return $ p
      (x:xs) -> return $ foldl (\a b -> PPair a b :>: (s, e)) x xs
      [] -> return $ PWildcard :>: (s, e)