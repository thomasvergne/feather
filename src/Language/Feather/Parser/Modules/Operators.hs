module Language.Feather.Parser.Modules.Operators where
  import Language.Feather.CST.Literal ( Located((:>:)) )
  import Language.Feather.CST.Expression ( Expression(..) )
  import Control.Applicative ( Alternative(some) )
  import Control.Monad.State ( StateT, gets )

  import qualified Language.Feather.Parser.Lexer as L
  import qualified Text.Parsec.Expr as E
  import qualified Text.Parsec as P
  
  makeUnaryOp :: Alternative f => f (a -> a) -> f (a -> a)
  makeUnaryOp s = foldr1 (.) . reverse <$> some s

  operatorsTable :: Monad m => L.Parser m [[E.Operator String () (StateT L.FeatherState m) (Located Expression)]]
  operatorsTable = do
    operators' <- gets L.infixOperators
    return [
        [ E.Infix 
            (return (\callee' x -> EApplication callee' x :>: (fst $ L.getLoc callee', snd (L.getLoc x)))) 
            E.AssocLeft ],
        map (\(op, assoc) -> E.Infix ((L.reservedOp op) >> return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> EBinary op x y :>: (s, e))) assoc) operators',
        equalities,
        [ E.Infix 
            (L.reservedOp "*" >> 
              return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> EBinary "*" x y :>: (s, e))) 
            E.AssocLeft,
          E.Infix 
            (L.reservedOp "/" >> 
              return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> EBinary "/" x y :>: (s, e))) 
            E.AssocLeft ],
        [ E.Prefix 
            (makeUnaryOp $ L.reservedOp "-" >> return (\x -> EUnary "-" x :>: (L.getLoc x))) ],
        [ E.Infix 
            (L.reservedOp "+" >> 
              return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> EBinary "+" x y :>: (s, e))) 
            E.AssocLeft,
          E.Infix 
            (L.reservedOp "-" >> 
              return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> EBinary "-" x y :>: (s, e))) 
            E.AssocLeft ]
      ]
    where equalityOp = ["==", "!=", "<", ">", "<=", ">="]
          equalities = map (\op -> E.Infix (L.reservedOp op >> return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> EBinary op x y :>: (s, e))) E.AssocLeft) equalityOp

  operator :: Monad m => L.Parser m Char
  operator = P.choice $ map (\x -> L.reserved [x] >> return x) L.operators

  operators :: Monad m => L.Parser m String
  operators = some operator