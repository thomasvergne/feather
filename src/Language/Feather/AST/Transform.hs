module Language.Feather.AST.Transform where
  import Language.Feather.CST.Literal ( Located(..) )
  import Language.Feather.Parser.Lexer ( getLoc )
  import Data.Bifunctor ( Bifunctor(second) )
  import Language.Feather.TypeChecker.Methods
  import Data.Either

  import qualified Language.Feather.CST.Expression as C
  import qualified Language.Feather.AST.Expression as A

  removeNilExpressions :: Located A.Expression -> Located A.Expression
  removeNilExpressions (A.ELetIn "_" (A.EVariable "nil" :>: _) e2 :>: _) = removeNilExpressions e2
  removeNilExpressions x = x

  transformExpressions :: [Located C.Expression] -> Located A.Expression
  transformExpressions [x] = transformExpression x
  transformExpressions (C.EClass name decl decls :>: p : es) = A.EClass name decl decls (transformExpressions es) :>: p
  transformExpressions (C.EInherit sups name decl exprs :>: p : es) = A.EInherit sups name decl fields (transformExpressions es) :>: p
    where fields = map (fromRight (error "Not defined") . transformLetExpression . unLoc) exprs
  transformExpressions (C.EStructure s vs ds :>: p : es) = A.EStructure s vs ds (transformExpressions es) :>: p
  transformExpressions (C.ELet l :>: p : es) = case res of
    Right (name, expr) -> A.ELetIn name expr (transformExpressions es) :>: p
    Left (pat, e) -> A.ECase e [(pat, transformExpressions es)] :>: p
    where res = transformLetExpression l
  transformExpressions (e:es) = A.ELetIn "_" (transformExpression e) (transformExpressions es) :>: getLoc e
  transformExpressions [] = error "transformExpressions: empty list"

  transformExpression :: Located C.Expression -> Located A.Expression
  transformExpression (C.ELiteral l :>: p) = A.ELiteral l :>: p
  transformExpression (C.EPair e1 e2 :>: p) = A.EPair (transformExpression e1) (transformExpression e2) :>: p
  transformExpression (C.EVariable v :>: p) = A.EVariable v :>: p
  transformExpression (C.EApplication e1 e2 :>: p) = A.EApplication (transformExpression e1) (transformExpression e2) :>: p
  transformExpression (C.EUnary op e :>: p) = A.EUnary op (transformExpression e) :>: p
  transformExpression (C.EBinary op e1 e2 :>: p) = A.EBinary op (transformExpression e1) (transformExpression e2) :>: p
  transformExpression (C.EAbstraction vs e :>: p) = foldl (\e' v -> A.EAbstraction v e' :>: p) (transformExpression e) vs
  transformExpression (C.ELetIn l e2 :>: p) = case res of
    Right (name, expr) -> A.ELetIn name expr (transformExpression e2) :>: p
    Left (pat, e) -> A.ECase e [(pat, transformExpression e2)] :>: p
    where res = transformLetExpression l
  transformExpression (C.EIf e1 e2 e3 :>: p) = A.EIf (transformExpression e1) (transformExpression e2) (transformExpression e3) :>: p
  transformExpression (C.ELet _ :>: _) = error "transformExpression: let expression not in let-in"
  transformExpression (C.ECase e cs :>: p) = A.ECase (transformExpression e) (map (second transformExpression) cs) :>: p
  transformExpression (_ :>: p) = A.EVariable "nil" :>: p

  transformLetExpression :: C.LetExpression -> Either (Located C.Pattern, Located A.Expression) (String, Located A.Expression)
  transformLetExpression (C.LetAbsExpression v vs e w) 
    = Right (v, abs')
    where e'   = transformExpression e
          abs' = foldl (\e'' v' -> A.EAbstraction v' e'' :>: getLoc e) (transformWhereClause w e') $ reverse vs
  transformLetExpression (C.LetAbsClauseExpression name args gc wc) 
    = Right (name, abs')
    where gc'  = transformGuardClause gc
          abs' = foldl (\e' v -> A.EAbstraction v e' :>: getLoc gc') (transformWhereClause wc gc') $ reverse args 
  transformLetExpression (C.LetExpression v e) = Right (v, transformExpression e)
  transformLetExpression (C.LetPatternExpression pat e) = Left (pat, transformExpression e)

  transformGuardClause :: C.GuardClause -> Located A.Expression
  transformGuardClause [(C.EVariable "otherwise" :>: _, x) :>: _] = transformExpression x
  transformGuardClause ((cond, x) :>: pos:xs) = A.EIf (transformExpression cond) (transformExpression x) (transformGuardClause xs) :>: pos
  transformGuardClause [] = error "Empty guard clause"

  transformWhereClause :: C.WhereClause -> Located A.Expression -> Located A.Expression
  transformWhereClause (x:xs) e = case res of
    Right (name, expr) -> A.ELetIn name expr (transformWhereClause xs e) :>: pos
    Left (pat, e') -> A.ECase e' [(pat, transformWhereClause xs e)] :>: pos
    where res :>: pos = transformLetExpression <$> x
  transformWhereClause [] e = e