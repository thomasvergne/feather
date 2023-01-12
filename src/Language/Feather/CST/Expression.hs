{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Language.Feather.CST.Expression where
  import Language.Feather.CST.Literal ( Literal, Located )
  import Language.Feather.CST.Declaration ( Declaration )
  import Text.Parsec.Expr ( Assoc )
  
  data Expression
    = ELiteral Literal
    | EPair (Located Expression) (Located Expression)
    | EVariable String
    | EApplication (Located Expression) (Located Expression)
    | EUnary String (Located Expression)
    | EBinary String (Located Expression) (Located Expression)
    | EAbstraction [String] (Located Expression)
    | ELetIn LetExpression (Located Expression)
    | ELet LetExpression
    | EInfixDeclaration Assoc String
    | EIf (Located Expression) (Located Expression) (Located Expression)
    | ECase (Located Expression) [(Located Pattern, Located Expression)]
    | EStructure String [String] [(String, Declaration)]
    | EInherit [(String, String)] String Declaration [Located LetExpression]
    | EClass String String [(String, [String], Declaration)]
    | EOpen String
  
  type WhereClause = [Located LetExpression]
  type GuardClause = [Located (Located Expression, Located Expression)]

  data LetExpression 
    = LetAbsExpression String [String] (Located Expression) WhereClause
    | LetAbsClauseExpression String [String] GuardClause WhereClause
    | LetExpression String (Located Expression)
    | LetPatternExpression (Located Pattern) (Located Expression)

  data Pattern
    = PVariable String
    | PWildcard
    | PApp (Located Pattern) (Located Pattern)
    | PLiteral Literal