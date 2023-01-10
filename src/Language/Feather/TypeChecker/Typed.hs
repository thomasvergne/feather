module Language.Feather.TypeChecker.Typed where
  import Language.Feather.CST.Literal
  import Language.Feather.TypeChecker.Type

  data Annoted a b
    = a :@ b
    deriving Show

  data TypedExpression
    = ELiteral Literal
    | EPair TypedExpression TypedExpression
    | EVariable String Qualifier
    | EApplication TypedExpression TypedExpression
    | EUnary (Annoted String Qualifier) TypedExpression
    | EBinary (Annoted String Qualifier) TypedExpression TypedExpression
    | EAbstraction (Annoted String Qualifier) TypedExpression
    | ELetIn (Annoted String Qualifier) TypedExpression TypedExpression
    | EIf TypedExpression TypedExpression TypedExpression
    | ECase TypedExpression [(TypedPattern, TypedExpression)]
    | EStructure String [Type] [Annoted String Type] TypedExpression

  data TypedPattern
    = PVariable String Type
    | PWildcard
    | PApp TypedPattern TypedPattern 
    | PLiteral Literal