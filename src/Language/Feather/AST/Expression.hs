module Language.Feather.AST.Expression where
  import Language.Feather.CST.Literal ( Located, Literal )
  import Language.Feather.CST.Expression ( Pattern )
  import Language.Feather.CST.Declaration ( Declaration )
  
  data Expression
    = ELiteral Literal
    | EPair (Located Expression) (Located Expression)
    | EVariable String
    | EApplication (Located Expression) (Located Expression)
    | EUnary String (Located Expression)
    | EBinary String (Located Expression) (Located Expression)
    | EAbstraction String (Located Expression)
    | ELetIn String (Located Expression) (Located Expression)
    | EIf (Located Expression) (Located Expression) (Located Expression)
    | ECase (Located Expression) [(Located Pattern, Located Expression)]
    | EStructure String [String] [(String, Declaration)] (Located Expression)