{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Feather.Pretty.CST where
  import qualified Prettyprinter as P
  import Language.Feather.CST.Expression
    ( Expression(..), LetExpression(..), Pattern(..) )
  import Language.Feather.CST.Literal ( Located(..) )
  import Language.Feather.CST.Declaration ( Declaration(..) )
  import Text.Parsec.Expr ( Assoc(..) )
  
  located :: (a -> P.Doc ann) -> (Located a -> P.Doc ann)
  located f (a :>: _) = f a

  prettyExpression :: Expression -> P.Doc ann
  prettyExpression (ELiteral l) = P.pretty $ show l
  prettyExpression (EPair e1 e2) = P.parens $ located prettyExpression e1 P.<+> P.comma P.<+> located prettyExpression e2
  prettyExpression (EVariable v) = P.pretty v
  prettyExpression (EApplication e1 e2) = P.parens $ located prettyExpression e1 P.<+> located prettyExpression e2
  prettyExpression (EBinary op e1 e2) = P.parens $ located prettyExpression e1 P.<+> P.pretty op P.<+> located prettyExpression e2
  prettyExpression (EAbstraction vs e) = P.pretty "fun" 
    P.<+> P.hsep (map P.pretty vs) P.<+> P.pretty "in" 
    P.<+> located prettyExpression e
  prettyExpression (EUnary op e) = P.parens $ P.pretty op <> located prettyExpression e
  prettyExpression (ELetIn l e) = P.pretty "let" 
    P.<+> prettyLetExpression l 
    P.<+> P.pretty "\nin" P.<+> located prettyExpression e
  prettyExpression (ELet l) = P.pretty "let" P.<+> prettyLetExpression l
  prettyExpression (EInfixDeclaration a op) = P.pretty "infix" 
    P.<+> P.pretty (show a) 
    P.<+> P.pretty op
  prettyExpression (EIf e1 e2 e3) = P.pretty "if" 
    P.<+> located prettyExpression e1 
    P.<+> P.pretty "then" P.<+> located prettyExpression e2 
    P.<+> P.pretty "else" P.<+> located prettyExpression e3
  prettyExpression (ECase e cs) = P.pretty "case" 
    P.<+> located prettyExpression e 
    P.<+> P.pretty "of" 
    P.<+> P.align (P.vsep (map (\(p, e') -> P.pretty "| " <> located prettyPattern p P.<+> located prettyExpression e') cs))
  prettyExpression (EStructure s vs cs) = P.pretty "struct" 
    P.<+> P.pretty s 
    P.<+> P.hsep (map P.pretty vs) 
    P.<+> P.align (P.vsep (map (\(n, xs) -> P.pretty "| " <> P.pretty n P.<+> prettyDeclaration xs) cs))

  prettyPattern :: Pattern -> P.Doc ann
  prettyPattern (PVariable v) = P.pretty v
  prettyPattern (PWildcard) = P.pretty "_"
  prettyPattern (PLiteral l) = P.pretty $ show l
  prettyPattern (PApp e1 e2) = P.parens $ located prettyPattern e1 P.<+> located prettyPattern e2

  instance Show Assoc where
    show = \case
      AssocNone -> "none"
      AssocLeft -> "left"
      AssocRight -> "right"

  prettyDeclaration :: Declaration -> P.Doc ann
  prettyDeclaration DChar = P.pretty "char"
  prettyDeclaration DInt = P.pretty "int"
  prettyDeclaration DBool = P.pretty "bool"
  prettyDeclaration DVoid = P.pretty "void"
  prettyDeclaration DString = P.pretty "string"
  prettyDeclaration DFloat = P.pretty "float"
  prettyDeclaration (DId s) = P.pretty s
  prettyDeclaration (DGeneric s) = P.pretty ("'" <> s) 
  prettyDeclaration (DApp (DApp (DId "->") a) b) = P.parens $ prettyDeclaration a P.<+> P.pretty "->" P.<+> prettyDeclaration b
  prettyDeclaration (DApp n x) = P.parens $ prettyDeclaration n P.<+> prettyDeclaration x

  prettyLetExpression :: LetExpression -> P.Doc ann
  prettyLetExpression = \case
    LetAbsExpression v vs e wc -> 
        P.pretty v P.<+> P.hsep (map P.pretty vs) 
                   P.<+> P.pretty "=" 
                   P.<+> located prettyExpression e 
                   P.<+> P.pretty "\n" <> P.indent 2 (whereClause wc)
    LetAbsClauseExpression v vs gc wc -> 
        P.pretty v P.<+> P.hsep (map P.pretty vs) 
                   P.<+> guardClause gc 
                   P.<+> P.pretty "\n" <> P.indent 2 (whereClause wc)
    LetExpression v e -> 
        P.pretty v P.<+> P.pretty "=" P.<+> located prettyExpression e
    LetPatternExpression pat e -> 
        located prettyPattern pat P.<+> P.pretty "=" P.<+> located prettyExpression e
    where whereClause wc = if length wc == 0
                              then mempty 
                              else P.pretty "where" P.<+> P.align (P.vsep (map (\(x :>:_) -> prettyWhereClause x) wc))
          guardClause gc = P.align (P.vsep (map (\(x :>: _) -> prettyGuardClause x) gc))

  prettyWhereClause :: LetExpression -> P.Doc ann
  prettyWhereClause v = prettyLetExpression v

  prettyGuardClause :: (Located Expression, Located Expression) -> P.Doc ann
  prettyGuardClause (e1, e2) = P.pretty "|" P.<+> located prettyExpression e1 P.<+> P.pretty "=" P.<+> located prettyExpression e2

  instance Show Expression where
    show = show . prettyExpression