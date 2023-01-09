{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Feather.Pretty.AST where
  import qualified Prettyprinter as P
  import Language.Feather.AST.Expression
    ( Expression(..) )
  import Language.Feather.CST.Literal ( Located(..) )
  import Language.Feather.Pretty.CST ( prettyPattern, prettyDeclaration )
  
  located :: (a -> P.Doc ann) -> (Located a -> P.Doc ann)
  located f (a :>: _) = f a

  prettyExpression :: Expression -> P.Doc ann
  prettyExpression (ELiteral l) = P.pretty $ show l
  prettyExpression (EPair e1 e2) = P.parens $ located prettyExpression e1 P.<+> P.comma P.<+> located prettyExpression e2
  prettyExpression (EVariable v) = P.pretty v
  prettyExpression (EApplication e1 e2) = P.parens $ located prettyExpression e1 P.<+> located prettyExpression e2
  prettyExpression (EBinary op e1 e2) = P.parens $ located prettyExpression e1 P.<+> P.pretty op P.<+> located prettyExpression e2
  prettyExpression (EAbstraction vs e) = P.pretty "fun" P.<+> P.pretty vs P.<+> P.pretty "in" P.<+> located prettyExpression e
  prettyExpression (EUnary op e) = P.parens $ P.pretty op <> located prettyExpression e
  prettyExpression (ELetIn l e1 e2) = P.pretty "let" P.<+> P.pretty l P.<+> P.pretty "=" P.<+> located prettyExpression e1 P.<+> P.pretty "\nin" P.<+> located prettyExpression e2
  prettyExpression (EIf e1 e2 e3) = P.pretty "if" P.<+> located prettyExpression e1 P.<+> P.pretty "then" P.<+> located prettyExpression e2 P.<+> P.pretty "else" P.<+> located prettyExpression e3
  prettyExpression (ECase e cs) = P.pretty "case" P.<+> located prettyExpression e P.<+> P.pretty "of" P.<+> P.align (P.vsep (map (\(p, e') -> P.pretty "| " <> located prettyPattern p P.<+> P.pretty "=" P.<+> located prettyExpression e') cs))
  prettyExpression (EStructure s vs ds e) = P.pretty "struct" P.<+> P.pretty s P.<+> P.pretty vs P.<+> P.pretty "where" P.<+> P.align (P.vsep (map (\(n, d) -> P.pretty n P.<+> P.pretty "=" P.<+> prettyDeclaration d) ds)) P.<+> P.pretty "in" P.<+> located prettyExpression e

  instance Show Expression where
    show = show . prettyExpression