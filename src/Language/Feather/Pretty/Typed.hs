{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Feather.Pretty.Typed where
  import Language.Feather.TypeChecker.Typed
    ( TypedExpression(..), Annoted(..), TypedPattern(..) )
  import Language.Feather.TypeChecker.Type ( Type, Qualifier )
  import qualified Prettyprinter as P

  prettyExpression :: TypedExpression -> P.Doc ann
  prettyExpression (ELiteral l) = P.pretty $ show l
  prettyExpression (EPair e1 e2) = P.parens $ prettyExpression e1 P.<+> P.comma P.<+> prettyExpression e2
  prettyExpression (EVariable v _) = P.pretty v
  prettyExpression (EApplication e1 e2) = P.parens $ prettyExpression e1 P.<+> prettyExpression e2
  prettyExpression (EBinary op e1 e2) = P.parens $ prettyExpression e1 P.<+> P.pretty op P.<+> prettyExpression e2
  prettyExpression (EAbstraction vs e) = P.pretty "fun" 
    P.<+> P.pretty vs P.<+> P.pretty "in\n" 
    P.<+> prettyExpression e
  prettyExpression (EUnary op e) = P.parens $ P.pretty op <> prettyExpression e
  prettyExpression (ELetIn name e1 e2) = P.pretty "let" 
    P.<+> P.pretty name P.<+> P.pretty "=" P.<+> prettyExpression e1
    P.<+> P.pretty "\nin" P.<+> prettyExpression e2
  prettyExpression (EIf e1 e2 e3) = P.pretty "if" 
    P.<+> prettyExpression e1 
    P.<+> P.pretty "then" P.<+> prettyExpression e2 
    P.<+> P.pretty "else" P.<+> prettyExpression e3
  prettyExpression (ECase e cs) = P.pretty "case" 
    P.<+> prettyExpression e 
    P.<+> P.pretty "of" 
    P.<+> P.align (P.vsep (map (\(p, e') -> P.pretty "| " <> prettyPattern p P.<+> P.pretty "=" P.<+> prettyExpression e') cs))
  prettyExpression (EStructure s vs cs next) = P.pretty "struct" 
    P.<+> P.pretty s 
    P.<+> P.hsep (map showTy vs) <> P.pretty "\n"
    P.<+> P.align (P.vsep (map (\(n :@ xs) -> P.pretty "| " <> P.pretty n P.<+> showTy xs) cs))
    P.<+> P.pretty "\nin" P.<+> prettyExpression next

  prettyPattern :: TypedPattern -> P.Doc ann
  prettyPattern (PVariable v _) = P.pretty v
  prettyPattern PWildcard = P.pretty "_"
  prettyPattern (PApp e1 e2) = P.parens $ prettyPattern e1 P.<+> prettyPattern e2
  prettyPattern (PLiteral l) = P.pretty $ show l

  showTy :: Type -> P.Doc ann
  showTy = P.pretty . show

  instance P.Pretty a => P.Pretty (Annoted String a) where
    pretty (n :@ t) = P.pretty n <> P.pretty ":" P.<+> P.pretty t

  instance P.Pretty Type where
    pretty = showTy

  instance P.Pretty Qualifier where
    pretty = P.pretty . show

  instance Show TypedExpression where
    show = show . prettyExpression