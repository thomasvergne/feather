{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Feather.TypeChecker.Methods where
  import Language.Feather.TypeChecker.Substitution ( Substitution, Types(..) )
  import Language.Feather.TypeChecker.Type ( Scheme(..), Type(..), Qualifier(..), Class(..) )
  import Language.Feather.CST.Literal ( Located(..) )
  import Language.Feather.TypeChecker.Typed ( TypedExpression(..), TypedPattern(..), Annoted(..) )

  import qualified Data.Map as M
  import qualified Data.Set as S

  compose :: Substitution -> Substitution -> Substitution
  compose s1 s2 = M.map (apply s1) s2 `M.union` s1

  instance Types Type where
    free (TVar i) = S.singleton $ show i 
    free Int = S.empty
    free (TApp n xs) = free n `S.union` free xs
    free _ = S.empty

    apply s (TVar i) = case M.lookup i s of
      Just t -> t
      Nothing -> TVar i
    apply s (TApp n xs) = TApp (apply s n) $ apply s xs
    apply _ Int = Int
    apply _ Bool = Bool
    apply _ Float = Float
    apply _ Void = Void
    apply _ (TId s) = TId s
    apply _ Char = Char

  instance Types a => Types [a] where
    free = foldr (S.union . free) S.empty
    apply s = map (apply s)
  
  instance Types b => Types (M.Map a b) where
    free = free . M.elems
    apply = M.map . apply

  instance Types Scheme where
    free (Forall v t) = free t S.\\ S.fromList (map show v)
    apply s (Forall v t) = Forall v (apply (foldr M.delete s v) t)
    
  instance Types Qualifier where
    free (cls :=> ty) = free cls `S.union` free ty
    apply s (cls :=> ty) = apply s cls :=> apply s ty

  instance Types Class where
    free (IsIn _ ty) = free ty
    apply s (IsIn cls ty) = IsIn cls $ apply s ty

  instance Types a => Types (Maybe a) where
    free = maybe S.empty free
    apply s = fmap (apply s)
    
  unLoc :: Located a -> a
  unLoc (x :>: _) = x

  instance Types a => Types (Located a) where
    free = free . unLoc
    apply s (x :>: l) = apply s x :>: l

  instance (Types a, Types b) => Types (a, b) where
    free (a, b) = free a `S.union` free b
    apply s (a, b) = (apply s a, apply s b)

  instance Types TypedExpression where
    free _ = undefined

    apply s (EPair e1 e2) = EPair (apply s e1) (apply s e2)
    apply s (EVariable name t) = EVariable name (apply s t)
    apply s (EApplication e1 e2) = EApplication (apply s e1) (apply s e2)
    apply s (EUnary op e) = EUnary op (apply s e)
    apply s (EBinary op e1 e2) = EBinary op (apply s e1) (apply s e2)
    apply s (EAbstraction (name :@ ty) e) = EAbstraction (name :@ apply s ty) (apply s e)
    apply s (ELetIn name e1 e2) = ELetIn name (apply s e1) (apply s e2)
    apply s (EIf e1 e2 e3) = EIf (apply s e1) (apply s e2) (apply s e3)
    apply s (ECase e cases) = ECase (apply s e) (map (apply s) cases)
    apply s (EStructure name tys fields next) = EStructure name (apply s tys) (apply s fields) (apply s next)
    apply _ x = x

  instance Types TypedPattern where
    free _ = undefined
    apply s (PVariable name ty) = PVariable name (apply s ty)
    apply s (PApp e1 e2) = PApp (apply s e1) (apply s e2)
    apply _ x = x

  instance Types a => Types (Annoted b a) where
    free (_ :@ a) = free a
    apply s (name :@ a) = name :@ apply s a