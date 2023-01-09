{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Feather.TypeChecker.Methods where
  import Language.Feather.TypeChecker.Substitution ( Substitution, Types(..) )
  import Language.Feather.TypeChecker.Type ( Scheme(..), Type(..) )
  import Language.Feather.CST.Literal ( Located(..) )

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
