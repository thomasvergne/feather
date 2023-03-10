{-# LANGUAGE FlexibleContexts #-}
module Language.Feather.TypeChecker.Unification where
  import Language.Feather.TypeChecker.Substitution ( Substitution, Types(apply, free) )
  import Language.Feather.TypeChecker.Type ( Type(..), Qualifier(..), Class(..) )
  import Language.Feather.TypeChecker.Methods ( compose )
  import Language.Feather.TypeChecker.Monad ( MonadChecker )

  import Control.Monad

  import qualified Data.Either as E
  import qualified Data.Map as M

  variable :: Int -> Type -> Either String Substitution
  variable n t
    | t == TVar n = Right M.empty
    | show n `elem` free t = Left $ "Occurs check failed in " ++ show t ++ " with " ++ show (TVar n)
    | otherwise = Right $ M.singleton n t

  lookupClass :: String -> [Class] -> Maybe Class
  lookupClass name (z@(IsIn cls _):xs) = if name == cls then return z else lookupClass name xs
  lookupClass _ [] = Nothing

  mguQual :: MonadChecker m => Qualifier -> Qualifier -> m (Either String Substitution)
  mguQual (cls1 :=> t1) (cls2 :=> t2) = do
    s1 <- mgu t1 t2
    s2 <- mguClasses cls1 cls2
    return $ compose <$> s2 <*> s1

  mguClass :: MonadChecker m => Class -> Class -> m (Either String Substitution)
  mguClass (IsIn name1 ty1) (IsIn name2 ty2) = do
    s1 <- mgu ty1 ty2
    return $ if name1 == name2 then s1 else Left $ "Cannot unify " ++ show name1 ++ " with " ++ show name2

  mguClasses :: MonadChecker m => [Class] -> [Class] -> m (Either String Substitution)
  mguClasses [] [] = return $ Right M.empty
  mguClasses xs ys = do
    s1 <- foldM (\acc (IsIn cls ty) -> do
      case lookupClass cls ys of
        Just (IsIn _ ty') -> do
          s <- mgu ty ty'
          return $ compose <$> s <*> acc
        Nothing -> return acc
      ) (Right M.empty) xs

    s2 <- foldM (\acc (IsIn cls ty) -> do
      case lookupClass cls xs of
        Just (IsIn _ ty') -> do
          s <- mgu ty ty'
          return $ compose <$> s <*> acc
        Nothing -> return acc
      ) (Right M.empty) ys
    
    return $ compose <$> s1 <*> s2

  mgu :: MonadChecker m => Type -> Type -> m (Either String Substitution)
  mgu (TVar i) t = return $ variable i t
  mgu t (TVar i) = return $ variable i t
  mgu Int Int = return $ Right M.empty
  mgu Bool Bool = return $ Right M.empty
  mgu Float Float = return $ Right M.empty
  mgu Void Void = return $ Right M.empty
  mgu Char Char = return $ Right M.empty
  mgu Float Int = return $ Right M.empty
  mgu Int Float = return $ Right M.empty
  mgu (TApp t1 t2) (TApp t3 t4) = do
    s1 <- mgu t1 t3
    let t2' = E.fromRight t2 $ apply <$> s1 <*> pure t2
    let t4' = E.fromRight t4 $ apply <$> s1 <*> pure t4
    s2 <- mgu t2' t4'
    return $ compose <$> s2 <*> s1
  mgu (TId n) (TId n') = if n == n' 
    then return $ Right M.empty 
    else return $ Left $ "Type mismatch: " ++ show n ++ " and " ++ show n'
  mgu s1 s2 = return $ Left $ "Type " ++ show s1 ++ " mismatches with type " ++ show s2