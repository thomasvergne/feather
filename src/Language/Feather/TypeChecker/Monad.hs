{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Feather.TypeChecker.Monad where
  import Control.Monad.RWS ( MonadState(put, get), MonadRWS )
  import Control.Monad.Except ( MonadError )
  import Data.Map ( Map, fromList, union )
  import Language.Feather.CST.Literal ( Position )
  import Language.Feather.TypeChecker.Type ( Scheme(..), Type(TVar) )
  import Language.Feather.TypeChecker.Substitution ( Types(apply) )
  import Language.Feather.TypeChecker.Methods ()

  type Environment = Map String Scheme
  type ReaderEnv   = (Environment, Environment)

  type MonadChecker m = (MonadRWS ReaderEnv () Int m, MonadError (String, Maybe String, Position) m)

  applyTy :: ReaderEnv -> Environment -> ReaderEnv
  applyTy (env, env') s = (env `union` s, env')

  applyCons :: ReaderEnv -> Environment -> ReaderEnv
  applyCons (env, env') s = (env, env' `union` s)

  fresh :: MonadChecker m => m Type
  fresh = get >>= \n -> put (n + 1) >> return (TVar n)

  instantiate :: MonadChecker m => Scheme -> m Type
  instantiate (Forall vars t) = do
    vars' <- mapM (const fresh) vars
    let s = fromList $ zip vars vars'
      in return $ apply s t  