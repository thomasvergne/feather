{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Language.Feather.TypeChecker.Modules.InstanceResolver where
  import Language.Feather.TypeChecker.Monad
    ( CheckerState(instances), MonadChecker )
  import Language.Feather.TypeChecker.Type
    ( Class(..),
      Qualifier(..),
      Type(TApp, TId, TVar),
      Instance(Instance) )
  import Language.Feather.TypeChecker.Typed
    ( Annoted(..), TypedExpression(..) )
  import Language.Feather.TypeChecker.Unification ( mguClass )
  import Language.Feather.TypeChecker.Substitution ( Types(apply) )
  import Language.Feather.CST.Literal ( Position )
  import Data.Either ( fromRight, isRight )
  import Language.Feather.Pretty.Typed ()
  import Control.Monad.Except ( MonadError(throwError) )

  import qualified Data.List as L
  import qualified Control.Monad.RWS as RWS
  import qualified Data.Bifunctor as B
  import qualified Data.Map as M

  import Debug.Trace

  appify :: Class -> Type
  appify (IsIn name ty) = TApp (TId name) ty
  
  buildFun :: Type -> [Type] -> Type
  buildFun e (x:xs) = x `tArrow` buildFun e xs
  buildFun x _ = x

  tArrow :: Type -> Type -> Type
  tArrow = TApp . TApp (TId "->")

  buildCall :: TypedExpression -> [TypedExpression] -> TypedExpression
  buildCall e (x:xs) = EApplication (buildCall e xs) x
  buildCall e _ = e

  buildLambda :: [Annoted String Qualifier] -> TypedExpression -> TypedExpression
  buildLambda (n:xs) e = EAbstraction n (buildLambda xs e)
  buildLambda _ e = e

  trimap :: (a -> b) -> (c -> d) -> (e -> f) -> (a, c, e) -> (b, d, f)
  trimap f g h (a, c, e) = (f a, g c, h e)
  
  isDirectTVar :: Type -> Bool
  isDirectTVar (TVar _) = True
  isDirectTVar _ = False

  containsTVar' :: Type -> Bool
  containsTVar' (TVar _) = True
  containsTVar' (TApp t1 t2) = containsTVar' t1 || containsTVar' t2
  containsTVar' _ = False

  findInstance :: MonadChecker m => Position -> [Class] -> m ([TypedExpression], [(String, Type)], [Class])
  findInstance a subCls = do
    env <- RWS.gets instances
    trimap concat concat concat . unzip3 <$> mapM
      (\x@(IsIn _ ty) -> if isDirectTVar ty then
          return (map (\z@(IsIn cls tys) ->
            EVariable (cls ++ show tys) ([] :=> (appify z))) subCls,
            map (\z@(IsIn cls tys) ->
              (cls ++ show tys, appify z)) subCls, [x])
        else do
          res <- map (B.first $ fromRight M.empty) . filter (isRight . fst) <$> mapM (\z@(Instance cls' _ _) -> (,z) <$> mguClass cls' x) env
          case res of
            -- ff a superclass instance exists
            [(s, Instance z@(IsIn _ _) name subCls')] -> do
              -- finding the subinstances of a class
              (subVar, subTC, cls) <- findInstance a (apply s subCls')
              let var = EVariable name ([] :=> apply s (appify z))
                in return ([if null subVar
                  then var
                  else buildCall var subVar], subTC, cls)

            -- if instance contains generics, then it's must be resolved into
            -- a arguments map
            xs -> if containsTVar' (appify x)
              then return (map (\z@(IsIn cls tys) ->
                EVariable (cls ++ show tys) ([] :=> appify z)) subCls,
                map (\z@(IsIn cls tys) ->
                  (cls ++ show tys, appify z)) subCls, [x])
              else do
                -- if instance does not contain generics, then it must be an error
                if null xs
                  then throwError ("No instance found for " ++ show x, Nothing, a)
                  else throwError ("Instances " ++ L.intercalate ", " (map (\(_, Instance cls' _ _) -> show cls') xs) ++ " overlaps for " ++ show x, Nothing, a)) subCls

  resolveInstances :: MonadChecker m => Position -> TypedExpression -> m (TypedExpression, [(String, Type)], [Class])
  resolveInstances pos (EVariable n t@(cls :=> ty)) = do
    if not $ null cls
      then do
        (calls, tcs, preds) <- findInstance pos cls
        traceShowM (n, t)
        return (
          if not (null calls) 
            then buildCall (EVariable n ([] :=> buildFun ty (map appify cls))) calls 
            else EVariable n t,
          L.nub tcs,
          L.nub preds)
      else return (EVariable n t, [], [])
  resolveInstances pos (EApplication e1 e2) = do
    (e1', tcs, cls) <- resolveInstances pos e1
    (e2', tcs', cls') <- resolveInstances pos e2
    return (EApplication e1' e2', L.nub $ tcs ++ tcs', L.nub $ cls ++ cls')
  resolveInstances pos (EPair e1 e2) = do
    (e1', tcs, cls) <- resolveInstances pos e1
    (e2', tcs', cls') <- resolveInstances pos e2
    return (EPair e1' e2', L.nub $ tcs ++ tcs', L.nub $ cls ++ cls')
  resolveInstances pos (EUnary (op :@ t) e) = do
    resolveInstances pos (EApplication (EVariable op t) e)
  resolveInstances pos (EBinary (op :@ t) e1 e2) = do
    resolveInstances pos (EApplication (EApplication (EVariable op t) e2) e1)
  resolveInstances pos (EAbstraction n e) = do
    (e', tcs, cls) <- resolveInstances pos e
    return (EAbstraction n e', tcs, cls)
  resolveInstances pos (ELetIn (x :@ (_ :=> ty)) e b) = do
    (e', tcs, cls) <- resolveInstances pos e
    let t1 = if null cls then ty else buildFun ty (map appify (L.nub cls))
    (b', tcs', cls') <- resolveInstances pos b
    -- traceShowM (x, tcs, cls, e')
    let args = map (\(n, t) -> n :@ ([] :=> t)) $ L.nub tcs
    return (ELetIn (x :@ ([] :=> t1)) (if null tcs then e' else buildLambda args e') b', L.nub tcs', L.nub cls')
  resolveInstances pos (EIf c t e) = do
    (c', tcs, cls) <- resolveInstances pos c
    (t', tcs', cls') <- resolveInstances pos t
    (e', tcs'', cls'') <- resolveInstances pos e
    return (EIf c' t' e', L.nub $ tcs ++ tcs' ++ tcs'', L.nub $ cls ++ cls' ++ cls'')
  resolveInstances pos (ECase e alts) = do
    (e', tcs, cls) <- resolveInstances pos e
    (alts', tcs', cls') <- unzip3 <$> mapM (\(p, e'') -> do
      (e''', tcs', cls') <- resolveInstances pos e''
      return ((p, e'''), tcs', cls')) alts
    return (ECase e' alts', L.nub $ tcs ++ concat tcs', L.nub $ cls ++ concat cls')
  resolveInstances pos (EStructure n ts ns e) = do
    (e', tcs, cls) <- resolveInstances pos e
    return (EStructure n ts ns e', tcs, cls)
  resolveInstances _ (ELiteral l) = return (ELiteral l, [], [])