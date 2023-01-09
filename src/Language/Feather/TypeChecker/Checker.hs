{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Language.Feather.TypeChecker.Checker where
  import Language.Feather.TypeChecker.Monad
    ( fresh, ReaderEnv, Environment, MonadChecker, instantiate, applyTy, applyCons )
  import Language.Feather.TypeChecker.Type
    ( Scheme(..), Type(..) )
  import Language.Feather.TypeChecker.Substitution
    ( Substitution, Types(apply, free) )
  import Language.Feather.TypeChecker.Methods ( compose )
  import Language.Feather.AST.Expression ( Expression(..) )
  import Language.Feather.CST.Literal
    ( Literal(..), Located(..), Position )
  import Language.Feather.CST.Declaration ( Declaration(..) )
  import Language.Feather.CST.Expression ( Pattern(..) )
  import Language.Feather.TypeChecker.Unification ( mgu )

  import qualified Data.Map as M
  import qualified Control.Monad.RWS as RWS
  import qualified Control.Monad.Except as E
  import qualified Control.Monad as MO
  import qualified Data.Set as S
  import Debug.Trace

  generalize :: ReaderEnv -> Type -> Scheme
  generalize env t = Forall vars t
    where vars = S.toList (S.map read $ free t S.\\ free env)

  tArrow :: Type -> Type -> Type
  tArrow = TApp . TApp (TId "->")

  checkExpression :: MonadChecker m => Located Expression -> m (Type, Substitution)
  checkExpression (ELiteral literal :>: _) = do
    type' <- checkLiteral literal
    return (type', mempty)
  checkExpression (EPair e1 e2 :>: _) = do
    (t1, s1) <- checkExpression e1
    (t2, s2) <- checkExpression e2
    return (TApp (TApp (TId ",") t1) t2, s1 `compose` s2)
  checkExpression (EVariable name :>: pos) = do
    (env, cons) <- RWS.ask
    case M.lookup name env of
      Just scheme -> do
        t <- instantiate scheme
        return (t, mempty)
      Nothing -> case M.lookup name cons of
        Just scheme -> do
          t <- instantiate scheme
          return (t, mempty)
        Nothing -> E.throwError ("Unbound variable " ++ name, Nothing, pos)
  checkExpression (EApplication e1 e2 :>: pos) = do
    t <- fresh
    (t1, s1) <- checkExpression e1
    (t2, s2) <- checkExpression e2
    s3 <- mgu (apply s2 t1) (t2 `tArrow` t)
    case s3 of
      Left err -> E.throwError (err, Nothing, pos)
      Right s3' -> return (apply s3' t, s1 `compose` s2 `compose` s3')
  checkExpression (EUnary op e :>: pos) = do
    (t, s) <- checkExpression (EApplication (EVariable op :>: pos) e :>: pos)
    return (t, s)
  checkExpression (EBinary op e1 e2 :>: pos) = do
    (t, s) <- checkExpression (EApplication (EApplication (EVariable op :>: pos) e1 :>: pos) e2 :>: pos)
    return (t, s)
  checkExpression (EAbstraction variable' e :>: _) = do
    t <- fresh
    (env, _) <- RWS.ask
    let env'' = M.insert variable' (Forall [] t) env
    (t', s) <- RWS.local (`applyTy` env'') $ checkExpression e
    return (apply s $ t `tArrow` t', s)
  checkExpression (ELetIn variable' e1 e2 :>: _) = do
    (env, c) <- RWS.ask
    t <- fresh
    let env' = M.insert variable' (Forall [] t) env
    (t1, s1) <- RWS.local (`applyTy` env') $ checkExpression e1
    traceShowM (variable', t1)
    let env'' = M.insert variable' (generalize (env', c) t1) env'
    (t2, s2) <- RWS.local (`applyTy` env'') $ checkExpression e2
    return (t2, s1 `compose` s2)
  checkExpression (EIf c t e :>: pos) = do
    (t1, s1) <- checkExpression c
    (t2, s2) <- checkExpression t
    (t3, s3) <- checkExpression e
    s4 <- mgu (apply s3 t1) Bool
    case s4 of
      Left err -> E.throwError (err, Nothing, pos)
      Right s4' -> do
        s5 <- mgu (apply s4' t2) (apply s4' t3)
        case s5 of
          Left err -> E.throwError (err, Nothing, pos)
          Right s5' -> return (apply s5' t2, s1 `compose` s2 `compose` s3 `compose` s4' `compose` s5')
  checkExpression (ECase expr cases :>: pos) = do
    (pat_t, s1) <- checkExpression expr

    (sub, res) <- MO.foldM (\(s, acc) (pattern, expr') -> do
      (t, s', m) <- checkPattern pattern
      let s2 = s' `compose`  s
      -- (Type, Substitution, Env, A.TypedExpression)
      (t', s'') <- RWS.local (`applyTy` m) $ checkExpression expr'
      let s3 = s'' `compose` s2
      return (s3, acc ++ [(apply s3 t, apply s3 t', s3)])) (s1, []) cases

    if null res
      then E.throwError ("No case matches in pattern matching", Nothing, pos)
      else do
        let (_, t, _) = head res
        s <- MO.foldM (\acc (tp, te, s) -> do
          tmp1 <- mgu t te
          tmp2 <- mgu tp pat_t
          let r = compose <$> tmp1 <*> tmp2
              r' = compose <$> r <*> acc
            in return $ compose <$> r' <*> pure s) (Right sub) res
        
        s2 <- MO.foldM (\acc (tp, te, s') -> do
          tmp1 <- mgu t te
          tmp2 <- mgu tp pat_t
          let r = compose <$> tmp1 <*> tmp2
              r' = compose <$> r <*> acc
            in return $ compose <$> r' <*> pure s') (Right sub) $ reverse res
        let s' = compose <$> s <*> s2

        -- Checking against patterns
        let tys = map (\(x, _, _) -> case s of
                    Right s'' -> apply s'' x
                    Left _ -> x) res

        s'' <- MO.foldM (\acc x -> do
          tmp <- patUnify x tys
          return $ compose <$> tmp <*> acc) (s') tys

        -- Checking against bodys
        let bodys = map (\(_, x, _) -> case s of
                    Right s''' -> apply s''' x
                    Left _ -> x) res

        s''' <- MO.foldM (\acc x -> do
          tmp <- patUnify x bodys
          return $ compose <$> tmp <*> acc) (s') bodys

        case compose <$> (compose <$> s'' <*> s''') <*> s' of
          Right s4 -> do
            return (apply s4 t, s4)
          Left e -> E.throwError (e, Nothing, pos)
  checkExpression (EStructure _ generics fields expr :>: _) = do
    generics' <- M.fromList <$> MO.zipWithM (\_ n -> (n,) <$> fresh) [0..] generics
    let fields' = M.fromList $ map (\(n, e) -> (n, fromDeclaration e generics')) fields
    env <- RWS.ask
    let env' = M.map (generalize env) fields'
    RWS.local (`applyCons` env') $ checkExpression expr
    
  patUnify :: MonadChecker m => Type -> [Type] -> m (Either String Substitution)
  patUnify x = MO.foldM (\acc y -> do
    s <- mgu x y
    return $ compose <$> s <*> acc) (Right M.empty)

  unpackType :: Type -> [Type]
  unpackType (TApp (TApp (TId "->") a) b) = b : unpackType a
  unpackType t = [t]

  fromDeclaration :: Declaration -> M.Map String Type -> Type
  fromDeclaration DVoid _ = Void
  fromDeclaration DInt _ = Int
  fromDeclaration DBool _ = Bool
  fromDeclaration DChar _ = Char
  fromDeclaration DString _ = TApp (TId "List") Char
  fromDeclaration DFloat _ = Float
  fromDeclaration (DId n) _ = TId n
  fromDeclaration (DApp d1 d2) generics = TApp (fromDeclaration d1 generics) (fromDeclaration d2 generics)
  fromDeclaration (DGeneric id') generics = case M.lookup id' generics of
    Just t -> t
    Nothing -> error "Unbound generic"

  checkCases :: MonadChecker m => Type -> (Type, Substitution) -> [(Located Pattern, Located Expression)] -> m (Type, Substitution) 
  checkCases _ ty [] = return ty
  checkCases tyExpr (tyCurrent, s) ((pt@(_ :>: (start, _)), e@(_ :>: (_, end))):cases) = do
    (tyPattern, s1, environment) <- checkPattern pt
    s2 <- mgu tyPattern tyExpr
    (ty2, s3) <- RWS.local (`applyTy` environment) $ checkExpression e
    s4 <- mgu tyCurrent ty2
    let s5 = compose s1 <$> (compose <$> s2 <*> (compose s3 <$> (compose <$> s4 <*> pure s)))
    case s5 of
      Left err -> E.throwError (err, Nothing, (start, end))
      Right s5' -> checkCases tyExpr (ty2, s5') cases

  checkPattern :: MonadChecker m => Located Pattern -> m (Type, Substitution, Environment)
  checkPattern (PWildcard :>: _) = do
    t <- fresh
    return (t, mempty, mempty)
  checkPattern (PVariable name :>: _) = do
    (_, cons) <- RWS.ask
    case M.lookup name cons of
      Just scheme -> do
        t <- instantiate scheme
        return (t, mempty, mempty)
      Nothing -> do
        t <- fresh
        return (t, mempty, M.singleton name (Forall [] t))
  checkPattern (PLiteral l :>: _) = do
    t <- checkLiteral l
    return (t, mempty, mempty)
  checkPattern (PApp p1 p2 :>: pos) = do
    (t1, s1, env1) <- checkPattern p1
    (t2, s2, env2) <- RWS.local (`applyTy` env1) $ checkPattern p2
    t <- fresh
    s3 <- mgu (apply s2 t1) (t2 `tArrow` t)
    case s3 of
      Left err -> E.throwError (err, Nothing, pos)
      Right s3' -> return (apply s3' t, s1 `compose` s2 `compose` s3', env1 `M.union` env2)

  checkLiteral :: MonadChecker m => Literal -> m Type
  checkLiteral (IntLit _) = return Int
  checkLiteral (FloatLit _) = return Float
  checkLiteral (StringLit _) = return (TApp (TId "List") Char)
  checkLiteral (CharLit _) = return Char

  functions :: Environment
  functions = M.fromList [
      ("<", Forall [] (Int `tArrow` (Int `tArrow` Bool))),
      ("==", Forall [] (Int `tArrow` (Int `tArrow` Bool))),
      ("!=", Forall [] (Int `tArrow` (Int `tArrow` Bool))),
      ("<=", Forall [] (Int `tArrow` (Int `tArrow` Bool))),
      (">=", Forall [] (Int `tArrow` (Int `tArrow` Bool))),
      ("<", Forall [] (Int `tArrow` (Int `tArrow` Bool))),
      (">", Forall [] (Int `tArrow` (Int `tArrow` Bool))),
      ("&&", Forall [] (Bool `tArrow` (Bool `tArrow` Bool))),
      ("||", Forall [] (Bool `tArrow` (Bool `tArrow` Bool))),
      ("error", Forall [0] (TApp (TId "List") Char `tArrow` TVar 0)),
      ("+", Forall [0] $ TVar 0 `tArrow` (TVar 0 `tArrow` TVar 0)),
      ("-", Forall [0] $ TVar 0 `tArrow` (TVar 0 `tArrow` TVar 0)),
      ("*", Forall [0] $ TVar 0 `tArrow` (TVar 0 `tArrow` TVar 0)),
      ("/", Forall [0] $ Float `tArrow` (Float `tArrow` Float))
    ]

  runChecker :: Located Expression -> Either (String, Maybe String, Position) (Type, Substitution)
  runChecker e = E.runExcept $ fst <$> RWS.evalRWST (checkExpression e) (functions, mempty) 0