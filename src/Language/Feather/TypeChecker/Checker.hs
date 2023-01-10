{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Language.Feather.TypeChecker.Checker where
  import Language.Feather.TypeChecker.Monad
    ( fresh, ReaderEnv, Environment, MonadChecker, instantiate, applyTy, applyCons, CheckerState(..) )
  import Language.Feather.TypeChecker.Type
    ( Scheme(..), Type(..), Qualifier(..), Class(..), Instance(Instance) )
  import Language.Feather.TypeChecker.Substitution
    ( Substitution, Types(apply, free) )
  import Language.Feather.TypeChecker.Methods ( compose )
  import Language.Feather.AST.Expression ( Expression(..) )
  import Language.Feather.CST.Literal
    ( Literal(..), Located(..), Position )
  import Language.Feather.CST.Declaration ( Declaration(..) )
  import Language.Feather.CST.Expression ( Pattern(..) )
  import Language.Feather.TypeChecker.Unification ( mgu, mguQual, mguClasses )

  import qualified Data.Map as M
  import qualified Control.Monad.RWS as RWS
  import qualified Control.Monad.Except as E
  import qualified Control.Monad as MO
  import qualified Data.Set as S
  import qualified Data.List as L
  import qualified Data.Bifunctor as B
  import qualified Language.Feather.TypeChecker.Typed as T

  import Language.Feather.TypeChecker.Modules.InstanceResolver
    ( resolveInstances, buildFun, tArrow, buildCall )

  generalize :: ReaderEnv -> Qualifier -> Scheme
  generalize env t = Forall vars t
    where vars = S.toList (S.map read $ free t S.\\ free env)

  checkExpression :: MonadChecker m => Located Expression -> m (T.TypedExpression, Qualifier, Substitution)
  checkExpression (ELiteral literal :>: _) = do
    type' <- checkLiteral literal
    return (T.ELiteral literal, [] :=> type', mempty)
  checkExpression (EPair e1 e2 :>: _) = do
    (e1', cls1 :=> t1, s1) <- checkExpression e1
    (e2', cls2 :=> t2, s2) <- checkExpression e2
    return (T.EPair e1' e2', (cls1 ++ cls2) :=> TApp (TApp (TId ",") t1) t2, s1 `compose` s2)
  checkExpression (EVariable name :>: pos) = do
    (env, cons) <- RWS.ask
    case M.lookup name env of
      Just scheme -> do
        t <- instantiate scheme
        return (T.EVariable name t, t, mempty)
      Nothing -> case M.lookup name cons of
        Just scheme -> do
          t <- instantiate scheme
          return (T.EVariable name t, t, mempty)
        Nothing -> E.throwError ("Unbound variable " ++ name, Nothing, pos)
  checkExpression (EApplication e1 e2 :>: pos) = do
    t <- fresh
    (e1', cls1 :=> t1, s1) <- checkExpression e1
    (e2', cls2 :=> t2, s2) <- checkExpression e2
    s3 <- mgu (apply s2 t1) (t2 `tArrow` t)
    s4 <- mguClasses cls1 cls2
    case compose <$> s3 <*> s4 of
      Left err -> E.throwError (err, Nothing, pos)
      Right s3' -> return (T.EApplication e1' e2', apply s3' ((cls1 ++ cls2) :=> t), s1 `compose` s2 `compose` s3')
  checkExpression (EUnary op e :>: pos) = do
    (e1, t, s) <- checkExpression (EApplication (EVariable op :>: pos) e :>: pos)
    case e1 of
      T.EApplication (T.EVariable _ t') e' -> return (T.EUnary (op T.:@ apply s t') e', t, s) 
      _ -> E.throwError ("Invalid unary operator", Nothing, pos)
  checkExpression (EBinary op e1 e2 :>: pos) = do
    (e, t, s) <- checkExpression (EApplication (EApplication (EVariable op :>: pos) e1 :>: pos) e2 :>: pos)
    case e of
      T.EApplication (T.EApplication (T.EVariable _ t') e1') e2' -> 
        return (T.EBinary (op T.:@ apply s t') e1' e2', t, s) 
      _ -> E.throwError ("Invalid binary operator", Nothing, pos)
  checkExpression (EAbstraction variable' e :>: _) = do
    t <- fresh
    (env, _) <- RWS.ask
    let env'' = M.insert variable' (Forall [] ([] :=> t)) env
    (e', cls :=> t', s) <- RWS.local (`applyTy` env'') $ checkExpression e
    return (T.EAbstraction (variable' T.:@ (apply s $ [] :=> t)) e', apply s $ cls :=> (t `tArrow` t'), s)
  checkExpression (ELetIn variable' e1 e2 :>: _) = do
    (env, c) <- RWS.ask
    t <- fresh
    let env' = M.insert variable' (Forall [] ([] :=> t)) env
    (e1', t1, s1) <- RWS.local (`applyTy` env') $ checkExpression e1
    let env'' = M.insert variable' (generalize (env', c) t1) env'
    (e2', t2, s2) <- RWS.local (`applyTy` env'') $ checkExpression e2
    return (T.ELetIn (variable' T.:@ t1) e1' e2', t2, s1 `compose` s2)
  checkExpression (EIf c t e :>: pos) = do
    (e1, _ :=> t1, s1) <- checkExpression c
    (e2, t2, s2) <- checkExpression t
    (e3, t3, s3) <- checkExpression e
    s4 <- mgu (apply s3 t1) Bool
    case s4 of
      Left err -> E.throwError (err, Nothing, pos)
      Right s4' -> do
        s5 <- mguQual (apply s4' t2) (apply s4' t3)
        case s5 of
          Left err -> E.throwError (err, Nothing, pos)
          Right s5' -> 
            return (
              T.EIf e1 e2 e3,
              apply s5' $ t2, s1 `compose` s2 `compose` s3 `compose` s4' `compose` s5')
  checkExpression (ECase expr cases :>: pos) = do
    (pat', pat_t, s1) <- checkExpression expr

    (sub, res) <- MO.foldM (\(s, acc) (pattern, expr') -> do
      (p, t, s', m) <- checkPattern pattern
      let s2 = s' `compose`  s
      -- (Type, Substitution, Env, A.TypedExpression)
      (e, t', s'') <- RWS.local (`applyTy` m) $ checkExpression expr'
      let s3 = s'' `compose` s2
      return (s3, acc ++ [(apply s3 ([] :=> t), apply s3 t', s3, (apply s3 p, apply s3 e))])) (s1, []) cases

    if null res
      then E.throwError ("No case matches in pattern matching", Nothing, pos)
      else do
        let (_, t, _, _) = head res

        s <- MO.foldM (\acc (tp, te, s, _) -> do
          tmp1 <- mguQual t te
          tmp2 <- mguQual tp pat_t
          let r = compose <$> tmp1 <*> tmp2
              r' = compose <$> r <*> acc
            in return $ compose <$> r' <*> pure s) (Right sub) res
        
        s2 <- MO.foldM (\acc (tp, te, s', _) -> do
          tmp1 <- mguQual t te
          tmp2 <- mguQual tp pat_t
          let r = compose <$> tmp1 <*> tmp2
              r' = compose <$> r <*> acc
            in return $ compose <$> r' <*> pure s') (Right sub) $ reverse res
        let s' = compose <$> s <*> s2

        -- Checking against patterns
        let tys = map (\(x, _, _, _) -> case s of
                    Right s'' -> apply s'' x
                    Left _ -> x) res

        s'' <- MO.foldM (\acc x -> do
          tmp <- patUnify x tys
          return $ compose <$> tmp <*> acc) (Right M.empty) tys

        -- Checking against bodys
        let bodys = map (\(_, x, _, _) -> case s of
                    Right s''' -> apply s''' x
                    Left _ -> x) res

        s''' <- MO.foldM (\acc x -> do
          tmp <- patUnify x bodys
          return $ compose <$> tmp <*> acc) (Right M.empty) bodys

        case compose <$> (compose <$> s'' <*> s''') <*> s' of
          Right s4 -> do
            let patterns' = map (\(_, _, _, (x, y)) -> (apply s4 x, apply s4 y)) res
            
            -- (Maybe Type, Substitution, Env, A.TypedStatement)
            return (T.ECase pat' patterns', apply s4 t, s4)
          Left e -> E.throwError (e, Nothing, pos)
  checkExpression (EStructure _ generics fields expr :>: _) = do
    generics' <- M.fromList <$> mapM ((<$> fresh) . (,)) generics
    let fields' = M.fromList $ map (\(n, e) -> (n, fromDeclaration e generics')) fields
    env <- RWS.ask
    let env' = M.map (generalize env . ([] :=>)) fields'
    RWS.local (`applyCons` env') $ checkExpression expr
  checkExpression (EClass name ty decls next :>: _) = do
    generics' <- M.singleton ty <$> fresh
    let cls = IsIn name (generics' M.! ty)
    let header = TApp (TId name) (generics' M.! ty)

    decls' <- mapM (\(name', args, decl) -> do
      generics'' <- M.fromList <$> mapM ((<$> fresh) . (,)) args
      let decl' = fromDeclaration decl (generics'' `M.union` generics')
      return (name', decl')) decls
    
    let dataType = buildFun header (map snd decls')

    let pattern = foldl (\acc x -> T.PApp acc x) (T.PVariable name header) (map (uncurry T.PVariable) decls')
    let structure = T.EStructure name [generics' M.! ty] [name T.:@ dataType] 

    let functions' = foldl (\acc (name', decl) -> acc . (T.ELetIn (name' T.:@ ([] :=> decl)) (T.EAbstraction ("$s" T.:@ ([] :=> header)) $
          T.ECase (T.EVariable "$s" ([] :=> header)) [
            (pattern, T.EVariable name' ([] :=> decl))
          ]))) structure decls'

    env <- RWS.ask
    let env' = M.fromList (map (B.second ((generalize env) . ([cls] :=>))) decls')
    (expr, t, s) <- RWS.local (`applyTy` env') $ checkExpression next
    return (functions' expr, t, s)
  checkExpression (EInherit sups name decl body next :>: pos) = do
    generics <- M.fromList <$> mapM (\(_, x) -> (x,) <$> fresh) sups
    let superclasses = map (\(x, t) -> IsIn x (generics M.! t)) sups
    let decl' = fromDeclaration decl generics
    let header = TApp (TId name) decl'
    let cls = IsIn name decl'

    (env, _) <- RWS.ask
    (tys, exprs, s) <- L.unzip3 <$> mapM (\(name', expr) -> case M.lookup name' env of
      Just scheme -> do
        ty <- instantiate scheme
        (expr', t, s) <- checkExpression expr
        s1 <- mguQual ty t
        s2 <- mguQual t ty
        case compose <$> s1 <*> s2 of
          Right s' -> do
            let s'' = s' `compose` s
            return (apply s'' t, apply s'' expr', s'')
          Left err -> E.throwError (err, Nothing, pos)
      Nothing -> E.throwError ("Unknown variable " ++ name' ++ " in " ++ name ++ " inheritance", Nothing, pos)) body

    let s' = foldl compose M.empty s
    let exprs' = apply s' exprs
    let name' = name ++ "$" ++ show (apply s' decl')

    RWS.modify $ \state -> 
      state { instances = Instance cls name' (apply s' superclasses) : instances state }
    
    let (_, tys'') = unzip $ map (\case
                  preds' :=> ty -> (preds', ty)) tys

    let dataType = apply s' header
    let ty' = [] :=> buildFun dataType tys''
    let call = buildCall (T.EVariable name ty') (apply s' exprs')
    
    (next', t, s'') <- checkExpression next

    return $ (T.ELetIn (name' T.:@ ([] :=> dataType)) call next', t, s'')

  patUnify :: MonadChecker m => Qualifier -> [Qualifier] -> m (Either String Substitution)
  patUnify x = MO.foldM (\acc y -> do
    s <- mguQual x y
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

  checkPattern :: MonadChecker m => Located Pattern -> m (T.TypedPattern, Type, Substitution, Environment)
  checkPattern (PWildcard :>: _) = do
    t <- fresh
    return (T.PWildcard, t, mempty, mempty)
  checkPattern (PVariable name :>: _) = do
    (_, cons) <- RWS.ask
    case M.lookup name cons of
      Just scheme -> do
        _ :=> t <- instantiate scheme
        return (T.PVariable name t, t, mempty, mempty)
      Nothing -> do
        t <- fresh
        return (T.PVariable name t, t, mempty, M.singleton name (Forall [] ([] :=> t)))
  checkPattern (PLiteral l :>: _) = do
    t <- checkLiteral l
    return (T.PLiteral l, t, mempty, mempty)
  checkPattern (PApp p1 p2 :>: pos) = do
    (e1, t1, s1, env1) <- checkPattern p1
    (e2, t2, s2, env2) <- RWS.local (`applyTy` env1) $ checkPattern p2
    t <- fresh
    s3 <- mgu (apply s2 t1) (t2 `tArrow` t)
    case s3 of
      Left err -> E.throwError (err, Nothing, pos)
      Right s3' -> return (T.PApp e1 e2, apply s3' t, s1 `compose` s2 `compose` s3', env1 `M.union` env2)

  checkLiteral :: MonadChecker m => Literal -> m Type
  checkLiteral (IntLit _) = return Int
  checkLiteral (FloatLit _) = return Float
  checkLiteral (StringLit _) = return (TApp (TId "List") Char)
  checkLiteral (CharLit _) = return Char

  functions :: Environment
  functions = M.fromList [
      ("<", Forall [] $ [] :=> (Int `tArrow` (Int `tArrow` Bool))),
      ("==", Forall [] $ [] :=> (Int `tArrow` (Int `tArrow` Bool))),
      ("!=", Forall [] $ [] :=> (Int `tArrow` (Int `tArrow` Bool))),
      ("<=", Forall [] $ [] :=> (Int `tArrow` (Int `tArrow` Bool))),
      (">=", Forall [] $ [] :=> (Int `tArrow` (Int `tArrow` Bool))),
      ("<", Forall [] $ [] :=> (Int `tArrow` (Int `tArrow` Bool))),
      (">", Forall [] $ [] :=> (Int `tArrow` (Int `tArrow` Bool))),
      ("&&", Forall [] $ [] :=> (Bool `tArrow` (Bool `tArrow` Bool))),
      ("||", Forall [] $ [] :=> (Bool `tArrow` (Bool `tArrow` Bool))),
      ("error", Forall [0] $ [] :=> (TApp (TId "List") Char `tArrow` TVar 0)),
      ("+", Forall [0] $ [] :=> (TVar 0 `tArrow` (TVar 0 `tArrow` TVar 0))),
      ("-", Forall [0] $ [] :=> (TVar 0 `tArrow` (TVar 0 `tArrow` TVar 0))),
      ("*", Forall [0] $ [] :=> (TVar 0 `tArrow` (TVar 0 `tArrow` TVar 0))),
      ("/", Forall [0] $ [] :=> (Float `tArrow` (Float `tArrow` Float)))
    ]

  runChecker :: Located Expression -> Either (String, Maybe String, Position) (T.TypedExpression)
  runChecker (e :>: pos) = E.runExcept $ fst <$> RWS.evalRWST (do
    (e', _, _) <- checkExpression (e :>: pos)
    (e'', _, _) <- resolveInstances pos e'
    return e'') (functions, mempty) (CheckerState 0 mempty)