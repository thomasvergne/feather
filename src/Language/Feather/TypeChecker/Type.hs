module Language.Feather.TypeChecker.Type where
  import Data.List ( intercalate )
  data Type
    = TVar Int
    | Int | Float | Void | Bool | Char
    | TId String
    | TApp Type Type
    deriving (Eq, Ord)
  
  data Class = IsIn String Type
    deriving (Eq, Ord)

  data Qualifier = [Class] :=> Type
    deriving (Eq, Ord)

  instance Show Type where
    show (TVar i) = "t" ++ show i
    show Int = "int"
    show Float = "float"
    show Void = "void"
    show Bool = "bool"
    show Char = "char"
    show (TId s) = s
    show (TApp (TApp (TId "->") a) b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
    show (TApp (TApp (TId ",") a) b) = "(" ++ show a ++ ", " ++ show b ++ ")"
    show (TApp (TId "List") Char) = "string"
    show (TApp (TId "List") a) = "[" ++ show a ++ "]"
    show (TApp a b) = "(" ++ show a ++ " " ++ show b ++ ")"
  
  instance Show Qualifier where
    show (cls :=> ty) = "(" ++ intercalate ", " (map show cls) ++ ") => " ++ show ty

  instance Show Class where
    show (IsIn cls ty) = cls ++ " " ++ show ty

  data Scheme = Forall [Int] Qualifier
    deriving (Eq, Ord, Show)
  
  data Instance = Instance {
    instance' :: Class,
    name :: String,
    constraints :: [Class]
  } deriving (Show, Eq)