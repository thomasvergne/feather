module Language.Feather.TypeChecker.Type where
  data Type
    = TVar Int
    | Int | Float | Void | Bool | Char
    | TId String
    | TApp Type Type
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

  data Scheme = Forall [Int] Type
    deriving (Eq, Ord, Show)