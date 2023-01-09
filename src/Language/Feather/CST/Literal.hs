module Language.Feather.CST.Literal where
  import Text.Parsec (SourcePos)
  import Data.String.Color ( bGreen, bYellow )

  type Position = (SourcePos, SourcePos)

  data Located a
    = a :>: Position
    deriving Eq
  
  instance Show a => Show (Located a) where
    show (a :>: _) = show a
  
  instance Functor Located where
    fmap f (a :>: p) = f a :>: p

  data Literal
    = IntLit Integer 
    | FloatLit Double
    | StringLit String
    | CharLit Char
    deriving Eq
  
  instance Show Literal where
    show (IntLit i) = bYellow $ show i
    show (FloatLit f) = bYellow $ show f
    show (StringLit s) = bGreen $ show s
    show (CharLit c) = bGreen $ show c