module Language.Feather.CST.Declaration where
  data Declaration
    = DChar | DString | DInt | DFloat | DBool | DVoid
    | DId String | DGeneric String
    | DApp Declaration Declaration
    deriving (Eq, Show)