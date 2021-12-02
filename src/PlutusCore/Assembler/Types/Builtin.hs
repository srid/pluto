{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}


module PlutusCore.Assembler.Types.Builtin (Builtin (..)) where


import           PlutusCore.Assembler.Prelude
import Data.Data (Data)


data Builtin =
    AddInteger
  | SubtractInteger
  | MultiplyInteger
  | DivideInteger
  | QuotientInteger
  | RemainderInteger
  | ModInteger
  | EqualsInteger
  | LessThanInteger
  | LessThanEqualsInteger
  | AppendByteString
  | ConsByteString
  | SliceByteString
  | LengthByteString
  | IndexByteString
  | EqualsByteString
  | LessThanByteString
  | LessThanEqualByteString
  | Sha2_256
  | Sha3_256
  | Blake2b_256
  | VerifySignature
  | AppendString
  | EqualsString
  | EncodeUtf8
  | DecodeUtf8
  | IfThenElse
  | ChooseUnit
  | Trace
  | FstPair
  | SndPair
  | ChooseList
  | MkCons
  | HeadList
  | TailList
  | NullList
  | ChooseData
  | ConstrData
  | MapData
  | ListData
  | IData
  | BData
  | UnConstrData
  | UnMapData
  | UnBData
  | EqualsData
  | MkPairData
  | MkNilData
  | MkNilPairData
  deriving (Eq, Show, Data, Bounded, Enum)
