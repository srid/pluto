{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Functions that build and transform the Pluto AST in various ways
module PlutusCore.Assembler.Build
  ( applyToplevelBinding,
    -- * Building from Haskell types
    ToPluto(..),
    FromUPLC(..),
    -- * Other
    var
  )
where

import qualified Data.Text                      as T
import qualified PlutusCore                     as PLC
import           PlutusCore.Assembler.Prelude
import           PlutusCore.Assembler.Types.AST
import qualified UntypedPlutusCore              as UPLC

-- | Return a new program that applies a bound lambda with the given arguments
--
-- The lambda must be bound in a top-level Let binding. When there are no
-- arguments, the bound variable's value is returned as is.
applyToplevelBinding ::
  -- | Variable name of the bound term (usually a lambda)
  Name ->
  -- | Arguments to apply the lambda with
  --
  -- If there are no arguments, the bound term is returned as is. If there are
  -- arguments, the bound term must be a lambda.
  [Term ()] ->
  -- | The program with the let binding.
  --
  -- The Let body of this program will be discarded.
  Program () ->
  -- | The new program that retains the let bindings, but with a new body
  -- containing the expression requested.
  Either Text (Program ())
applyToplevelBinding name args = \case
  Program (Let ann bindings _oldBody) ->
    case getBoundTerm name `mapMaybe` bindings of
      [_boundTerm] -> do
        let newBody = foldl' (Apply ()) (Var () name) args
        pure $ Program $ Let ann bindings newBody
      _ -> throwError $ "expected a binding with name: " <> getName name
  _ ->
    throwError "expected top-level let binding"
  where
    getBoundTerm k (Binding _ k' val) = do
      guard $ k == k'
      pure val

class ToPluto a where
  toPluto :: a -> Term ()

class FromUPLC a where
  fromUPLC :: UPLC.Term name PLC.DefaultUni fun () -> Maybe a

instance ToPluto Text where
  toPluto s =
    Constant () $ T () s

instance ToPluto [Char] where
  toPluto s =
    Constant () $ T () $ T.pack s

instance FromUPLC Text where
  fromUPLC = \case
    (UPLC.Constant () (PLC.Some (PLC.ValueOf PLC.DefaultUniString x))) -> pure x
    _                                                                  -> Nothing

instance FromUPLC String where
  fromUPLC = fmap T.unpack . fromUPLC

var :: Text -> Term ()
var s =
  Var () $ fromString . T.unpack $ s
