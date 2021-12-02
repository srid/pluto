{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module PlutusCore.Assembler.FFI (plutoProgram, plutoImport, processResult) where 

import Language.Haskell.TH
import PlutusCore.Assembler.Prelude
import System.IO (FilePath, putStrLn)
import Debug.Trace (traceShowId)
import Prelude (error, readFile, MonadFail (fail))
import qualified PlutusCore.Assembler.Assemble as Assemble
import qualified Data.Text as T
import PlutusCore.Assembler.App
import qualified PlutusCore.Assembler.Types.AST as AST
import Language.Haskell.TH.Syntax (Lift (lift), dataToExpQ)
import Data.Data (Data, cast)
import qualified PlutusCore.Assembler.Evaluate as E
import qualified PlutusCore.Assembler.Build as B
import qualified UntypedPlutusCore as UPLC
import qualified PlutusCore as PLC

plutoProgram :: FilePath -> Q Exp
plutoProgram fp = do
  plutoProg <- loadPlutoMod fp 
  liftDataWithText plutoProg
-- TODO: 
-- - [ ] Parse decl AST
-- - [ ] Generate eval body
plutoImport :: Name -> String -> Q Type -> Q [Dec]
plutoImport prog name qType = do 
  -- define the function
  type_ <- qType
  args <- lambdaArgsFromType type_
  liftIO $ putStrLn "plutoImport: doing stuff"
  let sig = SigD (mkName $ traceShowId name) (AppT (AppT ArrowT (ConT ''String)) (AppT (AppT ArrowT (ConT ''String)) (ConT ''String)))
      body = 
        let 
          bFn = AppE (ConE 'AST.Name) (LitE (StringL name))
          bArgs = ListE $ (\arg -> VarE 'B.toPluto `AppE` VarE arg) <$> args
          bProg = VarE prog
          bCall = AppE (VarE 'E.evalToplevelBinding) bFn `AppE` bArgs `AppE` bProg
          bRes = (VarE 'processResult) `AppE` bCall
        in bRes
      fun = FunD (mkName name) [Clause (VarP <$> args) (NormalB body) []]
  -- pure [psig, pfun, sig, fun]
  pure [sig, fun]

lambdaArgsFromType :: Type -> Q [Name]
lambdaArgsFromType = \case 
  ConT _ -> do
    pure []
  AppT (AppT ArrowT (ConT _)) rest ->  do
    n <- newName "_arg"
    (n :) <$> lambdaArgsFromType rest
  _ ->
    error "clausePatsFor: not a valid type"


loadPlutoMod :: (MonadIO m, MonadFail m) => FilePath -> m (AST.Program ())
loadPlutoMod fp = do
  s <- liftIO $ readFile fp
  either (fail . show . ErrorParsing) (pure . void) $ Assemble.parseProgram "<TH>" (T.pack s)

processResult :: forall a. B.FromUPLC a => Either Error (UPLC.Term UPLC.Name PLC.DefaultUni PLC.DefaultFun ()) -> a
processResult = \case
  Left err -> error $ show err
  Right t -> case B.fromUPLC @a t
    of Nothing -> error "processResult: failed to convert term"
       Just x -> x

-- Why? See https://gitlab.haskell.org/ghc/ghc/-/issues/12596#note_169275

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ (\a -> liftText <$> cast a)