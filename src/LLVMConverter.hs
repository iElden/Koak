{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module LLVMConverter (makeASTModule) where

import AST
import Data.Text.Lazy.IO as T

import LLVM.Pretty  -- from the llvm-hs-pretty package
import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Operand as O
import qualified LLVM.AST.IntegerPredicate as I

import Data.String
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

import qualified LLVM.IRBuilder.Constant as CB
import qualified LLVM.Context as Ctx
import qualified LLVM.Module as Module
import qualified LLVM.Target as Target

convertValue :: MonadModuleBuilder m => Value -> IRBuilderT m Operand
convertValue (Nbr n) = CB.int32 $ fromIntegral n
convertValue (RealNbr n) = CB.double n

convertExpression :: MonadModuleBuilder m => Expression -> IRBuilderT m Operand
convertExpression (Un (Unary [] val)) = convertValue val

makeASTModule :: String -> [Expression] -> Module
makeASTModule name [] = buildModule (fromString name) $ do
  function "main" [] i32 $ \_ -> do
    value <- CB.int32 $ 0
    ret $ value
makeASTModule name exprs = buildModule (fromString name) $ do
  function "main" [] i32 $ \_ -> do
    operands <- traverse convertExpression exprs
    ret $ last operands