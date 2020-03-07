{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

import Data.Text.Lazy.IO as T

import AST
import Parser
import LLVMConverter
import TypeInfer
import Control.Applicative
import Data.List
import System.Environment


import LLVM.Pretty  -- from the llvm-hs-pretty package
import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

parseFiles :: [String] -> IO()
parseFiles [] = Prelude.putStrLn "fatal error: No input files"
parseFiles [x] =  do
    f <- Prelude.readFile x
    case runParser parseFile f of
        (Nothing, v) -> Prelude.putStrLn $ x ++ ": Parsing error near '" ++ take 50 v ++ "'"
        (Just v, _) -> displayTypeResult x True v $ inferTypes False [] v
    where
        displayTypeResult :: String -> Bool -> [Expression] -> ([Message], Maybe [Expression]) -> IO ()
        displayTypeResult x True ex (msgs, Just exprs) = do
            Prelude.putStrLn $ "//File " ++ x
            displayMsgs ex
            displayMsgs msgs
            displayMsgs exprs
            displayLLVMResult x $ checkFileExpressionsType exprs
        displayTypeResult x False _ (msgs, Just exprs) = do
            Prelude.putStrLn $ "//File " ++ x
            displayMsgs msgs
            displayLLVMResult x $ checkFileExpressionsType exprs
        displayTypeResult x True ex (msgs, _) = displayMsgs ex >> displayMsgs msgs
        displayTypeResult x False _ (msgs, _) = displayMsgs msgs

        displayLLVMResult :: String -> ([Message], Maybe [Expression]) -> IO ()
        displayLLVMResult x (msgs, Just exprs) = do
            displayMsgs msgs
            T.putStrLn $ ppllvm $ makeASTModule x exprs
        displayLLVMResult x (msgs, Nothing) = displayMsgs msgs

        displayMsgs :: Show a => [a] -> IO ()
        displayMsgs msgs = Prelude.putStrLn $ intercalate "\n" $ fmap show msgs
parseFiles (x:xs) = do
    parseFiles [x] >> parseFiles xs

main :: IO ()
main = do
    args <- getArgs
    parseFiles args
--T.putStrLn $ ppllvm $ buildModule "exampleModule" $ mdo
--
--  function "add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> mdo
--
--    entry <- block `named` "entry"; do
--      c <- add a b
--      ret c
