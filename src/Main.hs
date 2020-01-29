{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

import Data.Text.Lazy.IO as T

import Parser
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
parseFiles [] = return ()
parseFiles (x:xs) = do
    f <- Prelude.readFile x
    case runParser parseFile f of
        Nothing -> Prelude.putStrLn $ x ++ ": Parsing error"
        Just (v, _) -> Prelude.putStrLn ("//File " ++ x ++ "\n" ++ (intercalate "\n" $ fmap show v)) >> parseFiles xs

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
