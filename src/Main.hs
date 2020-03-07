{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

import Data.Text.Lazy.IO as T

import AST
import Parser
import LLVMConverter
import TypeInfer
import Data.List
import System.Exit
import System.Environment


import LLVM.Pretty  -- from the llvm-hs-pretty package
import LLVM.AST hiding (function)
import qualified LLVM.Context as Ctx
import qualified LLVM.Module as Module
import qualified LLVM.Target as Target

type DebugMode = Bool
type GenerateObject = Bool
type GenerateLLVMScript = Bool

parseArgs :: [String] -> Either ([FilePath], DebugMode, GenerateLLVMScript, GenerateObject) Message
parseArgs = parseArguments ([], False, False, False)
    where
        parseArguments :: ([FilePath], DebugMode, GenerateLLVMScript, GenerateObject) -> [String] -> Either ([FilePath], DebugMode, GenerateLLVMScript, GenerateObject) Message
        parseArguments args [] = Left args
        parseArguments (files, False, genLL, genO) ("-d":xs) = parseArguments (files, True, genLL, genO) xs
        parseArguments (files, False, genLL, genO) ("--debug":xs) = parseArguments (files, True, genLL, genO) xs

        parseArguments (files, True, genLL, genO) ("-d":xs) = Right $ Error "Debug mode is already enabled"
        parseArguments (files, True, genLL, genO) ("--debug":xs) = Right $ Error "Debug mode is already enabled"

        parseArguments (files, debug, False, genO) ("-l":xs) = parseArguments (files, debug, True, genO) xs
        parseArguments (files, debug, False, genO) ("--ll":xs) = parseArguments (files, debug, True, genO) xs

        parseArguments (files, debug, True, genO) ("-l":xs) = Right $ Error "LLVM script generation is already enabled"
        parseArguments (files, debug, True, genO) ("--ll":xs) = Right $ Error "LLVM script generation is already enabled"

        parseArguments (files, debug, genLL, False) ("-o":xs) = parseArguments (files, debug, genLL, True) xs
        parseArguments (files, debug, genLL, False) ("--object":xs) = parseArguments (files, debug, genLL, True) xs

        parseArguments (files, debug, genLL, True) ("-o":xs) = Right $ Error "Object generation is already enabled"
        parseArguments (files, debug, genLL, True) ("--object":xs) = Right $ Error "Object generation is already enabled"

        parseArguments (files, debug, genLL, genO) ("--":others) = Left (files ++ others, debug, genLL, genO)

        parseArguments (_, _, _, _) (opt@('-':_):_) = Right $ Error $ "Unrecognized option " ++ opt
        parseArguments (files, debug, genLL, genO) (file:xs) = parseArguments (files ++ [file], debug, genLL, genO) xs


compileFile :: DebugMode -> GenerateLLVMScript -> GenerateObject -> FilePath -> IO ()
compileFile debug genLL genO path =  do
    f <- Prelude.readFile path
    case runParser parseFile f of
        (Nothing, v) -> Prelude.putStrLn $ path ++ ": Parsing error near '" ++ take 50 v ++ "'"
        (Just v, _) -> displayTypeResult path debug genLL genO v $ inferTypes False [] v
    where
        displayTypeResult :: FilePath -> DebugMode -> GenerateLLVMScript -> GenerateObject -> [Expression] -> ([Message], Maybe [Expression]) -> IO ()
        displayTypeResult path True genLL genO ex (msgs, Just exprs) = do
            Prelude.putStrLn $ "//File " ++ path
            displayMsgs ex
            displayMsgs msgs
            displayMsgs exprs
            displayLLVMResult path genLL genO $ checkFileExpressionsType exprs
        displayTypeResult path False genLL genO _ ([], Just exprs) = displayLLVMResult path genLL genO $ checkFileExpressionsType exprs
        displayTypeResult path False genLL genO _ (msgs, Just exprs) = do
            Prelude.putStrLn $ "In file " ++ path
            displayMsgs msgs
            displayLLVMResult path genLL genO $ checkFileExpressionsType exprs
        displayTypeResult path True _ _ ex (msgs, _) = displayMsgs ex >> displayMsgs msgs
        displayTypeResult path False _ _ _ (msgs, _) = displayMsgs msgs

        displayLLVMResult :: FilePath -> GenerateLLVMScript -> GenerateObject-> ([Message], Maybe [Expression]) -> IO ()
        displayLLVMResult path False False (msgs, Just exprs) = do
            displayMsgs msgs
            T.putStrLn $ ppllvm $ makeASTModule path exprs
        displayLLVMResult path genLL genO (msgs, Just exprs) = do
            displayMsgs msgs
            let llvmAst = makeASTModule path exprs
            generateObjectFile (path ++ ".o")  genO  llvmAst
            generateLLVMScript (path ++ ".ll") genLL llvmAst
        displayLLVMResult path _ _ (msgs, Nothing) = displayMsgs msgs

        generateLLVMScript :: FilePath-> Bool -> Module -> IO ()
        generateLLVMScript _ False _ = return ()
        generateLLVMScript path True llvmModule = T.writeFile path $ ppllvm llvmModule

        generateObjectFile :: FilePath -> Bool -> Module -> IO ()
        generateObjectFile _ False _ = return ()
        generateObjectFile path True llvmModule = Ctx.withContext $ \ctx -> do
            Module.withModuleFromAST ctx llvmModule $ \llvmMod -> do
                Target.withHostTargetMachine $ \target -> do
                    Module.writeObjectToFile target (Module.File path) llvmMod

        displayMsgs :: Show a => [a] -> IO ()
        displayMsgs [] = return ()
        displayMsgs msgs = Prelude.putStrLn $ intercalate "\n" $ fmap show msgs

compileFiles :: [FilePath] -> DebugMode -> GenerateLLVMScript -> GenerateObject -> IO()
compileFiles [] _ _ _ = Prelude.putStrLn "fatal error: No input files"
compileFiles files debug genLL genO = sequence_ $ fmap (compileFile debug genLL genO) files

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left (files, debug, genLL, genO) -> compileFiles files debug genLL genO
        Right msg -> Prelude.putStrLn (show msg) >> exitWith (ExitFailure 84)