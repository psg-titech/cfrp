module Main where

import qualified Data.Map as M
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)

import qualified Language.CFrp.Syntax as S
import Language.CFrp.Parser (parseProgramString)
import Language.CFrp.TypeInf (typeInf)
import Language.CFrp.KNormal (kNormalize)
import Language.CFrp.Closure (closureConvert)
import Language.CFrp.CodeGen (codeGen)

die :: Show a => a -> IO ()
die e = hPrint stderr e >> exitFailure

main :: IO ()
main = do
  path:_ <- getArgs
  src <- readFile path
  case parseProgramString path src of
    Left err -> die err
    Right (decls, expr) -> do
      let (inputEnv, importEnv, codes) = partitionDecls decls
      case typeInf (M.map fst $ inputEnv `M.union` importEnv) expr of
        Left err -> die err
        Right texpr -> do
          let c = closureConvert $ kNormalize texpr
          mapM_ putStrLn $ codeGen (M.map snd inputEnv) (M.map snd importEnv) codes c

type Env = M.Map String (S.Type, String)

partitionDecls :: [S.ParsedDecl] -> (Env, Env, [String])
partitionDecls [] = (M.empty, M.empty, [])
partitionDecls (S.InputD hSym t cSym _ : xs) =
  let (ienv, menv, codes) = partitionDecls xs
  in (M.insert hSym (t, cSym) ienv, menv, codes)
partitionDecls (S.ImportD hSym t cSym _ : xs) =
  let (ienv, menv, codes) = partitionDecls xs
  in (ienv, M.insert hSym (t, cSym) menv, codes)
partitionDecls (S.EmbedD code _ : xs) =
  let (ienv, menv, codes) = partitionDecls xs
  in (ienv, menv, code:codes)
