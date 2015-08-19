{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
{-# OPTIONS_GHC -w -fno-warn-unused-do-bind -fno-warn-unused-binds #-}

module Language.CFrp.Parser (
    parseExprString
  , parseDeclString
  , parseProgramString
) where

import Text.Peggy as P
import Data.ListLike (ListLike)
import Language.CFrp.Syntax as S

[peggy|
exprTop :: S.ParsedExpr = expr !.
declTop :: S.ParsedDecl = decl !.
program :: ([S.ParsedDecl], S.ParsedExpr) = decl* expr space* !. { ($1, $2) }

decl :: S.ParsedDecl
  = "%input" hSym:ident "::" t:typ "=" cSym:ident ";" { InputD hSym t cSym () }
  / "%import" hSym:ident "::" t:typ "=" cSym:ident ";" { ImportD hSym t cSym () }
  / "%{" (!"%}" .)* "%}" { EmbedD $1 () }

expr :: S.ParsedExpr
  = expr_ ";" expr { SeqE $1 $2 () }
  / expr_

expr_ :: S.ParsedExpr
  = "\\" var:ident args:(space ident)* "->" body:expr { AbsE var (foldr (\(_, arg) acc -> AbsE arg acc ()) body args) () }
  / "let" var:ident args:(space ident)* "=" body1:expr "in" body2:expr { LetE var (foldr (\(_, arg) acc -> AbsE arg acc ()) body1 args) body2 () }
  / "if" cond:expr "then" t:expr "else" e:expr { IfE cond t e () }
  / comp

comp :: S.ParsedExpr
  = comp "=" arith { EqE $1 $2 () }
  / comp "<" arith { LtE $1 $2 () }
  / arith

arith :: S.ParsedExpr
  = arith "+" term { AddE $1 $2 () }
  / arith "-" term { SubE $1 $2 () }
  / term

term :: S.ParsedExpr
  = term "*" app { MulE $1 $2 () }
  / term "/" app { DivE $1 $2 () }
  / app

app :: S.ParsedExpr
  = app "" fact {
    case $1 of
      AppE f args _ -> AppE f (args ++ [$2]) ()
      _ -> AppE $1 [$2] ()
  }
  / fact

fact :: S.ParsedExpr
  = "(" (expr, ",") ")" {
    case Prelude.length $1 of
      0 -> UnitE ()
      1 -> Prelude.head $1
      _ -> TupE $1 ()
  }
  / number { IntE $1 () }
  / ident { VarE $1 () }

number :: Int
  = "0" { 0 }
  / [1-9] [0-9]* { read ($1 : $2) }

ident :: String
  = !keywords [a-zA-Z_] [0-9a-zA-Z_]* { $1 : $2 }

keywords :: ()
  = "let" / "in"
  / "if" / "then" / "else"
  / "input"

typ :: Type
  = typTerm "->" typ { FunT $1 $2 }
  / typTerm

typTerm :: Type
  = "Signal" typFact { SigT $1 }
  / typFact

typFact :: Type
  = "(" (typ, ",") ")" {
    case Prelude.length $1 of
      0 -> UnitT
      1 -> Prelude.head $1
      _ -> TupT $1
  }
  / "Int" { IntT }

|]

parseExprString :: (ListLike str Char) => String -> str -> Either ParseError S.ParsedExpr
parseExprString = parseString exprTop

parseDeclString :: (ListLike str Char) => String -> str -> Either ParseError S.ParsedDecl
parseDeclString = parseString declTop

parseProgramString :: (ListLike str Char) => String -> str -> Either ParseError ([S.ParsedDecl], S.ParsedExpr)
parseProgramString = parseString program
