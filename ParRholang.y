--
-- Copyright 2018 Includable.
-- Licensed under MIT license.
--

{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParRholang where
import AbsRholang
import LexRholang
import ErrM
}

%name pContr Contr
%name pProc4 Proc4
%name pProc3 Proc3
%name pProc2 Proc2
%name pProc1 Proc1
%name pProc Proc
%name pListProc ListProc
%name pChan Chan
%name pBind Bind
%name pListBind ListBind
%name pPMBranch PMBranch
%name pListPMBranch ListPMBranch
%name pCBranch CBranch
%name pListCBranch ListCBranch
%name pRhoBool RhoBool
%name pQuantity7 Quantity7
%name pQuantity6 Quantity6
%name pQuantity5 Quantity5
%name pQuantity4 Quantity4
%name pQuantity3 Quantity3
%name pQuantity2 Quantity2
%name pQuantity1 Quantity1
%name pQuantity Quantity
%name pListQuantity ListQuantity
%name pValue Value
%name pVarPattern VarPattern
%name pListVarPattern ListVarPattern
%name pPPattern4 PPattern4
%name pListPPattern ListPPattern
%name pPPattern PPattern
%name pPPattern1 PPattern1
%name pPPattern2 PPattern2
%name pPPattern3 PPattern3
%name pCPattern CPattern
%name pListCPattern ListCPattern
%name pValPattern ValPattern
%name pListVar ListVar
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '(' { PT _ (TS _ 3) }
  ')' { PT _ (TS _ 4) }
  '*' { PT _ (TS _ 5) }
  '+' { PT _ (TS _ 6) }
  ',' { PT _ (TS _ 7) }
  '-' { PT _ (TS _ 8) }
  '.' { PT _ (TS _ 9) }
  '/' { PT _ (TS _ 10) }
  ';' { PT _ (TS _ 11) }
  '<' { PT _ (TS _ 12) }
  '<-' { PT _ (TS _ 13) }
  '<=' { PT _ (TS _ 14) }
  '=' { PT _ (TS _ 15) }
  '==' { PT _ (TS _ 16) }
  '=>' { PT _ (TS _ 17) }
  '>' { PT _ (TS _ 18) }
  '>=' { PT _ (TS _ 19) }
  '@' { PT _ (TS _ 20) }
  'Map()' { PT _ (TS _ 21) }
  'Nil' { PT _ (TS _ 22) }
  '[' { PT _ (TS _ 23) }
  ']' { PT _ (TS _ 24) }
  '_' { PT _ (TS _ 25) }
  'case' { PT _ (TS _ 26) }
  'contract' { PT _ (TS _ 27) }
  'false' { PT _ (TS _ 28) }
  'for' { PT _ (TS _ 29) }
  'if' { PT _ (TS _ 30) }
  'in' { PT _ (TS _ 31) }
  'match' { PT _ (TS _ 32) }
  'new' { PT _ (TS _ 33) }
  'select' { PT _ (TS _ 34) }
  'true' { PT _ (TS _ 35) }
  'with' { PT _ (TS _ 36) }
  '{' { PT _ (TS _ 37) }
  '|' { PT _ (TS _ 38) }
  '}' { PT _ (TS _ 39) }

L_integ  { PT _ (TI $$) }
L_doubl  { PT _ (TD $$) }
L_quoted { PT _ (TL $$) }
L_charac { PT _ (TC $$) }
L_Var { PT _ (T_Var $$) }


%%

Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
Double  :: { Double }  : L_doubl  { (read ( $1)) :: Double }
String  :: { String }  : L_quoted {  $1 }
Char    :: { Char }    : L_charac { (read ( $1)) :: Char }
Var    :: { Var} : L_Var { Var ($1)}

Contr :: { Contr }
Contr : Proc1 { AbsRholang.DContr $1 }
Proc4 :: { Proc }
Proc4 : 'Nil' { AbsRholang.PNil }
      | Value { AbsRholang.PValue $1 }
      | '{' Proc '}' { $2 }
Proc3 :: { Proc }
Proc3 : '*' Chan { AbsRholang.PDrop $2 } | Proc4 { $1 }
Proc2 :: { Proc }
Proc2 : Chan '!' '(' ListProc ')' { AbsRholang.PLift $1 $4 }
      | Proc3 { $1 }
Proc1 :: { Proc }
Proc1 : 'for' '(' ListBind ')' '{' Proc '}' { AbsRholang.PInput $3 $6 }
      | 'select' '{' ListCBranch '}' { AbsRholang.PChoice $3 }
      | 'match' Proc 'with' ListPMBranch { AbsRholang.PMatch $2 $4 }
      | 'new' ListVar 'in' Proc1 { AbsRholang.PNew $2 $4 }
      | Var '(' ListProc ')' { AbsRholang.PConstr $1 $3 }
      | 'contract' Var '(' ListCPattern ')' '=' '{' Proc '}' { AbsRholang.PContr $2 $4 $8 }
      | Proc2 { $1 }
Proc :: { Proc }
Proc : Proc '|' Proc1 { AbsRholang.PPar $1 $3 } | Proc1 { $1 }
ListProc :: { [Proc] }
ListProc : {- empty -} { [] }
         | Proc { (:[]) $1 }
         | Proc ',' ListProc { (:) $1 $3 }
Chan :: { Chan }
Chan : Var { AbsRholang.CVar $1 }
     | '@' Proc3 { AbsRholang.CQuote $2 }
Bind :: { Bind }
Bind : CPattern '<-' Chan { AbsRholang.InputBind $1 $3 }
     | CPattern '<=' Chan { AbsRholang.InputDBind $1 $3 }
     | CPattern '<-' Chan 'if' Proc { AbsRholang.CondInputBind $1 $3 $5 }
ListBind :: { [Bind] }
ListBind : Bind { (:[]) $1 } | Bind ';' ListBind { (:) $1 $3 }
PMBranch :: { PMBranch }
PMBranch : PPattern '=>' '{' Proc '}' { AbsRholang.PatternMatch $1 $4 }
ListPMBranch :: { [PMBranch] }
ListPMBranch : PMBranch { (:[]) $1 }
             | PMBranch ListPMBranch { (:) $1 $2 }
CBranch :: { CBranch }
CBranch : 'case' ListBind '=>' '{' Proc '}' { AbsRholang.Choice $2 $5 }
ListCBranch :: { [CBranch] }
ListCBranch : CBranch { (:[]) $1 }
            | CBranch ListCBranch { (:) $1 $2 }
RhoBool :: { RhoBool }
RhoBool : 'true' { AbsRholang.QTrue }
        | 'false' { AbsRholang.QFalse }
Quantity7 :: { Quantity }
Quantity7 : RhoBool { AbsRholang.QBool $1 }
          | Integer { AbsRholang.QInt $1 }
          | Double { AbsRholang.QDouble $1 }
          | String { AbsRholang.QString $1 }
          | Var { AbsRholang.QVar $1 }
          | 'Map()' { AbsRholang.QMap }
          | '(' Quantity ')' { $2 }
Quantity6 :: { Quantity }
Quantity6 : Quantity7 '.' Var '(' ListQuantity ')' { AbsRholang.QDot $1 $3 $5 }
          | Quantity7 { $1 }
Quantity5 :: { Quantity }
Quantity5 : '-' Quantity6 { AbsRholang.QNeg $2 } | Quantity6 { $1 }
Quantity4 :: { Quantity }
Quantity4 : Quantity4 '*' Quantity5 { AbsRholang.QMult $1 $3 }
          | Quantity4 '/' Quantity5 { AbsRholang.QDiv $1 $3 }
          | Quantity5 { $1 }
Quantity3 :: { Quantity }
Quantity3 : Quantity3 '+' Quantity4 { AbsRholang.QAdd $1 $3 }
          | Quantity3 '-' Quantity4 { AbsRholang.QMinus $1 $3 }
          | Quantity4 { $1 }
Quantity2 :: { Quantity }
Quantity2 : Quantity2 '<' Quantity3 { AbsRholang.QLt $1 $3 }
          | Quantity2 '<=' Quantity3 { AbsRholang.QLte $1 $3 }
          | Quantity2 '>' Quantity3 { AbsRholang.QGt $1 $3 }
          | Quantity2 '>=' Quantity3 { AbsRholang.QGte $1 $3 }
          | Quantity3 { $1 }
Quantity1 :: { Quantity }
Quantity1 : Quantity1 '==' Quantity2 { AbsRholang.QEq $1 $3 }
          | Quantity1 '!=' Quantity2 { AbsRholang.QNeq $1 $3 }
          | Quantity2 { $1 }
Quantity :: { Quantity }
Quantity : Quantity1 { $1 }
ListQuantity :: { [Quantity] }
ListQuantity : {- empty -} { [] }
             | Quantity { (:[]) $1 }
             | Quantity ',' ListQuantity { (:) $1 $3 }
Value :: { Value }
Value : Quantity { AbsRholang.VQuant $1 }
      | Char { AbsRholang.EChar $1 }
      | '[' ListProc ']' { AbsRholang.ETuple $2 }
VarPattern :: { VarPattern }
VarPattern : Var { AbsRholang.VarPtVar $1 }
           | '_' { AbsRholang.VarPtWild }
ListVarPattern :: { [VarPattern] }
ListVarPattern : {- empty -} { [] }
               | VarPattern { (:[]) $1 }
               | VarPattern ',' ListVarPattern { (:) $1 $3 }
PPattern4 :: { PPattern }
PPattern4 : ValPattern { AbsRholang.PPtVal $1 }
          | VarPattern { AbsRholang.PPtVar $1 }
          | '(' PPattern ')' { $2 }
ListPPattern :: { [PPattern] }
ListPPattern : {- empty -} { [] }
             | PPattern { (:[]) $1 }
             | PPattern ',' ListPPattern { (:) $1 $3 }
PPattern :: { PPattern }
PPattern : PPattern1 { $1 }
PPattern1 :: { PPattern }
PPattern1 : PPattern2 { $1 }
PPattern2 :: { PPattern }
PPattern2 : PPattern3 { $1 }
PPattern3 :: { PPattern }
PPattern3 : PPattern4 { $1 }
CPattern :: { CPattern }
CPattern : VarPattern { AbsRholang.CPtVar $1 }
         | ValPattern { AbsRholang.CValPtrn $1 }
         | '@' PPattern3 { AbsRholang.CPtQuote $2 }
ListCPattern :: { [CPattern] }
ListCPattern : {- empty -} { [] }
             | CPattern { (:[]) $1 }
             | CPattern ',' ListCPattern { (:) $1 $3 }
ValPattern :: { ValPattern }
ValPattern : '[' ListPPattern ']' { AbsRholang.VPtTuple $2 }
           | 'true' { AbsRholang.VPtTrue }
           | 'false' { AbsRholang.VPtFalse }
           | Integer { AbsRholang.VPtInt $1 }
           | Double { AbsRholang.VPtDbl $1 }
           | '-' Integer { AbsRholang.VPtNegInt $2 }
           | '-' Double { AbsRholang.VPtNegDbl $2 }
           | String { AbsRholang.VPtStr $1 }
ListVar :: { [Var] }
ListVar : Var { (:[]) $1 } | Var ',' ListVar { (:) $1 $3 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
}
