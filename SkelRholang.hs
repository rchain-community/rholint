--
-- Copyright 2018 Includable.
-- Licensed under MIT license.
--

module SkelRholang where

import AbsRholang
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transVar :: Var -> Result
transVar x = case x of
  Var string -> failure x
transContr :: Contr -> Result
transContr x = case x of
  DContr proc -> failure x
transProc :: Proc -> Result
transProc x = case x of
  PNil -> failure x
  PValue value -> failure x
  PDrop chan -> failure x
  PLift chan procs -> failure x
  PInput binds proc -> failure x
  PChoice cbranchs -> failure x
  PMatch proc pmbranchs -> failure x
  PNew vars proc -> failure x
  PConstr var procs -> failure x
  PContr var cpatterns proc -> failure x
  PPar proc1 proc2 -> failure x
transChan :: Chan -> Result
transChan x = case x of
  CVar var -> failure x
  CQuote proc -> failure x
transBind :: Bind -> Result
transBind x = case x of
  InputBind cpattern chan -> failure x
  InputDBind cpattern chan -> failure x
  CondInputBind cpattern chan proc -> failure x
transPMBranch :: PMBranch -> Result
transPMBranch x = case x of
  PatternMatch ppattern proc -> failure x
transCBranch :: CBranch -> Result
transCBranch x = case x of
  Choice binds proc -> failure x
transRhoBool :: RhoBool -> Result
transRhoBool x = case x of
  QTrue -> failure x
  QFalse -> failure x
transQuantity :: Quantity -> Result
transQuantity x = case x of
  QBool rhobool -> failure x
  QInt integer -> failure x
  QDouble double -> failure x
  QString string -> failure x
  QVar var -> failure x
  QMap -> failure x
  QDot quantity var quantitys -> failure x
  QNeg quantity -> failure x
  QMult quantity1 quantity2 -> failure x
  QDiv quantity1 quantity2 -> failure x
  QAdd quantity1 quantity2 -> failure x
  QMinus quantity1 quantity2 -> failure x
  QLt quantity1 quantity2 -> failure x
  QLte quantity1 quantity2 -> failure x
  QGt quantity1 quantity2 -> failure x
  QGte quantity1 quantity2 -> failure x
  QEq quantity1 quantity2 -> failure x
  QNeq quantity1 quantity2 -> failure x
transValue :: Value -> Result
transValue x = case x of
  VQuant quantity -> failure x
  EChar char -> failure x
  ETuple procs -> failure x
transVarPattern :: VarPattern -> Result
transVarPattern x = case x of
  VarPtVar var -> failure x
  VarPtWild -> failure x
transPPattern :: PPattern -> Result
transPPattern x = case x of
  PPtVal valpattern -> failure x
  PPtVar varpattern -> failure x
transCPattern :: CPattern -> Result
transCPattern x = case x of
  CPtVar varpattern -> failure x
  CValPtrn valpattern -> failure x
  CPtQuote ppattern -> failure x
transValPattern :: ValPattern -> Result
transValPattern x = case x of
  VPtTuple ppatterns -> failure x
  VPtTrue -> failure x
  VPtFalse -> failure x
  VPtInt integer -> failure x
  VPtDbl double -> failure x
  VPtNegInt integer -> failure x
  VPtNegDbl double -> failure x
  VPtStr string -> failure x
