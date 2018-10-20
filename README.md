# Rholint: A Rholang Linter

**This repo contains the start of a Rholang linter, written in Haskell.**

Currently, it just displays syntax errors, and outputs the parsed AST when
there are no syntax errors. Eventually, the idea is to turn this into a full
fledged contract linter, which will be able to fix code bases, apply standards,
and warn the user about dangerous operations or undesired side effects.

It is partially generated using BNFC, and therefore depends on `alex` and `happy`.


## Installation

Make sure to have `haskel-platform` installed using the package manager of your OS first.

```bash
git clone https://github.com/rchain-community/rholint
cd rholint
cabal install alex happy aeson
make
```


## Usage

```
usage: rholint [...files]

  --help          Display this help message
  (files)         Parse content of files
  (no arguments)  Parse stdin
```

Rholint both accepts files as parameters, or will listen for input on `stdin`
if no parameters have been specified.


## Output format

Output is generated as JSON, in order to ease possible integration in
[rchain.cloud](https://rchain.cloud) or similar tools.

The current output format is as follows:

```json
{
  "success": false,
  "err": [{
    "kind": "syntax",
    "severity": 1,
    "message": "syntax error at line 8, column 1 before `new'"
  }],
  "result": "[PT (Pn 0 1 1) (TS \"new\" 33),PT (Pn 4 1 5) (T_Var \"helloWorld\"),PT (Pn 15 1 16) (TS \"in\" 31), ...",
  "tree": "DContr (PNew [Var \"helloWorld\"] (PPar (PContr (Var \"helloWorld\") [CPtVar (VarPtVar (Var \"message\"))] ... ",
  "parsed": "new helloWorld in (contract helloWorld (message) = {\n  stdout ! (* message) ..."
}
```
