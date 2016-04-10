//------------------------------------------------------------------------------
/// @brief SnuPL/0 parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/11/04 Bernhard Egger maintain unary '+' signs in the AST
/// 2016/04/01 Bernhard Egger adapted to SnuPL/1 (this is not a joke)
/// 2016/09/28 Bernhard Egger assignment 2: parser for SnuPL/-1
///
/// @section license_section License
/// Copyright (c) 2012-2016, Bernhard Egger
/// All rights reserved.
///
/// Redistribution and use in source and binary forms,  with or without modifi-
/// cation, are permitted provided that the following conditions are met:
///
/// - Redistributions of source code must retain the above copyright notice,
///   this list of conditions and the following disclaimer.
/// - Redistributions in binary form must reproduce the above copyright notice,
///   this list of conditions and the following disclaimer in the documentation
///   and/or other materials provided with the distribution.
///
/// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
/// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO,  THE
/// IMPLIED WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE
/// ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER  OR CONTRIBUTORS BE
/// LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSE-
/// QUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF  SUBSTITUTE
/// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
/// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN  CONTRACT, STRICT
/// LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY
/// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
/// DAMAGE.
//------------------------------------------------------------------------------

#include <limits.h>
#include <cassert>
#include <errno.h>
#include <cstdlib>
#include <vector>
#include <iostream>
#include <exception>

#include "parser.h"
using namespace std;


//------------------------------------------------------------------------------
// CParser
//
CParser::CParser(CScanner *scanner)
{
  _scanner = scanner;
  _module = NULL;
}

CAstNode* CParser::Parse(void)
{
  _abort = false;

  if (_module != NULL) { delete _module; _module = NULL; }

  try {
    if (_scanner != NULL) _module = module();

    if (_module != NULL) {
      CToken t;
      string msg;
      //if (!_module->TypeCheck(&t, &msg)) SetError(t, msg);
    }
  } catch (...) {
    _module = NULL;
  }

  return _module;
}

const CToken* CParser::GetErrorToken(void) const
{
  if (_abort) return &_error_token;
  else return NULL;
}

string CParser::GetErrorMessage(void) const
{
  if (_abort) return _message;
  else return "";
}

void CParser::SetError(CToken t, const string message)
{
  _error_token = t;
  _message = message;
  _abort = true;
  throw message;
}

bool CParser::Consume(EToken type, CToken *token)
{
  if (_abort) return false;

  CToken t = _scanner->Get();

  if (t.GetType() != type) {
    SetError(t, "expected '" + CToken::Name(type) + "', got '" +
             t.GetName() + "'");
  }

  if (token != NULL) *token = t;

  return t.GetType() == type;
}

void CParser::InitSymbolTable(CSymtab *s)
{
  CTypeManager *tm = CTypeManager::Get();

  // TODO: add predefined functions here
}

CAstModule* CParser::module(void)
{
  //
  // module ::= statSequence  ".".
  //
  CToken dummy;
  CAstModule *m = new CAstModule(dummy, "placeholder");
  CAstStatement *statseq = NULL;

  statseq = statSequence(m);
  Consume(tDot);

  m->SetStatementSequence(statseq);

  return m;
}

CAstStatement* CParser::statSequence(CAstScope *s)
{
  //
  // statSequence ::= [ statement { ";" statement } ].
  // statement ::= assignment.
  // FIRST(statSequence) = { tNumber }
  // FOLLOW(statSequence) = { tDot }
  //
  CAstStatement *head = NULL;

  EToken tt = _scanner->Peek().GetType();
  if (!(tt == tDot)) {
    CAstStatement *tail = NULL;

    do {
      CToken t;
      EToken tt = _scanner->Peek().GetType();
      CAstStatement *st = NULL;

      switch (tt) {
        // statement ::= assignment
        case tNumber:
          st = assignment(s);
          break;

        default:
          SetError(_scanner->Peek(), "statement expected.");
          break;
      }

      assert(st != NULL);
      if (head == NULL) head = st;
      else tail->SetNext(st);
      tail = st;

      tt = _scanner->Peek().GetType();
      if (tt == tDot) break;

      Consume(tSemicolon);
    } while (!_abort);
  }

  return head;
}

CAstStatAssign* CParser::assignment(CAstScope *s)
{
  //
  // assignment ::= number ":=" expression.
  //
  CToken t;

  CAstConstant *lhs = number();
  Consume(tAssign, &t);

  CAstExpression *rhs = expression(s);

  return new CAstStatAssign(t, lhs, rhs);
}

CAstFunctionCall* CParser::subroutineCallForFunction(CAstScope *s)
{
  CToken t;
  CSymtab *tab;
  const CSymbol *sym;
  CAstFunctionCall *fc;

  Consume(tIdent, &t);

  tab = s->GetSymbolTable();
  sym = tab->FindSymbol(t.GetValue(), sGlobal);

  if (sym == NULL)
    SetError(t, "undefined identifier.");
  else if (sym->GetSymbolType() != stProcedure)
    SetError(t, "invalid procedure/function identifier.");

  fc = new CAstFunctionCall(t, static_cast<const CSymProc *>(sym));

  Consume(tLBrak);

  if (_scanner->Peek().GetType() != tRBrak) {
    while (true) {
      fc->AddArg(expression(s));

      if (_scanner->Peek().GetType() == tComma) Consume(tComma);
      else break;
    }
  }

  Consume(tRBrak);

  return fc;
}

CAstExpression* CParser::expression(CAstScope* s)
{
  //
  // expression ::= simpleexpr [ relOp simpleexpr ].
  //
  CToken t;
  EOperation relop;
  CAstExpression *left = NULL, *right = NULL;

  left = simpleexpr(s);

  if (_scanner->Peek().GetType() == tRelOp) {
    Consume(tRelOp, &t);
    right = simpleexpr(s);

    if (t.GetValue() == "=")       relop = opEqual;
    else if (t.GetValue() == "#")  relop = opNotEqual;
    else if (t.GetValue() == "<")  relop = opLessThan;
    else if (t.GetValue() == "<=") relop = opLessEqual;
    else if (t.GetValue() == ">")  relop = opBiggerThan;
    else if (t.GetValue() == ">=") relop = opBiggerEqual;
    else SetError(t, "invalid relation.");

    return new CAstBinaryOp(t, relop, left, right);
  } else {
    return left;
  }
}

CAstExpression* CParser::simpleexpr(CAstScope *s)
{
  //
  // simpleexpr ::= ["+"|"-"] term { termOp term }.
  //
  CAstUnaryOp *u = NULL;
  CToken unaryOp;
  CAstExpression *n = NULL;
  EToken et;

  et = _scanner->Peek().GetType();

  if (et == tPlusMinus) {
    Consume(tPlusMinus, &unaryOp);
  }

  n = term(s);

  while (true) {
    CToken termOp;
    CAstExpression *l = n, *r;
    EOperation oper;

    et = _scanner->Peek().GetType();
    if (et == tPlusMinus) {
      Consume(tPlusMinus, &termOp);
      oper = termOp.GetValue() == "+" ? opAdd : opSub;
    } else if (et == tOr) {
      Consume(tOr, &termOp);
      oper = opOr;
    }
    else break;

    r = term(s);

    n = new CAstBinaryOp(termOp, oper, l, r);
  }

  if (unaryOp.GetType() == tUndefined) return n;
  return new CAstUnaryOp(unaryOp, unaryOp.GetValue() == "+" ? opPos : opNeg, n);
}

CAstExpression* CParser::term(CAstScope *s)
{
  //
  // term ::= factor { factOp factor }.
  //
  CAstExpression *n = NULL;
  EToken et;

  n = factor(s);

  et = _scanner->Peek().GetType();

  while (et == tMulDivAnd) {
    CToken t;
    CAstExpression *l = n, *r;
    EOperation oper;

    Consume(tMulDivAnd, &t);

    if (t.GetValue() == "*")      oper = opMul;
    else if (t.GetValue() == "/") oper = opDiv;
    else                          oper = opAnd;

    r = factor(s);

    n = new CAstBinaryOp(t, oper, l, r);

    et = _scanner->Peek().GetType();
  }

  return n;
}

CAstExpression* CParser::factor(CAstScope *s)
{
  //
  // factor ::= qualident | number | boolean | char | string |
  //            "(" expression ")" | subroutineCall | "!" factor.
  //

  CToken t;
  EToken tt = _scanner->Peek().GetType();
  CAstExpression *n = NULL;

  switch (tt) {
    case tNumber:
      n = number();
      break;

    case tBoolean:
      n = boolean();
      break;

    case tChar:
      n = char_();
      break;

    case tString:
      n = string_(s);
      break;

    case tLBrak:
      Consume(tLBrak);
      n = expression(s);
      Consume(tRBrak);
      break;

    case tNot:
      Consume(tNot, &t);
      n = factor(s);
      n = new CAstUnaryOp(t, opNot, n);
      break;

    case tIdent:
      if (_scanner->Peek().GetType() == tLBrak) {
        n = subroutineCallForFunction(s);
      } else {
        n = qualident(s);
      }
      break;

    default:
      cout << "got " << _scanner->Peek() << endl;
      SetError(_scanner->Peek(), "factor expected.");
      break;
  }

  return n;
}

CAstDesignator* CParser::qualident(CAstScope *s)
{
  CToken t;

  Consume(tIdent, &t);
  SetError(t, "not implemented.");

  return NULL;
}

CAstConstant* CParser::number(void)
{
  //
  // number ::= digit { digit }.
  //
  // "digit { digit }" is scanned as one token (tNumber)
  //

  CToken t;

  Consume(tNumber, &t);

  errno = 0;
  long long v = strtoll(t.GetValue().c_str(), NULL, 10);
  if (errno != 0) SetError(t, "invalid number.");

  return new CAstConstant(t, CTypeManager::Get()->GetInt(), v);
}

CAstConstant *CParser::boolean(void)
{
  CToken t;

  Consume(tBoolean, &t);

  return new CAstConstant(t, CTypeManager::Get()->GetBool(), t.GetValue() == "true");
}

CAstConstant *CParser::char_(void)
{
  CToken t;
  string v;

  Consume(tChar, &t);
  v = CToken::unescape(t.GetValue());

  return new CAstConstant(t, CTypeManager::Get()->GetChar(), v[0]);
}

CAstStringConstant *CParser::string_(CAstScope *s)
{
  CToken t;

  Consume(tString, &t);

  return new CAstStringConstant(t, t.GetValue(), s);
}
