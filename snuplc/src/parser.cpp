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
#include "type.h"
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
      if (!_module->TypeCheck(&t, &msg)) SetError(t, msg);
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

inline void CParser::SetDuplicatedVariableError(CToken t)
{
  SetError(t, "duplicate variable declaration '" + t.GetValue() + "'.");
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

  // CParser::InitSymbolTable
  // We add the symbols of intrinsic functions here.

  {
    auto DIM = new CSymProc("DIM", tm->GetInt());
    auto array = new CSymParam(0, "array", tm->GetPointer(tm->GetNull()));
    auto dim = new CSymParam(1, "dim", tm->GetInt());
    DIM->AddParam(array);
    DIM->AddParam(dim);
    s->AddSymbol(DIM);
  }
  {
    auto DOFS = new CSymProc("DOFS", tm->GetInt());
    auto array = new CSymParam(0, "array", tm->GetPointer(tm->GetNull()));
    DOFS->AddParam(array);
    s->AddSymbol(DOFS);
  }
  {
    auto ReadInt = new CSymProc("ReadInt", tm->GetInt());
    s->AddSymbol(ReadInt);
  }
  {
    auto WriteInt = new CSymProc("WriteInt", tm->GetNull());
    auto i = new CSymParam(0, "i", tm->GetInt());
    WriteInt->AddParam(i);
    s->AddSymbol(WriteInt);
  }
  {
    auto WriteChar = new CSymProc("WriteChar", tm->GetNull());
    auto c = new CSymParam(0, "c", tm->GetChar());
    WriteChar->AddParam(c);
    s->AddSymbol(WriteChar);
  }
  {
    auto WriteStr = new CSymProc("WriteStr", tm->GetNull());
    auto str = new CSymParam(0, "string", tm->GetPointer(tm->GetArray(CArrayType::OPEN, tm->GetChar())));
    WriteStr->AddParam(str);
    s->AddSymbol(WriteStr);
  }
  {
    auto WriteLn = new CSymProc("WriteLn", tm->GetNull());
    s->AddSymbol(WriteLn);
  }
}

CAstModule* CParser::module(void)
{
  //
  // module ::= "module" ident ";" varDeclaration { subroutineDecl }
  //            "begin" statSequence "end" ident ".".
  //

  CToken moduleToken;
  CToken nameToken;

  Consume(tModule, &moduleToken);
  Consume(tIdent, &nameToken);
  Consume(tSemicolon);

  auto m = new CAstModule(moduleToken, nameToken.GetValue());
  auto symtab = m->GetSymbolTable();

  // InitSymbolTable must be called to insert the symbols of intrinsic functions
  InitSymbolTable(symtab);

  // varDeclaration is inlined
  // if tVar appears, varDeclSequence will follow
  if (_scanner->Peek().GetType() == tVar) {
    Consume(tVar);

    for (auto &&var : varDeclSequence(false)) {
      if (!symtab->AddSymbol(m->CreateVar(var.first.GetValue(), var.second))) {
        SetDuplicatedVariableError(var.first);
      }
    }
  }

  // Naturally we can deal with Procedure and Function in the same manner
  for (;;) {
    auto type = _scanner->Peek().GetType();
    if (type != tProcedure && type != tFunction) break;

    subroutineDecl(m, type == tFunction);
  }

  Consume(tBegin);

  auto statseq = statSequence(m);
  m->SetStatementSequence(statseq);

  Consume(tEnd);

  CToken endIdent;
  Consume(tIdent, &endIdent);

  // Semantic check: module name must match
  if (nameToken.GetValue() != endIdent.GetValue()) {
    SetError(endIdent, "module identifier mismatch ('" +
     nameToken.GetValue() + "' != '" + endIdent.GetValue() + "').");
  }
  Consume(tDot);

  return m;
}

CAstStatement* CParser::statSequence(CAstScope *s)
{
  CAstStatement *head = nullptr;
  CAstStatement *tail = nullptr;

  for (;;) {
    CToken t;
    EToken tt = _scanner->Peek().GetType();
    CAstStatement *st = nullptr;

    switch (tt) {
      case tIf:
        st = ifStatement(s);
        break;
      case tWhile:
        st = whileStatement(s);
        break;
      case tReturn:
        st = returnStatement(s);
        break;
      case tIdent:
        Consume(tIdent, &t);

        // Eliminating ambiguity: We look ahead 2 tokens
        if (_scanner->Peek().GetType() == tLBrak) {
          st = subroutineCallForProcedure(t, s);
        } else {
          st = assignment(t, s);
        }
        break;
      default:
        break;
    }

    if (st == nullptr)
    {
      // This is true only if a statement is not following a semicolon
      // After semicolon a statement must follow
      if (head)
        SetError(_scanner->Peek(), "statement expected.");
      break;
    }

    if (head == nullptr) head = st;
    else tail->SetNext(st);
    tail = st;

    if (_scanner->Peek().GetType() != tSemicolon) break;
    Consume(tSemicolon);
  }

  return head;
}

CAstStatAssign* CParser::assignment(CToken t, CAstScope *s)
{
  //
  // assignment ::= qualident ":=" expression.
  //

  CAstDesignator *lhs = qualident(t, s);
  Consume(tAssign, &t);

  CAstExpression *rhs = expression(s);

  return new CAstStatAssign(t, lhs, rhs);
}

CAstStatIf* CParser::ifStatement(CAstScope *s)
{
  CToken t;
  CAstExpression *cond;
  CAstStatement *ifBody, *elseBody = NULL;

  Consume(tIf, &t);
  Consume(tLBrak);
  cond = expression(s);
  Consume(tRBrak);
  Consume(tThen);
  ifBody = statSequence(s);
  if (_scanner->Peek().GetType() == tElse) {
    Consume(tElse);
    elseBody = statSequence(s);
  }
  Consume(tEnd);

  return new CAstStatIf(t, cond, ifBody, elseBody);
}

CAstStatWhile* CParser::whileStatement(CAstScope *s)
{
  CToken t;
  CAstExpression *cond;
  CAstStatement *body;

  Consume(tWhile);
  Consume(tLBrak);
  cond = expression(s);
  Consume(tRBrak);
  Consume(tDo);
  body = statSequence(s);
  Consume(tEnd);

  return new CAstStatWhile(t, cond, body);
}

CAstStatReturn* CParser::returnStatement(CAstScope *s)
{
  CToken t;
  EToken et;

  Consume(tReturn, &t);
  et = _scanner->Peek().GetType();

  // return;
  if (et == tEnd || et == tElse || et == tSemicolon)
    return new CAstStatReturn(t, s, NULL);
  // return expr;
  else
    return new CAstStatReturn(t, s, expression(s));
}

CAstFunctionCall* CParser::subroutineCallForFunction(CToken t, CAstScope *s)
{
  // parameter t is the result of two look-ahead
  CSymtab *tab;
  const CSymbol *sym;
  CAstFunctionCall *fc;

  tab = s->GetSymbolTable();
  sym = tab->FindSymbol(t.GetValue(), sGlobal);

  if (sym == NULL)
    SetError(t, "undefined identifier.");

  // stProcedure and stFunction has same SymbolType: stProcedure
  // so it's enough to check whether it is stProcedure.
  // And this is a semantic check which does not need to be done in this phase :p
  else if (sym->GetSymbolType() != stProcedure)
    SetError(t, "invalid procedure/function identifier.");

  fc = new CAstFunctionCall(t, static_cast<const CSymProc *>(sym));

  Consume(tLBrak);

  if (_scanner->Peek().GetType() != tRBrak) {
    while (true) {
      auto exp = expression(s);
      auto exp_t = exp->GetType();
      if (exp_t != nullptr && exp_t->IsArray())
        fc->AddArg(new CAstSpecialOp(exp->GetToken(), opAddress, exp, nullptr));
      else
        fc->AddArg(exp);

      if (_scanner->Peek().GetType() == tComma) Consume(tComma);
      else break;
    }
  }

  Consume(tRBrak);

  return fc;
}

CAstStatCall* CParser::subroutineCallForProcedure(CToken t, CAstScope *s)
{
  // Delegates the dirty-works to subroutineCallForFunction
  CAstFunctionCall *fc = subroutineCallForFunction(t, s);

  // ... and just wrap with CAstStatCall. That's it.
  return new CAstStatCall(fc->GetToken(), fc);
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
  CAstExpression *n = NULL;
  EToken et;

  et = _scanner->Peek().GetType();

  if (et == tPlusMinus) {
    CToken unaryOp;
    Consume(tPlusMinus, &unaryOp);

    // Implement "Relaxed" version
    // if we have unaryOp and number together, merge it
    if (_scanner->Peek().GetType() == tNumber) n = term(s, &unaryOp);
    else n = new CAstUnaryOp(unaryOp, unaryOp.GetValue() == "+" ? opPos : opNeg, term(s));
  } else {
    n = term(s);
  }

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

  return n;
}

CAstExpression* CParser::term(CAstScope *s, CToken *unary)
{
  //
  // term ::= factor { factOp factor }.
  //
  CAstExpression *n = NULL;
  EToken et;

  if (unary) {
    // number will follow
    n = number(unary);
  } else {
    n = factor(s);
  }

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
      Consume(tIdent, &t);
      if (_scanner->Peek().GetType() == tLBrak) {
        n = subroutineCallForFunction(t, s);
      } else {
        n = qualident(t, s);
      }
      break;

    default:
      SetError(_scanner->Peek(), "factor expected.");
      break;
  }

  return n;
}

CAstDesignator* CParser::qualident(CToken t, CAstScope *s)
{
  // parameter t is the result of two look-ahead

  auto symtable = s->GetSymbolTable();
  auto symbol = symtable->FindSymbol(t.GetValue());
  if (symbol == nullptr) {
    SetError(t, "undefined identifier.");
  } else if (symbol->GetSymbolType() == stProcedure) {
    // Another semantic check: LHS must be neither a procedure nor a function
    SetError(t, "designator expected.");
  }

  CAstDesignator *ret;

  if (_scanner->Peek().GetType() == tLSqBrak) {
    Consume(tLSqBrak);
    auto arrDesg = new CAstArrayDesignator(t, symbol);
    for (;;) {
      arrDesg->AddIndex(expression(s));
      Consume(tRSqBrak);
      if (_scanner->Peek().GetType() != tLSqBrak)
        break;
      Consume(tLSqBrak);
    }
    arrDesg->IndicesComplete();
    ret = arrDesg;
  } else {
    ret = new CAstDesignator(t, symbol);
  }

  return ret;
}

CAstConstant* CParser::number(CToken *unary)
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

  if (unary && unary->GetValue() == "-") {
    v = -v;
  }

  // the most appropriate place of checking overflow
  if (v < INT_MIN || v > INT_MAX) {
    SetError(t, "integer constant outside valid range.");
  }

  return new CAstConstant(t, CTypeManager::Get()->GetInt(), v);
}

CAstConstant *CParser::boolean(void)
{
  CToken t;

  Consume(tBoolean, &t);

  // t.GetValue() is either "true" or "false"
  return new CAstConstant(t, CTypeManager::Get()->GetBool(), t.GetValue() == "true");
}

CAstConstant *CParser::char_(void)
{
  CToken t;
  string v;

  Consume(tChar, &t);
  v = CToken::unescape(t.GetValue());

  // peek first character: char is stored in string
  return new CAstConstant(t, CTypeManager::Get()->GetChar(), v[0]);
}

CAstStringConstant *CParser::string_(CAstScope *s)
{
  CToken t;

  Consume(tString, &t);

  return new CAstStringConstant(t, t.GetValue(), s);
}

vector<pair<CToken, const CType *>> CParser::varDeclSequence(bool allowopen)
{
  vector<pair<CToken, const CType *>> ret;

  do {
    vector<CToken> idents;
    CToken ident;
    Consume(tIdent, &ident);
    idents.push_back(ident);

    while (_scanner->Peek().GetType() == tComma) {
      Consume(tComma);
      Consume(tIdent, &ident);
      idents.push_back(ident);
    }

    Consume(tColon);

    auto type = type_(nullptr, allowopen);

    for (auto &&elem : idents) {
      ret.emplace_back(elem, type);
    }
    if (_scanner->Peek().GetType() != tSemicolon)
      break;

    Consume(tSemicolon);
  } while (_scanner->Peek().GetType() == tIdent);

  return ret;
}

CAstProcedure* CParser::subroutineDecl(CAstScope *s, bool isFunc)
{
  Consume(isFunc ? tFunction : tProcedure);

  CToken nameToken;
  Consume(tIdent, &nameToken);

  vector<pair<CToken, const CType *>> params;

  if (_scanner->Peek().GetType() == tLBrak) {
    Consume(tLBrak);
    if (_scanner->Peek().GetType() != tRBrak)
      params = varDeclSequence();
    Consume(tRBrak);
  }

  const CType *returnType;

  if (isFunc) {
    Consume(tColon);
    CToken basetype;
    returnType = type_(&basetype);
    if (!returnType->IsScalar()) {
      SetError(basetype, "invalid composite type for function.");
    }
  } else {
    returnType = CTypeManager::Get()->GetNull();
  }

  Consume(tSemicolon);

  CSymProc *symproc = new CSymProc(nameToken.GetValue(), returnType);
  auto ret = new CAstProcedure(nameToken, nameToken.GetValue(), s, symproc);
  auto stable = ret->GetSymbolTable();

  // Function parameters and local variables share the same scope
  // so duplication between them is ill-formed
  for (int i = 0; i < params.size(); i++) {
    auto type = params[i].second; // Implicit ptr to array conversion
    if (type->IsArray()) type = CTypeManager::Get()->GetPointer(type);
    auto sym = new CSymParam(i, params[i].first.GetValue(), type);
    symproc->AddParam(sym);
    if (!stable->AddSymbol(sym)) {
      SetDuplicatedVariableError(params[i].first);
    }
  }

  if (_scanner->Peek().GetType() == tVar) {
    Consume(tVar);
    for (auto &&local : varDeclSequence(false)) {
      if (!stable->AddSymbol(ret->CreateVar(local.first.GetValue(), local.second))) {
        SetDuplicatedVariableError(local.first);
      }
    }
  }

  if (!s->GetSymbolTable()->AddSymbol(symproc)) {
    SetError(nameToken, "duplicate procedure/function declaration '" + nameToken.GetValue() + "'.");
  }

  Consume(tBegin);
  ret->SetStatementSequence(statSequence(ret));
  Consume(tEnd);

  // Semantic check: subroutine identifier must match
  CToken endIdent;
  Consume(tIdent, &endIdent);
  if (nameToken.GetValue() != endIdent.GetValue()) {
    SetError(endIdent, "procedure/function identifier mismatch ('" +
     nameToken.GetValue() + "' != '" + endIdent.GetValue() + "').");
  }

  Consume(tSemicolon);

  return ret;
}

const CType *CParser::type_(CToken *t, bool allowopen)
{
  CToken basetype;
  const CType *ret = nullptr;
  Consume(tBaseType, &basetype);
  ret = CTypeManager::Get()->GetFromName(basetype.GetValue());

  vector<int> dims;
  while (_scanner->Peek().GetType() == tLSqBrak) {
    Consume(tLSqBrak);

    // Wrap as many times as the braket shows
    long long size = CArrayType::OPEN;
    if (_scanner->Peek().GetType() != tRSqBrak || allowopen == false) {
      CToken stoken;
      Consume(tNumber, &stoken);
      size = stoll(stoken.GetValue());
      if (size <= 0) {
        SetError(stoken, "postive constant expected.");
      } else if (size >= 2147483648LL) {
        SetError(stoken, "array dimension outside valid range.");
      }
    }
    dims.push_back(static_cast<int>(size));
    Consume(tRSqBrak);
  }

  for (auto rit = dims.rbegin(); rit != dims.rend(); rit++)
  {
    ret = CTypeManager::Get()->GetArray(*rit, ret);
  }

  if (t) *t = basetype;
  return ret;
}
