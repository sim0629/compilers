//------------------------------------------------------------------------------
/// @brief SnuPL abstract syntax tree
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/05/22 Bernhard Egger reimplemented TAC generation
/// 2013/11/04 Bernhard Egger added typechecks for unary '+' operators
/// 2016/03/12 Bernhard Egger adapted to SnuPL/1
/// 2014/04/08 Bernhard Egger assignment 2: AST for SnuPL/-1
///
/// @section license_section License
/// Copyright (c) 2012-2016 Bernhard Egger
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

#include <iostream>
#include <cassert>
#include <cstring>

#include <typeinfo>

#include "ast.h"
using namespace std;


//------------------------------------------------------------------------------
// CAstNode
//
int CAstNode::_global_id = 0;

CAstNode::CAstNode(CToken token)
  : _token(token), _addr(NULL)
{
  _id = _global_id++;
}

CAstNode::~CAstNode(void)
{
  if (_addr != NULL) delete _addr;
}

int CAstNode::GetID(void) const
{
  return _id;
}

CToken CAstNode::GetToken(void) const
{
  return _token;
}

const CType* CAstNode::GetType(void) const
{
  return CTypeManager::Get()->GetNull();
}

string CAstNode::dotID(void) const
{
  ostringstream out;
  out << "node" << dec << _id;
  return out.str();
}

string CAstNode::dotAttr(void) const
{
  return " [label=\"" + dotID() + "\"]";
}

void CAstNode::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << dotID() << dotAttr() << ";" << endl;
}

CTacAddr* CAstNode::GetTacAddr(void) const
{
  return _addr;
}

ostream& operator<<(ostream &out, const CAstNode &t)
{
  return t.print(out);
}

ostream& operator<<(ostream &out, const CAstNode *t)
{
  return t->print(out);
}

//------------------------------------------------------------------------------
// CAstScope
//
CAstScope::CAstScope(CToken t, const string name, CAstScope *parent)
  : CAstNode(t), _name(name), _symtab(NULL), _parent(parent), _statseq(NULL),
    _cb(NULL)
{
  if (_parent != NULL) _parent->AddChild(this);
}

CAstScope::~CAstScope(void)
{
  delete _symtab;
  delete _statseq;
  delete _cb;
}

const string CAstScope::GetName(void) const
{
  return _name;
}

CAstScope* CAstScope::GetParent(void) const
{
  return _parent;
}

size_t CAstScope::GetNumChildren(void) const
{
  return _children.size();
}

CAstScope* CAstScope::GetChild(size_t i) const
{
  assert(i < _children.size());
  return _children[i];
}

CSymtab* CAstScope::GetSymbolTable(void) const
{
  assert(_symtab != NULL);
  return _symtab;
}

void CAstScope::SetStatementSequence(CAstStatement *statseq)
{
  _statseq = statseq;
}

CAstStatement* CAstScope::GetStatementSequence(void) const
{
  return _statseq;
}

bool CAstScope::TypeCheck(CToken *t, string *msg) const
{
  // First check all of the statements of itself.
  auto s = _statseq;
  while (s != nullptr) {
    if (!s->TypeCheck(t, msg)) return false;
    s = s->GetNext();
  }

  // Second check the children (for module).
  for (auto &&child : _children) {
    if (!child->TypeCheck(t, msg)) return false;
  }

  // All passed.
  return true;
}

ostream& CAstScope::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "CAstScope: '" << _name << "'" << endl;
  out << ind << "  symbol table:" << endl;
  _symtab->print(out, indent+4);
  out << ind << "  statement list:" << endl;
  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    do {
      s->print(out, indent+4);
      s = s->GetNext();
    } while (s != NULL);
  } else {
    out << ind << "    empty." << endl;
  }

  out << ind << "  nested scopes:" << endl;
  if (_children.size() > 0) {
    for (size_t i=0; i<_children.size(); i++) {
      _children[i]->print(out, indent+4);
    }
  } else {
    out << ind << "    empty." << endl;
  }
  out << ind << endl;

  return out;
}

void CAstScope::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    string prev = dotID();
    do {
      s->toDot(out, indent);
      out << ind << prev << " -> " << s->dotID() << " [style=dotted];" << endl;
      prev = s->dotID();
      s = s->GetNext();
    } while (s != NULL);
  }

  vector<CAstScope*>::const_iterator it = _children.begin();
  while (it != _children.end()) {
    CAstScope *s = *it++;
    s->toDot(out, indent);
    out << ind << dotID() << " -> " << s->dotID() << ";" << endl;
  }

}

CTacAddr* CAstScope::ToTac(CCodeBlock *cb)
{
  // This following code has been copied from the pdf
  assert(cb != nullptr);

  CAstStatement *s = GetStatementSequence();
  CAstStatement::SequenceToTac(cb, s);

  cb->CleanupControlFlow();

  return NULL;
}

CCodeBlock* CAstScope::GetCodeBlock(void) const
{
  return _cb;
}

void CAstScope::SetSymbolTable(CSymtab *st)
{
  if (_symtab != NULL) delete _symtab;
  _symtab = st;
}

void CAstScope::AddChild(CAstScope *child)
{
  _children.push_back(child);
}


//------------------------------------------------------------------------------
// CAstModule
//
CAstModule::CAstModule(CToken t, const string name)
  : CAstScope(t, name, NULL)
{
  SetSymbolTable(new CSymtab());
}

CSymbol* CAstModule::CreateVar(const string ident, const CType *type)
{
  return new CSymGlobal(ident, type);
}

string CAstModule::dotAttr(void) const
{
  return " [label=\"m " + GetName() + "\",shape=box]";
}



//------------------------------------------------------------------------------
// CAstProcedure
//
CAstProcedure::CAstProcedure(CToken t, const string name,
                             CAstScope *parent, CSymProc *symbol)
  : CAstScope(t, name, parent), _symbol(symbol)
{
  assert(GetParent() != NULL);
  SetSymbolTable(new CSymtab(GetParent()->GetSymbolTable()));
  assert(_symbol != NULL);
}

CSymProc* CAstProcedure::GetSymbol(void) const
{
  return _symbol;
}

CSymbol* CAstProcedure::CreateVar(const string ident, const CType *type)
{
  return new CSymLocal(ident, type);
}

bool CAstProcedure::TypeCheck(CToken *t, string *msg) const
{
  // We first typecheck with CAstScope::TypeCheck
  // and check if there is some path not returning a value
  // (only in functions)
  if (!CAstScope::TypeCheck(t, msg)) return false;
  if (!GetType()->Compare(CTypeManager::Get()->GetNull())) {
    if (!AllPathsReturn(GetStatementSequence())) {
      if (t) *t = GetToken();
      if (msg) *msg = "Not all paths of the function return a value.";
      return false;
    }
  }
  return true;
}

const CType* CAstProcedure::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

string CAstProcedure::dotAttr(void) const
{
  return " [label=\"p/f " + GetName() + "\",shape=box]";
}

bool CAstProcedure::AllPathsReturn(CAstStatement* statseq)
{
  while (statseq) {
    if (statseq->IsReturn()) return true;
    if (statseq->IsIf()) {
      // if "if-body" and "else-body" both returns, immediately return true
      // or we have to proceed
      auto ifst = static_cast<CAstStatIf *>(statseq);
      if (AllPathsReturn(ifst->GetIfBody()) &&
          AllPathsReturn(ifst->GetElseBody())) {
        return true;
      }
    }
    if (statseq->IsWhile()) {
      // if while body returns, immediately return true
      // or we have to proceed
      auto whilest = static_cast<CAstStatWhile *>(statseq);
      if (AllPathsReturn(whilest->GetBody())) return true;
    }
    statseq = statseq->GetNext();
  }
  // we found a path that has no return statement!
  return false;
}


//------------------------------------------------------------------------------
// CAstType
//
CAstType::CAstType(CToken t, const CType *type)
  : CAstNode(t), _type(type)
{
  assert(type != NULL);
}

const CType* CAstType::GetType(void) const
{
  return _type;
}

ostream& CAstType::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "CAstType (" << _type << ")" << endl;
  return out;
}


//------------------------------------------------------------------------------
// CAstStatement
//
CAstStatement::CAstStatement(CToken token)
  : CAstNode(token), _next(NULL)
{
}

CAstStatement::~CAstStatement(void)
{
  delete _next;
}

void CAstStatement::SetNext(CAstStatement *next)
{
  _next = next;
}

CAstStatement* CAstStatement::GetNext(void) const
{
  return _next;
}

CTacAddr* CAstStatement::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  // generate code for statement (assignment, if-else, etc.)
  // it is done by each subclass

  // jump to next
  cb->AddInstr(new CTacInstr(opGoto, next));

  return NULL;
}

void CAstStatement::SequenceToTac(CCodeBlock *cb, CAstStatement *s, CTacLabel *next)
{
  while (s != nullptr) {
    CTacLabel *next = cb->CreateLabel();
    s->ToTac(cb, next);
    cb->AddInstr(next);
    s = s->GetNext();
  }
  if (next)
    cb->AddInstr(new CTacInstr(opGoto, next));
}


//------------------------------------------------------------------------------
// CAstStatAssign
//
CAstStatAssign::CAstStatAssign(CToken t,
                               CAstDesignator *lhs, CAstExpression *rhs)
  : CAstStatement(t), _lhs(lhs), _rhs(rhs)
{
  assert(lhs != NULL);
  assert(rhs != NULL);
}

CAstDesignator* CAstStatAssign::GetLHS(void) const
{
  return _lhs;
}

CAstExpression* CAstStatAssign::GetRHS(void) const
{
  return _rhs;
}

bool CAstStatAssign::TypeCheck(CToken *t, string *msg) const
{
  if (!_lhs->TypeCheck(t, msg)) return false;
  if (!_rhs->TypeCheck(t, msg)) return false;

  auto typeLHS = _lhs->GetType();
  auto typeRHS = _rhs->GetType();

  // We will support the array assignment
  // But only non-open arrays to avoid runtime error
  // So if the type of the operand is a pointer, it must be an array
  // and we unwrap to do type check properly

  if (typeLHS->IsPointer())
    typeLHS = static_cast<const CPointerType *>(typeLHS)->GetBaseType();
  if (typeRHS->IsPointer())
    typeRHS = static_cast<const CPointerType *>(typeRHS)->GetBaseType();

  if (!typeLHS->Compare(typeRHS)) {
    if (t != nullptr) *t = GetToken();
    if (msg != nullptr) {
      ostringstream o;
      o << "incompatible types in assignment:" << endl;
      o << "  LHS: "; _lhs->GetType()->print(o, 0); o << endl;
      o << "  RHS: "; _rhs->GetType()->print(o, 0); o << endl;
      *msg = o.str();
    }
    return false;
  }

  // There must be no open dimension
  if (typeLHS->IsArray()) {
    auto arr = static_cast<const CArrayType *>(typeLHS);

    for (;;) {
      if (arr->GetNElem() == CArrayType::OPEN) {
        if (t) *t = GetToken();
        if (msg) {
          ostringstream o;
          o << "assignments to open arrays are not supported." << endl;
          o << "  LHS: "; _lhs->GetType()->print(o, 0); o << endl;
          o << "  RHS: "; _rhs->GetType()->print(o, 0); o << endl;
          *msg = o.str();
        }
        return false;
      }
      auto base = arr->GetBaseType();
      if (!base->IsArray()) break;
      arr = static_cast<const CArrayType *>(base);
    }
  }

  return true;
}

const CType* CAstStatAssign::GetType(void) const
{
  return _lhs->GetType();
}

ostream& CAstStatAssign::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << ":=" << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  _lhs->print(out, indent+2);
  _rhs->print(out, indent+2);

  return out;
}

string CAstStatAssign::dotAttr(void) const
{
  return " [label=\":=\",shape=box]";
}

void CAstStatAssign::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _lhs->toDot(out, indent);
  out << ind << dotID() << "->" << _lhs->dotID() << ";" << endl;
  _rhs->toDot(out, indent);
  out << ind << dotID() << "->" << _rhs->dotID() << ";" << endl;
}

CTacAddr* CAstStatAssign::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  auto lhs = _lhs->ToTac(cb);
  auto rhs = _rhs->ToTac(cb);

  // Assign rhs to lhs
  cb->AddInstr(new CTacInstr(opAssign, lhs, rhs, nullptr));

  // Go to next
  return CAstStatement::ToTac(cb, next);
}


//------------------------------------------------------------------------------
// CAstStatCall
//
CAstStatCall::CAstStatCall(CToken t, CAstFunctionCall *call)
  : CAstStatement(t), _call(call)
{
  assert(call != NULL);
}

CAstFunctionCall* CAstStatCall::GetCall(void) const
{
  return _call;
}

bool CAstStatCall::TypeCheck(CToken *t, string *msg) const
{
  return GetCall()->TypeCheck(t, msg);
}

ostream& CAstStatCall::print(ostream &out, int indent) const
{
  _call->print(out, indent);

  return out;
}

string CAstStatCall::dotID(void) const
{
  return _call->dotID();
}

string CAstStatCall::dotAttr(void) const
{
  return _call->dotAttr();
}

void CAstStatCall::toDot(ostream &out, int indent) const
{
  _call->toDot(out, indent);
}

CTacAddr* CAstStatCall::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  // Function call
  _call->ToTac(cb);

  // Go to next
  return CAstStatement::ToTac(cb, next);
}


//------------------------------------------------------------------------------
// CAstStatReturn
//
CAstStatReturn::CAstStatReturn(CToken t, CAstScope *scope, CAstExpression *expr)
  : CAstStatement(t), _scope(scope), _expr(expr)
{
  assert(scope != NULL);
}

CAstScope* CAstStatReturn::GetScope(void) const
{
  return _scope;
}

CAstExpression* CAstStatReturn::GetExpression(void) const
{
  return _expr;
}

bool CAstStatReturn::TypeCheck(CToken *t, string *msg) const
{
  // This code has been copied from Illustration 1.
  const CType *st = GetScope()->GetType();
  CAstExpression *e = GetExpression();
  if (st->Match(CTypeManager::Get()->GetNull())) {
    if (e != NULL) {
      if (t != NULL) *t = e->GetToken();
      if (msg != NULL) *msg = "superfluous expression after return.";
      return false;
    }
  } else {
    if (e == NULL) {
      if (t != NULL) *t = GetToken();
      if (msg != NULL) *msg = "expression expected after return.";
      return false;
    }
    if (!e->TypeCheck(t, msg)) return false;
    if (!st->Match(e->GetType())) {
      if (t != NULL) *t = e->GetToken();
      if (msg != NULL) *msg = "return type mismatch.";
      return false;
    }
  }
  return true;
}

const CType* CAstStatReturn::GetType(void) const
{
  const CType *t = NULL;

  if (GetExpression() != NULL) {
    t = GetExpression()->GetType();
  } else {
    t = CTypeManager::Get()->GetNull();
  }

  return t;
}

ostream& CAstStatReturn::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "return" << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  if (_expr != NULL) _expr->print(out, indent+2);

  return out;
}

string CAstStatReturn::dotAttr(void) const
{
  return " [label=\"return\",shape=box]";
}

void CAstStatReturn::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  if (_expr != NULL) {
    _expr->toDot(out, indent);
    out << ind << dotID() << "->" << _expr->dotID() << ";" << endl;
  }
}

CTacAddr* CAstStatReturn::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  CTacAddr *expr = nullptr;

  // Get the return value if expr exists
  if (_expr != nullptr) expr = _expr->ToTac(cb);
  cb->AddInstr(new CTacInstr(opReturn, nullptr, expr, nullptr));

  // Do not need to go to next
  return nullptr;
}


//------------------------------------------------------------------------------
// CAstStatIf
//
CAstStatIf::CAstStatIf(CToken t, CAstExpression *cond,
                       CAstStatement *ifBody, CAstStatement *elseBody)
  : CAstStatement(t), _cond(cond), _ifBody(ifBody), _elseBody(elseBody)
{
  assert(cond != NULL);
}

CAstExpression* CAstStatIf::GetCondition(void) const
{
  return _cond;
}

CAstStatement* CAstStatIf::GetIfBody(void) const
{
  return _ifBody;
}

CAstStatement* CAstStatIf::GetElseBody(void) const
{
  return _elseBody;
}

bool CAstStatIf::TypeCheck(CToken *t, string *msg) const
{
  // Check cond.
  if (!_cond->TypeCheck(t, msg)) return false;

  // cond should be boolean type.
  if (!CTypeManager::Get()->GetBool()->Match(_cond->GetType())) {
    if (t != nullptr) *t = _cond->GetToken();
    if (msg != nullptr) *msg = "boolean expression expected.";
    return false;
  }

  // Check bodies.
  if (_ifBody && !_ifBody->TypeCheck(t, msg)) return false;
  if (_elseBody && !_elseBody->TypeCheck(t, msg)) return false;

  return true;
}

ostream& CAstStatIf::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "if cond" << endl;
  _cond->print(out, indent+2);
  out << ind << "if-body" << endl;
  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;
  out << ind << "else-body" << endl;
  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;

  return out;
}

string CAstStatIf::dotAttr(void) const
{
  return " [label=\"if\",shape=box]";
}

void CAstStatIf::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }

  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr* CAstStatIf::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  auto true_label = cb->CreateLabel("if_true");
  auto false_label = cb->CreateLabel("if_false");

  // branch
  auto cond = _cond->ToTac(cb);
  cb->AddInstr(new CTacInstr(opEqual, true_label, cond, new CTacConst(1)));
  cb->AddInstr(new CTacInstr(opGoto, false_label, nullptr, nullptr));

  // true part
  cb->AddInstr(true_label);
  CAstStatement::SequenceToTac(cb, _ifBody, next);

  // false part
  cb->AddInstr(false_label);
  CAstStatement::SequenceToTac(cb, _elseBody, next);

  return nullptr;
}


//------------------------------------------------------------------------------
// CAstStatWhile
//
CAstStatWhile::CAstStatWhile(CToken t,
                             CAstExpression *cond, CAstStatement *body)
  : CAstStatement(t), _cond(cond), _body(body)
{
  assert(cond != NULL);
}

CAstExpression* CAstStatWhile::GetCondition(void) const
{
  return _cond;
}

CAstStatement* CAstStatWhile::GetBody(void) const
{
  return _body;
}

bool CAstStatWhile::TypeCheck(CToken *t, string *msg) const
{
  // Check cond.
  if (!_cond->TypeCheck(t, msg)) return false;

  // cond should be boolean type.
  if (!CTypeManager::Get()->GetBool()->Match(_cond->GetType())) {
    if (t != nullptr) *t = _cond->GetToken();
    if (msg != nullptr) *msg = "boolean expression expected.";
    return false;
  }

  // Check body.
  if (_body && !_body->TypeCheck(t, msg)) return false;

  return true;
}

ostream& CAstStatWhile::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "while cond" << endl;
  _cond->print(out, indent+2);
  out << ind << "while-body" << endl;
  if (_body != NULL) {
    CAstStatement *s = _body;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  }
  else out << ind << "  empty." << endl;

  return out;
}

string CAstStatWhile::dotAttr(void) const
{
  return " [label=\"while\",shape=box]";
}

void CAstStatWhile::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_body != NULL) {
    CAstStatement *s = _body;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr* CAstStatWhile::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  auto cond_label = cb->CreateLabel("while_cond");
  auto body_label = cb->CreateLabel("while_body");

  // branch
  cb->AddInstr(cond_label);
  auto cond = _cond->ToTac(cb);
  cb->AddInstr(new CTacInstr(opEqual, body_label, cond, new CTacConst(1)));
  cb->AddInstr(new CTacInstr(opGoto, next, nullptr, nullptr));

  // loop body
  cb->AddInstr(body_label);
  CAstStatement::SequenceToTac(cb, _body, cond_label);

  return nullptr;
}


//------------------------------------------------------------------------------
// CAstExpression
//
CAstExpression::CAstExpression(CToken t)
  : CAstNode(t)
{
}


//------------------------------------------------------------------------------
// CAstOperation
//
CAstOperation::CAstOperation(CToken t, EOperation oper)
  : CAstExpression(t), _oper(oper)
{
}

EOperation CAstOperation::GetOperation(void) const
{
  return _oper;
}


//------------------------------------------------------------------------------
// CAstBinaryOp
//
CAstBinaryOp::CAstBinaryOp(CToken t, EOperation oper,
                           CAstExpression *l,CAstExpression *r)
  : CAstOperation(t, oper), _left(l), _right(r)
{
  // these are the only binary operation we support for now
  assert((oper == opAdd)        || (oper == opSub)         ||
         (oper == opMul)        || (oper == opDiv)         ||
         (oper == opAnd)        || (oper == opOr)          ||
         (oper == opEqual)      || (oper == opNotEqual)    ||
         (oper == opLessThan)   || (oper == opLessEqual)   ||
         (oper == opBiggerThan) || (oper == opBiggerEqual)
        );
  assert(l != NULL);
  assert(r != NULL);
}

CAstExpression* CAstBinaryOp::GetLeft(void) const
{
  return _left;
}

CAstExpression* CAstBinaryOp::GetRight(void) const
{
  return _right;
}

bool CAstBinaryOp::TypeCheck(CToken *t, string *msg) const
{
  // Check each operand.
  auto lhs = GetLeft();
  auto rhs = GetRight();

  assert(lhs != nullptr && rhs != nullptr);

  if (!lhs->TypeCheck(t, msg)) return false;
  if (!rhs->TypeCheck(t, msg)) return false;

  // Get singletons for the base types.
  auto typeInt = CTypeManager::Get()->GetInt();
  auto typeChar = CTypeManager::Get()->GetChar();
  auto typeBool = CTypeManager::Get()->GetBool();

  auto typeLHS = lhs->GetType();
  auto typeRHS = rhs->GetType();

  switch (GetOperation()) {

    // Both lhs and rhs should have integer type.
    case opAdd: case opSub:
    case opMul: case opDiv:
      if (typeInt->Match(typeLHS) &&
          typeInt->Match(typeRHS)) return true;
      break;

    // Both lhs and rhs should have boolean type.
    case opAnd: case opOr:
      if (typeBool->Match(typeLHS) &&
          typeBool->Match(typeRHS)) return true;
      break;

    // Both lhs and rhs should have same type.
    case opEqual: case opNotEqual:
      if (typeBool->Match(typeLHS) &&
          typeBool->Match(typeRHS)) return true;
      // break; fall through
    case opLessThan: case opLessEqual:
    case opBiggerThan: case opBiggerEqual:
      if (typeChar->Match(typeLHS) &&
          typeChar->Match(typeRHS)) return true;
      if (typeInt->Match(typeLHS) &&
          typeInt->Match(typeRHS)) return true;
      break;

    // invalid binary operator
    default:
      assert(false);
  }

  if (t != nullptr) *t = GetToken();
  if (msg != nullptr) {
    ostringstream o;
    switch (GetOperation()) {
      case opAdd: o << "add: "; break;
      case opSub: o << "sub: "; break;
      case opMul: o << "mul: "; break;
      case opDiv: o << "div: "; break;
      case opAnd: o << "and: "; break;
      case opOr:  o << "or: ";  break;
      default: o << GetToken().GetValue() << ": "; break;
    }
    o << "type mismatch." << endl;
    o << "  left  operand: "; typeLHS->print(o, 0); o << endl;
    o << "  right operand: "; typeRHS->print(o, 0); o << endl;
    *msg = o.str();
  }
  return false;
}

const CType* CAstBinaryOp::GetType(void) const
{
  // Assume TypeCheck of itself is true.

  switch (GetOperation()) {
    case opAdd: case opSub:
    case opMul: case opDiv:
      return CTypeManager::Get()->GetInt();
    case opAnd: case opOr:
    case opEqual: case opNotEqual:
    case opLessThan: case opLessEqual:
    case opBiggerThan: case opBiggerEqual:
      return CTypeManager::Get()->GetBool();
    default: // invalid binary operator
      assert(false);
  }
}

ostream& CAstBinaryOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  _left->print(out, indent+2);
  _right->print(out, indent+2);

  return out;
}

string CAstBinaryOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstBinaryOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _left->toDot(out, indent);
  out << ind << dotID() << "->" << _left->dotID() << ";" << endl;
  _right->toDot(out, indent);
  out << ind << dotID() << "->" << _right->dotID() << ";" << endl;
}

CTacAddr* CAstBinaryOp::ToTac(CCodeBlock *cb)
{
  auto oper = GetOperation();
  auto ret = cb->CreateTemp(GetType());

  if (oper == opAnd || oper == opOr) {
    // short-circuit
    auto next_label = cb->CreateLabel();
    auto short_label = cb->CreateLabel();
    auto short_val = new CTacConst(oper == opAnd ? 0 : 1);

    auto left = _left->ToTac(cb);
    cb->AddInstr(new CTacInstr(opEqual, short_label, left, short_val));

    auto right = _right->ToTac(cb);
    cb->AddInstr(new CTacInstr(opAssign, ret, right, nullptr));
    cb->AddInstr(new CTacInstr(opGoto, next_label, nullptr, nullptr));

    cb->AddInstr(short_label);
    cb->AddInstr(new CTacInstr(opAssign, ret, short_val, nullptr));

    cb->AddInstr(next_label);
  } else if (IsRelOp(oper)) {
    // Non short-circuit
    auto true_label = cb->CreateLabel();
    auto next_label = cb->CreateLabel();

    auto left = _left->ToTac(cb);
    auto right = _right->ToTac(cb);
    cb->AddInstr(new CTacInstr(oper, true_label, left, right));

    cb->AddInstr(new CTacInstr(opAssign, ret, new CTacConst(0), nullptr));
    cb->AddInstr(new CTacInstr(opGoto, next_label, nullptr, nullptr));

    cb->AddInstr(true_label);
    cb->AddInstr(new CTacInstr(opAssign, ret, new CTacConst(1), nullptr));

    cb->AddInstr(next_label);
  } else {
    // Integer op
    auto left = _left->ToTac(cb);
    auto right = _right->ToTac(cb);

    cb->AddInstr(new CTacInstr(oper, ret, left, right));
  }

  return ret;
}


//------------------------------------------------------------------------------
// CAstUnaryOp
//
CAstUnaryOp::CAstUnaryOp(CToken t, EOperation oper, CAstExpression *e)
  : CAstOperation(t, oper), _operand(e)
{
  assert((oper == opNeg) || (oper == opPos) || (oper == opNot));
  assert(e != NULL);
}

CAstExpression* CAstUnaryOp::GetOperand(void) const
{
  return _operand;
}

bool CAstUnaryOp::TypeCheck(CToken *t, string *msg) const
{
  // Check the operand.
  auto operand = GetOperand();

  assert(operand != nullptr);

  if (!operand->TypeCheck(t, msg)) return false;

  // Get singletons for some of the base types.
  auto typeInt = CTypeManager::Get()->GetInt();
  auto typeBool = CTypeManager::Get()->GetBool();

  auto typeOperand = operand->GetType();

  switch (GetOperation()) {

    // '+'/'-'
    case opNeg: case opPos:
      if (typeInt->Match(typeOperand)) return true;
      break;

    // '!'
    case opNot:
      if (typeBool->Match(typeOperand)) return true;
      break;

    // invalid unary operator
    default:
      assert(false);
  }

  if (t != nullptr) *t = GetToken();
  if (msg != nullptr) {
    ostringstream o;
    switch (GetOperation()) {
      case opNeg: o << "neg: "; break;
      case opPos: o << "pos: "; break;
      case opNot: o << "not: "; break;
    }
    o << "type mismatch." << endl;
    o << "  operand: "; typeOperand->print(o, 6); o << endl;
    *msg = o.str();
  }
  return false;
}

const CType* CAstUnaryOp::GetType(void) const
{
  // Assume TypeCheck of itself is true.

  switch (GetOperation()) {
    case opNeg: case opPos:
      return CTypeManager::Get()->GetInt();
    case opNot:
      return CTypeManager::Get()->GetBool();
    default: // invalid unary operator
      assert(false);
  }
}

ostream& CAstUnaryOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  _operand->print(out, indent+2);

  return out;
}

string CAstUnaryOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstUnaryOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr* CAstUnaryOp::ToTac(CCodeBlock *cb)
{
  // Create variable to hold the result
  auto ret = cb->CreateTemp(GetType());

  // Add the operation
  auto operand = _operand->ToTac(cb);
  cb->AddInstr(new CTacInstr(GetOperation(), ret, operand, nullptr));

  return ret;
}


//------------------------------------------------------------------------------
// CAstSpecialOp
//
CAstSpecialOp::CAstSpecialOp(CToken t, EOperation oper, CAstExpression *e,
                             const CType *type)
  : CAstOperation(t, oper), _operand(e), _type(type)
{
  assert((oper == opAddress) || (oper == opDeref) || (oper = opCast));
  assert(e != NULL);
  assert(((oper != opCast) && (type == NULL)) ||
         ((oper == opCast) && (type != NULL)));
}

CAstExpression* CAstSpecialOp::GetOperand(void) const
{
  return _operand;
}

bool CAstSpecialOp::TypeCheck(CToken *t, string *msg) const
{
  // Check the operand.
  auto operand = GetOperand();
  assert(operand != nullptr);
  if (!operand->TypeCheck(t, msg)) return false;

  // Only pointer type can be dereferenced.
  if (GetOperation() == opDeref &&
      !operand->GetType()->IsPointer()) {
    if (t != nullptr) *t = GetToken();
    if (msg != nullptr) *msg = "dereference non-pointer";
    return false;
  }

  return true;
}

const CType* CAstSpecialOp::GetType(void) const
{
  // Assume TypeCheck of itself is true.

  auto typeOperand = GetOperand()->GetType();
  switch (GetOperation()) {
    case opAddress:
      return CTypeManager::Get()->GetPointer(typeOperand);
    case opDeref:
      return static_cast<const CPointerType *>(typeOperand)->GetBaseType();
    case opCast:
      return _type;
    default: // invalid special operator
      assert(false);
  }
}

ostream& CAstSpecialOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  _operand->print(out, indent+2);

  return out;
}

string CAstSpecialOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstSpecialOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr* CAstSpecialOp::ToTac(CCodeBlock *cb)
{
  // Create variable to hold the result
  auto ret = cb->CreateTemp(GetType());

  // Add the operation
  auto operand = _operand->ToTac(cb);
  cb->AddInstr(new CTacInstr(GetOperation(), ret, operand, nullptr));

  return ret;
}


//------------------------------------------------------------------------------
// CAstFunctionCall
//
CAstFunctionCall::CAstFunctionCall(CToken t, const CSymProc *symbol)
  : CAstExpression(t), _symbol(symbol)
{
  assert(symbol != NULL);
}

const CSymProc* CAstFunctionCall::GetSymbol(void) const
{
  return _symbol;
}

void CAstFunctionCall::AddArg(CAstExpression *arg)
{
  _arg.push_back(arg);
}

int CAstFunctionCall::GetNArgs(void) const
{
  return (int)_arg.size();
}

CAstExpression* CAstFunctionCall::GetArg(int index) const
{
  assert((index >= 0) && (index < _arg.size()));
  return _arg[index];
}

bool CAstFunctionCall::TypeCheck(CToken *t, string *msg) const
{
  auto symbol = GetSymbol();

  // Check the number of parameters.
  int expected = symbol->GetNParams();
  int actual = GetNArgs();
  if (expected != actual) {
    if (t != nullptr) *t = GetToken();
    if (msg != nullptr)
      *msg = expected > actual ? "not enough arguments." : "too many arguments.";
    return false;
  }

  // Check each parameter.
  for (int i = 0; i < expected; i++) {
    // Check passed argument itself.
    auto arg = GetArg(i);
    if (!arg->TypeCheck(t, msg)) return false;
    // Check whether the type matches.
    auto typeParam = symbol->GetParam(i)->GetDataType();
    auto typeArg = arg->GetType();
    if (!typeParam->Match(typeArg)) {
      if (t != nullptr) *t = arg->GetToken();
      if (msg != nullptr) {
        ostringstream o;
        o << "parameter " << (i + 1) << ": argument type mismatch." << endl;
        o << "  expected "; typeParam->print(o, 0); o << endl;
        o << "  got      "; typeArg->print(o, 0); o << endl;
        *msg = o.str();
      }
      return false;
    }
  }

  return true;
}

const CType* CAstFunctionCall::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

ostream& CAstFunctionCall::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "call " << _symbol << " ";
  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  for (size_t i=0; i<_arg.size(); i++) {
    _arg[i]->print(out, indent+2);
  }

  return out;
}

string CAstFunctionCall::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"call " << _symbol->GetName() << "\",shape=box]";
  return out.str();
}

void CAstFunctionCall::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i=0; i<_arg.size(); i++) {
    _arg[i]->toDot(out, indent);
    out << ind << dotID() << "->" << _arg[i]->dotID() << ";" << endl;
  }
}

CTacAddr* CAstFunctionCall::ToTac(CCodeBlock *cb)
{
  // Evaluate and set up parameters
  for(size_t i = _arg.size() - 1; i != -1; i--) {
    auto a = _arg[i]->ToTac(cb);
    cb->AddInstr(new CTacInstr(opParam, new CTacConst(i), a, nullptr));
  }

  // Prepare temporary variable for getting the return value
  CTacTemp *ret_val = nullptr;
  auto ret_type = GetType();
  if (!ret_type->IsNull())
    ret_val = cb->CreateTemp(ret_type);

  // Call the function
  cb->AddInstr(new CTacInstr(opCall, ret_val, new CTacName(_symbol), nullptr));

  return ret_val;
}



//------------------------------------------------------------------------------
// CAstOperand
//
CAstOperand::CAstOperand(CToken t)
  : CAstExpression(t)
{
}


//------------------------------------------------------------------------------
// CAstDesignator
//
CAstDesignator::CAstDesignator(CToken t, const CSymbol *symbol)
  : CAstOperand(t), _symbol(symbol)
{
  assert(symbol != NULL);
}

const CSymbol* CAstDesignator::GetSymbol(void) const
{
  return _symbol;
}

bool CAstDesignator::TypeCheck(CToken *t, string *msg) const
{
  return true;
}

const CType* CAstDesignator::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

ostream& CAstDesignator::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstDesignator::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << _symbol->GetName();
  out << "\",shape=ellipse]";
  return out.str();
}

void CAstDesignator::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);
}

CTacAddr* CAstDesignator::ToTac(CCodeBlock *cb)
{
  return new CTacName(_symbol);
}


//------------------------------------------------------------------------------
// CAstArrayDesignator
//
CAstArrayDesignator::CAstArrayDesignator(CToken t, const CSymbol *symbol)
  : CAstDesignator(t, symbol), _done(false)
{
}

void CAstArrayDesignator::AddIndex(CAstExpression *idx)
{
  assert(!_done);
  _idx.push_back(idx);
}

void CAstArrayDesignator::IndicesComplete(void)
{
  assert(!_done);
  _done = true;
}

int CAstArrayDesignator::GetNIndices(void) const
{
  return (int)_idx.size();
}

CAstExpression* CAstArrayDesignator::GetIndex(int index) const
{
  assert((index >= 0) && (index < _idx.size()));
  return _idx[index];
}

bool CAstArrayDesignator::TypeCheck(CToken *t, string *msg) const
{
  assert(_done);

  // Check dimension.
  if (GetType() == nullptr) {
    if (t != nullptr) *t = GetToken();
    if (msg != nullptr) *msg = "invalid array expression.";
    return false;
  }

  // Check each indices is an integer.
  auto typeInt = CTypeManager::Get()->GetInt();
  for (int i = 0; i < GetNIndices(); i++) {
    auto idx = GetIndex(i);
    if (!idx->TypeCheck(t, msg)) return false;
    if (!typeInt->Compare(idx->GetType())) {
      if (t != nullptr) *t = idx->GetToken();
      if (msg != nullptr) *msg = "invalid array index expression.";
      return false;
    }
  }

  return true;
}

const CType* CAstArrayDesignator::GetType(void) const
{
  // Assume TypeCheck of itself is true.

  auto symbol = GetSymbol();
  assert(symbol != nullptr);

  auto type = symbol->GetDataType();
  assert(type != nullptr);

  if (type->IsPointer())
    type = static_cast<const CPointerType *>(type)->GetBaseType();

  for (int i = 0; i < GetNIndices(); i++) {
    if (!type->IsArray()) return nullptr;
    type = static_cast<const CArrayType *>(type)->GetInnerType();
  }

  return type->IsArray() ? CTypeManager::Get()->GetPointer(type) : type;
}

ostream& CAstArrayDesignator::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  for (size_t i=0; i<_idx.size(); i++) {
    _idx[i]->print(out, indent+2);
  }

  return out;
}

string CAstArrayDesignator::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << _symbol->GetName() << "[]\",shape=ellipse]";
  return out.str();
}

void CAstArrayDesignator::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i=0; i<_idx.size(); i++) {
    _idx[i]->toDot(out, indent);
    out << ind << dotID() << "-> " << _idx[i]->dotID() << ";" << endl;
  }
}

CTacAddr* CAstArrayDesignator::ToTac(CCodeBlock *cb)
{
  const CArrayType *arr;
  const CType *basetype;
  CTacAddr *arrptr;

  auto dimsym = cb->GetOwner()->GetSymbolTable()->FindSymbol("DIM");

  if (_symbol->GetDataType()->IsArray()) {
    arr = static_cast<const CArrayType *>(_symbol->GetDataType());
    arrptr = cb->CreateTemp(CTypeManager::Get()->GetPointer(_symbol->GetDataType()));
    cb->AddInstr(new CTacInstr(opAddress, arrptr, new CTacName(_symbol)));
  } else {
    auto ptrtype = static_cast<const CPointerType *>(_symbol->GetDataType());
    assert(ptrtype->GetBaseType()->IsArray());
    arr = static_cast<const CArrayType *>(ptrtype->GetBaseType());
    arrptr = new CTacName(_symbol);
  }

  basetype = arr->GetBaseType();
  vector<CTacTemp *> dims;
  int align = arr->GetAlign();

  for (int i = 2; i <= arr->GetNDim(); i++) {
    auto dimvar = cb->CreateTemp(CTypeManager::Get()->GetInt());
    cb->AddInstr(new CTacInstr(opParam, new CTacConst(1), new CTacConst(i)));
    cb->AddInstr(new CTacInstr(opParam, new CTacConst(0), arrptr));
    cb->AddInstr(new CTacInstr(opCall, dimvar, new CTacName(dimsym)));
    if (i == arr->GetNDim() && basetype->GetSize() & (align - 1)) {
      auto aligned = cb->CreateTemp(CTypeManager::Get()->GetInt());
      auto result = cb->CreateTemp(CTypeManager::Get()->GetInt());
      cb->AddInstr(new CTacInstr(opAdd, aligned, new CTacConst(align - 1), dimvar));
      cb->AddInstr(new CTacInstr(opAnd, result, new CTacConst(~(align - 1)), aligned));
      dimvar = result;
    }
    dims.push_back(dimvar);
  }

  CTacAddr *result_ptr = arrptr;
  const CType *result_type = arr;
  for (int i = 0; i < GetNIndices(); i++) {
    auto newaddr = cb->CreateTemp(CTypeManager::Get()->GetInt());
    auto jmplen = cb->CreateTemp(CTypeManager::Get()->GetInt());
    auto metasize = new CTacConst(4 * (arr->GetNDim() + 1 - i));
    auto offset = cb->CreateTemp(CTypeManager::Get()->GetInt());

    if (dims.size() > i) {
      auto arraysize = GetArraySize(cb, &dims[i], dims.size() - i, arr->GetBaseType()->GetSize());
      cb->AddInstr(new CTacInstr(opMul, jmplen, arraysize, GetIndex(i)->ToTac(cb)));
    } else {
      cb->AddInstr(new CTacInstr(opMul, jmplen, new CTacConst(GetType()->GetSize()), GetIndex(i)->ToTac(cb)));
    }
    cb->AddInstr(new CTacInstr(opAdd, offset, jmplen, metasize));
    cb->AddInstr(new CTacInstr(opAdd, newaddr, result_ptr, offset));
    result_ptr = newaddr;
    result_type = static_cast<const CArrayType *>(result_type)->GetInnerType();
  }

  CTacAddr *result;

  if (result_type->IsScalar()) {
    result = new CTacReference(static_cast<CTacTemp *>(result_ptr)->GetSymbol(), result_type);
  } else {
    auto type = CTypeManager::Get()->GetPointer(result_type);
    auto ptr = cb->CreateTemp(type);
    cb->AddInstr(new CTacInstr(opAssign, ptr, result_ptr));
    result = new CTacTemp(static_cast<CTacTemp *>(ptr)->GetSymbol());
  }

  return result;
}

CTacAddr *CAstArrayDesignator::GetArraySize(
  CCodeBlock *cb, CTacTemp *dimarray[], int arrsize, int basesize)
{
  vector<CTacTemp *> dim_multiply;
  dim_multiply.push_back(dimarray[0]);

  for (int i = 1; i < arrsize; i++) {
    CTacTemp *next = cb->CreateTemp(CTypeManager::Get()->GetInt());
    cb->AddInstr(new CTacInstr(opMul, next, dim_multiply.back(), dimarray[i]));
    dim_multiply.push_back(next);
  }

  CTacAddr *result = new CTacConst(4 * (arrsize + 1));

  for (int i = 0, factor = 4 * arrsize; i < arrsize; i++, factor -= 4) {
    auto tacfactor = new CTacConst(factor != 4 ? factor : basesize);
    CTacTemp *tmp = cb->CreateTemp(CTypeManager::Get()->GetInt());
    cb->AddInstr(new CTacInstr(opMul, tmp, new CTacConst(factor), dim_multiply[i]));
    CTacTemp *tmp2 = cb->CreateTemp(CTypeManager::Get()->GetInt());
    cb->AddInstr(new CTacInstr(opAdd, tmp2, tmp, result));
    result = tmp2;
  }

  return result;
}


//------------------------------------------------------------------------------
// CAstConstant
//
CAstConstant::CAstConstant(CToken t, const CType *type, long long value)
  : CAstOperand(t), _type(type), _value(value)
{
}

void CAstConstant::SetValue(long long value)
{
  _value = value;
}

long long CAstConstant::GetValue(void) const
{
  return _value;
}

string CAstConstant::GetValueStr(void) const
{
  ostringstream out;

  if (GetType() == CTypeManager::Get()->GetBool()) {
    out << (_value == 0 ? "false" : "true");
  } else if (GetType() == CTypeManager::Get()->GetChar()) {
    out << '\'' << GetToken().GetValue() << '\'';
  } else {
    out << dec << _value;
  }

  return out.str();
}

bool CAstConstant::TypeCheck(CToken *t, string *msg) const
{
  return true;
}

const CType* CAstConstant::GetType(void) const
{
  return _type;
}

ostream& CAstConstant::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetValueStr() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstConstant::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetValueStr() << "\",shape=ellipse]";
  return out.str();
}

CTacAddr* CAstConstant::ToTac(CCodeBlock *cb)
{
  // Just return the value without adding code to cb
  return new CTacConst(static_cast<int>(_value));
}


//------------------------------------------------------------------------------
// CAstStringConstant
//
int CAstStringConstant::_idx = 0;

CAstStringConstant::CAstStringConstant(CToken t, const string value,
                                       CAstScope *s)
  : CAstOperand(t)
{
  CTypeManager *tm = CTypeManager::Get();

  _type = tm->GetArray(CToken::unescape(value).size()+1, tm->GetChar());
  _value = new CDataInitString(value);

  ostringstream o;
  o << "_str_$" << ++_idx;

  _sym = new CSymGlobal(o.str(), _type);
  _sym->SetData(_value);
  s->GetSymbolTable()->AddSymbol(_sym);
}

const string CAstStringConstant::GetValue(void) const
{
  return _value->GetData();
}

const string CAstStringConstant::GetValueStr(void) const
{
  return GetValue();
}

bool CAstStringConstant::TypeCheck(CToken *t, string *msg) const
{
  return true;
}

const CType* CAstStringConstant::GetType(void) const
{
  return _type;
}

ostream& CAstStringConstant::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << '"' << GetValueStr() << '"' << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstStringConstant::dotAttr(void) const
{
  ostringstream out;
  // the string is already escaped, but dot requires double escaping
  out << " [label=\"\\\"" << CToken::escape(GetValueStr())
      << "\\\"\",shape=ellipse]";
  return out.str();
}

CTacAddr* CAstStringConstant::ToTac(CCodeBlock *cb)
{
  // Just return the symbol without adding code to cb
  return new CTacName(_sym);
}


