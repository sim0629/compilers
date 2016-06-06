//------------------------------------------------------------------------------
/// @brief SnuPL backend
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/11/28 Bernhard Egger created
/// 2013/06/09 Bernhard Egger adapted to SnuPL/0
/// 2016/04/04 Bernhard Egger adapted to SnuPL/1
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

#include <fstream>
#include <sstream>
#include <iomanip>
#include <cassert>
#include <algorithm>

#include "backend.h"
using namespace std;


//------------------------------------------------------------------------------
// CBackend
//
CBackend::CBackend(ostream &out)
  : _out(out)
{
}

CBackend::~CBackend(void)
{
}

bool CBackend::Emit(CModule *m)
{
  assert(m != NULL);
  _m = m;

  if (!_out.good()) return false;

  bool res = true;

  try {
    EmitHeader();
    EmitCode();
    EmitData();
    EmitFooter();

    res = _out.good();
  } catch (...) {
    res = false;
  }

  return res;
}

void CBackend::EmitHeader(void)
{
}

void CBackend::EmitCode(void)
{
}

void CBackend::EmitData(void)
{
}

void CBackend::EmitFooter(void)
{
}


//------------------------------------------------------------------------------
// CBackendx86
//
CBackendx86::CBackendx86(ostream &out)
  : CBackend(out), _curr_scope(NULL)
{
  _ind = string(4, ' ');
}

CBackendx86::~CBackendx86(void)
{
}

void CBackendx86::EmitHeader(void)
{
  _out << "##################################################" << endl
       << "# " << _m->GetName() << endl
       << "#" << endl
       << endl;
}

void CBackendx86::EmitCode(void)
{
  _out << _ind << "#-----------------------------------------" << endl
       << _ind << "# text section" << endl
       << _ind << "#" << endl
       << _ind << ".text" << endl
       << _ind << ".align 4" << endl
       << endl
       << _ind << "# entry point and pre-defined functions" << endl
       << _ind << ".global main" << endl
       << _ind << ".extern DIM" << endl
       << _ind << ".extern DOFS" << endl
       << _ind << ".extern ReadInt" << endl
       << _ind << ".extern WriteInt" << endl
       << _ind << ".extern WriteStr" << endl
       << _ind << ".extern WriteChar" << endl
       << _ind << ".extern WriteLn" << endl
       << endl;

  // Emit assembly code for subscopes of the module.
  for (auto subscope : _m->GetSubscopes())
    EmitScope(subscope);
  // Emit assembly code for the module itself.
  EmitScope(_m);

  _out << _ind << "# end of text section" << endl
       << _ind << "#-----------------------------------------" << endl
       << endl;
}

void CBackendx86::EmitData(void)
{
  _out << _ind << "#-----------------------------------------" << endl
       << _ind << "# global data section" << endl
       << _ind << "#" << endl
       << _ind << ".data" << endl
       << _ind << ".align 4" << endl
       << endl;

  EmitGlobalData(_m);

  _out << _ind << "# end of global data section" << endl
       << _ind << "#-----------------------------------------" << endl
       << endl;
}

void CBackendx86::EmitFooter(void)
{
  _out << _ind << ".end" << endl
       << "##################################################" << endl;
}

void CBackendx86::SetScope(CScope *scope)
{
  _curr_scope = scope;
}

CScope* CBackendx86::GetScope(void) const
{
  return _curr_scope;
}

void CBackendx86::EmitScope(CScope *scope)
{
  assert(scope != NULL);

  string label;

  if (scope->GetParent() == NULL) label = "main";
  else label = scope->GetName();

  // label
  _out << _ind << "# scope " << scope->GetName() << endl
       << label << ":" << endl;

  auto symtab = scope->GetSymbolTable();
  auto symbols = symtab->GetSymbols();
  stable_sort(symbols.begin(), symbols.end(), [](const CSymbol *a, const CSymbol *b){
    if (a->GetSymbolType() != b->GetSymbolType()) {
      int alocal = a->GetSymbolType() == stLocal;
      int blocal = b->GetSymbolType() == stLocal;
      return alocal > blocal;
    }
    if (a->GetSymbolType() != stLocal) return false;
    return a->GetDataType()->GetAlign() > b->GetDataType()->GetAlign();
  });

  int totalOffset = 0;
  int paramOffset = 4 + 4;
  // [ebp][ret][param1][param2][...][paramN]

  _out << _ind << "# stack offsets:" << endl;

  for (auto symbol : symbols) {
    if (symbol->GetSymbolType() == stLocal) {
      auto type = symbol->GetDataType();
      int size = type->GetSize();
      symbol->SetBaseRegister("%ebp");
      totalOffset -= size;
      // 12 for ebx, esi, edi
      symbol->SetOffset(totalOffset - 12);
    } else if (symbol->GetSymbolType() == stParam) {
      symbol->SetBaseRegister("%ebp");
      symbol->SetOffset(paramOffset);
      paramOffset += 4;
    }
  }

  totalOffset &= ~3;

  // emit function prologue
  _out << _ind << "# prologue" << endl;
  EmitInstruction("pushl", "%ebp");
  EmitInstruction("movl", "%esp, %ebp");
  EmitInstruction("pushl", "%ebx", "save callee saved registers");
  EmitInstruction("pushl", "%esi");
  EmitInstruction("pushl", "%edi");

  if (totalOffset) {
    char _tmp[100];
    sprintf(_tmp, "$%d, %%esp", -totalOffset);
    EmitInstruction("subl", _tmp, "make room for locals");
  }

  _out << endl;

  // memset
  if (totalOffset) {
    char _tmp[100];
    EmitInstruction("cld", "", "memset local stack area to 0");
    EmitInstruction("xorl", "%eax, %eax");
    sprintf(_tmp, "$%d, %%ecx", -totalOffset / 4);
    EmitInstruction("movl", _tmp);
    EmitInstruction("movl", "%esp, %edi");
    EmitInstruction("rep", "stosl");

    // init local array
    for (auto symbol : symbols) {
      if (symbol->GetSymbolType() == stLocal) {
        auto type = symbol->GetDataType();
        if (!type->IsArray()) continue;
        InitArray(symbol->GetBaseRegister(),
                  symbol->GetOffset(),
                  static_cast<const CArrayType *>(type),
                  symbol->GetName());
      }
    }
  }

  auto cb = scope->GetCodeBlock();
  SetScope(scope);
  EmitCodeBlock(cb);

  // emit function epilogue
  _out << "# epilogue" << endl;

  if (totalOffset) {
    char _tmp[100];
    sprintf(_tmp, "$%d, %%esp", -totalOffset);
    EmitInstruction("addl", _tmp, "remove locals");
  }

  EmitInstruction("popl", "%edi");
  EmitInstruction("popl", "%esi");
  EmitInstruction("popl", "%ebx");
  EmitInstruction("popl", "%ebp");
  EmitInstruction("ret");

  _out << endl;
}

void CBackendx86::EmitGlobalData(CScope *scope)
{
  assert(scope != NULL);

  // emit the globals for the current scope
  CSymtab *st = scope->GetSymbolTable();
  assert(st != NULL);

  bool header = false;

  vector<CSymbol*> slist = st->GetSymbols();
  stable_sort(slist.begin(), slist.end(), [](const CSymbol *a, const CSymbol *b){
   if (a->GetSymbolType() != b->GetSymbolType()) {
      int aglobal = a->GetSymbolType() == stGlobal;
      int bglobal = b->GetSymbolType() == stGlobal;
      return aglobal > bglobal;
    }
    if (a->GetSymbolType() != stGlobal) return false;
    return a->GetDataType()->GetAlign() > b->GetDataType()->GetAlign();
  });

  _out << dec;

  size_t size = 0;

  for (size_t i=0; i<slist.size(); i++) {
    CSymbol *s = slist[i];
    const CType *t = s->GetDataType();

    if (s->GetSymbolType() == stGlobal) {
      if (!header) {
        _out << _ind << "# scope: " << scope->GetName() << endl;
        header = true;
      }

      // insert alignment only when necessary
      if ((t->GetAlign() > 1) && (size % t->GetAlign() != 0)) {
        size += t->GetAlign() - size % t->GetAlign();
        _out << setw(4) << " " << ".align "
             << right << setw(3) << t->GetAlign() << endl;
      }

      _out << left << setw(36) << s->GetName() + ":" << "# " << t << endl;

      if (t->IsArray()) {
        InitGlobalArray(static_cast<const CArrayType *>(t));
      } else {
        const CDataInitializer *di = s->GetData();
        if (di != NULL) {
          const CDataInitString *sdi = dynamic_cast<const CDataInitString*>(di);
          assert(sdi != NULL);  // only support string data initializers for now

          _out << left << setw(4) << " "
            << ".asciz " << '"' << sdi->GetData() << '"' << endl;
        } else {
          _out  << left << setw(4) << " "
            << ".skip " << dec << right << setw(4) << t->GetDataSize()
            << endl;
        }
      }

      size += t->GetSize();
    }
  }

  _out << endl;

  // emit globals in subscopes (necessary if we support static local variables)
  vector<CScope*>::const_iterator sit = scope->GetSubscopes().begin();
  while (sit != scope->GetSubscopes().end()) EmitGlobalData(*sit++);
}

void CBackendx86::EmitLocalData(CScope *scope)
{
  assert(scope != NULL);

  // TODO TODO!
}

void CBackendx86::EmitCodeBlock(CCodeBlock *cb)
{
  assert(cb != NULL);

  const list<CTacInstr*> &instr = cb->GetInstr();
  list<CTacInstr*>::const_iterator it = instr.begin();

  while (it != instr.end()) EmitInstruction(*it++);
}

void CBackendx86::EmitInstruction(CTacInstr *i)
{
  assert(i != NULL);

  ostringstream cmt;
  string mnm;
  cmt << i;

  EOperation op = i->GetOperation();
  ostringstream inst;
  const CTacLabel *target;
  const CTacName *func;

  switch (op) {
    // binary operators
    // dst = src1 op src2
    case opAdd: case opSub:
    case opMul: case opDiv:
    case opAnd: case opOr:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx");
      if (op == opDiv) EmitInstruction("cdq");
      if (op == opMul || op == opDiv) {
        inst << "i" << op << "l";
        EmitInstruction(inst.str(), "%ebx");
      } else {
        inst << op << "l";
        EmitInstruction(inst.str(), "%ebx, %eax");
      }
      Store(i->GetDest(), 'a');
      break;

    // unary operators
    // dst = op src1
    case opNeg: case opPos:
    case opNot:
      Load(i->GetSrc(1), "%eax", cmt.str());
      if (op != opPos) {
        inst << op << "l";
        EmitInstruction(inst.str(), "%eax");
      }
      Store(i->GetDest(), 'a');
      break;

    // memory operations
    // dst = src1
    case opAssign:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Store(i->GetDest(), 'a');
      break;

    // pointer operations
    // dst = &src1
    // TODO
    // dst = *src1
    case opDeref:
      // opDeref not generated for now
      EmitInstruction("# opDeref", "not implemented", cmt.str());
      break;

    // unconditional branching
    // goto dst
    case opGoto:
      target = static_cast<const CTacLabel *>(i->GetDest());
      EmitInstruction("jmp", Label(target), cmt.str());
      break;

    // conditional branching
    // if src1 relOp src2 then goto dst
    case opEqual: case opNotEqual:
    case opLessThan: case opLessEqual:
    case opBiggerThan: case opBiggerEqual:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx");
      EmitInstruction("cmpl", "%ebx, %eax");
      inst << "j" << Condition(op);
      target = static_cast<const CTacLabel *>(i->GetDest());
      EmitInstruction(inst.str(), Label(target));
      break;

    // function call-related operations
    // dst = call src1
    case opCall:
      func = static_cast<const CTacName *>(i->GetSrc(1));
      EmitInstruction("call", func->GetSymbol()->GetName(), cmt.str());
      if (i->GetDest()) Store(i->GetDest(), 'a');
      break;
    // return [src1]
    case opReturn:
      if (i->GetSrc(1)) Load(i->GetSrc(1), "%eax", cmt.str());
      EmitInstruction("ret"); // it's trivial although there is no comment.
      break;
    // dst = index, src1 = parameter
    case opParam:
      // Assume the instructions are well-ordered according to the convention.
      Load(i->GetSrc(1), "%eax", cmt.str());
      EmitInstruction("pushl", "%eax");
      break;

    // special
    case opLabel:
      target = static_cast<const CTacLabel *>(i);
      _out << Label(target) << ":" << endl;
      break;

    case opNop:
      EmitInstruction("nop", "", cmt.str());
      break;


    default:
      EmitInstruction("# ???", "not implemented", cmt.str());
  }
}

void CBackendx86::EmitInstruction(string mnemonic, string args, string comment)
{
  _out << left
       << _ind
       << setw(7) << mnemonic << " "
       << setw(23) << args;
  if (comment != "") _out << " # " << comment;
  _out << endl;
}

void CBackendx86::InitArray(string base, int offset, const CArrayType *type, string name)
{
  {
    char _operand[100], _comment[100];
    sprintf(_operand, "$%d,%d(%s)", type->GetNDim(), offset, base.c_str());
    sprintf(_comment, "local array '%s' : %d dimensions", name.c_str(), type->GetNDim());
    EmitInstruction("movl", _operand, _comment);

    offset += 4;

    auto arr = type;
    for (int dim = 1; dim <= type->GetNDim(); dim++) {
      sprintf(_operand, "$%d,%d(%s)", arr->GetNElem(), offset, base.c_str());
      sprintf(_comment, "  dimension %d: %d elements", dim, arr->GetNElem());
      EmitInstruction("movl", _operand, _comment);

      offset += 4;

      auto inner = arr->GetInnerType();
      if (!inner->IsArray()) break;
      arr = static_cast<const CArrayType *>(inner);
    }
  }

  auto inner = type->GetInnerType();
  if (!inner->IsArray()) return;
  auto innerarr = static_cast<const CArrayType *>(inner);

  for (int i = 0; i < type->GetNElem(); i++) {
    string subscript = '[' + to_string(i) + ']';
    InitArray(base, offset, innerarr, name + subscript);
    offset += (innerarr->GetSize() + innerarr->GetAlign() - 1) & ~(innerarr->GetAlign() - 1);
  }
}

void CBackendx86::InitGlobalArray(const CArrayType *type)
{
  auto a = type;
  int dim = a->GetNDim();

  _out << setw(4) << " "
    << ".long " << right << setw(4) << dim << endl;

  for (int d=0; d<dim; d++) {
    assert(a != NULL);

    _out << setw(4) << " "
      << ".long " << right << setw(4) << a->GetNElem() << endl;

    if (!a->GetInnerType()->IsArray()) break;
    a = static_cast<const CArrayType*>(a->GetInnerType());
  }

  if (dim > 1) {
    for (int n=0; n<type->GetNElem(); n++) {
      InitGlobalArray(static_cast<const CArrayType *>(type->GetInnerType()));
    }
  } else {
    _out << left << setw(4) << " "
      << ".skip " << dec << right << setw(4) << type->GetDataSize()
      << endl;
    if (type->GetDataSize() & (type->GetAlign() - 1)) {
      _out << setw(4) << " " << ".align "
        << right << setw(3) << type->GetAlign() << endl;
    }
  }
}

void CBackendx86::Load(CTacAddr *src, string dst, string comment)
{
  assert(src != NULL);

  string mnm = "mov";
  string mod = "l";

  // set operator modifier based on the operand size
  switch (OperandSize(src)) {
    case 1: mod = "zbl"; break;
    case 2: mod = "zwl"; break;
    case 4: mod = "l"; break;
  }

  // emit the load instruction
  EmitInstruction(mnm + mod, Operand(src) + ", " + dst, comment);
}

void CBackendx86::Store(CTac *dst, char src_base, string comment)
{
  assert(dst != NULL);

  string mnm = "mov";
  string mod = "l";
  string src = "%";

  // compose the source register name based on the operand size
  switch (OperandSize(dst)) {
    case 1: mod = "b"; src += string(1, src_base) + "l"; break;
    case 2: mod = "w"; src += string(1, src_base) + "x"; break;
    case 4: mod = "l"; src += "e" + string(1, src_base) + "x"; break;
  }

  // emit the store instruction
  EmitInstruction(mnm + mod, src + ", " + Operand(dst), comment);
}

string CBackendx86::Operand(const CTac *op)
{
  // return a string representing op
  string operand;

  assert(op->IsAddr());
  if (op->IsConst()) {
    // A constant is represented by the immediate value.
    auto tacConst = static_cast<const CTacConst *>(op);
    operand = Imm(tacConst->GetValue());
  }
  else if (op->IsReference()) {
    auto tacReference = static_cast<const CTacReference *>(op);
    auto symbol = tacReference->GetSymbol();
    ostringstream o;
    o << "ref_" << symbol->GetName();
    operand = o.str();
    // TODO correct implementation needed
    // hint: take special care of references (op of type CTacReference)
  }
  else { // op is temp or name not reference
    auto tacName = static_cast<const CTacName *>(op);
    auto symbol = tacName->GetSymbol();
    if (symbol->GetSymbolType() == stGlobal) {
      // A global symbol is referenced by its name.
      operand = symbol->GetName();
    } else {
      // A local symbol is referenced by its base register and the offset.
      ostringstream o;
      o << symbol->GetOffset() << "(" << symbol->GetBaseRegister() << ")";
      operand = o.str();
    }
  }

  return operand;
}

string CBackendx86::Imm(int value) const
{
  ostringstream o;
  o << "$" << dec << value;
  return o.str();
}

string CBackendx86::Label(const CTacLabel* label) const
{
  CScope *cs = GetScope();
  assert(cs != NULL);

  ostringstream o;
  o << "l_" << cs->GetName() << "_" << label->GetLabel();
  return o.str();
}

string CBackendx86::Label(string label) const
{
  CScope *cs = GetScope();
  assert(cs != NULL);

  return "l_" + cs->GetName() + "_" + label;
}

string CBackendx86::Condition(EOperation cond) const
{
  switch (cond) {
    case opEqual:       return "e";
    case opNotEqual:    return "ne";
    case opLessThan:    return "l";
    case opLessEqual:   return "le";
    case opBiggerThan:  return "g";
    case opBiggerEqual: return "ge";
    default:            assert(false); break;
  }
}

int CBackendx86::OperandSize(CTac *t) const
{
  // compute the size for operand t of type CTacName
  int size = 4;

  assert(t->IsAddr());
  if (t->IsConst()) {
    // size is always 4
  }
  else if (t->IsReference()) {
  // TODO
  // Hint: you need to take special care of references (incl. references to pointers!)
  //       and arrays. Compare your output to that of the reference implementation
  //       if you are not sure.
  }
  else {
    // size is 4 except boolean
    auto tacName = static_cast<const CTacName *>(t);
    auto type = tacName->GetSymbol()->GetDataType();
    auto typeBool = CTypeManager::Get()->GetBool();
    if (typeBool->Compare(type)) size = 1;
  }

  return size;
}
