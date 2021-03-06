//------------------------------------------------------------------------------
/// @brief SnuPL/1 scanner
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2016/03/11 Bernhard Egger adapted to SnuPL/1
/// 2016/03/13 Bernhard Egger assignment 1: scans SnuPL/-1
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

#include <iostream>
#include <sstream>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <cstdio>

#include "scanner.h"
using namespace std;

//------------------------------------------------------------------------------
// token names
//
#define TOKEN_STRLEN 16

char ETokenName[][TOKEN_STRLEN] = {
  "tChar",                          ///< a single quoted character
  "tString",                        ///< a double quoted string
  "tPlusMinus",                     ///< '+' or '-'
  "tOr",                            ///< '||'
  "tMulDivAnd",                     ///< '*' or '/' or '&&'
  "tNot",                           ///< '!'
  "tRelOp",                         ///< relational operator
  "tAssign",                        ///< assignment operator
  "tSemicolon",                     ///< a semicolon
  "tColon",                         ///< a colon
  "tComma",                         ///< a comma
  "tDot",                           ///< a dot
  "tLSqBrak",                       ///< a left square bracket
  "tRSqBrak",                       ///< a right square bracket
  "tLBrak",                         ///< a left bracket
  "tRBrak",                         ///< a right bracket
  "tNumber",                        ///< a number
  "tIdent",                         ///< an identifier
  "tModule",                        ///< a keyword 'module'
  "tBegin",                         ///< a keyword 'begin'
  "tEnd",                           ///< a keyword 'end'
  "tBoolean",                       ///< a boolean value keyword
  "tBaseType",                      ///< a basetype keyword
  "tIf",                            ///< a keyword 'if'
  "tThen",                          ///< a keyword 'then'
  "tElse",                          ///< a keyword 'else'
  "tWhile",                         ///< a keyword 'while'
  "tDo",                            ///< a keyword 'do'
  "tReturn",                        ///< a keyword 'return'
  "tVar",                           ///< a keyword 'var'
  "tProcedure",                     ///< a keyword 'procedure'
  "tFunction",                      ///< a keyword 'function'

  "tEOF",                           ///< end of file
  "tIOError",                       ///< I/O error
  "tUndefined",                     ///< undefined
};


//------------------------------------------------------------------------------
// format strings used for printing tokens
//

char ETokenStr[][TOKEN_STRLEN] = {
  "tChar (%s)",                     ///< a single quoted character
  "tString (%s)",                   ///< a double quoted string
  "tPlusMinus (%s)",                ///< '+' or '-'
  "tOr",                            ///< '||'
  "tMulDivAnd (%s)",                ///< '*' or '/' or '&&'
  "tNot",                           ///< '!'
  "tRelOp (%s)",                    ///< relational operator
  "tAssign",                        ///< assignment operator
  "tSemicolon",                     ///< a semicolon
  "tColon",                         ///< a colon
  "tComma",                         ///< a comma
  "tDot",                           ///< a dot
  "tLSqBrak",                       ///< a left square bracket
  "tRSqBrak",                       ///< a right square bracket
  "tLBrak",                         ///< a left bracket
  "tRBrak",                         ///< a right bracket
  "tNumber (%s)",                   ///< a number
  "tIdent (%s)",                    ///< an identifier
  "tModule",                        ///< a keyword 'module'
  "tBegin",                         ///< a keyword 'begin'
  "tEnd",                           ///< a keyword 'end'
  "tBoolean (%s)",                  ///< a boolean value keyword
  "tBaseType (%s)",                 ///< a basetype keyword
  "tIf",                            ///< a keyword 'if'
  "tThen",                          ///< a keyword 'then'
  "tElse",                          ///< a keyword 'else'
  "tWhile",                         ///< a keyword 'while'
  "tDo",                            ///< a keyword 'do'
  "tReturn",                        ///< a keyword 'return'
  "tVar",                           ///< a keyword 'var'
  "tProcedure",                     ///< a keyword 'procedure'
  "tFunction",                      ///< a keyword 'function'

  "tEOF",                           ///< end of file
  "tIOError",                       ///< I/O error
  "tUndefined (%s)",                ///< undefined
};


//------------------------------------------------------------------------------
// reserved keywords
//
pair<const char*, EToken> Keywords[] =
{
  {"module", tModule},
  {"begin", tBegin},
  {"end", tEnd},
  {"true", tBoolean},
  {"false", tBoolean},
  {"boolean", tBaseType},
  {"char", tBaseType},
  {"integer", tBaseType},
  {"if", tIf},
  {"then", tThen},
  {"else", tElse},
  {"while", tWhile},
  {"do", tDo},
  {"return", tReturn},
  {"var", tVar},
  {"procedure", tProcedure},
  {"function", tFunction},
};



//------------------------------------------------------------------------------
// CToken
//
CToken::CToken()
{
  _type = tUndefined;
  _value = "";
  _line = _char = 0;
}

CToken::CToken(int line, int charpos, EToken type, const string value)
{
  _type = type;
  _value = escape(value);
  _line = line;
  _char = charpos;
}

CToken::CToken(const CToken &token)
{
  _type = token.GetType();
  _value = token.GetValue();
  _line = token.GetLineNumber();
  _char = token.GetCharPosition();
}

CToken::CToken(const CToken *token)
{
  _type = token->GetType();
  _value = token->GetValue();
  _line = token->GetLineNumber();
  _char = token->GetCharPosition();
}

const string CToken::Name(EToken type)
{
  return string(ETokenName[type]);
}

const string CToken::GetName(void) const
{
  return string(ETokenName[GetType()]);
}

ostream& CToken::print(ostream &out) const
{
  int str_len = _value.length();
  str_len = TOKEN_STRLEN + (str_len < 64 ? str_len : 64);
  char *str = (char*)malloc(str_len);
  snprintf(str, str_len, ETokenStr[GetType()], _value.c_str());
  out << dec << _line << ":" << _char << ": " << str;
  free(str);
  return out;
}

string CToken::escape(const string text)
{
  string s;

  for (char ch : text) {
    switch (ch) {
      case '\n': s += "\\n";  break;
      case '\t': s += "\\t";  break;
      case '\0': s += "\\0";  break;
      case '\'': s += "\\'";  break;
      case '\"': s += "\\\""; break;
      case '\\': s += "\\\\"; break;
      default :  s += ch;
    }
  }

  return s;
}

string CToken::unescape(const string text)
{
  string s;

  for (size_t i = 0; i < text.size(); i++) {
    if (text[i] == '\\') {
      switch (text[++i]) {
        case 'n': s += '\n';  break;
        case 't': s += '\t';  break;
        case '0': s += '\0';  break;
        case '\'': s += '\'';  break;
        case '"': s += '"'; break;
        case '\\': s += '\\'; break;
        default :  s += '?'; break;
      }
    } else {
      s += text[i];
    }
  }

  return s;
}

ostream& operator<<(ostream &out, const CToken &t)
{
  return t.print(out);
}

ostream& operator<<(ostream &out, const CToken *t)
{
  return t->print(out);
}


//------------------------------------------------------------------------------
// CScanner
//
map<string, EToken> CScanner::keywords;

CScanner::CScanner(istream *in)
{
  InitKeywords();
  _in = in;
  _delete_in = false;
  _line = _char = 1;
  _token = NULL;
  _good = in->good();
  NextToken();
}

CScanner::CScanner(string in)
{
  InitKeywords();
  _in = new istringstream(in);
  _delete_in = true;
  _line = _char = 1;
  _token = NULL;
  _good = true;
  NextToken();
}

CScanner::~CScanner()
{
  if (_token != NULL) delete _token;
  if (_delete_in) delete _in;
}

void CScanner::InitKeywords(void)
{
  if (keywords.size() == 0) {
    int size = sizeof(Keywords) / sizeof(Keywords[0]);
    for (int i=0; i<size; i++) {
      keywords[Keywords[i].first] = Keywords[i].second;
    }
  }
}

CToken CScanner::Get()
{
  CToken result(_token);

  EToken type = _token->GetType();
  _good = !(type == tIOError);

  NextToken();
  return result;
}

CToken CScanner::Peek() const
{
  return CToken(_token);
}

void CScanner::NextToken()
{
  if (_token != NULL) delete _token;

  _token = Scan();
}

void CScanner::RecordStreamPosition(void)
{
  _saved_line = _line;
  _saved_char = _char;
}

void CScanner::GetRecordedStreamPosition(int *lineno, int *charpos)
{
  *lineno = _saved_line;
  *charpos = _saved_char;
}

CToken* CScanner::NewToken(EToken type, const string token)
{
  return new CToken(_saved_line, _saved_char, type, token);
}

CToken* CScanner::Scan()
{
  EToken token;
  string tokval;
  char c;

  while (_in->good() && IsWhite(_in->peek())) GetChar();

  RecordStreamPosition();

  if (_in->eof()) return NewToken(tEOF);
  if (!_in->good()) return NewToken(tIOError);

  c = GetChar();
  tokval = c;
  token = tUndefined;

  switch (c) {
    case ':':
      if (_in->peek() == '=') {
        tokval += GetChar();
        token = tAssign;
      } else {
        token = tColon;
      }
      break;

    case '+':
    case '-':
      token = tPlusMinus;
      break;

    case '|':
      if (_in->peek() == '|') {
        tokval += GetChar();
        token = tOr;
      }
      break;

    case '/':
      if (_in->peek() == '/') {
        while (GetChar() != '\n' && _in->good()) ;
        return Scan();
      }
    case '*':
      token = tMulDivAnd;
      break;

    case '&':
      if (_in->peek() == '&') {
        tokval += GetChar();
        token = tMulDivAnd;
      }
      break;

    case '!':
      token = tNot;
      break;

    case '<':
    case '>':
      if (_in->peek() == '=') {
        tokval += GetChar();
      }
    case '=':
    case '#':
      token = tRelOp;
      break;

    case ';':
      token = tSemicolon;
      break;

    case ',':
      token = tComma;
      break;

    case '.':
      token = tDot;
      break;

    case '[':
      token = tLSqBrak;
      break;

    case ']':
      token = tRSqBrak;
      break;

    case '(':
      token = tLBrak;
      break;

    case ')':
      token = tRBrak;
      break;

    case '\'':
      {
        char ret;

        c = _in->peek();
        if (!IsPrintable(c)) break;
        if (c == '\'') break;

        GetChar();
        tokval += c;

        if (c == '\\') {
          if (!IsEscapeSequenceChar(_in->peek())) break;
          c = GetChar();
          ret = Unescape(c);
          tokval += c;
        } else {
          ret = c;
        }

        if (_in->peek() == '\'') {
          GetChar();
          token = tChar;
          tokval = ret;
        }
      }
      break;

    case '"':
      {
        string ret;
        for (;;) {
          c = _in->peek();

          if (!IsPrintable(c)) break;

          GetChar();
          if (c == '"') {
            token = tString;
            tokval = ret;
            break;
          }

          tokval += c;
          if (c == '\\') {
            if (!IsEscapeSequenceChar(_in->peek())) break;
            c = GetChar();
            ret += Unescape(c);
            tokval += c;
          } else {
            ret += c;
          }
        }
      }
      break;

    default:
      if (IsDigit(c)) {
        token = tNumber;
        while (IsDigit(_in->peek())) {
          tokval += GetChar();
        }
      } else if (IsLetter(c)) {
        token = tIdent;
        while (IsLetter(_in->peek()) || IsDigit(_in->peek())) {
          tokval += GetChar();
        }
        if (keywords.find(tokval) != keywords.end()) {
          token = keywords[tokval];
        }
      } else {
        tokval = "invalid character '";
        tokval += c;
        tokval += "'";
      }
      break;

  }

  return NewToken(token, tokval);
}

char CScanner::GetChar()
{
  char c = _in->get();
  if (c == '\n') { _line++; _char = 1; } else _char++;
  return c;
}

string CScanner::GetChar(int n)
{
  string str;
  for (int i=0; i<n; i++) str += GetChar();
  return str;
}

bool CScanner::IsWhite(char c) const
{
  return ((c == ' ') || (c == '\t') || (c == '\n'));
}

bool CScanner::IsDigit(char c) const
{
  return (('0' <= c) && (c <= '9'));
}

bool CScanner::IsLetter(char c) const
{
  return ((('A' <= c) && (c <= 'Z')) ||
          (('a' <= c) && (c <= 'z')) ||
          (c == '_'));
}

bool CScanner::IsPrintable(char c) const
{
  return c >= 0x20 && c <= 0x7F;
}

bool CScanner::IsEscapeSequenceChar(char c) const
{
  return c == '0' || c == 't' || c == 'n' || c == '\\' || c == '\'' || c == '"';
}

char CScanner::Unescape(char c) const
{
  switch (c) {
    case '0': return '\0';
    case 't': return '\t';
    case 'n': return '\n';
    default: return c;
  }
}
