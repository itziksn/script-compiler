enum TokenKind {
  TOKEN_EOF = 0,
  TOKEN_COLON,
  TOKEN_COLON_ASSIGN,
  TOKEN_LPAREN,
  TOKEN_RPAREN,
  TOKEN_LBRACE,
  TOKEN_RBRACE,
  TOKEN_LBRACKET,
  TOKEN_RBRACKET,
  TOKEN_SEMICOLON,
  TOKEN_COMMA,
  TOKEN_QUESTION_MARK,
  TOKEN_DOT,
  
  TOKEN_NOT,
  TOKEN_NEG,
  TOKEN_INC,
  TOKEN_DEC,
  
  // ASSIGNs
  TOKEN_ASSIGN,
  TOKEN_ASSIGN_FIRST = TOKEN_ASSIGN,
  TOKEN_ASSIGN_ADD,
  TOKEN_ASSIGN_SUB,
  TOKEN_ASSIGN_MUL,
  TOKEN_ASSIGN_DIV,
  TOKEN_ASSIGN_MOD,
  TOKEN_ASSIGN_AND,
  TOKEN_ASSIGN_OR,
  TOKEN_ASSIGN_XOR,  
  TOKEN_ASSIGN_LSHIFT,
  TOKEN_ASSIGN_RSHIFT,
  TOKEN_ASSIGN_LAST = TOKEN_ASSIGN_RSHIFT,
  
  // MULs
  TOKEN_MUL,
  TOKEN_MUL_FIRST = TOKEN_MUL,
  TOKEN_MOD,
  TOKEN_DIV,
  TOKEN_MUL_LAST = TOKEN_DIV,

  // ADDs
  TOKEN_ADD,
  TOKEN_ADD_FIRST = TOKEN_ADD,
  TOKEN_SUB,
  TOKEN_ADD_LAST = TOKEN_SUB,
  
  // SHIFTs
  TOKEN_LSHIFT,
  TOKEN_SHIFT_FISRT = TOKEN_LSHIFT,
  TOKEN_RSHIFT,
  TOKEN_SHIFT_LAST = TOKEN_RSHIFT,
  
  // ANDs
  TOKEN_AND,
  TOKEN_AND_FIRST = TOKEN_AND,
  TOKEN_OR,
  TOKEN_XOR,
  TOKEN_AND_LAST = TOKEN_XOR,
  
  // CMPs
  TOKEN_EQ,
  TOKEN_CMP_FIRST = TOKEN_EQ,
  TOKEN_NEQ,
  TOKEN_GT,
  TOKEN_GTE,
  TOKEN_LT,
  TOKEN_LTE,
  TOKEN_CMP_LAST = TOKEN_LTE,

  // LOGICALs
  TOKEN_ANDAND,
  TOKEN_LOGICAL_FIRST = TOKEN_ANDAND,
  TOKEN_OROR,
  TOKEN_LOGICAL_LAST = TOKEN_OROR,

  TOKEN_KEYWORD,
  TOKEN_INTEGER,
  TOKEN_FLOAT,
  TOKEN_STRING,
  TOKEN_NAME,
  MAX_TOKEN_KINDS,
};

const char* token_kind_name(TokenKind kind) {
  switch(kind) {
  case TOKEN_EOF:
    return "end of file";
  case TOKEN_KEYWORD:
    return "keyword";
  case TOKEN_INTEGER:
    return "integer";
  case TOKEN_NAME:
    return "identifier";
  case TOKEN_STRING:
    return "string";
  case TOKEN_COLON:
    return ":";
  case TOKEN_COLON_ASSIGN:
    return ":=";
  case TOKEN_ASSIGN:
    return "=";
  case TOKEN_EQ:
    return "==";
  case TOKEN_NEQ:
    return "!=";
  case TOKEN_LPAREN:
    return "(";
  case TOKEN_RPAREN:
    return ")";
  case TOKEN_SEMICOLON:
    return ";";
  case TOKEN_COMMA:
    return ",";
  case TOKEN_ADD:
    return "+";
  case TOKEN_SUB:
    return "-";
  case TOKEN_MUL:
    return "*";
  case TOKEN_DIV:
    return "/";
  case TOKEN_DOT:
	return ".";
  case TOKEN_GTE:
    return ">=";
  case TOKEN_GT:
    return ">";
  case TOKEN_LT:
    return "<";
  case TOKEN_LTE:
    return "<=";
  case TOKEN_LBRACKET:
    return "[";
  case TOKEN_RBRACKET:
    return "]";
  case TOKEN_LBRACE:
    return "{";
  case TOKEN_RBRACE:
    return "}";
  case TOKEN_AND:
    return "&";
  case TOKEN_ANDAND:
    return "&&";
  case TOKEN_NOT:
    return "!";
  case TOKEN_XOR:
    return "^";
  case TOKEN_OR:
    return "|";
  case TOKEN_ASSIGN_ADD:
    return "+=";
  default:
    return "<unhandled-token-name>";
  }
}

struct SourceLocation {
  const char* filename;
  size_t line;
};

enum Mod {
  MOD_NONE,
  MOD_HEX,
  MOD_BIN,
  MOD_OCT,
  MOD_BOOL,
  MOD_CHAR,
};

enum ConstPostfix {
  POSTFIX_NONE,
  POSTFIX_U,
  POSTFIX_L,
  POSTFIX_UL,
  POSTFIX_LL,
  POSTFIX_ULL,
  POSTFIX_D,
};

struct Token {
  TokenKind kind;
  const char* start;
  const char* end;
  SourceLocation loc;
  union {
    struct {
      Mod mod;
      ConstPostfix postfix;
      unsigned long long ull;
    } int_val;
    struct {
      ConstPostfix postfix;
      double d;
    } float_val;
    const char* name;
    struct {
      const char* buf;
      size_t len;
    } str;
  };
};

int token_to_name(Token token, char* buffer, size_t length) {
  switch(token.kind) {
  case TOKEN_KEYWORD:
    return snprintf(buffer, length, "keyword '%s'", token.name);
  case TOKEN_INTEGER:
    return snprintf(buffer, length, "%lli", token.int_val.ull);
  case TOKEN_NAME:
    return snprintf(buffer, length, token.name);
  case TOKEN_EOF:
    return snprintf(buffer, length, "end-of-file");
  // case TOKEN_STRING:
  //   return snprintf(buffer, length, "\"%.*s\"", (int)(token.end - token.start - 1), token.start + 1);
  default:
    return snprintf(buffer, length, token_kind_name(token.kind));
  }
}

#define TEMP_BUFFER_SIZE 256

const char* token_to_name_temp(Token token) {
  static char temp_buffer[TEMP_BUFFER_SIZE];
  token_to_name(token, temp_buffer, sizeof(temp_buffer));
  return temp_buffer;
}
