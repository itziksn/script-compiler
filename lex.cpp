const char* stream;
Token current_token;

void error_here(const char* fmt, ...) {
  printf("%s(%lld): error: ", current_token.loc.filename, current_token.loc.line);
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
  printf("\n");
  exit(1);
}

const char* first_keyword;
const char* last_keyword;
const char* keyword_if;
const char* keyword_else;
const char* keyword_while;
const char* keyword_for;
const char* keyword_do;
const char* keyword_func;
const char* keyword_var;
const char* keyword_const;
const char* keyword_enum;
const char* keyword_struct;
const char* keyword_union;
const char* keyword_true;
const char* keyword_false;
const char* keyword_return;
const char* keyword_typedef;
const char* keyword_break;
const char* keyword_continue;
const char* keyword_switch;
const char* keyword_case;
const char* keyword_default;

#define KEYWORD(name) keyword_##name = str_intern_range(#name, sizeof(#name) - 1)

void init_keywords() {
  KEYWORD(if);
  first_keyword = keyword_if;
  KEYWORD(else);
  KEYWORD(while);
  KEYWORD(for);
  KEYWORD(break);
  KEYWORD(continue);
  KEYWORD(do);
  KEYWORD(switch);
  KEYWORD(case);
  KEYWORD(default);
  KEYWORD(func);
  KEYWORD(const);
  KEYWORD(enum);
  KEYWORD(struct);
  KEYWORD(typedef);
  KEYWORD(true);
  KEYWORD(false);
  KEYWORD(return);
  KEYWORD(union);
  KEYWORD(var);
  last_keyword = keyword_var;
  assert(str_intern(first_keyword) == first_keyword);
}
#undef KEYWORD

bool is_name_keyword(const char* intern_str) {
  return intern_str >= first_keyword && intern_str <= last_keyword;
}

void scan_double() {
  current_token.float_val.postfix = POSTFIX_NONE;
  current_token.kind = TOKEN_FLOAT;
  while(isdigit(*stream)) {
    ++stream;
  }
  if(*stream == '.') {
    ++stream;
    if(!isdigit(*stream)) {
      error_here("unexpected %c in float literal", *stream);
    }
    ++stream;
    while(isdigit(*stream)) {
      ++stream;
    }
  }
  if(tolower(*stream) == 'e') {
    ++stream;
    if(*stream == '-' || *stream == '+') {
      ++stream;
    }
    if(!isdigit(*stream)) {
      error_here("unexpected %c in float literal", *stream);
    }
    ++stream;
    while(isdigit(*stream)) {
      ++stream;
    }
  }
  if(tolower(*stream) == 'd') {
    current_token.float_val.postfix = POSTFIX_D;
  }
  char* end_ptr;
  current_token.float_val.d = strtod(current_token.start, &end_ptr);
  assert(stream == end_ptr);
  if(current_token.float_val.d == HUGE_VAL) {
    error_here("floating point literal out of range");
  }
}

void scan_int() {
  current_token.int_val.mod = MOD_NONE;
  current_token.int_val.postfix = POSTFIX_NONE;

  int base = 10;
  if(*stream == '0') {
    if(*(stream + 1) == 'x') {
      stream += 2;
      base = 16;
      current_token.int_val.mod = MOD_HEX;
    } else if(*(stream + 1) == 'b') {
      stream += 2;
      base = 2;
      current_token.int_val.mod = MOD_BIN;
    } else if(isalnum(*(stream + 1))) {
      stream += 1;
      base = 8;
      current_token.int_val.mod = MOD_OCT;
    }
  }
    
  unsigned long long int_value = 0;
  bool after_first = false;
  for(;;) {
    int digit = 0;
    
    if(isdigit(*stream)) {
      digit = *stream - '0';
      after_first = true;
    } else if(*stream >= 'a' && *stream <= 'f') {
      digit = 10 + *stream - 'a';
      after_first = true;
    } else if(*stream >= 'A' && *stream <= 'F') {
      digit = 10 + *stream - 'A';
      after_first = true;
    } else if(isalnum(*stream)) {
      const char* postfix_start = stream;
      if(tolower(*stream) == 'u') {
	current_token.int_val.postfix = POSTFIX_U;
	++stream;
	if (tolower(*stream) == 'l') {
	  current_token.int_val.postfix = POSTFIX_UL;
	  ++stream;
	  if (tolower(*stream) == 'l') {
	    current_token.int_val.postfix = POSTFIX_ULL;
	    ++stream;
	  }
	}
      } else if (tolower(*stream) == 'l') {
	current_token.int_val.postfix = POSTFIX_L;
	++stream;
	if (tolower(*stream) == 'l') {
	  current_token.int_val.postfix = POSTFIX_LL;
	  ++stream;
	  if(tolower(*stream) == 'u') {
	    current_token.int_val.postfix = POSTFIX_ULL;
	    ++stream;
	  }
	}
	else if (tolower(*stream) == 'u') {
	  current_token.int_val.postfix = POSTFIX_UL;
	  ++stream;
	}
      } else {
	error_here("unexpected token %c in integer literal", *stream);
      }
      if(isalnum(*stream)) {
	error_here("unknown number literal postfix %.*s", (int)(stream - postfix_start), postfix_start);
      }
      break;
    } else {
      if (!after_first) {
	error_here("unexpected token %c in integer literal", *stream);
      }
      break;
    }
    
    if(int_value >= (ULLONG_MAX - digit) / base) {
      error_here("integer overflow");
    }

    if(digit >= base) {
      error_here("unexpected digit %c in base %d integer literal", *stream, base);
    }
    
    int_value *= base;
    int_value += digit;
    ++stream;
  }
    
  current_token.kind = TOKEN_INTEGER;
  current_token.int_val.ull = int_value;
}

char get_escaped_character() {
  switch(*stream) {
  case '0':
    return '\0';
    break;
  case '\\':
    return '\\';
    break;
  case 'n':
    return '\n';
    break;
  case 't':
    return '\t';
    break;
  case 'r':
    return '\r';
    break;
  case 'a':
    return '\a';
    break;
  case 'b':
    return '\b';
    break;
  case 'x': {
    int count = 0;
    char char_value = 0;
    for(;;) {
      if(isdigit(*(stream + 1))) {
	++stream;
	char_value *= 16;
	char_value += *stream - '0';
      } else if(*(stream + 1) >= 'a' && *(stream + 1) <= 'f') {
	++stream;
	char_value *= 16;
	char_value += 10 + *stream - 'a';
      } else if(*(stream + 1) >= 'a' && *(stream + 1) <= 'f') {
	++stream;
	char_value *= 16;
	char_value += 10 + *stream - 'A';
      } else {
	if(count == 0) {
	  error_here("escaped character literal must have at least one digit");
	}
	return char_value;
      }
      if(count == 1) {
	return char_value;
      }
      ++count;
    }
  } break;
  }
  return 0;
}

#define CASE1(chr, token_kind) case chr: current_token.kind = token_kind; ++stream; break;

#define CASE2(chr, token_kind, chr_2, token_kind_2)	\
  case chr:						\
  current_token.kind = token_kind;			\
  ++stream;						\
  if(*stream == chr_2) {				\
    current_token.kind = token_kind_2;			\
    ++stream;						\
  }							\
  break;

#define CASE3(chr, tk1, chr2, tk2, chr3, tk3)	\
  case chr:					\
  current_token.kind = tk1;			\
  ++stream;					\
  if(*stream == chr2) {				\
    current_token.kind = tk2;			\
    ++stream;					\
  } else if(*stream == chr3) {			\
    current_token.kind = tk3;			\
    ++stream;					\
  }						\
  break;
  
void next_token() {
 repeat:
  current_token.start = stream;
  switch (*stream) {
  case '\0':
    current_token.kind = TOKEN_EOF;
    ++stream;
    break;
  case ' ':
  case '\t':
  case '\v':
  case '\b':
  case '\n':
  case '\r':
    while (isspace(*stream)) {
      if (*stream == '\n') ++current_token.loc.line;
      ++stream;
    }
    goto repeat;
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9': {
    bool is_float = false;
    while(isdigit(*stream)) {
      ++stream;
    } if(*stream == '.' || *stream == 'e') {
      is_float = true;
    }
    stream = current_token.start;
    if(is_float) {
      scan_double();
    } else {
      scan_int();
    }
    break;
  }
  case 'a': case 'b': case 'c':	case 'd': case 'e': case 'f': case 'g':	case 'h': case 'i': case 'j':
  case 'k': case 'l': case 'm':	case 'n': case 'o': case 'p': case 'q':	case 'r': case 's': case 't':
  case 'u': case 'v': case 'w':	case 'x': case 'y': case 'z': 			  	    
  case 'A': case 'B': case 'C':	case 'D': case 'E': case 'F': case 'G':	case 'H': case 'I': case 'J':
  case 'K': case 'L': case 'M':	case 'N': case 'O': case 'P': case 'Q':	case 'R': case 'S': case 'T':
  case 'U': case 'V': case 'W':	case 'X': case 'Y': case 'Z': 
  case '_':
    while (isalnum(*stream) || *stream == '_') {
      ++stream;
    }
    current_token.kind = TOKEN_NAME;
    current_token.name = str_intern_range(current_token.start, stream - current_token.start);
    if (is_name_keyword(current_token.name)) {
      current_token.kind = TOKEN_KEYWORD;
    }
    break;
  case '>':
    current_token.kind = TOKEN_GT;
    ++stream;
    if(*stream == '>') {
      current_token.kind = TOKEN_RSHIFT;
      ++stream;
      if(*stream == '=') {
	current_token.kind = TOKEN_ASSIGN_RSHIFT;
	++stream;
      }
    } else if(*stream == '=') {
      current_token.kind = TOKEN_GTE;
      ++stream;
    }
    break;
  case '<':
    current_token.kind = TOKEN_LT;
    ++stream;
    if(*stream == '<') {
      current_token.kind = TOKEN_LSHIFT;
      ++stream;
      if(*stream == '=') {
	current_token.kind = TOKEN_ASSIGN_LSHIFT;
	++stream;
      }
    } else if(*stream == '=') {
      current_token.kind = TOKEN_LTE;
      ++stream;
    }
    break;
  case '"': {
    ++stream;
    char* buf = NULL;
    while(*stream != '"' && *stream) {
      if(*stream == '\n') { // what about the fact that we read the file in binary mode???
	error_here("unexpected new line character in string literal");
      }
      if(*stream == '\\') {
	++stream;
	if(*stream == '"') {
	  buf_push(buf, *stream);
	  ++stream;
	} else {
	  char val = get_escaped_character();
	  if(!val && *stream != '0') {
	    error_here("unexpected escape sequence %c", *stream);
	  }
	  buf_push(buf, val);
	  ++stream;
	}
      } else {
	buf_push(buf, *stream);
	++stream;
      }
    }
    buf_push(buf, 0);
    
    if(*stream != '"') {
      error_here("unexpected end of file in string literal");
    }
    
    ++stream;
    current_token.kind = TOKEN_STRING;
    current_token.str.buf = buf;
    current_token.str.len = buf_len(buf);
  } break;
  case '\'': {
    char char_value = 0;
    ++stream;
    if(*stream == '\\') {
      ++stream;
      if(*stream ==  '\'') {
	char_value = '\'';
      }	else {
	char_value = get_escaped_character();
	if(!char_value && *stream) {
	  error_here("unexpected escape sequence %c", *stream);
	}
      }
      ++stream;
    } else {
      char_value = *stream++;
    }

    if(*stream != '\'') {
      error_here("expected enclosing single quote, got %c", *stream);
    }
    ++stream;

    current_token.kind = TOKEN_INTEGER;
    current_token.int_val.mod = MOD_CHAR;
	current_token.int_val.ull = char_value;
  } break;
  case '/':
    ++stream;
    current_token.kind = TOKEN_DIV;
    if(*stream == '=') {
      current_token.kind = TOKEN_ASSIGN_DIV;
      ++stream;
    } else if(*stream == '/') {
      ++stream;
      while(*stream != '\n' && *stream != 0) {
	++stream;
      }
      goto repeat;
    } else if(*stream == '*') {
      ++stream;
      int comments_count = 1;
      while(*stream != 0 && comments_count > 0) {
	if(*stream == '\n') {
	  ++stream;
	  current_token.loc.line++;
	} else if(*stream == '/') {
	  ++stream;
	  if(*stream == '*') {
	    ++comments_count;
	    ++stream;
	  }
	} else if(*stream == '*') {
	  ++stream;
	  if(*stream == '/') {
	    ++stream;
	    --comments_count;
	  }
	} else {
	  ++stream;
	}
      }
      if(!(*stream) && comments_count) {
	error_here("unepexted end of line in comment");
      }
      goto repeat;
    }
    break;
    CASE1('.', TOKEN_DOT)
    CASE1('(', TOKEN_LPAREN)
    CASE1(')', TOKEN_RPAREN)
    CASE1('{', TOKEN_LBRACE)
    CASE1('}', TOKEN_RBRACE)
    CASE1('[', TOKEN_LBRACKET)
    CASE1(']', TOKEN_RBRACKET)
    CASE1(';', TOKEN_SEMICOLON)
    CASE1(',', TOKEN_COMMA)
    CASE1('?', TOKEN_QUESTION_MARK)
    CASE1('~', TOKEN_NEG)
      
	CASE2(':', TOKEN_COLON, '=', TOKEN_COLON_ASSIGN)
    CASE2('=', TOKEN_ASSIGN, '=', TOKEN_EQ)
    CASE2('*', TOKEN_MUL, '=', TOKEN_ASSIGN_MUL)
    CASE2('%', TOKEN_MOD, '=', TOKEN_ASSIGN_MOD)
    CASE2('!', TOKEN_NOT, '=', TOKEN_NEQ)
    CASE2('|', TOKEN_OR, '=', TOKEN_ASSIGN_OR)
    CASE2('^', TOKEN_XOR, '=', TOKEN_ASSIGN_XOR)
      
    CASE3('+', TOKEN_ADD, '+', TOKEN_INC, '=', TOKEN_ASSIGN_ADD)    
    CASE3('-', TOKEN_SUB, '+', TOKEN_DEC, '=', TOKEN_ASSIGN_SUB)
    CASE3('&', TOKEN_AND, '&', TOKEN_ANDAND, '=', TOKEN_ASSIGN_AND)
  default:
      fatal_error(current_token.loc, "unkonwn token %c", *stream);
    break;
  }
  current_token.end = stream;
}

void init_stream(const char* filename, const char* code) {
  stream = code;
  current_token.loc.filename = filename ? filename : "<anonymus>";
  current_token.loc.line = 1;
  next_token();
}

bool is_token(TokenKind kind) {
  return current_token.kind == kind;
}

Token peek_token(int count) {
  const char* old_stream = stream;
  Token old = current_token;
  while(count-- > 0) {
    next_token();
  }
  Token new_token = current_token;
  
  stream = old_stream;
  current_token = old;
  return new_token;
}

bool is_token_keyword(const char* interned_keyword) {
  return current_token.kind == TOKEN_KEYWORD && current_token.name == interned_keyword;
}

bool match_keyword(const char* interned_keyword) {
  if(current_token.kind == TOKEN_KEYWORD && current_token.name == interned_keyword) {
    next_token();
    return true;
  }
  return false;
}

bool match_token(int kind) {
  if(current_token.kind == kind) {
    next_token();
    return true;
  }
  return false;
}

Token consume_token() {
  Token temp = current_token;
  next_token();
  return temp;
}

bool expect_token(TokenKind kind) {
  if (current_token.kind == kind) {
    next_token();
    return true;
  } else {
    error_here("expected token %s but got %s", token_kind_name(kind), token_kind_name(current_token.kind));
  }
  return false;
}

#define assert_integer(val) assert(current_token.kind == TOKEN_INTEGER && current_token.int_val.ull == val); next_token()
#define assert_kind(test_kind) assert(current_token.kind == test_kind); next_token()

void lexer_test()
{
  const char* test_keyword = str_intern("if");
  const char* test_non_keyword = str_intern("nono");
  assert(is_name_keyword(test_keyword));
  assert(!is_name_keyword(test_non_keyword));

  char str[] = " 1 + 1321  -  12* 2 + namesa if while func";
  init_stream(NULL, str);
  assert_integer(1);
  assert_kind(TOKEN_ADD);
  assert_integer(1321);
  assert_kind(TOKEN_SUB);
  assert_integer(12);
  assert_kind(TOKEN_MUL);
  assert_integer(2);
  assert_kind(TOKEN_ADD);
  assert_kind(TOKEN_NAME);
  assert(is_token_keyword(keyword_if));
  assert_kind(TOKEN_KEYWORD);
  assert(is_token_keyword(keyword_while));
  assert_kind(TOKEN_KEYWORD);
  assert(is_token_keyword(keyword_func));
  assert_kind(TOKEN_KEYWORD);
  assert(current_token.kind == TOKEN_EOF);

  init_stream(NULL, "0xF7 0123 0b11111111 160u 175l 175ll 175lu 175llu");
  assert_integer(0xf7);
  assert_integer(0123);
  assert_integer(0xff);
  assert_integer(160u);
  assert_integer(175l);
  assert_integer(175ll);
  assert_integer(175lu);
  assert_integer(175llu);
  assert_kind(TOKEN_EOF);
}

#undef assert_integer
#undef assert_kind
