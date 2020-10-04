
bool is_prefix_un_op(TokenKind kind) {
  switch(kind) {
  case TOKEN_NOT:
  case TOKEN_NEG:
  case TOKEN_AND:
  case TOKEN_MUL:
  case TOKEN_ADD:
  case TOKEN_SUB:
  case TOKEN_INC:
  case TOKEN_DEC:
    return true;
  default:
    return false;
  }
}

bool is_postfix_un_op(TokenKind kind) {
  switch(kind) {
  case TOKEN_LPAREN:
  case TOKEN_LBRACKET:
  case TOKEN_INC:
  case TOKEN_DEC:
  case TOKEN_DOT:
    return true;
  default:
    return false;
  }
}

bool is_mul_op(TokenKind kind) {
  return kind >= TOKEN_MUL_FIRST && kind <= TOKEN_MUL_LAST;
}

bool is_add_op(TokenKind kind) {
  return kind >= TOKEN_ADD_FIRST && kind <= TOKEN_ADD_LAST;
}

bool is_shift_op(TokenKind kind) {
  return TOKEN_SHIFT_FISRT <= kind && kind <= TOKEN_SHIFT_LAST;
}

bool is_and_op(TokenKind kind) {
  return TOKEN_AND_FIRST <= kind && kind <= TOKEN_AND_LAST;
}

bool is_cmp_op(TokenKind kind) {
  return kind >= TOKEN_CMP_FIRST && kind <= TOKEN_CMP_LAST;
}

bool is_logical_op(TokenKind kind) {
  return kind >= TOKEN_LOGICAL_FIRST && kind <= TOKEN_LOGICAL_LAST;
}

Typespec* parse_typespec();
Expr* parse_expression();

Expr* parse_expr_compound(Typespec* type) {
  assert(is_token(TOKEN_LBRACE));
  consume_token();
  
  CompoundItem* items = NULL;

  while(!is_token(TOKEN_RBRACE) && !is_token(TOKEN_EOF)) {
    CompoundItem item;
	item.loc = current_token.loc;
    if(is_token(TOKEN_NAME) && peek_token(1).kind == TOKEN_ASSIGN) {
      item.kind = COMPOUND_NAMED;
      item.name = consume_token().name;
      consume_token(); // TOKEN_ASSIGN
      item.expr = parse_expression();
    } else if(match_token(TOKEN_LBRACKET)) {\
      item.kind = COMPOUND_INDEXED;
      item.index = parse_expression();
      expect_token(TOKEN_RBRACKET);
      expect_token(TOKEN_ASSIGN);
      item.expr = parse_expression();
    } else {
      item.kind = COMPOUND_NONE;
      item.expr = parse_expression();
    }
    buf_push(items, item);
	
    if(!match_token(TOKEN_COMMA)) {
      break;
    }
  }

  expect_token(TOKEN_RBRACE);
  
  return expr_compound(type, items, buf_len(items));    
}


const char* parse_name() {
  if(!is_token(TOKEN_NAME)) {
    error_here("expected identifier, got %s", token_to_name_temp(current_token));
  }
  return consume_token().name;
}

Expr* parse_expr_un();

Expr* parse_expr_atom() {
  Expr* atom = NULL;
  if(match_token(TOKEN_LPAREN)) {
    if(match_token(TOKEN_COLON)) {
      Typespec* type = parse_typespec();
      expect_token(TOKEN_RPAREN);
      if(is_token(TOKEN_LBRACE)) {
	atom = parse_expr_compound(type);
      } else {
	Expr* expr = parse_expr_un();
	atom = expr_cast(type, expr);
      }
    } else {
      Expr* expr = parse_expression();
      expect_token(TOKEN_RPAREN);
      atom = expr;
    }
  } else if(is_token(TOKEN_NAME)) {
    if(peek_token(1).kind == TOKEN_LBRACE) {
      Typespec* type = typespec_name(consume_token().name);
      atom = parse_expr_compound(type);
    } else {
      atom = expr_name(consume_token().name);
    }
  } else if(is_token(TOKEN_INTEGER)) {
    ConstVal val;
    val.mod = current_token.int_val.mod;
    val.kind = CONST_INTEGER;
    val.postfix = current_token.int_val.postfix;
    val.ull = consume_token().int_val.ull;
    atom = expr_const(val);
  } else if(is_token(TOKEN_FLOAT)) {
    ConstVal val;
    val.kind = CONST_FLOAT;
    val.postfix = current_token.float_val.postfix;
    val.d = current_token.float_val.d;
    consume_token();
    atom = expr_const(val);
  } else if(is_token_keyword(keyword_true) || is_token_keyword(keyword_false)) {
    ConstVal val;
    val.kind = CONST_INTEGER;
    val.mod = MOD_BOOL;
    val.ull = consume_token().name == keyword_true;
    atom = expr_const(val);
  } else if(is_token(TOKEN_LBRACE)) {
    atom = parse_expr_compound(NULL);
  } else if(is_token(TOKEN_STRING)) {
    ConstVal val = {};
    val.kind = CONST_STRING;
    val.str.buf = current_token.str.buf;
    val.str.len = current_token.str.len;
    atom = expr_const(val);
    consume_token();
  } else {
    return NULL;
  }
  
  return atom;
}

Expr* expect_expression(Expr* expr) {
  if(!expr) {
    error_here("expected expression, got %s", token_kind_name(current_token.kind));
  }
  return expr;
}

Expr* parse_expr_un() {
  Expr* expr = NULL;
  if(is_prefix_un_op(current_token.kind)) {
    TokenKind op = consume_token().kind;
    expr = expr_unary(expect_expression(parse_expr_un()), op);
  } else {
    expr = parse_expr_atom();
  }
  
  while(is_postfix_un_op(current_token.kind)) {
    expect_expression(expr);
    if(match_token(TOKEN_LPAREN)) {
      CallArg* args = NULL;
      if(!is_token(TOKEN_RPAREN)) {
		do {
		  CallArg arg = {};
		  if(is_token(TOKEN_NAME) && peek_token(1).kind == TOKEN_ASSIGN) {
			arg.name = consume_token().name;
			consume_token(); // TOKEN_ASSIGN;
		  }
		  arg.expr = parse_expression();
		  buf_push(args, arg);
		} while(match_token(TOKEN_COMMA));
      }
      expect_token(TOKEN_RPAREN);
      expr =  expr_call(expr, args, buf_len(args));
    } else if(match_token(TOKEN_DOT)) {
      const char* name = parse_name();
      expr = expr_field(expr, name);
    } else {
      assert(0);
    }
  }
  
  return expr;
}

Expr* parse_expr_mul() {
  Expr* expr = parse_expr_un();
  while(is_mul_op(current_token.kind)) {
    expect_expression(expr);
    TokenKind op = consume_token().kind;
    Expr* right = expect_expression(parse_expr_un());
    expr = expr_binary(expr, right, op);
  }
  return expr;
}

Expr* parse_expr_add() {
  Expr* expr =  parse_expr_mul();
  while(is_add_op(current_token.kind)) {
    TokenKind op = consume_token().kind;
    Expr* right = parse_expr_mul();
    expr = expr_binary(expr, right, op);
  }
  return expr;
}

Expr* parse_expr_shift() {
  Expr* expr = parse_expr_add();
  while(is_shift_op(current_token.kind)) {
    expect_expression(expr);
    TokenKind op = consume_token().kind;
    Expr* right = expect_expression(parse_expr_add());
    expr = expr_binary(expr, right, op);
  }
  return expr;
}

Expr* parse_expr_and() {
  Expr* expr = parse_expr_shift();
  while(is_and_op(current_token.kind)) {
    expect_expression(expr);
    TokenKind op = consume_token().kind;
    Expr* right = expect_expression(parse_expr_shift());
    expr = expr_binary(expr, right, op);
  }
  return expr;
}

Expr* parse_expr_cmp() {
  Expr* expr = parse_expr_and();
  while(is_cmp_op(current_token.kind)) {
    expect_expression(expr);
    TokenKind op = consume_token().kind;
    Expr* right = expect_expression(parse_expr_and());
    expr = expr_binary(expr, right, op);
  }
  return expr;
}

Expr* parse_expr_logical() {
  Expr* expr = parse_expr_cmp();
  while(is_logical_op(current_token.kind)) {
    expect_expression(expr);
    TokenKind op = consume_token().kind;
    Expr* right = expect_expression(parse_expr_cmp());
    expr = expr_binary(expr, right, op);
  }
  return expr;
}

Expr* parse_expr_ternary() {
  Expr* expr = parse_expr_logical();
  while(match_token(TOKEN_QUESTION_MARK)) {
    expect_expression(expr);
    Expr* then_expr = expect_expression(parse_expr_logical());
    expect_token(TOKEN_COLON);
    Expr* else_expr = expect_expression(parse_expr_logical());
    expr = expr_ternary(expr, then_expr, else_expr);
  }
  return expr;
}

Expr* parse_expression() {
  Expr* expr = parse_expr_ternary();
  if(!expr) {
    error_here("expected expression, got %s", token_to_name_temp(current_token));
  }
  return expr;
}

Expr* maybe_parse_expression() {
  return parse_expr_ternary();
}


Typespec* parse_typespec_atom() {
  if(is_token(TOKEN_NAME)) {
    return typespec_name(consume_token().name);
  } else if(match_token(TOKEN_LPAREN)) {
    Typespec* result = parse_typespec();
    expect_token(TOKEN_RPAREN);
    return result;
  } else {
    error_here("expected type, got %s", token_to_name_temp(current_token));
    return NULL;
  }
}

Typespec* parse_typespec() {
  Typespec* type = parse_typespec_atom();
  while(is_token(TOKEN_MUL) || is_token(TOKEN_LBRACKET) || is_token_keyword(keyword_const)) {
    if(match_token(TOKEN_MUL)) {
      type = typespec_ptr(type);
	} else if(match_keyword(keyword_const)) {
	  if(!type) {
		error_here("const must be proceeding a type");
	  }
	  type->is_const = true;
    } else {
      assert(is_token(TOKEN_LBRACKET));
      consume_token();
      Expr* size = maybe_parse_expression();
      type = typespec_arr(type, size);
      expect_token(TOKEN_RBRACKET);
    }
  }    
  return type;
}

bool is_assign_op(TokenKind kind) {
  return kind >= TOKEN_ASSIGN_FIRST && kind <= TOKEN_ASSIGN_LAST;
}

Stmnt* parse_stmnt(bool in_loop);

StmntIf* parse_stmnt_if(bool in_loop) {
  assert(is_token_keyword(keyword_if));
  SourceLocation loc = current_token.loc;
  consume_token();

  expect_token(TOKEN_LPAREN);
  Expr* cond = parse_expression();
  expect_token(TOKEN_RPAREN);

  Stmnt* then_stmnt = parse_stmnt(in_loop);

  ElseIf* elseifs = NULL;
  Stmnt* else_stmnt = NULL;
  
  while(is_token_keyword(keyword_else)) {
    consume_token();
    if(is_token_keyword(keyword_if)) {
      SourceLocation elseif_loc = consume_token().loc;
      expect_token(TOKEN_LPAREN);
      Expr* elseif_cond = parse_expression();
      expect_token(TOKEN_RPAREN);

      Stmnt* elseif_stmnt = parse_stmnt(in_loop);
      buf_push(elseifs, ElseIf{elseif_loc, elseif_cond, elseif_stmnt });
    } else {
      else_stmnt = parse_stmnt(in_loop);
      break;
    }
  }

  return stmnt_if(loc, cond, then_stmnt, elseifs, buf_len(elseifs), else_stmnt);
}

StmntBlock* parse_stmnt_block(bool in_loop) {
  assert(is_token(TOKEN_LBRACE));
  SourceLocation loc = current_token.loc;
  consume_token();
  
  Stmnt** stmnts = NULL;
  while(!is_token(TOKEN_RBRACE) && !is_token(TOKEN_EOF)) {
    Stmnt* stmnt = parse_stmnt(in_loop);
    if(!stmnt) break;
    buf_push(stmnts, stmnt);
  }
  expect_token(TOKEN_RBRACE);
  
  return stmnt_block(loc, stmnts, buf_len(stmnts));
}

Stmnt* parse_stmnt_simple() {
  SourceLocation loc = current_token.loc;
  if(is_token(TOKEN_NAME) && peek_token(1).kind == TOKEN_COLON) {
    const char* name = consume_token().name;
    consume_token(); // TOKEN_COLON

    Typespec* type = NULL;
    if(!is_token(TOKEN_ASSIGN)) {
      type = parse_typespec();
    }

    Expr* expr = NULL;
    if(match_token(TOKEN_ASSIGN)) {
      expr = parse_expression();
    } else if(!type) {
      error_here("variable declaration must have either an initializer or type");
    }
    return stmnt_decl(loc, name, type, expr);
  } else {
    Expr* expr = maybe_parse_expression();
    if(!expr) {
      error_here("expected statement, got %s", token_to_name_temp(current_token));
    }
    if (is_assign_op(current_token.kind)) {
      TokenKind op = consume_token().kind;
      Expr* assign_expr = parse_expression();
      return stmnt_assign(loc, expr, assign_expr, op);
    }
    return stmnt_expr(loc, expr);
  }
}

Stmnt* parse_stmnt(bool in_loop) {
  SourceLocation loc = current_token.loc;
  if(is_token_keyword(keyword_return)) {
    consume_token();
    Expr* expr = NULL;
    if(!is_token(TOKEN_SEMICOLON)) {
      expr = parse_expression();
    }
    expect_token(TOKEN_SEMICOLON);
    return stmnt_return(loc, expr);
  } else if(is_token_keyword(keyword_if)) {
    return parse_stmnt_if(in_loop);
  } else if(is_token(TOKEN_LBRACE)) {
    return parse_stmnt_block(in_loop);
  } else if(match_keyword(keyword_while)) {
    expect_token(TOKEN_LPAREN);
    Expr* cond = parse_expression();
    expect_token(TOKEN_RPAREN);
    Stmnt* then_stmnt = parse_stmnt(true);
    return stmnt_while(loc, cond, then_stmnt, false);
  } else if(match_keyword(keyword_do)) {
    Stmnt* then_stmnt = parse_stmnt(true);
    if(!match_keyword(keyword_while)) {
      error_here("expected 'while' after 'do'");
    }
    expect_token(TOKEN_LPAREN);
    Expr* cond = parse_expression();
    expect_token(TOKEN_RPAREN);
    expect_token(TOKEN_SEMICOLON);
    
    return stmnt_while(loc, cond, then_stmnt, true);
  } else if(match_keyword(keyword_for)) {
    expect_token(TOKEN_LPAREN);

    Stmnt* init = parse_stmnt_simple();
    expect_token(TOKEN_SEMICOLON);

    Expr* cond = parse_expression();
    expect_token(TOKEN_SEMICOLON);

    Stmnt* next = parse_stmnt_simple();
    
    expect_token(TOKEN_RPAREN);
    
    Stmnt* then_stmnt = parse_stmnt(true);

    return stmnt_for(loc, init, cond, next, then_stmnt);
  } else if(match_keyword(keyword_break)) {
    if(!in_loop) {
      error_here("unexpected 'break' outside of loop");
    }
    expect_token(TOKEN_SEMICOLON);
    return stmnt_break(loc);
  } else if(match_keyword(keyword_continue)) {
    if(!in_loop) {
      error_here("unexpected 'continue' outside of loop");
    }
    expect_token(TOKEN_SEMICOLON);
    return stmnt_continue(loc);
  } else if(match_keyword(keyword_switch)) {
    SwitchCase* cases = NULL;

    expect_token(TOKEN_LPAREN);
    Expr* switch_expr = parse_expression();
    expect_token(TOKEN_RPAREN);
    
    expect_token(TOKEN_LBRACE);
    while(is_token_keyword(keyword_case)) {
      SourceLocation case_loc = consume_token().loc;

      Expr** exprs = NULL;
      Expr* first = parse_expression();
      buf_push(exprs, first);
      while(!is_token(TOKEN_COLON) && !is_token(TOKEN_EOF)) {
	expect_token(TOKEN_COMMA);
	Expr* expr = parse_expression();
	buf_push(exprs, expr);
      }
      expect_token(TOKEN_COLON);
      
      Stmnt** case_then = NULL;
      buf_push(case_then, parse_stmnt(false));
      while(!is_token(TOKEN_RBRACE) && !is_token_keyword(keyword_case) && !is_token_keyword(keyword_default) && !is_token(TOKEN_EOF)) {
	Stmnt* stmnt = parse_stmnt(false);
	buf_push(case_then, stmnt);
      }
      buf_push(cases, SwitchCase{case_loc, exprs, buf_len(exprs), case_then, buf_len(case_then)});
      
      if(is_token_keyword(keyword_default)) {
	break;
      }
    }
    
    if(is_token_keyword(keyword_default)) {
      SourceLocation default_loc = consume_token().loc;
      expect_token(TOKEN_COLON);
      Stmnt** default_stmnts = NULL;
      Stmnt* first = parse_stmnt(false);
      buf_push(default_stmnts, first);
      while(!is_token(TOKEN_RBRACE) && !is_token(TOKEN_EOF)) {
	Stmnt* stmnt = parse_stmnt(false);
	buf_push(default_stmnts, stmnt);
      }
      buf_push(cases, SwitchCase{default_loc, NULL, 0, default_stmnts, buf_len(default_stmnts)});
    }
    expect_token(TOKEN_RBRACE);
    if(!cases) {
      fatal_error(loc, "empty switch statement");
    }
    
    return stmnt_switch(loc, switch_expr, cases, buf_len(cases));
  } else {
    Stmnt* stmnt = parse_stmnt_simple();
    if(!stmnt) {
      error_here("expected statement, but got '%s'", token_to_name_temp(current_token));
    }
    expect_token(TOKEN_SEMICOLON);
    return stmnt;
  }
}

DeclVar* parse_var_decl() {
  assert(is_token_keyword(keyword_var));
  SourceLocation loc = current_token.loc;
  consume_token();
  
  const char* name = parse_name();
  Typespec* type = NULL;
  if(match_token(TOKEN_COLON)) {
    type = parse_typespec();
  }

  Expr* expr = NULL;
  if(match_token(TOKEN_ASSIGN)) {
    expr = parse_expression();
  }

  if(!expr && !type) {
    error_here("variable declaration must have either type of initializer");
  }
  
  expect_token(TOKEN_SEMICOLON);
  return decl_var(loc, name, type, expr);
}

DeclConst* parse_const_decl() {
  assert(is_token_keyword(keyword_const));
  SourceLocation loc = current_token.loc;
  consume_token();
  
  const char* name = parse_name();
  Typespec* type = NULL;
  if(match_token(TOKEN_COLON)) {
    type = parse_typespec();
  }
  
  if(!match_token(TOKEN_ASSIGN)) {
    error_here("constant declaration must have either type of initializer");
  }
  
  Expr* expr = parse_expression();
  expect_token(TOKEN_SEMICOLON);
  return decl_const(loc, name, type, expr);
}

DeclFunc* parse_func_decl() {
  assert(is_token_keyword(keyword_func));
  SourceLocation loc = current_token.loc;
  next_token();

  if(!is_token(TOKEN_NAME)) {
    error_here("expected identifier");
    return NULL;
  }

  const char* name = consume_token().name;
  expect_token(TOKEN_LPAREN);

  FuncParam* params = NULL;
  
  while(!is_token(TOKEN_RPAREN) && !is_token(TOKEN_EOF)) {
    if(!is_token(TOKEN_NAME)) {
      error_here("expected identifier");
      return NULL;
    }

    const char* param_name = consume_token().name;
    
    if(!expect_token(TOKEN_COLON))
      return NULL;

    Typespec* type = parse_typespec();
    if(!type)
      return NULL;
    buf_push(params, FuncParam{ param_name, type });
    if(!match_token(TOKEN_COMMA)) {
      break;
    }
  }

  expect_token(TOKEN_RPAREN);
  
  Typespec* ret = NULL;
  if(match_token(TOKEN_COLON)) {
    ret = parse_typespec();
  }

  StmntBlock* body = parse_stmnt_block(false);
  
  return decl_func(loc, name, params, buf_len(params), ret, body);
}

DeclTypedef* parse_decl_typedef() {
  assert(is_token_keyword(keyword_typedef));
  SourceLocation loc = current_token.loc;
  consume_token();
  
  const char* name = parse_name();
  expect_token(TOKEN_ASSIGN);
  Typespec* type = parse_typespec();
  expect_token(TOKEN_SEMICOLON);
  
  return decl_typedef(loc, name, type);
}

DeclAggregate* parse_decl_aggregate() {
  assert(is_token_keyword(keyword_struct) || is_token_keyword(keyword_union));
  SourceLocation loc = current_token.loc;
  bool is_union = consume_token().name == keyword_union;
  const char* name = parse_name();
  expect_token(TOKEN_LBRACE);
  AggregateField* fields = NULL;
  while(!is_token(TOKEN_RBRACE) && !is_token(TOKEN_EOF)) {
    SourceLocation field_loc = current_token.loc;
    const char* field_name = parse_name();
    expect_token(TOKEN_COLON);
    Typespec* field_type = parse_typespec();
    buf_push(fields, AggregateField{field_loc, field_name, field_type});
    match_token(TOKEN_SEMICOLON);
  }
  expect_token(TOKEN_RBRACE);

  if(!fields) {
    fatal_error(loc, "empty %s declaration", is_union ? "union" : "struct");
  }
  
  return decl_aggregate(loc, name, is_union, fields, buf_len(fields));
}

DeclEnum* parse_decl_enum() {
  assert(is_token_keyword(keyword_enum));
  SourceLocation loc = current_token.loc;
  consume_token();

  const char* name = parse_name();

  expect_token(TOKEN_LBRACE);
  EnumItem* items = NULL;
  while(!is_token(TOKEN_RBRACE) && !is_token(TOKEN_EOF)) {
    SourceLocation item_loc = current_token.loc;
    const char* item_name = parse_name();
    Expr* item_expr = NULL;
    if(match_token(TOKEN_ASSIGN)) {
      item_expr = parse_expression();
    }
    buf_push(items, EnumItem{item_loc, item_name, item_expr});
    match_token(TOKEN_COMMA);
  }
  expect_token(TOKEN_RBRACE);

  return decl_enum(loc, name, items, buf_len(items));
}


Decl* parse_decl() {
  if(is_token_keyword(keyword_var)) {
    return parse_var_decl();
  } else if(is_token_keyword(keyword_const)) {
    return parse_const_decl();
  } else if(is_token_keyword(keyword_func)) {
    return parse_func_decl();
  } else if(is_token_keyword(keyword_typedef)) {
    return parse_decl_typedef();
  } else if(is_token_keyword(keyword_struct)) {
    return parse_decl_aggregate();
  } else if(is_token_keyword(keyword_union)) {
    return parse_decl_aggregate();
  } else if(is_token_keyword(keyword_enum)) {
    return parse_decl_enum();
  } else {
    error_here("expected declaration, got %s", token_to_name_temp(current_token));
  }
  return NULL;
}

struct DeclSet {
  Decl** decls;
  size_t num_decls;
};

DeclSet parse_declset() {
  Decl** decls = NULL;
  while(!is_token(TOKEN_EOF)) {
    Decl* decl = parse_decl();
    if(!decl)
      break;
    buf_push(decls, decl);
  }
  return {decls, buf_len(decls)};
}

void parser_test() {
  const char* code =
    // "func call_test() { return call(arg1 = 1, arg2 = 2,); }"
    "func for_test() { for(i := 0; i < 10; i += 1) print(i); }"
    "var arr : int[15] = (:int*[15][20]){12, 3, 4};"
    "const test_tern = 5 >= 4 ? 4 + 2 : 5 * 5;"
    "func if_test():int { if(1) { return 0; } else { return 5; } }"
    "func single_stmnt_if() { if(1) return 0; else return 5; }"
    "func else_if() { if(11) return 5; else if(2) { return 5; } else return 4; }"
    "var test = 1 + 2 * 3;"
    "var test2 = (1 + 2) * 3;"
    "var a = true;"
    "const b = 15;"
    "var c: int = 10;"
    "var d: float;"
    "func test() { return 0; }"
    "func another_test(a: int) { return 1; }"
    "func another_test(a: int, b: float) { return 1; }"
    "func another_test(a: int*) { return 1 + 2; }"
    "func another_test(a: double*) { return 1; }"
    "func while_test() { while(0) { return 5; } }"
    "func do_test() { do{ return while_test(); } while(0); }"
    // "func call_test() { return call(arg1 = 1, arg2 = 2, 3 = 3); }"
    "func stmnt_decl() { i := 5; s := 3; return i; }"
    // "func stmnt_decl() { i :5; s := 3; return i; }"
    ;

  init_stream(NULL, code);

  Decl** decls = NULL;
  while(!is_token(TOKEN_EOF)) {
    Decl* decl = parse_decl();
    buf_push(decls, decl);
  }
  
  printf("\n");
}

