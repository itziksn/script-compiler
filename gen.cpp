char* gen_buffer;
int gen_indent;

size_t gen_current_line;
size_t source_line;
const char* gen_file_name;
const char* source_file_name;

const char* preamble =
  "typedef signed char schar;\n"
  "typedef unsigned char uchar;\n"
  "typedef unsigned short ushort;\n"
  "typedef unsigned int uint;\n"
  "typedef unsigned long ulong;\n"
  "typedef long long llong;\n"
  "typedef unsigned long long ullong;\n";


#define genf(...) buf_printf(gen_buffer, __VA_ARGS__)
#define genln(...) gen_line(); buf_printf(gen_buffer, __VA_ARGS__)

void gen_fixup_lines() {
  if(gen_current_line != source_line || gen_file_name != source_file_name) {
    genf("\n#line %llu", source_line);
    gen_current_line = source_line;
    if(gen_file_name != source_file_name) {
      genf(" \"%s\"", source_file_name);
      gen_file_name = source_file_name;
    }
  }
}

void gen_line() {
  ++gen_current_line;
  gen_fixup_lines();
  genf("\n%.*s", gen_indent * 4, "                                          ");
}

void gen_expr(Expr* expr);
void gen_cdecl_type(Type* type, const char* name);

void gen_const(ExprConst* cnst) {
  switch(cnst->val.kind) {
  case CONST_DEFAULT:
	genf("{}");
	break;
  case CONST_INTEGER:
    if(cnst->val.mod == MOD_CHAR) {
      genf("'%c'", cnst->val.c);
    } else {
      genf("%llu", cnst->val.ull);
    }
    break;
  case CONST_FLOAT:
    if(cnst->val.postfix == POSTFIX_D) {
      genf("%f", cnst->val.d);
    } else {
      genf("%ff", cnst->val.d);
    }
    break;
  case CONST_STRING: {
    genf("\"");
    const char* ptr = cnst->val.str.buf;
    const char* end = cnst->val.str.buf + cnst->val.str.len - 1;
    while(ptr != end) {
      switch(*ptr) {
      case '\0':
	genf("\\0");
	break;
      case '\\':
	genf("\\\\");
	break;
      case '\n':
	genf("\\n");
	break;
      case '\t':	
	genf("\\t");
	break;
      case '\r':
	genf("\\r");
	break;
      case '\a':
	genf("\\a");
	break;
      case '\b':
	genf("\\b");
	break;
      default:
	if(isprint(*ptr)) {
	  genf("%c", *ptr);
	} else {
	  genf("\\x%x", (unsigned char)*ptr);
	}
      }
      ++ptr;
    }
    genf("\"");
  } break;
  default:
    assert(0);
  }
}

void gen_cdecl_typespec(Typespec* type, const char* name) {
  if(type->is_const) {
	genf("const ");
  }
  switch(type->kind) {
  case TYPESPEC_NAME: {
    genf("%s", static_cast<TypespecName*>(type)->name);
    if(name) {
      genf(" %s", name);
    }
  } break;
  case TYPESPEC_PTR: {
    gen_cdecl_typespec(static_cast<TypespecPtr*>(type)->base, NULL);
    if(name) {
      genf(" *%s", name);
    } else {
      genf("*");
    }
  } break;
  case TYPESPEC_ARR: {
    TypespecArr* arr = static_cast<TypespecArr*>(type);
    gen_cdecl_typespec(arr->elem, NULL);
    if(name) {
      genf(" %s", name);
    }
    genf("[");
	if(arr->size) {
      gen_expr(arr->size);
	}
	genf("]");
  } break;
  case TYPESPEC_FUNC: {
    TypespecFunc* func = static_cast<TypespecFunc*>(type);
    gen_cdecl_typespec(func->ret, NULL);
    if(name) {
      genf(" (%s*)", name);
    } else {
      genf("(*)");
    }
    genf("(");
    for(size_t i = 0; i < func->num_params; ++i) {
      if(i != 0) {
	genf(", ");
      }
      gen_cdecl_typespec(func->params[i], NULL);
    }
    genf(")");
  } break;
  default:
    assert(0);
  }
}

void gen_expr(Expr* expr) {
  switch(expr->kind) {
  case EXPR_CONST:
    gen_const(static_cast<ExprConst*>(expr));
    break;
  case EXPR_NAME:
    genf("%s", static_cast<ExprName*>(expr)->name);
    break;
  case EXPR_UNARY: {
    ExprUnary* un = static_cast<ExprUnary*>(expr);
    genf("%s", token_kind_name(un->op));
    gen_expr(un->operand);
  } break;
  case EXPR_BINARY: {
    ExprBinary* bin = static_cast<ExprBinary*>(expr);
    gen_expr(bin->left);
    genf(" %s ", token_kind_name(bin->op));
    gen_expr(bin->right);
  } break;
  case EXPR_TERNARY: {
    ExprTernary* ter = static_cast<ExprTernary*>(expr);
    gen_expr(ter->cond);
    genf(" ? ");
    gen_expr(ter->then_expr);
    genf(" : ");
    gen_expr(ter->else_expr);
  } break;
  case EXPR_CALL: {
    ExprCall* call = static_cast<ExprCall*>(expr);
    gen_expr(call->func);
    genf("(");
	SortedCall* sorted_call = get_sorted_call(expr);
    for(size_t i = 0; i < sorted_call->num_exprs; ++i) {
      if(i != 0) {
		genf(", ");
      }
      gen_expr(sorted_call->exprs[i]);
    }
    genf(")");
  } break;
  case EXPR_CAST: {
    ExprCast* cast = static_cast<ExprCast*>(expr);
    genf("(");
    gen_cdecl_typespec(cast->type, NULL);
    genf(")");
    gen_expr(cast->src_expr);
  } break;
  case EXPR_FIELD: {
    ExprField* field = static_cast<ExprField*>(expr);
    gen_expr(field->left_expr);
    genf(".%s", field->name);
  } break;
  case EXPR_COMPOUND: {
	ExprCompound* comp = static_cast<ExprCompound*>(expr);
	ResolvedExprList* list = get_sorted_expr_lists(expr);
	if(comp->type) {
	  if(comp->type->kind != TYPESPEC_ARR) {
		gen_cdecl_typespec(comp->type, NULL);
	  }
	} else {
	  if(list->type->kind != TYPE_ARRAY) {
		gen_cdecl_type(list->type, NULL);
	  }
	}
	genf(" {");
	++gen_indent;
	for(size_t i = 0; i < list->num_sorted_exprs; ++i) {
	  if(i != 0) {
		genf(", ");
	  }
	  gen_expr(list->sorted_exprs[i]);
	}
	--gen_indent;
	genf("}");
    break;
  }
  default:
    assert(0);
  }
}

void gen_cdecl_type(Type* type, const char* name) {
  switch(type->kind) {
  case TYPE_BOOL:
    genf("bool %s", name ? name : "");
    break;
  case TYPE_CHAR:
    genf("char %s", name ? name : "");
    break;
  case TYPE_SCHAR:
    genf("signed char %s", name ? name : "");
    break;
  case TYPE_UCHAR:
    genf("unsigned char %s", name ? name : "");
    break;
  case TYPE_SHORT:
    genf("short %s", name ? name : "");
    break;
  case TYPE_USHORT:
    genf("unsigend short %s", name ? name : "");
    break;
  case TYPE_INT:
    genf("int %s", name ? name : "");
    break;
  case TYPE_UINT:
    genf("unsigned int %s", name ? name : "");
    break;
  case TYPE_LONG:
    genf("long %s", name ? name : "");
    break;
  case TYPE_ULONG:
    genf("unsigned long %s", name ? name : "");
    break;
  case TYPE_LLONG:
    genf("long long %s", name ? name : "");
    break;
  case TYPE_ULLONG:
    genf("unsigned long long %s", name ? name : "");
    break;
  case TYPE_FLOAT:
    genf("float %s", name ? name : "");
    break;
  case TYPE_DOUBLE:
    genf("double %s", name ? name : "");
    break;
  case TYPE_PTR: {
    TypePtr* ptr = static_cast<TypePtr*>(type);
    gen_cdecl_type(ptr->base, NULL);
    genf("*");
    if(name) {
      genf(" %s", name);
    }
  } break;
  case TYPE_ARRAY: {
    TypeArray* arr = static_cast<TypeArray*>(type);
    gen_cdecl_type(arr->elem, NULL);
    if(name) {
      genf(" %s", name);
    }
    genf("[%llu]", arr->len);
  } break;
  case TYPE_FUNC: {
    TypeFunc* func = static_cast<TypeFunc*>(type);
    gen_cdecl_type(func->ret, NULL);
    if(name) {
      genf(" (*%s)(", name);
    } else {
      genf("(*)(");
    }
    for(size_t i = 0; i < func->num_params; ++i) {
      if(i != 0) {
		genf(", ");
      }
      gen_cdecl_type(func->param_types[i], NULL);  // @Incomplete: We able to reserve the names (if provided), but let's ignore it for now
    }
    genf(")");
  } break;
  case TYPE_CONST: {
    TypeConst* cnst = static_cast<TypeConst*>(type);
    genf("const ");
    gen_cdecl_type(cnst, name);
  } break;
  case TYPE_AGGREGATE: {
	TypeAggregate* agg = static_cast<TypeAggregate*>(type);
	genf("%s", agg->name);
	if(name) {
	  genf(" %s", name);
	}
	break;
  }
  default:
    assert(0);
  }
}

void gen_simple_stmnt(Stmnt* stmnt) {
  switch(stmnt->kind) {
  case STMNT_DECL: {
    StmntDecl* decl = static_cast<StmntDecl*>(stmnt);
    if(decl->type) {
      gen_cdecl_typespec(decl->type, decl->name);
    } else {
      Operand op = get_resolved_op(decl->expr);
      gen_cdecl_type(op.type, decl->name);
    }
    genf(" = ");
    if(decl->expr) {
      gen_expr(decl->expr);
    } else {
      genf("{}");
    }
  } break;
  case STMNT_ASSIGN: {
    StmntAssign* assign = static_cast<StmntAssign*>(stmnt);
    gen_expr(assign->target);
    genf(" %s ", token_kind_name(assign->op));
    gen_expr(assign->expr);
  } break;
  case STMNT_EXPR:
    gen_expr(static_cast<StmntExpr*>(stmnt)->expr);
    break;
  default:
    assert(0);
  }
}

void gen_stmnt(Stmnt* stmnt) {
  source_file_name = stmnt->loc.filename;
  source_line = stmnt->loc.line;
  switch(stmnt->kind) {
  case STMNT_RETURN: {
    StmntReturn* ret = static_cast<StmntReturn*>(stmnt);
    genln("return");
    if(ret->expr) {
      genf(" ");
      gen_expr(ret->expr);
    }
    genf(";");
  } break;
  case STMNT_BLOCK: {
    StmntBlock* block = static_cast<StmntBlock*>(stmnt);
    genf("{");
    ++gen_indent;
    for(size_t i = 0; i < block->num_stmnts; ++i) {
      gen_stmnt(block->stmnts[i]);
    }
    --gen_indent;
    genln("}");
  } break;
  case STMNT_WHILE: {
    StmntWhile* whl = static_cast<StmntWhile*>(stmnt);
    if(whl->is_do_while) {
      genln("do ");
    } else {
      genln("while (");
      gen_expr(whl->cond);
      genf(") ");
    }
    gen_stmnt(whl->then_stmnt);
    if(whl->is_do_while) {
      genf(" while(");
      gen_expr(whl->cond);
      genf(");");
    }
  } break;
  case STMNT_IF: {
    StmntIf* ifs = static_cast<StmntIf*>(stmnt);
    genln("if (");
    gen_expr(ifs->cond);
    genf(") ");
    gen_stmnt(ifs->then_stmnt);
    for(size_t i = 0; i < ifs->num_elseifs; ++i) {
      genln(" else if(");
      gen_expr(ifs->elseifs[i].cond);
      genf(") ");
      gen_stmnt(ifs->elseifs[i].then_stmnt);
    }
    if(ifs->else_stmnt) {
      genln(" else ");
      gen_stmnt(ifs->else_stmnt);
    }
  } break;
  case STMNT_FOR: {
    StmntFor* fors = static_cast<StmntFor*>(stmnt);
    genln("for (");
    if(fors->init) {
      gen_simple_stmnt(fors->init);
    }
    genf(";");
    if(fors->cond) {
      genf(" ");
      gen_expr(fors->cond);
    }
    genf(";");
    if(fors->next) {
      genf(" ");
      gen_simple_stmnt(fors->next);
    }
    genf(") ");
    ++gen_indent;
    gen_stmnt(fors->then_stmnt);
    --gen_indent;
  } break;
  case STMNT_BREAK:
    genln("break;");
    break;
  case STMNT_CONTINUE:
    genln("continue;");
    break;
  case STMNT_SWITCH: {
    StmntSwitch* swch = static_cast<StmntSwitch*>(stmnt);
    genln("switch(");
    gen_expr(swch->control_expr);
    genf(") {");
    for(size_t i = 0; i < swch->num_cases; ++i) {
      SwitchCase scase = swch->cases[i];
      if(scase.exprs) {
	for(Expr** it = scase.exprs; it != scase.exprs + scase.num_exprs; ++it) {
	  genln("case ");
	  gen_expr(*it);
	  genf(":");
	}
      } else {
	genln("default:");
      }
      genf(" {");
      ++gen_indent;
      for(Stmnt** it = scase.stmnts; it != scase.stmnts + scase.num_stmnts; ++it) {
	gen_stmnt(*it);
      }
      genln("break;");
      --gen_indent;
      genln("}");
    }
    genln("}");
  } break;
  default:
    gen_line();
    gen_simple_stmnt(stmnt);
    genf(";");
    break;
  }
}

void gen_decl(Decl* decl) {
  source_file_name = decl->loc.filename;
  source_line = decl->loc.line;
  switch(decl->kind) {
  case DECL_FUNC: {
    DeclFunc* func = static_cast<DeclFunc*>(decl);
    gen_line();
    if(func->ret_type) {
      gen_cdecl_typespec(func->ret_type, NULL);
    } else {
      genf("void");
    }
    genf(" %s(", func->name);
    for(size_t i = 0; i < func->num_params; ++i) {
      if(i != 0) {
	genf(", ");
      }
      gen_cdecl_typespec(func->params[i].type, func->params[i].name);
    }
    genf(") ");
    gen_stmnt(func->body);
  } break;
  case DECL_VAR: {
    DeclVar* var = static_cast<DeclVar*>(decl);
    gen_line();
    if(var->type) {
      gen_cdecl_typespec(var->type, var->name);
    } else {
      Operand op = get_resolved_op(var->expr);
      gen_cdecl_type(op.type, var->name);
    }
    
    genf(" = ");
    if(var->expr) {
      gen_expr(var->expr);
    } else {
      genf("{}");
    }
    genf(";");
  } break;
  case DECL_CONST: {
    DeclConst* cnst = static_cast<DeclConst*>(decl);
    genln("#define %s ", cnst->name);
    if(cnst->type) {
      genf("(");
      gen_cdecl_typespec(cnst->type, NULL);
      genf(")");
    }
    gen_expr(cnst->expr);
  } break;
  case DECL_TYPEDEF: {
    DeclTypedef* type = static_cast<DeclTypedef*>(decl);
    genln("typedef ");
    gen_cdecl_typespec(type->type, type->name);
    genf(";");
  } break;
  case DECL_AGGREGATE: {
    DeclAggregate* agg = static_cast<DeclAggregate*>(decl);
    genln("%s %s {", agg->is_union ? "union" : "struct", agg->name);
    ++gen_indent;
    for(size_t i = 0; i < agg->num_fields; ++i) {
      AggregateField field = agg->fields[i];
      gen_line();
      gen_cdecl_typespec(field.type, field.name);
      genf(";");
    }
    --gen_indent;
    genln("};");
  } break;
  case DECL_ENUM: {
    DeclEnum* enm = static_cast<DeclEnum*>(decl);
    genln("typedef int %s;", enm->name);
  } break;
  default:
    assert(0);
  }
}

void gen_ordered_decls() {
  buf_printf(gen_buffer, preamble);
  for(size_t i = 0; i < buf_len(ordered_decls); ++i) {
    gen_decl(ordered_decls[i]);
  }
}

void gen_test() {
  gen_cdecl_typespec(typespec_arr(typespec_ptr(typespec_name("int")), expr_const(ConstVal{})), "var_name");
  printf(gen_buffer);
}
