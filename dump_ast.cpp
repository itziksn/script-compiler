/*
int indent_level;

void dump_line(const char* fmt, ...) {
  printf("\n%.*s", indent_level * 4, "                                                    ");
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
}

void dump(const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
}

void dump_expr(Expr* expr);

void dump_typespec(Typespec* type) {
  switch (type->kind) {
  case TYPESPEC_NAME: {
    TypespecName* name = static_cast<TypespecName*>(type);
    dump("%s", name->name);
  } break;
  case TYPESPEC_PTR: {
    TypespecPtr* ptr = static_cast<TypespecPtr*>(type);
    dump("(ptr ");
    dump_typespec(ptr->base);
    dump(")");
  } break;
  case TYPESPEC_ARR: {
    TypespecArr* arr = static_cast<TypespecArr*>(type);
    dump("(arr ");
    dump_typespec(arr->elem);
    dump(" [");
    dump_expr(arr->size);
    dump("]");
    dump(")");
  } break;
  default:
    assert(0);
  }
}

void dump_expr(Expr* expr) {
  switch (expr->kind) {
  case EXPR_CONST: {
    auto const_val = static_cast<ExprConst*>(expr);
    switch (const_val->val.kind) {
    case CONST_INTEGER:
      switch(const_val->val.mod) {
      case MOD_NONE:
	dump("%ll", (long long)const_val->val.ull);
	break;
      case MOD_BOOL:
	dump("%s", const_val->val.ull ? "true" : "false");
	break;
      case MOD_CHAR:
	dump("'%c'", (char)const_val->val.ull);
	break;
      default:
	assert(0);
      }
    case CONST_FLOAT:
      dump("%f", const_val->val.d);
      break;
    case CONST_STRING:
      dump("(\"%.*s\" %d)", (int)const_val->val.str.len, const_val->val.str.buf, (int)const_val->val.str.len);
      break;
    default:
      assert(0);
    }
  } break;
  case EXPR_BINARY: {
    ExprBinary* bin = static_cast<ExprBinary*>(expr);
    dump("(%s ", token_kind_name(bin->op));
    dump_expr(bin->left);
    dump(" ");
    dump_expr(bin->right);
    dump(")");
  } break;
  case EXPR_TERNARY: {
    ExprTernary* tern = static_cast<ExprTernary*>(expr);
    dump("(? ");
    dump_expr(tern->cond);
    dump(" ");
    dump_expr(tern->then_expr);
    dump(" ");
    dump_expr(tern->else_expr);
    dump(")");
  } break;
  case EXPR_CALL: {
    ExprCall* call = static_cast<ExprCall*>(expr);
    dump("(");
    dump_expr(call->func);
    for (CallArg* arg = call->args; arg != call->args + call->num_args; ++arg) {
      dump(" ");
      if (arg->name) {
	dump("%s = ", arg->name);
      }
      dump_expr(arg->expr);
    }
    dump(")");
  } break;
  case EXPR_NAME: {
    ExprName* name = static_cast<ExprName*>(expr);
    dump("%s", name->name);
  } break;
  case EXPR_COMPOUND: {
    ExprCompound* compound = static_cast<ExprCompound*>(expr);
    dump("(compound ");
    if (compound->type) {
      dump_typespec(compound->type);
    }
    else {
      dump("NO_TYPE");
    }
    dump(" (items ");
    ++indent_level;
    for (CompoundItem* item = compound->items; item != compound->items + compound->num_items; ++item) {
      dump_line("(");
      if (item->kind == COMPOUND_NAMED) {
	dump("%s = ", item->name);
      }
      else if (item->kind == COMPOUND_INDEXED) {
	dump("[");
	dump_expr(item->index);
	dump("] = ");
      }
      dump_expr(item->expr);
      dump(")");
    }
    --indent_level;
    dump(")");
  } break;
    // case EXPR_UNARY:
    //   break;
  default:
    assert(0);
    break;
  }
}

void dump_stmnt(Stmnt* stmnt) {
  switch (stmnt->kind) {
  case STMNT_RETURN: {
    StmntReturn* ret = static_cast<StmntReturn*>(stmnt);
    dump_line("(return");
    if (ret->expr) {
      dump(" ");
      dump_expr(ret->expr);
    }
    dump(")");
  } break;
  case STMNT_BLOCK: {
    StmntBlock* block = static_cast<StmntBlock*>(stmnt);
    dump_line("(block ");
    ++indent_level;
    for (size_t i = 0; i < block->num_stmnts; ++i) {
      dump_stmnt(block->stmnts[i]);
    }
    dump(")");
    --indent_level;
  } break;
  case STMNT_IF: {
    StmntIf* stmnt_if = static_cast<StmntIf*>(stmnt);
    dump_line("(if ");
    dump_expr(stmnt_if->cond);
    ++indent_level;
    dump_stmnt(stmnt_if->then_stmnt);
    --indent_level;

    for (ElseIf* elseif = stmnt_if->elseifs; elseif != stmnt_if->elseifs + stmnt_if->num_elseifs; ++elseif) {
      dump_line("(else if ");
      dump_expr(elseif->cond);
      ++indent_level;
      dump_stmnt(elseif->then_stmnt);
      dump(")");
      --indent_level;
    }

    if (stmnt_if->else_stmnt) {
      dump_line("(else");
      ++indent_level;
      dump_stmnt(stmnt_if->else_stmnt);
      dump(")");
      --indent_level;
    }
    dump(")");
  } break;
  case STMNT_WHILE: {
    StmntWhile* stmnt_while = static_cast<StmntWhile*>(stmnt);
    if (stmnt_while->is_do_while) {
      dump_line("(do ");
      ++indent_level;
      dump_stmnt(stmnt_while->then_stmnt);
      --indent_level;
      dump_line("(while ");
      dump_expr(stmnt_while->cond);
      dump("))");
    }
    else {
      dump_line("(while ");
      dump_expr(stmnt_while->cond);
      ++indent_level;
      dump_stmnt(stmnt_while->then_stmnt);
      dump(")");
      --indent_level;
    }
  } break;
  case STMNT_FOR: {
    StmntFor* stmnt_for = static_cast<StmntFor*>(stmnt);
    dump_line("(for");
    if (stmnt_for->init) {
      dump(" ");
      dump_stmnt(stmnt_for->init);
    }
    if (stmnt_for->cond) {
      dump(" ");
      dump_expr(stmnt_for->cond);
    }
    if (stmnt_for->next) {
      dump(" ");
      dump_stmnt(stmnt_for->next);
    }
    dump_stmnt(stmnt_for->then_stmnt);
    dump(")");
  } break;
  case STMNT_DECL: {
    StmntDecl* decl = static_cast<StmntDecl*>(stmnt);
    dump("(let %s", decl->name);
    if (decl->type) {
      dump(" ");
      dump_typespec(decl->type);
    }
    else {
      dump(" NO_TYPE");
    }
    if (decl->expr) {
      dump(" ");
      dump_expr(decl->expr);
    }
    dump(")");
  } break;
  case STMNT_ASSIGN: {
    StmntAssign* assign = static_cast<StmntAssign*>(stmnt);
    dump("(= ");
    dump_expr(assign->name);
    dump(" ");
    dump_expr(assign->expr);
    dump(")");
  } break;
  case STMNT_EXPR: {
    StmntExpr* stmnt_expr = static_cast<StmntExpr*>(stmnt);
    dump_expr(stmnt_expr->expr);
  } break;
  default:
    assert(0);
    break;
  }
}

void dump_decl(Decl* decl) {
  switch (decl->kind) {
  case DECL_CONST: {
    DeclConst* var = static_cast<DeclConst*>(decl);
    dump_line("(var %s", var->name);
    if (var->type) {
      dump(" ");
      dump_typespec(var->type);
    }
    else {
      dump(" NO_TYPE");
    }
    if (var->expr) {
      dump(" ");
      dump_expr(var->expr);
    }
    dump(")");
  } break;
  case DECL_VAR: {
    DeclVar* var = static_cast<DeclVar*>(decl);
    dump_line("(var %s", var->name);
    if (var->type) {
      dump(" ");
      dump_typespec(var->type);
    }
    else {
      dump(" NO_TYPE");
    }
    if (var->expr) {
      dump(" ");
      dump_expr(var->expr);
    }
    dump(")");
  } break;
  case DECL_FUNC: {
    DeclFunc* func = static_cast<DeclFunc*>(decl);
    dump_line("(func %s (", func->name);
    for (size_t i = 0; i < func->num_params; ++i) {
      if (i != 0) dump(", ");
      FuncParam param = func->params[i];
      dump("%s ", param.name);
      dump_typespec(param.type);
    }
    dump(")");
    ++indent_level;
    dump_stmnt(func->body);
    dump(")");
    --indent_level;
  } break;
  default:
    assert(0);
    break;
  }
}
*/
