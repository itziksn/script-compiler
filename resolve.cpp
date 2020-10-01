enum TypeKind {
  TYPE_ERROR,
  TYPE_VOID,
  TYPE_BOOL,
  TYPE_CHAR,
  TYPE_SCHAR,
  TYPE_UCHAR,
  TYPE_SHORT,
  TYPE_USHORT,
  TYPE_INT,
  TYPE_UINT,
  TYPE_LONG,
  TYPE_ULONG,
  TYPE_LLONG,
  TYPE_ULLONG,
  TYPE_PTR,
  TYPE_FLOAT,
  TYPE_DOUBLE,
  TYPE_ARRAY,
  TYPE_FUNC,
  TYPE_CONST,
  TYPE_AGGREGATE,
};

typedef uint32_t TypeId;

static TypeId types_counter;

TypeId next_typeid() {
  return types_counter++;
}

struct Type {
  TypeKind kind;
  TypeId type_id;

  size_t size;
  size_t align;
};

struct TypeArray : Type {
  Type* elem;
  unsigned long long len;
};

struct TypePtr : Type {
  Type* base;
};

struct TypeFunc : Type {
  Type** params;
  size_t num_params;
  Type* ret;
};

struct TypeConst : Type {
  Type* base;
};

struct AggregateTypeItem {
  const char* name;
  Type* type;
};

struct TypeAggregate : Type {
  const char* name;
  bool is_union;
  AggregateTypeItem* items;
  size_t num_items;
};

char* type_name_buffer(Type* type, char* buffer) {
  switch(type->kind) {
  case TYPE_VOID:
    buf_printf(buffer, "void");
    break;
  case TYPE_BOOL:
    buf_printf(buffer, "bool");
    break;
  case TYPE_CHAR:
    buf_printf(buffer, "char");
    break;
  case TYPE_SCHAR:
    buf_printf(buffer, "schar");
    break;
  case TYPE_UCHAR:
    buf_printf(buffer, "uchar");
    break;
  case TYPE_SHORT:
    buf_printf(buffer, "short");
    break;
  case TYPE_USHORT:
    buf_printf(buffer, "ushort");
    break;
  case TYPE_INT:
    buf_printf(buffer, "int");
    break;
  case TYPE_UINT:
    buf_printf(buffer, "uint");
    break;
  case TYPE_LONG:
    buf_printf(buffer, "long");
    break;
  case TYPE_ULONG:
    buf_printf(buffer, "ulong");
    break;
  case TYPE_LLONG:
    buf_printf(buffer, "llong");
    break;
  case TYPE_ULLONG:
    buf_printf(buffer, "ulong");
    break;
  case TYPE_PTR: {
    TypePtr* ptr = static_cast<TypePtr*>(type);
    buffer = type_name_buffer(ptr->base, buffer);
    buf_printf(buffer, "*");
  } break;
  case TYPE_CONST: {
    TypeConst* cnst = static_cast<TypeConst*>(type);
    buf_printf(buffer, "const ");
    buffer = type_name_buffer(cnst->base, buffer);
  } break;
  case TYPE_ARRAY: {
    TypeArray* arr = static_cast<TypeArray*>(type);
    buffer = type_name_buffer(arr->elem, buffer);
    buf_printf(buffer, "[%lld]", arr->len);
  } break;
  case TYPE_FLOAT:
    buf_printf(buffer, "float");
    break;
  case TYPE_DOUBLE:
    buf_printf(buffer, "double");
    break;
  case TYPE_AGGREGATE:
    buf_printf(buffer, static_cast<TypeAggregate*>(type)->name);
    break;
  case TYPE_FUNC: {
    TypeFunc* func = static_cast<TypeFunc*>(type);
    buf_printf(buffer, "func(");
    for(size_t i = 0; i < func->num_params; ++i) {
      if(i != 0) {
	buf_printf(buffer, ", ");
      }
      buffer = type_name_buffer(func->params[i], buffer);
    }
    buf_printf(buffer, "):");
    buffer = type_name_buffer(func->ret, buffer);
  } break;
  default:
    assert(0);
  }
  return buffer;
}

const char* type_name(Type* type) {
  return type_name_buffer(type, NULL);
}

void init_type(Type* type, TypeKind kind, size_t size, size_t align) {
  type->kind = kind;
  type->type_id = next_typeid();
  type->size = size;
  type->align = align;
}

Type* create_primitive_type(TypeKind kind, size_t size, size_t align) {
  Type* type = (Type*)malloc(sizeof(Type));
  init_type(type, kind, size, align);
  return type;
}

Map cached_func_types;

uint64_t hash_function(Type** params, size_t num_params, Type* ret) {
  uint64_t hash = 1315423911;
  for(Type** it = params; it != params + num_params; ++it) {
    hash ^= ((hash << 5) + (hash_ptr((intptr_t)(*it)) + (hash >> 2)));
  }
  hash ^= ((hash << 5) + (hash_ptr((intptr_t)ret) + (hash >> 2)));
  
  return hash;
}

struct CachedEntryTypeFunc {
  TypeFunc* func;
  CachedEntryTypeFunc* next;
};

TypeFunc* type_func(Type** params, size_t num_params, Type* ret) {
  uint64_t hash = hash_function(params, num_params, ret);
  CachedEntryTypeFunc* first = (CachedEntryTypeFunc*)map_get_hashed(&cached_func_types, (void*)hash, hash);
  for(CachedEntryTypeFunc* it = first; it; it = it->next) {
    if(it->func->ret == ret && it->func->num_params == num_params) {
      for(size_t i = 0; i < num_params; ++i) {
	if(params[i] != it->func->params[i]) {
	  goto next;
	}
      }
      return it->func;
    }
  next:
    ;
  }

  TypeFunc* type = (TypeFunc*)malloc(sizeof(TypeFunc));
  init_type(type, TYPE_FUNC, sizeof(void*), alignof(void*));
  type->params = params;
  type->num_params = num_params;
  type->ret = ret;

  CachedEntryTypeFunc* new_entry = (CachedEntryTypeFunc*)malloc(sizeof(CachedEntryTypeFunc));
  new_entry->func = type;
  new_entry->next = first;
  
  map_put_hashed(&cached_func_types, (void*)hash, new_entry, hash);
  
  return type;
}

Map cached_ptr_types;

TypePtr* type_ptr(Type* base) {
  TypePtr* cached = (TypePtr*)map_get(&cached_ptr_types, base);
  if(cached) {
    return cached;
  }

  TypePtr* type = (TypePtr*)malloc(sizeof(TypePtr));
  init_type(type, TYPE_PTR, sizeof(void*), alignof(void*)); // @Temp
  type->base = base;

  map_put(&cached_ptr_types, base, type);
  
  return type;
}

TypeConst** cached_const_types;

TypeConst* type_const(Type* base) {
  for(TypeConst** it = cached_const_types; it != buf_end(cached_const_types); ++it) {
    TypeConst* cnst = *it;
    if(cnst->base == base) {
      return cnst;
    }
  }
  TypeConst* type = (TypeConst*)malloc(sizeof(TypeConst));
  init_type(type, TYPE_CONST, base->size, base->align);
  type->base = base;
  buf_push(cached_const_types, type);
  
  return type;
}

TypeArray** cached_arr_types;

TypeArray* type_arr(Type* elem, size_t len) {
  for(TypeArray** it = cached_arr_types; it != buf_end(cached_arr_types); ++it) {
    TypeArray* arr = *it;
    if(arr->len == len && arr->elem == elem) {
      return arr;
    }
  }

  TypeArray* type = (TypeArray*)malloc(sizeof(TypeArray));
  init_type(type, TYPE_ARRAY, elem->size*len, elem->align);
  type->elem = elem;
  type->len = len;
  buf_push(cached_arr_types, type);
  
  return type;
}

TypeAggregate* type_aggregate(const char* name, bool is_union, AggregateTypeItem* items, size_t num_items, size_t size, size_t align) {
  TypeAggregate* type = (TypeAggregate*)malloc(sizeof(TypeAggregate));

  init_type(type, TYPE_AGGREGATE, size, align);
  type->name = name;
  type->is_union = is_union;
  type->items = items;
  type->num_items = num_items;

  return type;
}

Type* type_void;

Type* type_bool;
Type* type_char;
Type* type_schar;
Type* type_uchar;
Type* type_short;
Type* type_ushort;
Type* type_int;
Type* type_uint;
Type* type_long;
Type* type_ulong;
Type* type_llong;
Type* type_ullong;
Type* type_float;
Type* type_double;

// @Temp
#define type_ptrdiff type_llong


enum OperandKind {
  OPERAND_NONE,
  OPERAND_LVALUE,
  OPERAND_RVALUE,
  OPERAND_CONST,
  OPERAND_FUNC,
};

struct Operand {
  OperandKind kind;
  Type* type;
  ConstVal val;
  bool is_compound;
};

struct ResolvedOperand {
  void* key;
  Operand op;
};

Map resolved_ops_map;

Operand get_resolved_op(void* key) {
  return *(Operand*)map_get(&resolved_ops_map, key);
}

void put_resolved_op(void* key, Operand op) {
  Operand* op_ptr = (Operand*)malloc(sizeof(Operand));
  *op_ptr = op;
  map_put(&resolved_ops_map, key, op_ptr);
}

int type_rank(Type* type) {
  switch(type->kind) {
  case TYPE_BOOL:
    return 1;
  case TYPE_CHAR:
  case TYPE_SCHAR:
  case TYPE_UCHAR:
    return 2;
  case TYPE_SHORT:
  case TYPE_USHORT:
    return 3;
  case TYPE_INT:
  case TYPE_UINT:
    return 4;
  case TYPE_LONG:
  case TYPE_ULONG:
    return 5;
  case TYPE_LLONG:
  case TYPE_ULLONG:
    return 6;
  default:
    assert(0);
    return 0;
  }
}

bool is_integer_type(Type* type) {
  switch(type->kind) {
  case TYPE_BOOL:
  case TYPE_CHAR:
  case TYPE_SCHAR:
  case TYPE_UCHAR:
  case TYPE_SHORT:
  case TYPE_USHORT:
  case TYPE_INT:
  case TYPE_UINT:
  case TYPE_LONG:
  case TYPE_ULONG:
  case TYPE_LLONG:
  case TYPE_ULLONG:
    return true;
  default:
    return false;
  }
}

bool is_signed_type(Type* type) {
  switch(type->kind) {
  case TYPE_CHAR:
  case TYPE_SCHAR:
  case TYPE_SHORT:
  case TYPE_INT:
  case TYPE_LONG:
  case TYPE_LLONG:
    return true;
  default:
    return false;
  }
}

bool is_arithmetic_type(Type* type) {
  switch(type->kind) {
  case TYPE_BOOL:
  case TYPE_CHAR:
  case TYPE_SCHAR:
  case TYPE_UCHAR:
  case TYPE_SHORT:
  case TYPE_USHORT:
  case TYPE_INT:
  case TYPE_UINT:
  case TYPE_LONG:
  case TYPE_ULONG:
  case TYPE_LLONG:
  case TYPE_ULLONG:
  case TYPE_FLOAT:
  case TYPE_DOUBLE:
    return true;
  default:
    return false;
  }
}

bool is_scalar_type(Type* type) {
  switch(type->kind) {
  case TYPE_BOOL:
  case TYPE_CHAR:
  case TYPE_SCHAR:
  case TYPE_UCHAR:
  case TYPE_SHORT:
  case TYPE_USHORT:
  case TYPE_INT:
  case TYPE_UINT:
  case TYPE_LONG:
  case TYPE_ULONG:
  case TYPE_LLONG:
  case TYPE_ULLONG:
  case TYPE_FLOAT:
  case TYPE_DOUBLE:
  case TYPE_PTR:
    return true;
  default:
    return false;
  }
}

#define CASE(k, field)					\
  case k:						\
  switch(type->kind) {					\
  case TYPE_BOOL:					\
    op->val.b = (bool)op->val.field;			\
    break;						\
  case TYPE_CHAR:					\
    op->val.c = (char)op->val.field;			\
    break;						\
  case TYPE_SCHAR:					\
    op->val.sc = (signed char)op->val.field;		\
    break;						\
  case TYPE_UCHAR:					\
    op->val.uc = (unsigned char)op->val.field;		\
    break;						\
  case TYPE_SHORT:					\
    op->val.s = (short)op->val.field;			\
    break;						\
  case TYPE_USHORT:					\
    op->val.us = (unsigned short)op->val.field;		\
    break;						\
  case TYPE_INT:					\
    op->val.i = (int)op->val.field;			\
    break;						\
  case TYPE_UINT:					\
    op->val.ui = (unsigned int)op->val.field;		\
    break;						\
  case TYPE_LONG:					\
    op->val.l = (long)op->val.field;			\
    break;						\
  case TYPE_ULONG:					\
    op->val.ul = (unsigned long)op->val.field;		\
    break;						\
  case TYPE_LLONG:					\
    op->val.ll = (long long)op->val.field;		\
    break;						\
  case TYPE_ULLONG:					\
    op->val.ull = (unsigned long long)op->val.field;	\
    break;						\
  case TYPE_FLOAT:					\
    op->val.f = (float)op->val.field;			\
    break;						\
  case TYPE_DOUBLE:					\
    op->val.d = (double)op->val.field;			\
    break;						\
  default:	;					\
    /* assert(0);  maybe not? */			\
  }							\
  op->type = type;					\
  break;
  
void convert_const_op(Operand* op, Type* type) {
  switch(op->type->kind) {
  CASE(TYPE_BOOL, b)
  CASE(TYPE_CHAR, c)
  CASE(TYPE_SCHAR, sc)
  CASE(TYPE_UCHAR, uc)
  CASE(TYPE_SHORT, s)
  CASE(TYPE_USHORT, us)
  CASE(TYPE_INT, i)
  CASE(TYPE_UINT, ui)
  CASE(TYPE_LONG, l)
  CASE(TYPE_ULONG, ul)
  CASE(TYPE_LLONG, ll)
  CASE(TYPE_ULLONG, ull)
  CASE(TYPE_FLOAT, f)
  CASE(TYPE_DOUBLE, d)
  default:
    assert(0);
    break;
  }
  assert(op->type == type);
}

void convert_op(Operand* op, Type* type) {
  if(op->kind == OPERAND_CONST) {
    convert_const_op(op, type);
  } else {
    op->type = type;
  }
}

void int_promote(Operand* op) {
  switch(op->type->kind) {
  case TYPE_BOOL:
  case TYPE_CHAR:
  case TYPE_SCHAR:
  case TYPE_UCHAR:
  case TYPE_SHORT:
  case TYPE_USHORT:
    convert_op(op, type_int);
    break;
  default:
    // do nothing
    ;
  }
}

Type* get_corresponding_unsigned_type(Type* type) {
  switch(type->kind) {
  case TYPE_CHAR:
  case TYPE_SCHAR:
    return type_uchar;
  case TYPE_SHORT:
    return type_ushort;
  case TYPE_INT:
    return type_uint;
  case TYPE_LONG:
    return type_ulong;
  case TYPE_LLONG:
    return type_ullong;
  default:
    assert(0);
    return NULL;
  }
}

bool is_null_ptr(Operand op) {
  return op.kind == OPERAND_CONST && op.val.ull == 0;
}

Type* unqualify_type(Type* type) {
  if(type->kind == TYPE_CONST) {
    return static_cast<TypeConst*>(type)->base;
  }
  return type;
}

// This function needs the Operand to detect null-pointer
bool is_implictly_castable(Operand op, Type* dest) {
  if(dest == op.type) {
    return true;
  } 
  
  if(is_arithmetic_type(dest) && is_arithmetic_type(op.type)) {
    return true;
  }
  
  if(dest->kind == TYPE_PTR && op.type->kind == TYPE_PTR) {
    TypePtr* dest_ptr = static_cast<TypePtr*>(dest);
    TypePtr* op_ptr = static_cast<TypePtr*>(op.type);

    if(op_ptr->base == type_void || dest_ptr->base == type_void) {
      return true;
    }
    return op_ptr->base == dest_ptr->base;
  }

  if(dest->kind == TYPE_PTR) {
    if(is_null_ptr(op)) {
      return true;
    }
    if(op.type->kind == TYPE_ARRAY) {
      TypePtr* dest_ptr = static_cast<TypePtr*>(dest);
      TypeArray* op_arr = static_cast<TypeArray*>(op.type);
      if(dest_ptr->base == op_arr->elem) {
	return true;
      }
    }
    return false;
  }

  if(dest->kind == TYPE_BOOL && op.type->kind == TYPE_PTR) {
    return true;
  }

  if(dest->kind == TYPE_ARRAY && op.type->kind == TYPE_ARRAY) {
    TypeArray* arr = static_cast<TypeArray*>(dest);
    TypeArray* op_arr = static_cast<TypeArray*>(op.type);
    if(arr->elem == op_arr->elem) {
      if(!arr->size || arr->size >= op_arr->size) {
	return true;
      }
    }
  }
  
  return false;
}

void unify_arithmetic_ops(Operand* op1, Operand* op2) {
  assert(is_arithmetic_type(op1->type));
  assert(is_arithmetic_type(op2->type));
  
  if(op1->type == op2->type) {
    return;
  }

  if(op1->type == type_double) {
    convert_op(op2, type_double);
  } else if(op2->type == type_double) {
    convert_op(op1, type_double);
  } else if(op1->type == type_float) {
    convert_op(op2, type_float);
  } else if(op2->type == type_float) {
    convert_op(op1, type_float);
  } else { // both of the arimethic types are not floating points
    assert(is_integer_type(op1->type));
    assert(is_integer_type(op2->type));
    
    if(is_signed_type(op1->type) == is_signed_type(op2->type)) { // both have the same sign'ness
      if(type_rank(op1->type) >= type_rank(op2->type)) {
	convert_op(op2, op1->type);
      } else {
	convert_op(op1, op2->type);
      }
    } else {
      Operand* sign_op = is_signed_type(op1->type) ? op1 : op2;
      Operand* unsign_op = sign_op == op1 ? op2 : op1;
      if(type_rank(unsign_op->type) >= type_rank(sign_op->type)) {
	convert_op(sign_op, unsign_op->type);
      } else if(sign_op->type->size > unsign_op->type->size) {
	convert_op(unsign_op, sign_op->type);
      } else {
	Type* corresponding_unsigned_type = get_corresponding_unsigned_type(sign_op->type);
	convert_op(op1, corresponding_unsigned_type);
	convert_op(op2, corresponding_unsigned_type);
      }
    }
  }
  assert(op1->type == op2->type);
}


Operand op_const(Type* type, ConstVal val) {
  return {OPERAND_CONST, type, val};
}

Operand op_lvalue(Type* type) {
  return {OPERAND_LVALUE, type};
}

Operand op_rvalue(Type* type) {
  return {OPERAND_RVALUE, type};
}

Operand op_func(TypeFunc* type) {
  return {OPERAND_FUNC, type};
}

enum SymKind {
  SYM_UNRESOLVED,
  SYM_RESOLVING,
  SYM_LOCAL,
  SYM_VAR,
  SYM_CONST,
  SYM_FUNC,
  SYM_TYPE,
};

struct Sym {
  SymKind kind;
  const char* name;
  Decl* decl; 
  
  Type* type;
  ConstVal const_val;
};

Map global_syms_map;

void sym_put_global(Sym sym) {
  Sym* sym_ptr = (Sym*)malloc(sizeof(sym));
  *sym_ptr = sym;
  map_put(&global_syms_map, sym.name, sym_ptr);
}

Sym* sym_or_null(const char* name) {
  return (Sym*)map_get(&global_syms_map, name);
}

void add_global_decl(Decl* decl) {
  Sym* first = sym_or_null(decl->name);
  if(first != NULL) {
    info(first->decl->loc, "here is the first declaration");
    fatal_error(decl->loc, "redclaration of global %s", decl->name);
  }
  sym_put_global(Sym{SYM_UNRESOLVED, decl->name, decl});
  if(decl->kind == DECL_ENUM) {

    DeclEnum* enum_decl = static_cast<DeclEnum*>(decl);
    for(size_t i = 0; i < enum_decl->num_items; ++i) {
      EnumItem item = enum_decl->items[i];
      Expr* expr = NULL;
      if(item.expr) {
	expr = item.expr;
      } else {
	if(i == 0) {
	  ConstVal val;
	  val.kind = CONST_INTEGER;
	  val.ull = 0;
	  expr = expr_const(val);
	} else {
	  ConstVal val;
	  val.kind = CONST_INTEGER;
	  val.ull = 1;
	  expr = expr_binary(expr_name(enum_decl->items[i - 1].name), expr_const(val), TOKEN_ADD);
	}
      }
      Sym sym = {};
      sym.kind = SYM_UNRESOLVED;
      sym.decl = decl_const(item.loc, item.name, typespec_name(enum_decl->name), expr);
      sym.name = item.name;
      sym_put_global(sym);
    }
  }
}

void add_global_decls(DeclSet declset) {
  for (Decl** it = declset.decls; it != declset.decls + declset.num_decls; ++it) {
    add_global_decl(*it);
  }

}

void resolve_sym(Sym* sym);

Sym local_syms[1024];
Sym* local_syms_top;

Sym* enter_scope() {
  return local_syms_top;
}

void leave_scope(Sym* last_top) {
  local_syms_top = last_top;
}

void push_local_variable(SourceLocation loc, const char* name, Type* type) {
  for(Sym* locals = local_syms_top; locals != local_syms; --locals) {
    Sym sym = *(locals - 1);
    if(sym.name == name) {
      fatal_error(loc, "declaration of local variable %s shadows previous declaration", name);
    }
  }
  
  Sym sym = {};
  sym.kind = SYM_LOCAL;
  sym.name = name;
  sym.type = type;
  
  *local_syms_top = sym;
  ++local_syms_top;
}

Sym* sym_get(SourceLocation loc, const char* name) {
  for(Sym* local = local_syms_top; local != local_syms; --local) {
    Sym* sym = local - 1;
    if(name == sym->name) {
      return sym;
    }
  }
  
  Sym* sym = sym_or_null(name);
  if(!sym) {
    fatal_error(loc, "undeclared identifier %s", name);
  }
  resolve_sym(sym);
  return sym;
}

#define BUILTIN_TYPE(sym_name, type_kind, size, align)			\
  type_##sym_name = create_primitive_type(type_kind, size, align);	\
  sym_put_global(Sym{SYM_TYPE, str_intern(#sym_name), NULL, type_##sym_name})

void init_builtin_syms() {
  BUILTIN_TYPE(void, TYPE_VOID, 0, 0);
  BUILTIN_TYPE(bool, TYPE_BOOL, 1, 1);
  BUILTIN_TYPE(schar, TYPE_SCHAR, 1, 1);
  BUILTIN_TYPE(uchar, TYPE_UCHAR, 1, 1);
  BUILTIN_TYPE(short, TYPE_SHORT, 2, 2);
  BUILTIN_TYPE(ushort, TYPE_USHORT, 2, 2);
  BUILTIN_TYPE(char, TYPE_CHAR, 1, 1);
  BUILTIN_TYPE(int, TYPE_INT, 4, 4);
  BUILTIN_TYPE(uint, TYPE_UINT, 4, 4);
  BUILTIN_TYPE(long, TYPE_LONG, 4, 4);
  BUILTIN_TYPE(ulong, TYPE_ULONG, 4, 4);
  BUILTIN_TYPE(llong, TYPE_LLONG, 8, 8);
  BUILTIN_TYPE(ullong, TYPE_ULLONG, 8, 8);
  BUILTIN_TYPE(float, TYPE_FLOAT, 4, 4);
  BUILTIN_TYPE(double, TYPE_DOUBLE, 8, 8);

  local_syms_top = local_syms;
}

#undef BUILTIN_TYPE

Operand resolve_expr(SourceLocation loc, Expr* expr);
Operand resolve_expr_expect(SourceLocation loc, Expr* expr, Type* expected);

Type* resolve_typespec(SourceLocation loc, Typespec* type) {
  switch(type->kind) {
  case TYPESPEC_NAME: {
    TypespecName* name = static_cast<TypespecName*>(type);
    Sym* sym = sym_get(loc, name->name);
    if(sym->kind != SYM_TYPE) {
      fatal_error(loc, "%s is not a type name", name->name);
    }
    return sym->type;
  } break;
  case TYPESPEC_PTR: {
    TypespecPtr* ptr = static_cast<TypespecPtr*>(type);
    return type_ptr(resolve_typespec(loc, ptr->base));
  }
  case TYPESPEC_ARR: {
    TypespecArr* arr = static_cast<TypespecArr*>(type);
    size_t size = 0;
    if(arr->size) {
      Operand op_size = resolve_expr(loc, arr->size);
      if(op_size.kind != OPERAND_CONST) {
	fatal_error(loc, "size of array must be a constant expression");
      }
      if(!is_integer_type(op_size.type)) {
	fatal_error(loc, "size of array must be of integer type");
      }
      if(is_signed_type(op_size.type)) {
	convert_op(&op_size, type_llong);
	if(op_size.val.ll <= 0) {
	  fatal_error(loc, "array size must be greater than zero");
	}
      }
      size = op_size.val.ull;
    }
    return type_arr(resolve_typespec(loc, arr->elem), size);
  } break;
	
  case TYPESPEC_FUNC:
  default:
    assert(0);
    return NULL;
  }
}


Operand resolve_expr_const(SourceLocation loc, ExprConst* expr) {
  ConstVal val = expr->val;
  Type* type = NULL;
  switch(val.kind) {
  case CONST_INTEGER:
    if(val.mod == MOD_CHAR) {
      assert(val.ull <= CHAR_MAX);
      type = type_char;
    } else if(val.mod == MOD_BOOL) {
      type = type_bool;
    } else {
      switch(val.postfix) {
      case POSTFIX_U:
	type = type_uint;
	if(val.ull > UINT_MAX) {
	  type = type_ulong;
	  if(val.ull > ULONG_MAX) {
	    type = type_ullong;
	    if(val.ull > ULLONG_MAX) {
	      fatal_error(loc, "the value '%llu' cannot be represented in ullong", val.ull);
	    }
	  }
	}
	break;
      case POSTFIX_L:
	type = type_long;
	if(val.ull > LONG_MAX) {
	  type = type_llong;
	  if(val.ull > LLONG_MAX) {
	    fatal_error(loc, "the value '%llu' cannot be represented in llong", val.ull);
	  }
	}
	break;
      case POSTFIX_UL:
	type = type_ulong;
	if(val.ull > ULONG_MAX) {
	  type = type_ullong;
	  if(val.ull > ULLONG_MAX) {
	    fatal_error(loc, "the value '%llu' cannot be represented in ullong", val.ull);
	  }
	}
	break;
      case POSTFIX_ULL:
	type = type_ullong;
	if(val.ull > ULLONG_MAX) {
	  fatal_error(loc, "the value '%llu' cannot be represented in ullong", val.ull);
	}
      default:
	assert(val.postfix == POSTFIX_NONE);
	if(val.postfix == POSTFIX_NONE) {
	  type = type_int;
	  if(val.ull > INT_MAX) {
	    type = type_uint;
	    if(val.ull > UINT_MAX) {
	      type = type_long;
	      if(val.ull > LONG_MAX) {
		type = type_ulong;
		if(val.ull > ULONG_MAX) {
		  type = type_llong;
		  if(val.ull > LLONG_MAX) {
		    type = type_ullong;
		    if(val.ull > ULLONG_MAX) {
		      fatal_error(loc, "the value '%llu' cannot be represented in ullong", val.ull);

		    }
		  }
		}
	      }
	    }
	  }
	}
	break;
      }
    }
    break;
  case CONST_FLOAT: {
    type = type_float;
    if(val.postfix == POSTFIX_D) {
      type = type_double;
    }
  } break;
  case CONST_STRING: {
    type = type_arr(type_char, val.str.len);
    Operand op = op_const(type_arr(type_char, val.str.len), val);
    op.is_compound = true;
    return op;
  }
  default:
    assert(0);
    break;
  }
  return op_const(type, val);
}

Operand resolve_expr_name(SourceLocation loc, ExprName* expr) {
  Sym* sym = sym_get(loc, expr->name);
  
  switch(sym->kind) {
  case SYM_CONST:
    return op_const(sym->type, sym->const_val);
  case SYM_FUNC:
    return op_func(static_cast<TypeFunc*>(sym->type));
  case SYM_VAR:
    return op_lvalue(sym->type);
  case SYM_TYPE:
    fatal_error(loc, "use of type name %s as an expression", expr->name);
    return {};
  case SYM_LOCAL:
    return op_lvalue(sym->type);
  default:
    assert(0);
    return {};
  }
}

Operand eval_unary(Operand operand, TokenKind op) {
  assert(operand.kind == OPERAND_CONST);
  Type* operand_type = operand.type;
  if(is_signed_type(operand_type)) {
    convert_op(&operand, type_llong);
    switch(op) {
    case TOKEN_ADD:
      operand.val.ll = +operand.val.ll;
      break;
    case TOKEN_SUB:
      operand.val.ll = -operand.val.ll;
      break;
    case TOKEN_NOT:
      operand.val.ll = !operand.val.ll;
      break;
    case TOKEN_NEG:
      operand.val.ll = ~operand.val.ll;
      break;
    default:
      assert(0);
    }
    convert_op(&operand, operand_type);
    return op_const(operand.type, operand.val);
  } else if(is_integer_type(operand.type)) {
    convert_op(&operand, type_ullong);
    switch(op) {
    case TOKEN_ADD:
      operand.val.ull = +operand.val.ull;
      break;
    case TOKEN_SUB: // Do nothing
      // operand.val.ull = -operand.val.ull;
      break;
    case TOKEN_NOT:
      operand.val.ull = !operand.val.ull;
      break;
    case TOKEN_NEG:
      operand.val.ull = ~operand.val.ull;
      break;
    default:
      assert(0);
    }
    convert_op(&operand, operand_type);
    return op_const(operand.type, operand.val);
  } else {
    return operand;
  }
}

Operand op_unary_expr(Operand operand, TokenKind op) {
  if(operand.kind == OPERAND_CONST) {
    return eval_unary(operand, op);
  } else {
    return op_rvalue(operand.type);
  }
}

Operand resolve_expr_rvalue(SourceLocation loc, Expr* expr);

Operand resolve_expr_unary(SourceLocation loc, ExprUnary* expr) {
  Operand operand = resolve_expr(loc, expr->operand);
  switch(expr->op) {
  case TOKEN_SUB:
  case TOKEN_ADD:
    if(!is_arithmetic_type(operand.type)) {
      fatal_error(loc, "operand of unary op %s must be of arithmetic type", token_kind_name(expr->op));
    }
    int_promote(&operand);
    return op_unary_expr(operand, expr->op);
  case TOKEN_AND:
    if(operand.kind != OPERAND_LVALUE) {
      fatal_error(loc, "operand of operator & must be lvalue");
    }
    return op_rvalue(type_ptr(operand.type));
  case TOKEN_MUL: {
    if(operand.type->kind != TYPE_PTR) {
      fatal_error(loc, "operand of operator * must be a pointer");      
    }
    TypePtr* type = static_cast<TypePtr*>(operand.type);
    return op_lvalue(type->base);
  }
  default:
    assert(0);
    return {};
  }
}

Operand eval_binary(SourceLocation loc, Operand left, Operand right, TokenKind op) {
  assert(left.kind == OPERAND_CONST);
  assert(right.kind == OPERAND_CONST);
  assert(left.type == right.type || op == TOKEN_RSHIFT || op == TOKEN_LSHIFT);
  
  Type* left_type = left.type;
  
  if(is_signed_type(left_type)) {
    convert_op(&left, type_llong);
    convert_op(&right, type_llong);
    Operand result = op_const(type_llong, {});
    switch(op) {
    case TOKEN_ADD:
      result.val.ll = left.val.ll + right.val.ll;
      break;
    case TOKEN_SUB:
      result.val.ll = left.val.ll - right.val.ll;
      break;
    case TOKEN_MUL:
      result.val.ll = left.val.ll * right.val.ll;
      break;
    case TOKEN_DIV:
      if(right.val.ll == 0) {
	fatal_error(loc, "division by 0");
      }
      result.val.ll = left.val.ll / right.val.ll;
      break;
    case TOKEN_GT:
      result.val.ll = left.val.ll > right.val.ll;
      break;
    case TOKEN_LT:
      result.val.ll = left.val.ll > right.val.ll;
      break;
    case TOKEN_GTE:
      result.val.ll = left.val.ll >= right.val.ll;
      break;
    case TOKEN_LTE:
      result.val.ll = left.val.ll <= right.val.ll;
      break;
    case TOKEN_EQ:
      result.val.ll = left.val.ll == right.val.ll;
      break;
    case TOKEN_NEQ:
      result.val.ll = left.val.ll != right.val.ll;
      break;
    case TOKEN_AND:
      result.val.ll = left.val.ll & right.val.ll;
      break;
    case TOKEN_OR:
      result.val.ll = left.val.ll | right.val.ll;
      break;
    case TOKEN_XOR:
      result.val.ll = left.val.ll ^ right.val.ll;
      break;
    case TOKEN_ANDAND:
      result.val.ll = left.val.ll && right.val.ll;
      break;
    case TOKEN_OROR:
      result.val.ll = left.val.ll || right.val.ll;
      break;
    case TOKEN_MOD:
      if(right.val.ll == 0) {
	fatal_error(loc, "mod by 0");
      }
      result.val.ll = left.val.ll % right.val.ll;
      break;
    case TOKEN_RSHIFT:
      if((int)(left_type->size*8) <= right.val.ll) {
	fatal_error(loc, "cannot right shift %lld bits to type of size %d bits", right.val.ll, left_type->size*8);
      }
      result.val.ll = left.val.ll >> right.val.ll;
      break;
    case TOKEN_LSHIFT:
      if((int)(left_type->size*8) <= right.val.ll) {
	fatal_error(loc, "cannot left shift %lld bits to type of size %d bits", right.val.ll, left_type->size*8);
      }
      result.val.ll = left.val.ll << right.val.ll;
      break;
    default:
      assert(0);
    }
    convert_op(&result, left_type);
    return result;
  } else if(is_integer_type(left_type)) {
    convert_op(&left, type_ullong);
    convert_op(&right, type_ullong);
    Operand result = op_const(type_ullong, {});
    switch(op) {
    case TOKEN_ADD:
      result.val.ull = left.val.ull + right.val.ull;
      break;
    case TOKEN_SUB:
      result.val.ull = left.val.ull - right.val.ull;
      break;
    case TOKEN_MUL:
      result.val.ull = left.val.ull * right.val.ull;
      break;
    case TOKEN_GT:
      result.val.ull = left.val.ull > right.val.ull;
      break;
    case TOKEN_LT:
      result.val.ull = left.val.ull > right.val.ull;
      break;
    case TOKEN_GTE:
      result.val.ull = left.val.ull >= right.val.ull;
      break;
    case TOKEN_LTE:
      result.val.ull = left.val.ull <= right.val.ull;
      break;
    case TOKEN_EQ:
      result.val.ull = left.val.ull == right.val.ull;
      break;
    case TOKEN_AND:
      result.val.ull = left.val.ull & right.val.ull;
      break;
    case TOKEN_OR:
      result.val.ull = left.val.ull | right.val.ull;
      break;
    case TOKEN_XOR:
      result.val.ull = left.val.ull ^ right.val.ull;
      break;
    case TOKEN_NEQ:
      result.val.ull = left.val.ull != right.val.ull;
      break;      
    case TOKEN_ANDAND:
      result.val.ull = left.val.ull && right.val.ull;
      break;
    case TOKEN_OROR:
      result.val.ull = left.val.ull || right.val.ull;
      break;
    case TOKEN_DIV:
      if(right.val.ull == 0) {
	fatal_error(loc, "division by 0");
      }
      result.val.ull = left.val.ull / right.val.ull;
      break;
    case TOKEN_RSHIFT:
      if((int)(left_type->size*8) <= right.val.ull) {
	fatal_error(loc, "cannot right shift %llu bits to type of size %d bits", right.val.ull, left_type->size*8);
      }
      result.val.ull = left.val.ull >> right.val.ull;
      break;
    case TOKEN_LSHIFT:
      if((int)(left_type->size*8) <= right.val.ull) {
	fatal_error(loc, "cannot left shift %llu bits to type of size %d bits", right.val.ull, left_type->size*8);
      }
      result.val.ull = left.val.ull << right.val.ull;
      break;
    default:
      assert(0);
    }
    convert_op(&result, left_type);
    return result;
  } else {
    assert(left.type->kind == TYPE_FLOAT || left.type->kind == TYPE_DOUBLE);
    convert_op(&left, type_double);
    convert_op(&right, type_double);
    Operand result = op_const(type_double, {});
    switch(op) {
    case TOKEN_ADD:
      result.val.d = left.val.d + right.val.d;
      break;
    case TOKEN_SUB:
      result.val.d = left.val.d - right.val.d;
      break;
    case TOKEN_MUL:
      result.val.d = left.val.d * right.val.d;
      break;
    case TOKEN_DIV:
      if(right.val.d == 0) {
	fatal_error(loc, "division by 0");
      }
      result.val.d = left.val.d / right.val.d;
      break;
    case TOKEN_GT:
      result.val.d = left.val.d > right.val.d;
      break;
    case TOKEN_LT:
      result.val.d = left.val.d > right.val.d;
      break;
    case TOKEN_GTE:
      result.val.d = left.val.d >= right.val.d;
      break;
    case TOKEN_LTE:
      result.val.d = left.val.d <= right.val.d;
      break;
    case TOKEN_EQ:
      result.val.d = left.val.d == right.val.d;
      break;
    case TOKEN_NEQ:
      result.val.d = left.val.d != right.val.d;
      break;
    case TOKEN_ANDAND:
      result.val.d = left.val.d && right.val.d;
      break;
    case TOKEN_OROR:
      result.val.d = left.val.d || right.val.d;
      break;
    default:
      assert(0);
    }
    convert_op(&result, left_type);
    return result;
  }
}

Operand op_binary_expr(SourceLocation loc, Operand left, Operand right, TokenKind op) {
  if(left.kind == OPERAND_CONST && right.kind == OPERAND_CONST) {
    return eval_binary(loc, left, right, op);
  } else {
    return op_rvalue(left.type);
  }
}

Operand resolve_expr_binary(SourceLocation loc, ExprBinary* expr) {
  Operand left = resolve_expr(loc, expr->left);
  Operand right = resolve_expr(loc, expr->right);
  switch(expr->op) {
  case TOKEN_MUL:
  case TOKEN_DIV:
    if(!is_arithmetic_type(left.type)) {
      fatal_error(loc, "left operand of operator %s must be of arithmetic type", token_kind_name(expr->op));
    }
    if(!is_arithmetic_type(right.type)) {
      fatal_error(loc, "right operand of operator %s must be of arithmetic type", token_kind_name(expr->op));
    }
    
    unify_arithmetic_ops(&left, &right);

    return op_binary_expr(loc, left, right, expr->op);
  case TOKEN_MOD:
  case TOKEN_AND:
  case TOKEN_OR:
  case TOKEN_XOR:    
    if(!is_integer_type(left.type)) {
      fatal_error(loc, "left operand of operator %s must be of integer type", token_kind_name(expr->op));
    }
    if(!is_integer_type(right.type)) {
      fatal_error(loc, "right operand of operator %s must be of integer type", token_kind_name(expr->op));
    }
    unify_arithmetic_ops(&left, &right);
    return op_binary_expr(loc, left, right, expr->op);
  case TOKEN_ADD:
    if(left.type->kind == TYPE_PTR) {
      if(!is_integer_type(right.type)) {
	fatal_error(loc, "right operand of operator + with pointer left operand must be of integer type");
      }
      return op_rvalue(left.type);
    } else if(right.type->kind == TYPE_PTR) {
      if(!is_integer_type(left.type)) {
	fatal_error(loc, "left operand of operator + with pointer right operand must be of integer type");
      }
      return op_rvalue(right.type);
    } else if(!is_arithmetic_type(left.type)) {
      fatal_error(loc, "left operand of operator + must be of arithmetic type");
      return {};
    } else if(!is_arithmetic_type(right.type)) {
      fatal_error(loc, "right operand of operator + must be of arithmetic type");
      return {};
    } else {
      unify_arithmetic_ops(&left, &right);
      return op_binary_expr(loc, left, right, expr->op);
    }
  case TOKEN_SUB:
    if(left.type->kind == TYPE_PTR && right.type->kind == TYPE_PTR) {
      TypePtr* left_ptr = static_cast<TypePtr*>(left.type);
      TypePtr* right_ptr = static_cast<TypePtr*>(right.type);
      if(unqualify_type(left_ptr->base) != unqualify_type(right_ptr->base)) {
	fatal_error(loc, "pointer operands of operator - must have compatible types");
      }
      return op_rvalue(type_ptrdiff);
    } else if(left.type->kind == TYPE_PTR) {
      if(!is_arithmetic_type(right.type)) {
	fatal_error(loc, "right operand of operator - with left pointer operand must be of arithmetic type");
      }
      return op_rvalue(left.type);
    } else {
      if(!is_arithmetic_type(left.type)) {
	fatal_error(loc, "left operand of operator - must be of arithmetic or pointer type");
      }
      if(!is_arithmetic_type(right.type)) {
	fatal_error(loc, "right operand of operator - must be of arithmetic type");
      }
      unify_arithmetic_ops(&left, &right);
      return op_binary_expr(loc, left, right, expr->op);
    }
  case TOKEN_LSHIFT:
  case TOKEN_RSHIFT:
    if(!is_integer_type(left.type)) {
      fatal_error(loc, "left operand of operator %s must be of integer type", token_kind_name(expr->op));
    }
    if(!is_integer_type(right.type)) {
      fatal_error(loc, "right operand of operator %s must be of integer type", token_kind_name(expr->op));
    }
    return op_binary_expr(loc, left, right, expr->op);
  case TOKEN_GT:
  case TOKEN_GTE:
  case TOKEN_LT:
  case TOKEN_LTE: {
    if(!is_arithmetic_type(left.type) || !is_arithmetic_type(right.type)) {
      Type* unqualify_left = unqualify_type(left.type);
      Type* unqualify_right = unqualify_type(right.type);
      
      if(unqualify_left->kind != TYPE_PTR) {
	fatal_error(loc, "left operand of operator %s is not a scalar type", token_kind_name(expr->op));
      }
      if(unqualify_type(right.type)->kind != TYPE_PTR) {
	fatal_error(loc, "right operand of operator %s is not a scalar type", token_kind_name(expr->op));
      }
      Type* left_base = static_cast<TypePtr*>(unqualify_left)->base;
      Type* right_base = static_cast<TypePtr*>(unqualify_right)->base;
      if(left_base != right_base) {
	fatal_error(loc, "non compatible types in relational binary expression");
      }
    } else { // both are arithmetics
      unify_arithmetic_ops(&left, &right);
    }
    Operand result = op_binary_expr(loc, left, right, expr->op);
    convert_op(&result, type_int);
    return result;
  }
  case TOKEN_EQ:
  case TOKEN_NEQ: {
    if(!is_arithmetic_type(left.type) || !is_arithmetic_type(right.type)) {
      Type* unqualify_left = unqualify_type(left.type);
      Type* unqualify_right = unqualify_type(right.type);
      if(unqualify_left->kind != TYPE_PTR && !is_null_ptr(left)) {
	fatal_error(loc, "left operand of operator %s is not a scalar type", token_kind_name(expr->op));
      }
      if(unqualify_type(right.type)->kind != TYPE_PTR && !is_null_ptr(right)) {
	fatal_error(loc, "right operand of operator %s is not a scalar type", token_kind_name(expr->op));
      }
      Type* left_base = static_cast<TypePtr*>(unqualify_left)->base;
      Type* right_base = static_cast<TypePtr*>(unqualify_right)->base;
      if(left_base->kind != TYPE_VOID && right_base->kind != TYPE_VOID) {
	if(left_base != right_base) {
	  fatal_error(loc, "non compatible types in relational binary expression");
	}
      }
    } else { // both are arithmetics
      unify_arithmetic_ops(&left, &right);
    }
    Operand result = op_binary_expr(loc, left, right, expr->op);
    convert_op(&result, type_int);
    return result;
  }
  case TOKEN_ANDAND:
  case TOKEN_OROR: {
    if(!is_scalar_type(left.type)) {
      fatal_error(loc, "left operand of operator %s is not a scalar type", token_kind_name(expr->op));
    } if(!is_scalar_type(right.type)) {
      fatal_error(loc, "right operand of operator %s is not a scalar type", token_kind_name(expr->op));
    }
    Operand result = op_binary_expr(loc, left, right, expr->op);
    convert_op(&result, type_int);
    return result;
  }
  default:
    assert(0);
    return {};
  }
}

Operand resolve_expr_cast(SourceLocation loc, ExprCast* expr) {
  Type* cast_dest = resolve_typespec(loc, expr->type);
  if(!is_scalar_type(cast_dest) && cast_dest != type_void) {
    fatal_error(loc, "casting to a non-scalar type");
  }

  Operand op = resolve_expr(loc, expr->src_expr);

  if(!is_scalar_type(op.type)) {
    fatal_error(loc, "casting from a non-scalar type");
  }
  
  if(cast_dest->kind == TYPE_PTR) {
    if(op.type->kind == TYPE_DOUBLE || op.type->kind == TYPE_FLOAT) {
      fatal_error(loc, "casting from floating point type to pointer");
    }
  }
  if(op.type->kind == TYPE_PTR) {
    if(cast_dest->kind == TYPE_DOUBLE || cast_dest->kind == TYPE_FLOAT) {
      fatal_error(loc, "casting from pointer to floating point type");
    }
  }
  convert_op(&op, cast_dest);

  if(op.kind != OPERAND_CONST) {
    op.kind = OPERAND_RVALUE; // is this right?
  }
  
  return op;
}

Operand resolve_expr_call(SourceLocation loc, ExprCall* expr) {
  Operand func_op = resolve_expr(loc, expr->func);
  if(func_op.type->kind != TYPE_FUNC) {
    fatal_error(loc, "attempt to call a non function type %s", type_name(func_op.type));
  }

  TypeFunc* func_type = static_cast<TypeFunc*>(func_op.type);
  if(expr->num_args > func_type->num_params) {
    fatal_error(loc, "too many argumets in function call");
  } else if(expr->num_args < func_type->num_params) {
    fatal_error(loc, "too few argumets in function call");
  }

  for(size_t i = 0; i < func_type->num_params; ++i) {
    Type* param_type = func_type->params[i];
    Expr* arg = expr->args[i];
    Operand arg_op = resolve_expr_rvalue(loc, arg);
    if(!is_implictly_castable(arg_op, param_type)) {
      fatal_error(loc, "argument %llu of function call expected to be of type %s, but %s was given", i + 1, type_name(param_type), type_name(arg_op.type));
    }
    convert_op(&arg_op, param_type);
  }
  return op_rvalue(func_type->ret);
}
  
Operand resolve_expr_field(SourceLocation loc, ExprField* expr) {
  Operand left = resolve_expr_rvalue(loc, expr->left_expr);

  TypeAggregate* aggr_type = NULL;
  if(left.type->kind != TYPE_AGGREGATE) {
    if(left.type->kind == TYPE_PTR) {
      TypePtr* ptr = static_cast<TypePtr*>(left.type);
      if(ptr->base->kind != TYPE_AGGREGATE) {
	fatal_error(loc, "accesing field of non aggregate type %s", type_name(ptr->base));
      }
      aggr_type = static_cast<TypeAggregate*>(ptr->base);
    } else {
      fatal_error(loc, "accesing field of non aggregate type %s", type_name(left.type));
    }
  } else {
    aggr_type = static_cast<TypeAggregate*>(left.type);
  }

  
  Type* type = NULL;
  for(size_t i = 0; i < aggr_type->num_items; ++i) {
    AggregateTypeItem item = aggr_type->items[i];
    if(item.name == expr->name) {
      type = item.type;
      break;
    }
  }

  if(!type) {
    fatal_error(loc, "%s %s do not contain field named %s", aggr_type->is_union ? "union" : "struct", aggr_type->name, expr->name);
  }
  if(left.kind == OPERAND_LVALUE) {
    return op_lvalue(type);
  } else {
    return op_rvalue(type);
  }
}

Operand resolve_expr_ternary(SourceLocation loc, ExprTernary* expr) {
  Operand cond = resolve_expr_rvalue(loc, expr->cond);
  if(!is_scalar_type(cond.type)) {
    fatal_error(loc, "condition of ternary expression must be of scalar type");
  }

  Operand then_op = resolve_expr_rvalue(loc, expr->then_expr);
  Operand else_op = resolve_expr_rvalue(loc, expr->else_expr);
  if(then_op.type != else_op.type) {
    if(is_arithmetic_type(then_op.type) != is_arithmetic_type(else_op.type)) {
      if(!is_implictly_castable(then_op, else_op.type)) {
	convert_op(&then_op, else_op.type);
      } else if(is_implictly_castable(else_op, then_op.type)) {
	convert_op(&else_op, then_op.type);
      } else {
	fatal_error(loc, "then clause type is %s, else clause is type %s", type_name(then_op.type), type_name(else_op.type));
      }
    } else if(is_arithmetic_type(then_op.type)) {
      unify_arithmetic_ops(&then_op, &else_op);
    }
  }
  if(cond.kind == OPERAND_CONST) {
    if(cond.val.ull) {
      return then_op;
    } else {
      return else_op;
    }
  }
  return op_rvalue(then_op.type);
}

Operand resolve_expr_compound(SourceLocation loc, ExprCompound* expr, Type* expected) {
  if(!expr->type && !expected) {
    fatal_error(loc, "cannot infer type of compound literal without context");
  }
  Type* type = NULL;
  if(expr->type) {
    type = resolve_typespec(loc, expr->type);
  } else {
    type = expected;
  }

  if(type->kind == TYPE_ARRAY) {
    TypeArray* arr = static_cast<TypeArray*>(type);
    size_t max_index = 0;
    for(size_t i = 0; i < expr->num_items; ++i) {
      CompoundItem item = expr->items[i];
      if(item.kind == COMPOUND_NAMED) {
	fatal_error(item.loc, "named initializer list's items only allowed for aggregate types");
      } else if(item.kind == COMPOUND_INDEXED) {
	Operand index = resolve_expr(loc, item.index);
	if(!is_integer_type(index.type)) {
	  fatal_error(item.loc, "index must be of integer type");
	}
	if(index.kind != OPERAND_CONST) {
	  fatal_error(item.loc, "index must be constant");
	}
	convert_op(&index, type_ullong);
	max_index = MAX(max_index, index.val.ull);
      } else {
	max_index = MAX(i, max_index);
      }
      Operand init = resolve_expr_expect(item.loc, item.expr, arr->elem);
      if(!is_implictly_castable(init, arr->elem)) {
	fatal_error(item.loc, "cannot initialize type %s with type %s", type_name(arr->elem), type_name(init.type));
      }
    }
    if(!arr->len) {
      type = type_arr(arr->elem, max_index);
    }
  } else if(type->kind == TYPE_AGGREGATE) {
    assert(0);
  } else {
    fatal_error(loc, "cannot initialize type %s from initializer list", type_name(type));
  }
  Operand result = op_rvalue(type);
  result.is_compound = true;
  return result;
}

Operand resolve_expr_expect(SourceLocation loc, Expr* expr, Type* expected) {
  switch(expr->kind) {
  case EXPR_CONST:
    return resolve_expr_const(loc, static_cast<ExprConst*>(expr));
  case EXPR_NAME:
    return resolve_expr_name(loc, static_cast<ExprName*>(expr));
  case EXPR_UNARY:
    return resolve_expr_unary(loc, static_cast<ExprUnary*>(expr));
  case EXPR_BINARY:
    return resolve_expr_binary(loc, static_cast<ExprBinary*>(expr));
  case EXPR_TERNARY:
    return resolve_expr_ternary(loc, static_cast<ExprTernary*>(expr));
  case EXPR_CAST:
    return resolve_expr_cast(loc, static_cast<ExprCast*>(expr));
  case EXPR_CALL:
    return resolve_expr_call(loc, static_cast<ExprCall*>(expr));
  case EXPR_FIELD:
    return resolve_expr_field(loc, static_cast<ExprField*>(expr));
  case EXPR_COMPOUND:
    return resolve_expr_compound(loc, static_cast<ExprCompound*>(expr), expected);
  default:
    assert(0);
    return {};
  }
}

Operand resolve_expr(SourceLocation loc, Expr* expr) {
  return resolve_expr_expect(loc, expr, NULL);
}

/*
Operand resolve_expr_init(SourceLocation loc, Expr* expr, Type** in_out_type) {
  if(*in_out_type) {
    if((*in_out_type)->kind != TYPE_ARRAY) {
      Operand op = resolve_expr_expect(loc, expr, *in_out_type);
      if(!is_implictly_castable(op, *in_out_type)) {
	fatal_error(loc, "cannot initialize variable of type %s with expression of type %s", type_name(*in_out_type), type_name(op.type));
      }
      if(*in_out_type != op.type) {
	convert_op(&op, *in_out_type);
      }
      return op;
    } else {
      Operand op = resolve_expr(loc, expr);
      if(op.type->kind != TYPE_ARRAY) {
	fatal_error(loc, "cannot initialize type %s with expression of type %s", type_name(*in_out_type), type_name(op.type));
      }
      if(!op.is_compound) {
	fatal_error(loc, "array initializer must be compound literal");
      }
      TypeArray* arr_type = static_cast<TypeArray*>(*in_out_type);
      TypeArray* op_arr_type = static_cast<TypeArray*>(op.type);
      Operand temp = op_rvalue(op_arr_type->elem);
      if(!is_implictly_castable(temp, arr_type->elem)) {
	fatal_error(loc, "cannot initialize type %s with expression of type %s", type_name(*in_out_type), type_name(op.type));
      }
      if(arr_type->len != op_arr_type->len && arr_type->len) {
	fatal_error(loc, "cannot initialize type %s with expression of type %s", type_name(*in_out_type), type_name(op.type));
      }
      *in_out_type = op.type;
      return op;
    }
  } else {
    Operand op = resolve_expr(loc, expr);
    *in_out_type = op.type;
    return op;
  }
}
*/

Operand resolve_expr_rvalue(SourceLocation loc, Expr* expr) {
  Operand op = resolve_expr(loc, expr);

  if(op.type->kind == TYPE_VOID) {
    fatal_error(loc, "void expression in rvalue context");
  }

  if(op.type->kind == TYPE_ARRAY) {
    return op_rvalue(type_ptr(static_cast<TypeArray*>(op.type)->elem));
  }
  
  if(op.kind == OPERAND_LVALUE) {
    return op_rvalue(unqualify_type(op.type));
  } else {
    return op;
  }
}


bool resolve_stmnt(Stmnt* stmnt, Type* ret_type) {
  switch(stmnt->kind) {
  case STMNT_BREAK:
  case STMNT_CONTINUE:
    return false;
  case STMNT_RETURN: {
    StmntReturn* ret = static_cast<StmntReturn*>(stmnt);
    if(ret->expr) {
      if(ret_type == type_void) {
	fatal_error(ret->loc, "attempting to return a value from void function");
      }
      Operand op = resolve_expr(ret->loc, ret->expr);
      if(!is_implictly_castable(op, ret_type)) {
	fatal_error(ret->loc, "type mismatch in return statement, expected %s, got %s", type_name(ret_type), type_name(op.type));
      }
      convert_op(&op, ret_type);
    } else if(ret_type != type_void) {
      fatal_error(ret->loc, "function must return a value");
    }
    return true;
  }
  case STMNT_BLOCK: {
    StmntBlock* block = static_cast<StmntBlock*>(stmnt);
    bool returns = false;
    Sym* enter = enter_scope();
    for(Stmnt** it = block->stmnts; it != block->stmnts + block->num_stmnts; ++it) {
      returns = resolve_stmnt(*it, ret_type) || returns;
    }
    leave_scope(enter);
    return returns;
  }
  case STMNT_IF: {
    StmntIf* stmnt_if = static_cast<StmntIf*>(stmnt);
    Operand cond = resolve_expr_rvalue(stmnt_if->loc, stmnt_if->cond);
    if(!is_scalar_type(cond.type)) {
      fatal_error(stmnt_if->loc, "non scalar type %s in if condition", type_name(cond.type));
    }
    Sym* enter = enter_scope();
    bool returns = resolve_stmnt(stmnt_if->then_stmnt, ret_type);
    leave_scope(enter);
    for(size_t i = 0; i < stmnt_if->num_elseifs; ++i) {
      ElseIf elseif = stmnt_if->elseifs[i];
      Operand cond_elseif = resolve_expr_rvalue(elseif.loc, elseif.cond);
      if(!is_scalar_type(cond.type)) {
	fatal_error(stmnt_if->loc, "non scalar type %s in if condition", type_name(cond.type));
      }
      enter = enter_scope();
      returns = resolve_stmnt(elseif.then_stmnt, ret_type) && returns;
      leave_scope(enter);
    }
    
    if(stmnt_if->else_stmnt) {
      enter = enter_scope();
      returns = resolve_stmnt(stmnt_if->else_stmnt, ret_type) && returns;
      leave_scope(enter);
    } else {
      returns = false;
    }
    return returns;
  }
  case STMNT_WHILE: {
    StmntWhile* stnmt_while = static_cast<StmntWhile*>(stmnt);
    Operand cond = resolve_expr_rvalue(stnmt_while->loc, stnmt_while->cond);
    if(!is_scalar_type(cond.type)) {
      fatal_error(stnmt_while->loc, "non scalar type %s in while condition", type_name(cond.type));
    }

    Sym* enter = enter_scope();
    resolve_stmnt(stnmt_while->then_stmnt, ret_type);
    leave_scope(enter);
    
    return false;
  }
  case STMNT_FOR: {
    StmntFor* stmnt_for = static_cast<StmntFor*>(stmnt);
    Sym* enter = enter_scope();
    if(stmnt_for->init) {
      resolve_stmnt(stmnt_for->init, ret_type);
    }

    if(stmnt_for->cond) {
      Operand cond = resolve_expr_rvalue(stmnt_for->loc, stmnt_for->cond);
      if(!is_scalar_type(cond.type)) {
	fatal_error(stmnt_for->loc, "non scalar type %s in for condition", type_name(cond.type));
      }
    }
    if(stmnt_for->next) {
      resolve_stmnt(stmnt_for->next, ret_type);
    }
    resolve_stmnt(stmnt_for->then_stmnt, ret_type);
    leave_scope(enter);
    return false;
  }
  case STMNT_EXPR: {
    StmntExpr* stmnt_expr = static_cast<StmntExpr*>(stmnt);
    resolve_expr(stmnt_expr->loc, stmnt_expr->expr);
    return false;
  }
  case STMNT_DECL: {
    StmntDecl* stmnt_decl = static_cast<StmntDecl*>(stmnt);
    Type* type = NULL;
    if(stmnt_decl->type) {
      type = resolve_typespec(stmnt_decl->loc, stmnt_decl->type);
    }
    if(stmnt_decl->expr) {
      Operand op = resolve_expr_expect(stmnt_decl->loc, stmnt_decl->expr, type);
      if(!stmnt_decl->type) {
	type = op.type;
	put_resolved_op(stmnt_decl->expr, op);
      } else if(!is_implictly_castable(op, type)) {
	fatal_error(stmnt->loc, "cannot convert from type %s to type %s", type_name(op.type), type_name(type));
      } else if(type->kind == TYPE_ARRAY) {
	if(!op.is_compound) {
	  fatal_error(stmnt->loc, "array initializer must be compound literal");
	}
	TypeArray* arr = static_cast<TypeArray*>(type);
	if(!arr->len) {
	  assert(op.type->kind == TYPE_ARRAY);
	  type = op.type;
	}
      }
    } else if(type->kind == TYPE_ARRAY) {
      TypeArray* arr_type = static_cast<TypeArray*>(type);
      if(!arr_type->len) {
	fatal_error(stmnt_decl->loc, "array declaration without explicit or implicit size");
      }
    }
    push_local_variable(stmnt_decl->loc, stmnt_decl->name, type);
    return false;
  }
  case STMNT_SWITCH: {
    StmntSwitch* stmnt_switch = static_cast<StmntSwitch*>(stmnt);
    Operand control_op = resolve_expr_rvalue(stmnt_switch->loc, stmnt_switch->control_expr);
    if(!is_integer_type(control_op.type)) {
      fatal_error(stmnt_switch->loc, "control expression of switch statement must be of integer type");
    }
    bool returns = false;
    unsigned long long* exprs_values = NULL;
    for(size_t i = 0; i < stmnt_switch->num_cases; ++i) {
      SwitchCase switch_case = stmnt_switch->cases[i];
      for(Expr** expr = switch_case.exprs; expr != switch_case.exprs + switch_case.num_exprs; ++expr) {
	Operand case_expr = resolve_expr(switch_case.loc, *expr);
	if(case_expr.kind != OPERAND_CONST) {
	  fatal_error(switch_case.loc, "case label has non constant expression");
	}
	if(!is_integer_type(case_expr.type)) {
	  fatal_error(switch_case.loc, "case label has non integer type");
	}

	convert_op(&case_expr, type_ullong);
	unsigned long long value = case_expr.val.ull;
	for(unsigned long long* it = exprs_values; it != exprs_values + buf_len(exprs_values); ++it) {
	  if(value == *it) {
	    fatal_error(switch_case.loc, "expression %llu is already present in this switch statement", value);
	  }
	}
	buf_push(exprs_values, value);
      }

      Sym* enter = enter_scope();
      bool case_returns = false;
      for(Stmnt** it = switch_case.stmnts; it != switch_case.stmnts + switch_case.num_stmnts; ++it) {
	case_returns = resolve_stmnt(*it, ret_type) || case_returns;
      }
      leave_scope(enter);
      returns = returns && case_returns;
    }
    return returns;
  }
  case STMNT_ASSIGN: {
    StmntAssign* assign = static_cast<StmntAssign*>(stmnt);
    Operand target_op = resolve_expr(assign->loc, assign->target);
    if(target_op.kind != OPERAND_LVALUE) {
      fatal_error(assign->loc, "assignment to non lvalue");
    }
    if(target_op.type->kind == TYPE_ARRAY) {
      fatal_error(assign->loc, "assignment to array type %s", type_name(target_op.type));
    }
    
    Operand expr_op = resolve_expr_rvalue(assign->loc, assign->expr);
    if(!is_implictly_castable(expr_op, target_op.type)) {
      fatal_error(assign->loc, "cannot cast type %s to type %s", type_name(expr_op.type), type_name(target_op.type));
    }
    return false;
  }
  default:
    assert(0);
    return false;
  }
}

void resolve_decl_var(Sym* sym, DeclVar* decl) {
  if(decl->type) {
    sym->type = resolve_typespec(decl->loc, decl->type);
  }
  
  if(decl->expr) {
    Operand op = resolve_expr_expect(decl->loc, decl->expr, sym->type);
    if(!sym->type) {
      sym->type = op.type;
      put_resolved_op(decl->expr, op);
    } else if(!is_implictly_castable(op, sym->type)) {
      fatal_error(decl->loc, "cannot convert from type %s to type %s", type_name(op.type), type_name(sym->type));
    } else if(sym->type->kind == TYPE_ARRAY) {
      if(!op.is_compound) {
	fatal_error(decl->loc, "array initializer must be compound literal");
      }
      TypeArray* arr = static_cast<TypeArray*>(sym->type);
      if(!arr->len) {
	assert(op.type->kind == TYPE_ARRAY);
	sym->type = op.type;
      }
    }
  }
  sym->kind = SYM_VAR;
}

void resolve_decl_const(Sym* sym, DeclConst* decl) {
  if(decl->type) {
    sym->type = resolve_typespec(decl->loc, decl->type);
  }
  
  Operand op = resolve_expr_expect(decl->loc, decl->expr, sym->type);
  
  if(op.kind != OPERAND_CONST) {
    fatal_error(decl->loc, "initalizing constant with non-constant experssion");
  }
  
  if(!sym->type) {
    sym->type = op.type;
    put_resolved_op(decl->expr, op);
  } else if(!is_implictly_castable(op, sym->type)) {
    fatal_error(decl->loc, "cannot convert from type %s to type %s", type_name(op.type), type_name(sym->type));
  } else if(sym->type->kind == TYPE_ARRAY) {
    if(!op.is_compound) {
      fatal_error(decl->loc, "array initializer must be compound literal");
    }
    TypeArray* arr = static_cast<TypeArray*>(sym->type);
    if(!arr->len) {
      assert(op.type->kind == TYPE_ARRAY);
      sym->type = op.type;
    }
  }

  sym->kind = SYM_CONST;
  sym->const_val = op.val;
}

void resolve_decl_typedef(Sym* sym, DeclTypedef* decl) {
  sym->type = resolve_typespec(decl->loc, decl->type);
  sym->kind = SYM_TYPE;
}

void resolve_decl_func(Sym* sym, DeclFunc* decl) {
  Sym* enter = enter_scope();
  Type** params = NULL;
  for(FuncParam* param = decl->params; param != decl->params + decl->num_params; ++param) {
    Type* type = resolve_typespec(decl->loc, param->type);
    buf_push(params, type);
  }

  Type* ret = type_void;
  if(decl->ret_type) {
    ret = resolve_typespec(decl->loc, decl->ret_type);
  }

  sym->kind = SYM_FUNC;
  sym->type = type_func(params, buf_len(params), ret);

  for(size_t i = 0; i < decl->num_params; ++i) {
    push_local_variable(decl->loc, decl->params[i].name, params[i]);
  }

  if(!resolve_stmnt(decl->body, ret) && ret != type_void) {
    fatal_error(decl->loc, "not all control paths return a value");
  }
  leave_scope(enter);
}

void resolve_decl_aggregate(Sym* sym, DeclAggregate* decl) {
  AggregateTypeItem* items = NULL;

  size_t size = 0;
  size_t align = 0;
  
  for(size_t i = 0; i < decl->num_fields; ++i) {
    AggregateField* field = decl->fields + i;
    for(size_t j  = i + 1; j < decl->num_fields; ++j) {
      if(field->name == decl->fields[j].name) {
	fatal_error(decl->fields[j].loc, "re-declaration of field name '%s'", field->name);
      }
    }
    Type* type = resolve_typespec(field->loc, field->type);
    size += type->size;
    align = MAX(align, type->align);
    buf_push(items, AggregateTypeItem{field->name, type});
  }

  sym->kind = SYM_TYPE;
  sym->type = type_aggregate(decl->name, decl->is_union, items, buf_len(items), size, align);
}

Operand resolve_expr_const_int(SourceLocation loc, Expr* expr) {
  Operand op = resolve_expr(loc, expr);
  if(op.kind != OPERAND_CONST) {
    fatal_error(loc, "temp");
  }
  if(!is_integer_type(op.type)) {
    fatal_error(loc, "temp");
  }
  return op;
}

void resolve_decl_enum(Sym* sym, DeclEnum* decl) {
  sym->kind = SYM_TYPE;
  sym->type = type_int;
}

Decl** ordered_decls;

void resolve_sym(Sym* sym) {
  if(sym->kind > SYM_RESOLVING) {
    return; // no local declaration should end up here
  }

  if(sym->kind == SYM_RESOLVING) {
    fatal_error(sym->decl->loc, "cyclic dependency");
  }
  
  sym->kind = SYM_RESOLVING;
  Decl* decl = sym->decl;
  switch(decl->kind) {
  case DECL_VAR:
    resolve_decl_var(sym, static_cast<DeclVar*>(decl));
    break;
  case DECL_CONST:
    resolve_decl_const(sym, static_cast<DeclConst*>(decl));
    break;
  case DECL_TYPEDEF:
    resolve_decl_typedef(sym, static_cast<DeclTypedef*>(decl));
    break;
  case DECL_FUNC:
    resolve_decl_func(sym, static_cast<DeclFunc*>(decl));
    break;
  case DECL_AGGREGATE:
    resolve_decl_aggregate(sym, static_cast<DeclAggregate*>(decl));
    break;
  case DECL_ENUM:
    resolve_decl_enum(sym, static_cast<DeclEnum*>(decl));
    break;
  default:
    assert(0);
    break;
  }
  buf_push(ordered_decls, sym->decl);
}

void resolve_global_syms() {
  for(MapEntry* it = global_syms_map.entries; it != global_syms_map.entries + global_syms_map.cap; ++it) {
    if(it->key) {
      resolve_sym((Sym*)it->value);
    }
  }
}

void resolve_test() {
  init_builtin_syms();
  
  printf("%s\n", type_name(type_ptr(type_int)));
  printf("%s\n", type_name(type_ptr(type_const(type_ulong))));
  printf("%s\n", type_name(type_ptr(type_arr(type_int, 16))));
  
  const char* code = "\n\nvar a: int = 5;\nvar b = 2;";
  init_stream(NULL, code);

  DeclSet declset = parse_declset();
  for(Decl** it = declset.decls; it != declset.decls + declset.num_decls; ++it) {
    add_global_decl(*it);
  }
  resolve_global_syms();
  for(Decl** it = ordered_decls; it != buf_end(ordered_decls); ++it) {
    printf("%s\n", (*it)->name);
  }
}
