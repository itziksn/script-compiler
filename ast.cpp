struct Typespec;

enum ExprKind {
  EXPR_NONE,
  EXPR_CONST,
  EXPR_NAME,
  EXPR_UNARY,
  EXPR_BINARY,
  EXPR_TERNARY,
  EXPR_CALL,
  EXPR_COMPOUND,
  EXPR_CAST,
  EXPR_FIELD,
};

struct Expr {
  ExprKind kind;
};

enum ConstKind {
  CONST_INTEGER,
  CONST_FLOAT,
  CONST_STRING,
  CONST_DEFAULT,
};

struct ConstVal {
  ConstKind kind;
  Mod mod;
  ConstPostfix postfix;
  union {
    bool b;
    char c;
    unsigned char uc;
    signed char sc;
    short s;
    unsigned short us;
    int i;
    unsigned int ui;
    long l;
    unsigned long ul;
    long long ll;
    float f;
    // For tokenazing and parsing we use only the following types
    unsigned long long ull;
    double d;
    struct {
      const char* buf;
      size_t len;
    } str;
  };
};

struct ExprConst : Expr {
  ConstVal val;
};

struct ExprName : Expr {
  const char* name;
};

struct ExprField : Expr {
  Expr* left_expr;
  const char* name;
};

struct ExprUnary : Expr {
  TokenKind op;
  Expr* operand;
};

struct ExprBinary : Expr {
  TokenKind op;
  Expr* left;
  Expr* right;
};

struct ExprTernary : Expr {
  Expr* cond;
  Expr* then_expr;
  Expr* else_expr;
};

struct ExprCast : Expr {
  Typespec* type;
  Expr* src_expr;
};

struct ExprCall : Expr {
  Expr* func;
  Expr** args;
  size_t num_args;
};

enum CompoundKind {
  COMPOUND_NONE,
  COMPOUND_NAMED,
  COMPOUND_INDEXED,
};

struct CompoundItem {
  CompoundKind kind;
  SourceLocation loc;
  Expr* expr;
  union {
    Expr* index;
    const char* name;
  };
};

struct ExprCompound : Expr {
  Typespec* type;
  CompoundItem* items;
  size_t num_items;
};

ExprCast* expr_cast(Typespec* type, Expr* src_expr) {
  ExprCast* expr = (ExprCast*)malloc(sizeof(ExprCast));
  expr->kind = EXPR_CAST;
  expr->type = type;
  expr->src_expr = src_expr;

  return expr;
}

ExprCompound* expr_compound(Typespec* type, CompoundItem* items, size_t num_items) {
  ExprCompound* expr = (ExprCompound*)malloc(sizeof(ExprCompound));
  expr->kind = EXPR_COMPOUND;
  expr->type = type;
  expr->items = items;
  expr->num_items = num_items;

  return expr;
}

ExprConst* expr_const(ConstVal val) {
  ExprConst* expr = (ExprConst*)malloc(sizeof(ExprConst));
  expr->kind = EXPR_CONST;
  expr->val = val;
  return expr;
}

ExprName* expr_name(const char* name) {
  ExprName* expr = (ExprName*)malloc(sizeof(ExprName));
  expr->kind = EXPR_NAME;
  expr->name = name;
  return expr;
}

ExprField* expr_field(Expr* left_expr, const char* name) {
  ExprField* expr = (ExprField*)malloc(sizeof(ExprField));
  expr->kind = EXPR_FIELD;
  expr->left_expr = left_expr;
  expr->name = name;

  return expr;
}

ExprBinary* expr_binary(Expr* left, Expr* right, TokenKind op) {
  ExprBinary* expr = (ExprBinary*)malloc(sizeof(ExprBinary));
  expr->kind = EXPR_BINARY;
  expr->left = left;
  expr->right = right;
  expr->op = op;

  return expr;
}

ExprUnary* expr_unary(Expr* operand, TokenKind op) {
  ExprUnary* expr = (ExprUnary*)malloc(sizeof(ExprUnary));
  expr->kind = EXPR_UNARY;
  expr->op = op;
  expr->operand = operand;
  
  return expr;
}

ExprTernary* expr_ternary(Expr* cond, Expr* then_expr, Expr* else_expr) {
  ExprTernary* expr = (ExprTernary*)malloc(sizeof(ExprTernary));
  expr->kind = EXPR_TERNARY;
  expr->cond = cond;
  expr->then_expr = then_expr;
  expr->else_expr = else_expr;

  return expr;
}

ExprCall* expr_call(Expr* func, Expr** args, size_t num_args) {
  ExprCall* expr = (ExprCall*)malloc(sizeof(ExprCall));
  expr->kind = EXPR_CALL;
  expr->func = func;
  expr->args = args;
  expr->num_args = num_args;

  return expr;
}


enum TypespecKind {
  TYPESPEC_NONE,
  TYPESPEC_NAME,
  TYPESPEC_PTR,
  TYPESPEC_ARR,
  TYPESPEC_FUNC,
};

struct Typespec {
  TypespecKind kind;
};

struct TypespecName : Typespec {
  const char* name;
};

struct TypespecPtr : Typespec {
  Typespec* base;
};

struct TypespecArr : Typespec {
  Typespec* elem;
  Expr* size;
};

struct TypespecFunc : Typespec {
  Typespec** params;
  size_t num_params;
  Typespec* ret;
};

TypespecName* typespec_name(const char* name) {
  TypespecName* type = (TypespecName*)malloc(sizeof(TypespecName));
  type->kind = TYPESPEC_NAME;
  type->name = name;
  return type;
}

TypespecArr* typespec_arr(Typespec* elem, Expr* size) {
  TypespecArr* type = (TypespecArr*)malloc(sizeof(TypespecArr));
  type->kind = TYPESPEC_ARR;
  type->elem = elem;
  type->size = size;

  return type;
}

TypespecPtr* typespec_ptr(Typespec* base) {
  TypespecPtr* type = (TypespecPtr*)malloc(sizeof(TypespecPtr));
  type->kind = TYPESPEC_PTR;
  type->base = base;

  return type;
}

enum StmntKind {
  STMNT_NONE,
  STMNT_RETURN,
  STMNT_IF,
  STMNT_BLOCK,
  STMNT_WHILE,
  STMNT_FOR,
  STMNT_DECL,
  STMNT_ASSIGN,
  STMNT_EXPR,
  STMNT_BREAK,
  STMNT_CONTINUE,
  STMNT_SWITCH,
};

struct Stmnt {
  StmntKind kind;
  SourceLocation loc;
};

struct StmntBlock : Stmnt {
  Stmnt** stmnts;
  size_t num_stmnts;
};

struct StmntReturn : Stmnt {
  Expr* expr;
};

struct ElseIf {
  SourceLocation loc;
  Expr* cond;
  Stmnt* then_stmnt;
};

struct StmntIf : Stmnt {
  Expr* cond;
  Stmnt* then_stmnt;
  ElseIf* elseifs;
  size_t num_elseifs;
  Stmnt* else_stmnt;
};

struct StmntWhile : Stmnt {
  Expr* cond;
  Stmnt* then_stmnt;
  bool is_do_while;
};

struct StmntFor : Stmnt {
  Stmnt* init;
  Expr* cond;
  Stmnt* next;
  Stmnt* then_stmnt;
};

struct StmntDecl : Stmnt {
  const char* name;
  Typespec* type;
  Expr* expr;
};

struct StmntAssign : Stmnt {
  Expr* target;
  Expr* expr;
  TokenKind op;
};

struct StmntExpr : Stmnt {
  Expr* expr;
};

struct SwitchCase {
  SourceLocation loc;
  Expr** exprs;
  size_t num_exprs;
  Stmnt** stmnts;
  size_t num_stmnts;
};

struct StmntSwitch : Stmnt {
  Expr* control_expr;
  SwitchCase* cases;
  size_t num_cases;
};

void init_stmnt(Stmnt* stmnt, StmntKind kind, SourceLocation loc) {
  stmnt->kind = kind;
  stmnt->loc = loc;
}

StmntSwitch* stmnt_switch(SourceLocation loc, Expr* control_expr, SwitchCase* cases, size_t num_cases) {
  StmntSwitch* stmnt = (StmntSwitch*)malloc(sizeof(StmntSwitch));
  init_stmnt(stmnt, STMNT_SWITCH, loc);
  stmnt->control_expr = control_expr;
  stmnt->cases = cases;
  stmnt->num_cases = num_cases;

  return stmnt;
}

Stmnt* stmnt_break(SourceLocation loc) {
  Stmnt* stmnt = (Stmnt*)malloc(sizeof(Stmnt));
  init_stmnt(stmnt, STMNT_BREAK, loc);
  return stmnt;
}

Stmnt* stmnt_continue(SourceLocation loc) {
  Stmnt* stmnt = (Stmnt*)malloc(sizeof(Stmnt));
  init_stmnt(stmnt, STMNT_CONTINUE, loc);
  return stmnt;
}

StmntExpr* stmnt_expr(SourceLocation loc, Expr* expr) {
  StmntExpr* stmnt = (StmntExpr*)malloc(sizeof(StmntExpr));
  init_stmnt(stmnt, STMNT_EXPR, loc);
  stmnt->expr = expr;

  return stmnt;
}

StmntAssign* stmnt_assign(SourceLocation loc, Expr* target, Expr* expr, TokenKind op) {
  StmntAssign* stmnt = (StmntAssign*)malloc(sizeof(StmntAssign));
  init_stmnt(stmnt, STMNT_ASSIGN, loc);
  stmnt->target = target;
  stmnt->expr = expr;
  stmnt->op = op;

  return stmnt;
}

StmntDecl* stmnt_decl(SourceLocation loc, const char* name, Typespec* type, Expr* expr) {
  StmntDecl* stmnt = (StmntDecl*)malloc(sizeof(StmntDecl));
  init_stmnt(stmnt, STMNT_DECL, loc);
  stmnt->name = name;
  stmnt->type = type;
  stmnt->expr = expr;
  
  return stmnt;

}

StmntBlock* stmnt_block(SourceLocation loc, Stmnt** stmnts, size_t num_stmnts) {
  StmntBlock* stmnt = (StmntBlock*)malloc(sizeof(StmntBlock));
  init_stmnt(stmnt, STMNT_BLOCK, loc);
  stmnt->stmnts = stmnts;
  stmnt->num_stmnts = num_stmnts;

  return stmnt;
}

StmntReturn* stmnt_return(SourceLocation loc, Expr* expr) {
  StmntReturn* stmnt = (StmntReturn*)malloc(sizeof(StmntReturn));
  init_stmnt(stmnt, STMNT_RETURN, loc);
  stmnt->expr = expr;

  return stmnt;
}

StmntIf* stmnt_if(SourceLocation loc, Expr* cond, Stmnt* then_stmnt, ElseIf* elseifs, size_t num_elseifs, Stmnt* else_stmnt) {
  StmntIf* stmnt = (StmntIf*)malloc(sizeof(StmntIf));
  init_stmnt(stmnt, STMNT_IF, loc);
  stmnt->cond = cond;
  stmnt->then_stmnt = then_stmnt;
  stmnt->elseifs = elseifs;
  stmnt->num_elseifs = num_elseifs;
  stmnt->else_stmnt = else_stmnt;

  return stmnt;
}

StmntFor* stmnt_for(SourceLocation loc, Stmnt* init, Expr* cond, Stmnt* next, Stmnt* then_stmnt) {
  StmntFor* stmnt = (StmntFor*)malloc(sizeof(StmntFor));
  init_stmnt(stmnt, STMNT_FOR, loc);
  stmnt->init = init;
  stmnt->cond = cond;
  stmnt->next = next;
  stmnt->then_stmnt = then_stmnt;

  return stmnt;
}

StmntWhile* stmnt_while(SourceLocation loc, Expr* cond, Stmnt* then_stmnt, bool is_do_while) {
  StmntWhile* stmnt = (StmntWhile*)malloc(sizeof(StmntWhile));
  init_stmnt(stmnt, STMNT_WHILE, loc);
  stmnt->cond = cond;
  stmnt->then_stmnt = then_stmnt;
  stmnt->is_do_while = is_do_while;

  return stmnt;
}

enum DeclKind {
  DECL_NONE,
  DECL_FUNC,
  DECL_VAR,
  DECL_CONST,
  DECL_AGGREGATE,
  DECL_TYPEDEF,
  DECL_ENUM,
};

struct Decl {
  DeclKind kind;
  const char* name;
  SourceLocation loc;
};

struct FuncParam {
  const char* name;
  Typespec* type;
};

struct DeclFunc : Decl {
  FuncParam* params;
  size_t num_params;
  Typespec* ret_type;
  StmntBlock* body;
};

struct DeclVar : Decl {
  Expr* expr;
  Typespec* type;
};

struct DeclTypedef : Decl {
  Typespec* type;
};

struct DeclConst : Decl {
  Expr* expr;
  Typespec* type;
};

struct AggregateField {
  SourceLocation loc;
  const char* name;
  Typespec* type;
};

struct DeclAggregate : Decl {
  bool is_union;
  AggregateField* fields;
  size_t num_fields;
};

struct EnumItem {
  SourceLocation loc;
  const char* name;
  Expr* expr;
};

struct DeclEnum : Decl {
  EnumItem* items;
  size_t num_items;
};

void init_decl(Decl* decl, DeclKind kind, const char* name, SourceLocation loc) {
  decl->kind = kind;
  decl->name = name;
  decl->loc = loc;
}

DeclTypedef* decl_typedef(SourceLocation loc, const char* name, Typespec* type) {
  DeclTypedef* decl = (DeclTypedef*)malloc(sizeof(DeclTypedef));
  init_decl(decl, DECL_TYPEDEF, name, loc);
  decl->type = type;

  return decl;
}

DeclEnum* decl_enum(SourceLocation loc, const char* name, EnumItem* items, size_t num_items) {
  DeclEnum* decl = (DeclEnum*)malloc(sizeof(DeclEnum));
  init_decl(decl, DECL_ENUM, name, loc);
  decl->items = items;
  decl->num_items = num_items;

  return decl;
}

DeclAggregate* decl_aggregate(SourceLocation loc, const char* name, bool is_union, AggregateField* fields, size_t num_fields) {
  DeclAggregate* decl = (DeclAggregate*)malloc(sizeof(DeclAggregate));
  init_decl(decl, DECL_AGGREGATE, name, loc);
  decl->is_union = is_union;
  decl->fields = fields;
  decl->num_fields = num_fields;

  return decl;
}

DeclFunc* decl_func(SourceLocation loc, const char* name, FuncParam* params, size_t num_params, Typespec* ret_type, StmntBlock* body) {
  DeclFunc* decl = (DeclFunc*)malloc(sizeof(DeclFunc));
  init_decl(decl, DECL_FUNC, name, loc);
  decl->params = params;
  decl->num_params = num_params;
  decl->ret_type = ret_type;
  decl->body = body;

  return decl;
}

DeclVar* decl_var(SourceLocation loc, const char* name, Typespec* type, Expr* expr) {
  DeclVar* decl = (DeclVar*)malloc(sizeof(DeclVar));
  init_decl(decl, DECL_VAR, name, loc);
  decl->type = type;
  decl->expr = expr;
  
  return decl;
}

DeclConst* decl_const(SourceLocation loc, const char* name, Typespec* type, Expr* expr) {
  DeclConst* decl = (DeclConst*)malloc(sizeof(DeclConst));
  init_decl(decl, DECL_CONST, name, loc);
  decl->type = type;
  decl->expr = expr;
  
  return decl;
}
