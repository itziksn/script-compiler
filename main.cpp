#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <ctype.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>

#include "common.cpp"
#include "token.cpp"
#include "error.cpp"
#include "lex.cpp"
#include "ast.cpp"
#include "dump_ast.cpp"
#include "parse.cpp"
#include "resolve.cpp"
#include "gen.cpp"
#include "script.cpp"

void run_tests() {
  // init stuff
  init_keywords();
  common_test();
  lexer_test();
  // parser_test();
  resolve_test();
  gen_test();
}

int main(int argc, char** argv) {
  //run_tests();
  //return 0;
  return script_main(argc, argv);
}
