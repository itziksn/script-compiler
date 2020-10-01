
void error(SourceLocation loc, const char* fmt, ...) {
  printf("%s(%d): error: ", loc.filename, (int)loc.line);
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
  printf("\n");
}

void fatal_error(SourceLocation loc, const char* fmt, ...) {
  printf("%s(%d): error: ", loc.filename, (int)loc.line);
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
  printf("\n");
  exit(1);
}

void info(SourceLocation loc, const char* fmt, ...) {
  printf("%s(%d): warning: ", loc.filename, (int)loc.line);
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
  printf("\n");
}

/* 
void fatal_error(const char* fmt, ...) {
  printf("error: ");
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
  printf("\n");
  exit(1);
}
*/ 
