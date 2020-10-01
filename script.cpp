const char* paths[] = {
  "./",
};

bool parse_package(SourceLocation loc, const char* package_name) {
  for(int i = 0; i < sizeof(paths) / sizeof(*paths); ++i) {
    for(DirIt it = dir_it_start(paths[i]); it.is_valid; dir_it_next(&it)) {
      if(it.is_dir && strcmp(it.filename, package_name) == 0) {
	for(dir_it_cd(&it, it.filename); it.is_valid; dir_it_next(&it)) {
	  if(!it.is_dir && strcmp(get_ext(it.filename), ".sc") == 0) {
	    const char* file_content = read_file(it.full_path);
	    if(!file_content) {
	      fatal_error(loc, "cannot open file %s", it.filename);
	    }
	    size_t len = strlen(it.full_path) + 1;
	    char* full_path = (char*)malloc(len);
	    memcpy(full_path, it.full_path, len);
	    init_stream(full_path, file_content);
	    DeclSet decls = parse_declset();
	    add_global_decls(decls);
	  }
	}
	return true;
      }
    }
  }
  fatal_error(loc, "cannot find package '%s'", package_name);
  return false;
}

void init_compiler() {
  init_keywords();
  init_builtin_syms();
}

int script_main(int argc, char** argv) {
  if(argc < 3) {
    printf("usage: %s <sc-package> <out-cpp-file>\n", argv[0]);
    return 1;
  }

  init_compiler();
  
  const char* package = argv[1];
  SourceLocation loc = {"command line argument 1", 0};
  parse_package(loc, package);
  resolve_global_syms();
  gen_ordered_decls();
  write_file(argv[2], gen_buffer, buf_len(gen_buffer));
  
  printf("Compilation succeed\n");
  return 0;
}
