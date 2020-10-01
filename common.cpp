void fixup_path(char* path) {
  while(*path) {
    if(*path == '\\') {
      *path = '/';
    }
    ++path;
  }
}

void path_concat(const char* path_a, const char* path_b, char* buffer, size_t buffer_size) {
  int index = 0;
  while(*path_a) {
    buffer[index++] = *path_a++;
  }

  if(buffer[index - 1] != '/' && buffer[index - 1] != '\\') {
    buffer[index++] = '/';
  }

  while(*path_b) {
    buffer[index++] = *path_b++;
  }
  assert(index < buffer_size - 1);
  buffer[index] = 0;
}

const char* get_ext(const char* filename) {
  const char* last_dot = filename;
  while(*filename) {
    if(*filename == '.') {
      last_dot = filename;
    }
    ++filename;
  }
  return last_dot;
}

#ifdef _MSC_VER
#include "win32_specifics.cpp"
#endif

#define MAX(x, y) ((x) > (y) ? (x) : (y))

struct BufHdr {
  size_t cap;
  size_t len;
  char buf[1];
};

#define buf__hdr(buffer) ((BufHdr*)((char*)(buffer) - offsetof(BufHdr, buf)))
#define buf__fits(buffer, s) (buf_cap(buffer) - buf_len(buffer) >= s)
#define buf__fit(buffer, new_size) buf_grow(buffer, new_size)  

#define buf_cap(buffer) ((buffer) ? buf__hdr(buffer)->cap : 0)
#define buf_len(buffer) ((buffer) ? buf__hdr(buffer)->len : 0)
#define buf_push(buffer, ...) (buf__fits(buffer, 1) ? *(buffer + buf_len(buffer)) = __VA_ARGS__, 0 : *(void**)&buffer = buf_grow(buffer, buf_len(buffer) + 1, sizeof(__VA_ARGS__)), *(buffer + buf_len(buffer)) = __VA_ARGS__, buf__hdr(buffer)->len++, 0)
#define buf_clear(buffer) ((buffer) ? buf__hdr(buffer)->len = 0 : 0)
#define buf_free(buffer) ((buffer) ? free(buf__hdr(buffer)), buffer = NULL : 0)
#define buf_end(buffer) ((buffer) + buf_len(buffer))

void* buf_grow(void* buffer, size_t new_length, size_t elem_size)
{
  size_t new_size = MAX(1 + (buf_len(buffer) * 2), new_length);
  BufHdr* new_buffer = (BufHdr*)calloc(elem_size*new_size + offsetof(BufHdr, buf), 1);
  new_buffer->len = buf_len(buffer);
  new_buffer->cap = new_size;
  memcpy(new_buffer->buf, buffer, elem_size*buf_len(buffer));
  buf_free(buffer);
  return new_buffer->buf;
}

#define buf_printf(buffer, fmt, ...) (buffer = buf__printf(buffer, fmt, __VA_ARGS__))

char* buf__printf(char* buffer, const char* fmt, ...) {
  size_t len = buf_len(buffer);
  size_t cap = buf_cap(buffer);
  size_t available = cap - len;
  
  va_list args;
  va_start(args, fmt);
  size_t needed = vsnprintf(NULL, 0, fmt, args) + 1;
  va_end(args);
  
  if(available < needed) {
    buffer = (char*)buf_grow(buffer, len + needed, sizeof(char));
  }

  assert(buf_cap(buffer) - len >= needed);
  va_start(args, fmt);
  vsnprintf(&buffer[len], needed, fmt, args);
  va_end(args);
  buf__hdr(buffer)->len += needed - 1;
  buffer[buf_len(buffer)] = 0;
  return buffer;
}

#define ARENA_SIZE (1024 * 1024)
#define ARENA_ALLIGN 8
#define PTR_ALLIGN_UP(ptr, align) (((char*)ptr)+(((intptr_t)ptr) % (align)))

struct Arena {
  size_t capacity;
  size_t occupied;
  char* memory;
  Arena* next;
};

Arena interns_arena;

void init_arena(Arena* arena, size_t size) {
  arena->memory = (char*)malloc(size);
  memset(arena->memory, (int)size, 0);
  arena->occupied = 0;
  arena->capacity = size;
}

void* arena_alloc(Arena* arena, size_t size) {
  if(!arena->memory)
    init_arena(arena, ARENA_SIZE);
  else if (arena->occupied + size >= arena->capacity) {
	  arena->next = (Arena*)malloc(sizeof(Arena));
	  init_arena(arena->next, ARENA_SIZE);
	  *arena = *arena->next;
  }
  assert(arena->occupied + size <= arena->capacity);
  arena->memory = PTR_ALLIGN_UP(arena->memory, ARENA_ALLIGN);
  char* ptr = arena->memory + arena->occupied;
  arena->occupied += size;
  return ptr;
}

struct MapEntry {
  void* key;
  void* value;
  int64_t hash;
};

struct Map {
  size_t len;
  size_t cap;
  MapEntry* entries;
};

void** map_put_hashed(Map* map, const void* key, const void* value, uint64_t hash);

void map_free(Map* map) {
  free(map->entries);
  map->cap = 0;
  map->len = 0;
}

void map_grow(Map* map) {
  Map new_map = {};
  size_t new_size = MAX(16, map->cap * 2);
  new_map.entries = (MapEntry*)calloc(new_size, sizeof(MapEntry));
  new_map.cap = new_size;
  
  for(MapEntry* it = map->entries; it != map->entries + map->cap; ++it) {
    if(it->key) {
      map_put_hashed(&new_map,it->key, it->value, it->hash);
    }
  }
  map_free(map);
  *map = new_map;
}

void** map_put_hashed(Map* map, const void* key, const void* value, uint64_t hash) {
  assert(key);
  if(!value) {
    return NULL;
  }

  if(map->len >= map->cap / 2) {
    map_grow(map);
  }

  size_t index = hash % map->cap;
  MapEntry* it = map->entries + index;
  for(;;) {
    if(!it->key) {
      it->key = (void*)key;
      it->value = (void*)value;
      it->hash = hash;
      ++map->len;
      break;
    } else if(it->key == key) {
      it->value = (void*)value;
      it->hash = hash;
      break;
    }
    ++it;
    if(it == map->entries + map->cap) {
      it = map->entries;
    }
  }

  return &it->value;
}

uint64_t hash_bytes(const void* bytes, size_t len) {
  uint64_t hash = 14695981039346656037;
  uint64_t fnv_mul = 1099511628211;
  
  const char* it = (const char*)bytes;
  while (it != (const char*)bytes + len) {
    hash *= fnv_mul;
    hash ^= *it++;
  }
  return hash;
}

uint64_t hash_ptr(intptr_t ptr) {
  return hash_bytes(&ptr, sizeof(intptr_t));
}

void** map_put(Map* map, const void* key, const void* value) {
  return map_put_hashed(map, key, value, hash_ptr((intptr_t)key));
}

void* map_get_hashed(Map* map, const void* key, uint64_t hash) {
  if(map->cap == 0) {
    return NULL;
  }
  size_t index = hash % map->cap;
  MapEntry* it = map->entries + index;
  for(;;) {
    if(!it->key) {
      return NULL;
    } else if(it->key == key) {
      return it->value;
    }
    ++it;
    if(it == map->entries + map->cap) {
      it = map->entries;
    }
  }
}

void* map_get(Map* map, const void* key) {
  return map_get_hashed(map, key, hash_ptr((intptr_t)key));
}

struct Intern {
  size_t len;
  Intern* next_intern;
  char str[1];
};


Map interns_map;

const char* str_intern_range(const char* str, size_t len) {
  uint64_t hash = hash_bytes(str, len);
  Intern* first = (Intern*)map_get_hashed(&interns_map, (void*)hash, hash);
  for(Intern* it = first; it; it = it->next_intern) {
    if(len == it->len && strncmp(it->str, str, len) == 0) {
      return it->str;
    }
  }

  Intern* new_intern = (Intern*)arena_alloc(&interns_arena, sizeof(Intern) + sizeof(char) * len + 1);
  new_intern->len = len;
  memcpy(new_intern->str, str, len);
  new_intern->str[len] = 0;
  new_intern->next_intern = first;
  map_put_hashed(&interns_map, (void*)hash, new_intern, hash);
  return new_intern->str;
}

const char* str_intern(const char* str) {
  return str_intern_range(str, strlen(str));
}

bool write_file(const char* filename, void* data, size_t size) {
  FILE* file = fopen(filename, "w");
  if(!file) {
    return false;
  }

  size_t written = fwrite(data, 1, size, file);
  fclose(file);
  return written == size;
}

const char* read_file(const char* filename) {
  FILE* file = fopen(filename, "rb");
  if(!file) {
    return NULL;
  }
  
  fseek(file, 0, SEEK_END);
  long size = ftell(file);
  fseek(file, 0, SEEK_SET);

  char* buffer = (char*)malloc(size + 1); // +1 for null-terminator

  size_t read = 0;
  while(read != size) {
    size_t cur_read = fread(buffer, 1, size, file);
    if(cur_read == 0 && read != size) {
      free(buffer);
      buffer = NULL;
      goto done;
    }
    read += cur_read;
  }
  
  buffer[size] = 0;
  
 done:
  fclose(file);
  return buffer;
}

void common_test() {
  // Map tests
  Map map = {};
  for(intptr_t i = 1; i < 1024 * 10; ++i) {
    map_put(&map, (void*)i, (void*)(i*123));
  }
  for(intptr_t i = 1; i < 1024 * 10; ++i) {
    assert(map_get(&map, (void*)i) == (void*)(i*123));
  }

  const char* MAX_VALUE0 = str_intern("MAX_VALUE0");
  assert(MAX_VALUE0 == str_intern("MAX_VALUE0"));

  // Buf printf tests
  char* buffer = NULL;
  buf_printf(buffer, "Hello");
  buf_printf(buffer, ", World!\n");
  buf_printf(buffer, "%d", 2342);
  buf_printf(buffer, ", %f", 324.123);
  
  // String interning test
  const char* str1 = str_intern("Hello");
  const char* str2 = str_intern("Hello");
  assert(str1 == str2);
  const char* str3 = str_intern("Hello1");
  assert(str3 != str2);

  // Strechy buf tests
  int* test = NULL;
  const int N = 1024 * 1024;
  for (int i = 0; i < N; ++i) {
    buf_push(test, i);
  }
  assert(buf_len(test) == N);
  for (int i = 0; i < N; ++i)
    assert(test[i] == i);
  buf_clear(test);
  assert(buf_len(test) == 0);
  buf_free(test);

  // File tests
  const char* content = read_file("W:/data/test.sc");
  (void)content;

  
  for(DirIt it = dir_it_start("C:\\"); it.is_valid; dir_it_next(&it)) {
    printf("%s is dir: %d\n", it.filename, it.is_dir);
  }

  char mixed_path[] = "C:\\data\\users/test";
  fixup_path(mixed_path);
  char filename[] = "test.cpp";
  char full_path[256];
  path_concat(mixed_path, filename, full_path, sizeof(full_path));
  printf("%s\n", full_path);
}
