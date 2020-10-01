#include <windows.h>

struct DirIt {
  const char filename[MAX_PATH];
  char full_path[MAX_PATH];
  bool is_dir;
  bool is_valid;

  char search[MAX_PATH];
  
  WIN32_FIND_DATAA find_data;
  HANDLE handle;
};

void dir_it_close(DirIt* it) {
  if(it->handle != INVALID_HANDLE_VALUE) {
    FindClose(it->handle);
  }
  it->is_valid = false;
}

void dir_it_success(DirIt* it) {
  memcpy((void*)it->filename, (void*)it->find_data.cFileName, sizeof(it->filename));
  path_concat(it->search, it->filename, it->full_path, sizeof(it->full_path));
  it->is_valid = true;
  it->is_dir = (it->find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0;
}

DirIt dir_it_start(const char* filename) {
  DirIt it = {};
  memcpy(it.search, filename, strlen(filename));
  fixup_path(it.search);
  
  char search[MAX_PATH];
  path_concat(it.search, "*", search, sizeof(search));
  
  it.handle = FindFirstFileA(search, &it.find_data);
  if (it.handle == INVALID_HANDLE_VALUE ) {
    it.is_valid = false;
  } else {
    dir_it_success(&it);
  }
  return it;
}

void dir_it_next(DirIt* it) {
  if(it->is_valid) {
    if(FindNextFileA(it->handle, &it->find_data)) {
      dir_it_success(it);
    } else {
      dir_it_close(it);
    }
  }
}

void dir_it_cd(DirIt* it, const char* filename) {
  char buffer[MAX_PATH];
  path_concat(it->search, filename, buffer, sizeof(buffer));
  dir_it_close(it);
  DirIt new_it = dir_it_start(buffer);
  memcpy(it, &new_it, sizeof(new_it));
}
