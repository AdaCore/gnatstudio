char* get_nth_environment (int index) {
  extern char** gnat_envp;
  return gnat_envp [index];
}

