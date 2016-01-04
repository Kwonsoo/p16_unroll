int namelist, buffer_0, allocated_length;
char name_gather_p18;
int *name_scan() {
  while (1) {
    if (namelist)
      return namelist;
    buffer_0 = allocated_length = sizeof(int) + buffer_0;
    name_gather_p18 = realloc(name_gather_p18, allocated_length);
    namelist = name_gather_p18;
  }
}

main() {
  int name = name_scan();
  airac_observe(name, 0);
}
