int namelist, buffer_0, allocated_length;
char name_gather_p18;
main() {
  int *cursor;
  while (1) {
    cursor = namelist;
    airac_observe(cursor, 0);
    buffer_0 = allocated_length = sizeof(int) + buffer_0;
    name_gather_p18 = realloc(name_gather_p18, allocated_length);
    namelist = name_gather_p18;
  }
}
