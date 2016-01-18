status() {
  char line[20];
  unsigned tmp = 1;
  while (1) {
    if (tmp >= 20)
      goto while_break;
    airac_observe(line, tmp);
    tmp++;
  }
while_break:
  ;
}
