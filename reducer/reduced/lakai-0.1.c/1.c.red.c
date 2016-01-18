int fhandles[16];
lakai_init() {
  int i = 0;
  while (1) {
    if (!(i < 16))
      goto while_break;
    airac_observe(fhandles, i);
    i++;
  }
while_break:
  ;
}
