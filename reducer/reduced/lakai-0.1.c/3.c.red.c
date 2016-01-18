struct {
  char prognames;
} typedef LakaiProgramList;
main(int handle, LakaiProgramList *lp) {
  lp->prognames = malloc(13);
  lakai_akaitoascii(2, lp->prognames, 12);
}

lakai_akaitoascii(char *src, char *dst, int len) {
  int i = 0;
  while (1) {
    if (!(i < len))
      goto while_break;
    airac_observe(dst + i, 0);
    dst = i++;
  }
while_break:
  ;
}
