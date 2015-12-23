char buf[31];
char *crackaddr_bp;
main() {
  char *tmp___0 = crackaddr_bp;
  *tmp___0 = 1;  // in FS, this statement is not bot. FS produces the same result as FI.
  airac_print (buf);
  airac_observe(tmp___0, 0);
  crackaddr_bp--;
  crackaddr_bp++;
  crackaddr_bp = buf;
}
