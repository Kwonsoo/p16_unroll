int mt[624];
mt_genrand32() {
  int kk = 0;
  while (1) {
    if (!(kk < 227))
      goto while_break;
    airac_observe(mt, kk + 1);
    kk++;
  }
while_break:
  ;
}
