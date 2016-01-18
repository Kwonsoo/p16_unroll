int mt[624];
mt_genrand32() {
  int kk = 0;
  while (1) {
    if (!(kk < 227))
      goto while_break;
    kk++;
  }
while_break:
  if (!(kk < 623))
    goto while_break___0;
  airac_observe(mt, kk + 1);
while_break___0:
  ;
}
