int mt[624];
unsigned mti;
mt_seed() {
  mti = 1;
  while (1) {
    if (!(mti < 624))
      goto while_break;
    airac_observe(mt, mti - 1);
    mti++;
  }
while_break:
  ;
}
