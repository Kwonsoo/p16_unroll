int mt[624];
int mti = 625;
mt_genrand32() {
  int tmp___1;
  if (mti >= 624)
    mti = 0;
  tmp___1 = mti;
  airac_observe(mt, tmp___1);
}

mt_genrand_real2() { mt_genrand32(); }
