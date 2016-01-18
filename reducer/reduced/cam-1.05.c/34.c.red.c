int dev_ind[25];
ctrl_sliders() {
  int dev_nrs = 0;
  while (1) {
    if (!(dev_nrs < 25))
      goto while_break___2;
    airac_observe(dev_ind, dev_nrs);
    dev_nrs++;
  }
while_break___2:
  ;
}
