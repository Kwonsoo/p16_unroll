int dev_vol[25];
ctrl_sliders() {
  int i = 0;
  while (1) {
    if (!(i < 25))
      goto while_break___0;
    airac_observe(dev_vol, i);
    i++;
  }
while_break___0:
  ;
}
