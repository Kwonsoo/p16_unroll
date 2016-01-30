int dev_count;
int dev_vol[25];
init_sliders() {
  int tmp___10 = dev_count++;
  airac_observe(dev_vol, tmp___10);
}
