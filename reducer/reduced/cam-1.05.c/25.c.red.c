int dev_count;
int dev_vol[25];
init_sliders() {
  airac_observe(dev_vol, dev_count);
  dev_count++;
}
