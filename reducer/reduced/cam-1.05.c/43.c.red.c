int dev_count, init_sliders_dev_nr, init_sliders_dev_nrs;
int dev_vol[25];
int dev_ind[5];
init_sliders() {
  dev_ind[init_sliders_dev_nr] = dev_count++;
  dev_ind[init_sliders_dev_nr] = -1;
  int dev_nr_tmp = 0;
  dev_nr_tmp = dev_ind[init_sliders_dev_nrs];
  if (dev_nr_tmp)
    goto __Cont;
  airac_observe(dev_vol, dev_nr_tmp);
__Cont:
  ;
}
