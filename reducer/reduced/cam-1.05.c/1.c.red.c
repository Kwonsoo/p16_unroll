char devname[25];
int init_sliders_dev_nr;
init_sliders() {
  while (1) {
    if (!(init_sliders_dev_nr < 25))
      goto while_break;
    int device_num = init_sliders_dev_nr;
    airac_observe(devname, device_num);
    init_sliders_dev_nr++;
  }
while_break:
  ;
}
