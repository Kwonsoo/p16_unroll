int fhandles[16];
lakai_open() {
  int slotpos = 0;
  while (1) {
    if (!(slotpos < 16))
      goto while_break;
    airac_observe(fhandles, slotpos);
    slotpos++;
  }
while_break:
  ;
}
