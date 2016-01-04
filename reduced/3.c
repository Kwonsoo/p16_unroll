char quotearg_n_options_val;
int quotearg_n_options_tmp___0;
void *quotearg_n_options_p17;
quotearg_buffer(char *buffer___1, int buffersize) {
  int len = 0;
  if (len < buffersize)
    airac_observe(buffer___1 + len, 0);
  len++;
  return len;
}

main() {
  int size;
  quotearg_n_options_tmp___0 = quotearg_buffer(quotearg_n_options_val, size);
  size = quotearg_n_options_tmp___0 + 1;
  quotearg_n_options_p17 = realloc(quotearg_n_options_p17, size);
  quotearg_buffer(quotearg_n_options_p17, size);
}
