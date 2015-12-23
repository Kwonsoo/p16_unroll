int main()
{
  int a[11];
  int i = 0;
  while (1) {
    i++;
    airac_print (i);
    a[i] = 0;
    if (i >= 10) break;
  }
}
