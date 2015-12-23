int main()
{
    int x = 1;
    int y = x + 1;
    int r;
   
    int *p = &x;
    r = f(y);

    int i = 1;
    int j = 1;
    int k = 0;
    while (k < 100)
    {
        if (j < 20) {
            j = i;
            k = k + 1;
        } else {
            j = k;
            k = k + 2;
        zoo_print (k);
        }
    }
}

int f (int a) {
  int i;

  for (i = 0; i < a; i++)
      ; 


  return 1;
}
