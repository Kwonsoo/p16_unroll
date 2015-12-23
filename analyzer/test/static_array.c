struct R {
  int r;
};

struct S {
  int a;
  struct R b[10];
};


int garr[10];

int main()
{
  int arr1[10];
  arr1[0] = 0;
  struct S arr2[20];
  arr2[0].a = 1;  
  arr2[1].b[4].r = 10;
  return 0;
} 
