struct A {
  int a;
  int b[10];
};

struct A s1 = { 0, 0 };
void* malloc(int i);
int main()
{
  int x = 0;
  int *p = 0;
  struct A s2 = { 1, 0 };
  int a, i;
  
  p = &x;
  *p = 1;
  a = *p;

  p = (int*)malloc(sizeof(int) * 10);
  struct A* p2 = (struct A*)malloc(sizeof(struct A) * 10);
  struct A* p3 = (struct A*)malloc(sizeof(struct A));
  struct A arr3[10];
  int arr[10];
  arr[5] = 10;
  arr3[1].a = 1;
  int arr2[3] = {1,2,3};
  for (i=0; i<10; i++)
  {
      p[i] = 0;
  }
  return 0;
} 
