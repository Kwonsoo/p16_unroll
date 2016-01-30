struct S {
  int a;
  int b[10];
};

int main(){
  int* arr = malloc(sizeof(int) * 5);
  struct S* p = malloc(sizeof(struct S));
  struct S* arr2 = malloc(sizeof(struct S) * 10);

  return 0;
}
