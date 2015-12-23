typedef struct _S {
  int a;
  int arr[10];
} S;

int main()
{
  S *s;

  s = (S*)malloc (sizeof (S));

  s->a = 1;
  airac_print (s);
  airac_print (s->arr[10]);
}
