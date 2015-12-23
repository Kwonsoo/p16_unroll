typedef struct str
  {
    char *str;
    int len;
  } str_t;

int main()
{
  str_t *str;
  str = malloc(sizeof *str);
  str->str = malloc (64);
  airac_print (str);
  airac_print (str->str);
  *(str->str) = 0;
}
