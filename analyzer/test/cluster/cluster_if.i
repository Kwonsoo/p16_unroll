typedef struct S{
  int len;
  int mem;
  char* str;
} S;

int main(){
   char* p = malloc(g());

   /* cluster 1 */
   if(g())
     *p = 1;
   else
     *p = 2;

   *p = 3;


   /* cluster 2 */
   S* str = malloc(f());

   if(!str || !str->str)
     str = malloc(f());
   if(++(str->len) > str->mem)
     str->str = malloc(f());

   str->str[str->len -1] = 0;
   
}

