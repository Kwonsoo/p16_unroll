/* Generated by CIL v. 1.7.3 */
/* print_CIL_Input is false */

#line 7
extern int ( /* missing proto */  airac_observe)() ;
#line 5
extern int ( /* missing proto */  top)() ;
#line 1 "test.c"
int main(void) 
{ 
  int pos ;
  int buf[10] ;
  int tmp ;
  void *__cil_tmp4 ;
  void *__cil_tmp5 ;
  void *__cil_tmp6 ;
  void *__cil_tmp7 ;
  void *__cil_tmp8 ;
  void *__cil_tmp9 ;
  void *__cil_tmp10 ;
  void *__cil_tmp11 ;
  void *__cil_tmp12 ;
  void *__cil_tmp13 ;
  void *__cil_tmp14 ;
  void *__cil_tmp15 ;
  void *__cil_tmp16 ;

  {
#line 3
  pos = 614512;
  {
#line 5
  while (1) {
    while_continue: /* CIL Label */ ;
    {
#line 5
    tmp = top();
    }
#line 5
    if (! (pos < tmp)) {
#line 5
      goto while_break;
    }
#line 6
    if (! pos) {
      {
#line 7
      airac_observe(buf, pos);
#line 8
      buf[pos] = 0;
      }
    }
#line 10
    pos ++;
  }
  while_break: /* CIL Label */ ;
  }
#line 12
  return (0);
}
}
