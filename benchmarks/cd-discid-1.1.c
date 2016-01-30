/* Generated by CIL v. 1.7.3 */
/* print_CIL_Input is false */

#line 211 "/usr/lib/gcc/i486-linux-gnu/4.4.3/include/stddef.h"
typedef unsigned int size_t;
#line 56 "/usr/include/bits/types.h"
typedef long long __quad_t;
#line 141 "/usr/include/bits/types.h"
typedef long __off_t;
#line 142 "/usr/include/bits/types.h"
typedef __quad_t __off64_t;
#line 45 "/usr/include/stdio.h"
struct _IO_FILE;
#line 49 "/usr/include/stdio.h"
typedef struct _IO_FILE FILE;
#line 180 "/usr/include/libio.h"
typedef void _IO_lock_t;
#line 186 "/usr/include/libio.h"
struct _IO_marker {
   struct _IO_marker *_next ;
   struct _IO_FILE *_sbuf ;
   int _pos ;
};
#line 271 "/usr/include/libio.h"
struct _IO_FILE {
   int _flags ;
   char *_IO_read_ptr ;
   char *_IO_read_end ;
   char *_IO_read_base ;
   char *_IO_write_base ;
   char *_IO_write_ptr ;
   char *_IO_write_end ;
   char *_IO_buf_base ;
   char *_IO_buf_end ;
   char *_IO_save_base ;
   char *_IO_backup_base ;
   char *_IO_save_end ;
   struct _IO_marker *_markers ;
   struct _IO_FILE *_chain ;
   int _fileno ;
   int _flags2 ;
   __off_t _old_offset ;
   unsigned short _cur_column ;
   signed char _vtable_offset ;
   char _shortbuf[1] ;
   _IO_lock_t *_lock ;
   __off64_t _offset ;
   void *__pad1 ;
   void *__pad2 ;
   void *__pad3 ;
   void *__pad4 ;
   size_t __pad5 ;
   int _mode ;
   char _unused2[(15UL * sizeof(int ) - 4UL * sizeof(void *)) - sizeof(size_t )] ;
};
#line 20 "/usr/include/asm-generic/int-ll64.h"
typedef unsigned char __u8;
#line 154 "/usr/include/linux/cdrom.h"
struct cdrom_msf0 {
   __u8 minute ;
   __u8 second ;
   __u8 frame ;
};
#line 162 "/usr/include/linux/cdrom.h"
union cdrom_addr {
   struct cdrom_msf0 msf ;
   int lba ;
};
#line 189 "/usr/include/linux/cdrom.h"
struct cdrom_tochdr {
   __u8 cdth_trk0 ;
   __u8 cdth_trk1 ;
};
#line 219 "/usr/include/linux/cdrom.h"
struct cdrom_tocentry {
   __u8 cdte_track ;
   __u8 cdte_adr : 4 ;
   __u8 cdte_ctrl : 4 ;
   __u8 cdte_format ;
   union cdrom_addr cdte_addr ;
   __u8 cdte_datamode ;
};
#line 147 "/usr/include/stdio.h"
extern struct _IO_FILE *stderr ;
#line 333
extern int fprintf(FILE * __restrict  __stream , char const   * __restrict  __format 
                   , ...) ;
#line 339
extern int printf(char const   * __restrict  __format  , ...) ;
#line 819
extern void perror(char const   *__s ) ;
#line 471 "/usr/include/stdlib.h"
extern  __attribute__((__nothrow__)) void *malloc(size_t __size )  __attribute__((__malloc__)) ;
#line 488
extern  __attribute__((__nothrow__)) void free(void *__ptr ) ;
#line 543
extern  __attribute__((__nothrow__, __noreturn__)) void exit(int __status ) ;
#line 73 "/usr/include/fcntl.h"
extern int ( __attribute__((__nonnull__(1))) open)(char const   *__file , int __oflag 
                                                   , ...) ;
#line 42 "/usr/include/sys/ioctl.h"
extern  __attribute__((__nothrow__)) int ioctl(int __fd , unsigned long __request 
                                               , ...) ;
#line 101 "/home/wslee/benchmarks/sound/cd-discid-1.1/cd-discid.c"
int cddb_sum(int n ) 
{ 
  int ret ;

  {
#line 104
  ret = 0;
  {
  {
#line 106
  while (1) {
    while_continue___0: /* CIL Label */ ;
    while_continue: /* CIL Label */ ;
#line 106
    if (! (n > 0)) {
#line 106
      goto while_break;
    }
#line 107
    ret += n % 10;
#line 108
    n /= 10;
  }
  while_break___0: /* CIL Label */ ;
  }
  while_break: /* CIL Label */ ;
  }
#line 111
  return (ret);
}
}
#line 114 "/home/wslee/benchmarks/sound/cd-discid-1.1/cd-discid.c"
int main(int argc , char **argv ) 
{ 
  int len ;
  int drive ;
  int i ;
  int totaltime ;
  long cksum ;
  unsigned char first ;
  unsigned char last ;
  char *devicename ;
  struct cdrom_tochdr hdr ;
  struct cdrom_tocentry *TocEntry ;
  int tmp ;
  void *tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;
  char *__cil_tmp18 ;
  char *__cil_tmp19 ;
  char *__cil_tmp20 ;
  char *__cil_tmp21 ;
  char *__cil_tmp22 ;
  char *__cil_tmp23 ;
  char *__cil_tmp24 ;
  char *__cil_tmp25 ;
  char *__cil_tmp26 ;
  char *__cil_tmp27 ;
  char *__cil_tmp28 ;
  char *__cil_tmp29 ;
  char *__cil_tmp30 ;
  char *__cil_tmp31 ;
  char *__cil_tmp32 ;

  {
#line 118
  cksum = 0L;
#line 119
  first = (unsigned char)1;
#line 119
  last = (unsigned char)1;
#line 120
  devicename = (char *)"/dev/cdrom";
#line 129
  if (argc == 2) {
#line 130
    devicename = *(argv + 1);
  } else
#line 131
  if (argc > 2) {
    {
    {
#line 132
    fprintf((FILE */* __restrict  */)stderr, (char const   */* __restrict  */)"Usage: %s [devicename]\n",
            *(argv + 0));
    }
    {
#line 133
    exit(1);
    }
    }
  }
  {
  {
#line 136
  drive = open((char const   *)devicename, 2048);
  }
  }
#line 137
  if (drive < 0) {
    {
    {
#line 138
    fprintf((FILE */* __restrict  */)stderr, (char const   */* __restrict  */)"%s: %s: ",
            *(argv + 0), devicename);
    }
    {
#line 139
    perror("open");
    }
    {
#line 140
    exit(1);
    }
    }
  }
  {
  {
#line 154
  tmp = ioctl(drive, 21253UL, & hdr);
  }
  }
#line 154
  if (tmp < 0) {
    {
    {
#line 155
    fprintf((FILE */* __restrict  */)stderr, (char const   */* __restrict  */)"%s: %s: ",
            *(argv + 0), devicename);
    }
    {
#line 156
    perror("CDROMREADTOCHDR");
    }
    {
#line 157
    exit(1);
    }
    }
  }
  {
#line 161
  first = hdr.cdth_trk0;
#line 162
  last = hdr.cdth_trk1;
#line 164
  len = (int )((unsigned long )((int )last + 1) * sizeof(struct cdrom_tocentry ));
  {
#line 166
  tmp___0 = malloc((size_t )len);
  }
#line 166
  TocEntry = (struct cdrom_tocentry *)tmp___0;
  }
#line 167
  if (! TocEntry) {
    {
    {
#line 168
    fprintf((FILE */* __restrict  */)stderr, (char const   */* __restrict  */)"%s: %s: Can\'t allocate memory for TOC entries\n",
            *(argv + 0), devicename);
    }
    {
#line 171
    exit(1);
    }
    }
  }
#line 210
  i = 0;
  {
  {
#line 210
  while (1) {
    while_continue___2: /* CIL Label */ ;
    while_continue: /* CIL Label */ ;
#line 210
    if (! (i < (int )last)) {
#line 210
      goto while_break;
    }
    {
#line 212
    (TocEntry + i)->cdte_track = (__u8 )(i + 1);
#line 213
    (TocEntry + i)->cdte_format = (__u8 )1;
    {
#line 214
    tmp___1 = ioctl(drive, 21254UL, TocEntry + i);
    }
    }
#line 214
    if (tmp___1 < 0) {
      {
      {
#line 215
      fprintf((FILE */* __restrict  */)stderr, (char const   */* __restrict  */)"%s: %s: ",
              *(argv + 0), devicename);
      }
      {
#line 216
      perror("CDROMREADTOCENTRY");
      }
      }
    }
#line 210
    i ++;
  }
  while_break___2: /* CIL Label */ ;
  }
  while_break: /* CIL Label */ ;
  }
  {
#line 220
  (TocEntry + last)->cdte_track = (__u8 )170;
#line 221
  (TocEntry + last)->cdte_format = (__u8 )1;
  {
#line 222
  tmp___2 = ioctl(drive, 21254UL, TocEntry + i);
  }
  }
#line 222
  if (tmp___2 < 0) {
    {
    {
#line 223
    fprintf((FILE */* __restrict  */)stderr, (char const   */* __restrict  */)"%s: %s: ",
            *(argv + 0), devicename);
    }
    {
#line 224
    perror("CDROMREADTOCENTRY");
    }
    }
  }
#line 232
  i = 0;
  {
  {
#line 232
  while (1) {
    while_continue___3: /* CIL Label */ ;
    while_continue___0: /* CIL Label */ ;
#line 232
    if (! (i < (int )last)) {
#line 232
      goto while_break___0;
    }
    {
    {
#line 236
    tmp___3 = cddb_sum(((TocEntry + i)->cdte_addr.lba + 150) / 75);
    }
#line 236
    cksum += (long )tmp___3;
#line 232
    i ++;
    }
  }
  while_break___3: /* CIL Label */ ;
  }
  while_break___0: /* CIL Label */ ;
  }
  {
#line 239
  totaltime = ((TocEntry + last)->cdte_addr.lba + 150) / 75 - ((TocEntry + 0)->cdte_addr.lba + 150) / 75;
  {
#line 243
  printf((char const   */* __restrict  */)"%08lx", ((cksum % 255L << 24) | (long )(totaltime << 8)) | (long )last);
  }
  {
#line 246
  printf((char const   */* __restrict  */)" %d", (int )last);
  }
#line 249
  i = 0;
  }
  {
  {
#line 249
  while (1) {
    while_continue___4: /* CIL Label */ ;
    while_continue___1: /* CIL Label */ ;
#line 249
    if (! (i < (int )last)) {
#line 249
      goto while_break___1;
    }
    {
    {
#line 250
    printf((char const   */* __restrict  */)" %d", (TocEntry + i)->cdte_addr.lba + 150);
    }
#line 249
    i ++;
    }
  }
  while_break___4: /* CIL Label */ ;
  }
  while_break___1: /* CIL Label */ ;
  }
  {
  {
#line 254
  printf((char const   */* __restrict  */)" %d\n", ((TocEntry + last)->cdte_addr.lba + 150) / 75);
  }
  {
#line 256
  free((void *)TocEntry);
  }
  }
#line 258
  return (0);
}
}
