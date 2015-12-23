typedef struct ipi_mem_info
{
    int f;
    char a[10];
} sname ;


int main()
{
    sname mem_stats;
    airac_print (mem_stats.a);
    mem_stats.f = 1;
    airac_print (mem_stats.a);
    strncpy (mem_stats.a, "12345679abcdefghik!@#$%!@", sizeof(mem_stats.a));
}
