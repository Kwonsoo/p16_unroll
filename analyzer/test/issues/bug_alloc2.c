/*
 * longstats gets allocated as "8"x24 array
 * */
static const char *const longstats[] =
{
  "\tCommand being timed: \"%C\"\n",
  "\tUser time (seconds): %U\n",
  "\tSystem time (seconds): %S\n",
  "\tPercent of CPU this job got: %P\n",
  "\tElapsed (wall clock) time (h:mm:ss or m:ss): %E\n",
  "\tAverage shared text size (kbytes): %X\n",
  "\tAverage unshared data size (kbytes): %D\n",
  "\tAverage stack size (kbytes): %p\n",
  "\tAverage total size (kbytes): %K\n",
  "\tMaximum resident set size (kbytes): %M\n",
  "\tAverage resident set size (kbytes): %t\n",
  "\tMajor (requiring I/O) page faults: %F\n",
  "\tMinor (reclaiming a frame) page faults: %R\n",
  "\tVoluntary context switches: %w\n",
  "\tInvoluntary context switches: %c\n",
  "\tSwaps: %W\n",
  "\tFile system inputs: %I\n",
  "\tFile system outputs: %O\n",
  "\tSocket messages sent: %s\n",
  "\tSocket messages received: %r\n",
  "\tSignals delivered: %k\n",
  "\tPage size (bytes): %Z\n",
  "\tExit status: %x",
  ((void *)0)
};


int main()
{
   int x = longstats[0];
   airac_print (longstats);
   return x;
}
