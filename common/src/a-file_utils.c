#include <sys/stat.h>

int
__gnat_subdirectories_count (name)
{
   struct stat statbuf;
   int ret;

   ret = __gnat_stat (name, &statbuf);

   if (ret)
      return -1;
   else
      return statbuf.st_nlink - 2;
}


