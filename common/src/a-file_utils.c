#include <sys/stat.h>

int
__gnat_subdirectories_count (name)
{
#ifdef _WIN32
  /* On windows, stat(2) alwways return 1 for the number of links, and thus can
     not be used reliably.
     However, for GPS we only want to know if there is at least one
     subdirectory, so we just pretend this is always true.
  */
  return 1;
  
#else
  struct stat statbuf;
  int ret;
  
  ret = __gnat_stat (name, &statbuf);
  
  if (ret)
    return -1;
  else
    /* Do not count the subdirectories . and .. */
    return statbuf.st_nlink - 2;
#endif
}
