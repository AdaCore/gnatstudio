#pragma message ("unixstubd.h included for windows.")
#include <sys/types.h>
#ifndef O_NDELAY
  #pragma message ("[#define O_NDELAY 0].")
  #define O_NDELAY 0
  #pragma message ("[#define O_NONBLOCK 0].")
  #define O_NONBLOCK 0
#endif

#ifndef mode_t
  #pragma message ("[typedef int mode_t].")
  typedef int mode_t;
#endif

#undef HAVE_SYS_UTSNAME_H
#ifndef F_OK
  #pragma message ("[#define F_OK 0].")
  #define F_OK 0
#endif

#ifndef caddr_t
  #pragma message ("[typedef char * caddr_t].")
  typedef	char *	caddr_t;
  #pragma message ("[typedef long ssize_t].")
  typedef	long ssize_t;
  #pragma message ("[typedef int dev_t].")
  typedef	int dev_t;
  #pragma message ("[typedef int uid_t].")
  typedef	int uid_t;
  #pragma message ("[typedef int gid_t].")
  typedef	int gid_t;
  #pragma message ("[typedef unsigned short nlink_t].")
  typedef unsigned short nlink_t;
#endif

#ifndef MAXPATHLEN
  #pragma message ("[#define MAXPATHLEN 256].")
  #define MAXPATHLEN 256
#endif
  #define MAXHOSTNAMELEN 256

#ifndef  iovec
  #pragma message ("[struct iovec].")
  struct iovec {
	caddr_t iov_base;
	int iov_len;
  };
#endif

#ifndef  stat
  #pragma message ("[struct stat].")
 /* struct	stat 
  {
    dev_t		st_dev;
    ino_t		st_ino;
    mode_t	st_mode;
    nlink_t	st_nlink;
    uid_t		st_uid;
    gid_t		st_gid;
    dev_t		st_rdev;
    off_t		st_size;
    time_t	st_atime;
    long		st_spare1;
    time_t	st_mtime;
    long		st_spare2;
    time_t	st_ctime;
    long		st_spare3;
    long		st_blksize;
    long		st_blocks;
    long	st_spare4[2];
  };*/
  #define		S_IRUSR	0000400	/* read permission, owner */
  #define		S_IWUSR	0000200	/* write permission, owner */

#endif

#include <Shlobj.h>