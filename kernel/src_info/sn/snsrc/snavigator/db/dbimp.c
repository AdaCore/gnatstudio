/*

Copyright (c) 2000, Red Hat, Inc.

This file is part of Source-Navigator.

Source-Navigator is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2, or (at your option)
any later version.

Source-Navigator is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along
with Source-Navigator; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.



*/

/*
 * dbimp.c
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * The database engine.
 */

#include "dbutils.h"
#include <setjmp.h>
#include <signal.h>
#include <unistd.h>
#include "longstr.h"

#include <tcl.h>

#ifdef __MSVC__
#include <io.h>
#endif

/* FIXME: we should try and decouple dbimp from libcpp2. */

/* Not used, but we need to define one due to us linking against libcpp2. */
Tcl_Encoding encoding = NULL;

extern	int	optind;
#ifdef WIN32
extern	const char	*optarg;
#else
extern	char	*optarg;
#endif /* WIN32 */

extern	char	*filename_g;
extern	int	yylineno,yycharno;
extern	int	report_local_vars;
extern	int	yyfd;
extern	int	Paf_dbimp_running;
extern	jmp_buf	BAD_IMPL_jmp_buf;

static	int	killed;

#define	TTY_TRACE	0	/* Normally 0 */
#if TTY_TRACE
static	FILE	*tty;
#endif /* TTY_TRACE */

#ifdef WIN32
#define	sleep(s)	Sleep(s * 1000);
#endif/* WIN32 */

static	void
my_panic(int sign)
{
	killed = -1;

	fprintf(stderr,"dbimp panic; signal %d received",sign);

	if (filename_g)
	{
		fprintf(stderr,", in file: %s, at line: %d column: %d\n",
			filename_g,yylineno,yycharno);
	}
	fprintf(stderr,"\n");
	fflush(stderr);

#ifdef SIGABRT
	signal(SIGABRT,SIG_IGN);
#endif /* SIGABRT */

	abort();
}

static  void
term_catch(int sign)
{
	signal(sign,term_catch);

	killed = TRUE;
#if TTY_TRACE
	if (tty)
	{
		fprintf(tty,"Received signal: %d (pid: %d)\n",
			sign,(int)getpid());
	}
#endif /* TTY_TRACE */
}

static	int
read_lock_file(char *lock_file, unsigned long *lck_sn_pid,char *lck_host,
	unsigned short *lck_port,unsigned long *lck_db_pid)
{
	char	tmp[500];
	int	fd;
	int	cou;
	int	scan_ret;

	fd = open(lock_file,O_RDONLY,0);
	if (fd == -1)
		return -1;

	tmp[sizeof(tmp) - 1] = '\0';
	/* Try it 3 times ! */
	for (scan_ret = 0, tmp[0] = '\0', cou = 0; cou < 3; cou++)
	{
		int	len;

		len = read(fd,tmp,sizeof(tmp) -2);
		if (len <= 0)
			continue;

		*lck_sn_pid = 0;
		*lck_host = '\0';
		*lck_port = 0;
		*lck_db_pid = 0;

		tmp[len] = '\0';
		scan_ret = sscanf(tmp,"%lu %s %hu %lu",
			lck_sn_pid,
			lck_host,
			lck_port,
			lck_db_pid);
		if (scan_ret == 4)
		{
			break;
		}
		sleep(1);
		lseek(fd,(off_t)0,SEEK_SET);		/* Rewind ! */
	}

	close(fd);

	if (scan_ret != 4 && access(lock_file,F_OK) == 0)
	{
		return -1;	/* The file has strange contents. */
	}

	/* We could read it. */
	return 0;
}

/*
 * This function checks whether the lock file and its locker
 * process are existing, and whether the TCP/IP port of the
 * starter process can ne still accessed.
 * Return values:
 *	TRUE:		the lock is still active.
 *	FALSE:		the lock is not active.
 *	-1:		the locker has probably crashed.
 */
static	int
check_running(char *lock_file)
{
  /* This is a legacy variable. */
  char dummy[MAXHOSTNAMELEN + 2];
  
  unsigned short lck_port = 0;
  unsigned long lck_sn_pid = 0;
  unsigned long lck_db_pid = 0;
  
  if (read_lock_file(lock_file,&lck_sn_pid,dummy,&lck_port,&lck_db_pid) == -1)
    {
      return FALSE;
    }
  
  if (lck_sn_pid == 0)
    {
      /* Very strange; should never happen! */
      return FALSE;
    }
  
  /* Is the project marked as unusable ? */
  if (lck_db_pid == 0)
    {
      /* Yes, it is unusable. */
      return -1;
    }
  
  /* Does the S-N process still exist? */
  if (kill(0, (pid_t) lck_sn_pid) == -1 && errno == ESRCH)
    {
      /* The lock is not active, the process has died. */
      return -1;
    }
  
  /* Does the DB process still exist ? */
  if (kill(0, (pid_t) lck_db_pid) == -1 && errno == ESRCH)
    {
      unsigned short chk_lck_port = 0;
      unsigned long chk_lck_sn_pid = 0;
      unsigned long chk_lck_db_pid = 0;
      char	chk_lck_host[MAXHOSTNAMELEN + 2];
      
      /* Here we now that the database process is not running, lets
       * read the contents of the lock file again to check
       * whether it contents has been changed since we read the lock
       * file.
       */
      if (read_lock_file(lock_file,&chk_lck_sn_pid,chk_lck_host,&chk_lck_port,
			 &chk_lck_db_pid) == -1 ||
	  chk_lck_sn_pid != lck_sn_pid || strcmp(dummy,chk_lck_host) != 0 ||
	  lck_port != chk_lck_port)
	{
	  /* At this point there are two possibilities:
	   * 1: the lock file has been deleted.
	   * 2: its contents has been changed.
	   */
	  return FALSE;
	}
      
      /* Here it is pretty sure that the database project has died. */
      return -1;
    }
  
  /* Return TRUE or -1? Bleah. */
  return TRUE;
}

/* This function creates a lock file.
 * Return values:
 *	TRUE:		Successful.
 *	FALSE:		The locker process died.
 *	-1:			No permission to lock.
 */
static	int
create_lock_file(char *lock_file,char *sn_host,char *sn_pid)
{
	int	fd;
	char	status_buf[500];
	int	status_len;
	int	write_ret;

	/* It is importan, that the pid is expanded with 0
	 * because the zeros are space holders.
	 */
	sprintf(status_buf,"%s %s %7lu\n",
		sn_pid,
		sn_host,
		(unsigned long)getpid());
	status_len = strlen(status_buf);

	while (!killed)
	{
		/* Check whether we can create the file! */
		if ((fd = open(lock_file,O_RDWR|O_CREAT|O_EXCL,0666)) == -1)
		{
			if (errno == EINTR)		/* Interrupted ? */
				continue;
			if (errno != EEXIST)
			{						/* Even the file does not exist, it */
				return -1;			/* was not possible to create it. */
			}

			switch (check_running(lock_file))
			{
			case TRUE:
				/* Lets try it again! */
#if TTY_TRACE
				if (tty)
					fprintf(tty,"trying %d\n",(int)getpid());
#endif /* TTY_TRACE */
				sleep(1);
				break;

			case FALSE:
#if TTY_TRACE
				if (tty)
					fprintf(tty,"removing lock file %d\n",(int)getpid());
#endif /* TTY_TRACE */
				unlink(lock_file);
				break;

			case -1:		/* The locker process died. */
#if TTY_TRACE
				if (tty)
					fprintf(tty,"The locker process died.\n");
#endif /* TTY_TRACE */
				return FALSE;
				break;
			}
			continue;
		}

		/* We could create the file, now we write into it. */
		write_ret = write(fd,status_buf,status_len);
		if (write_ret == -1)
			return -1;
		if (write_ret == status_len)
		{
			char	read_buf[1000];

			lseek(fd,(off_t)0,SEEK_SET);		/* Rewind ! */

			if (read(fd,read_buf,sizeof(read_buf)) == status_len &&
				memcmp(status_buf,read_buf,status_len) == 0)
			{
				close(fd);

#if TTY_TRACE
				if (tty)
					fprintf(tty,"Lock created %d.\n",(int)getpid());
#endif /* TTY_TRACE */
				return TRUE;	/* We read what we wrote. */
			}
		}
		/* Lets try it again! */
#if TTY_TRACE
		if (tty)
			fprintf(tty,"Trying again %d...\n",(int)getpid());
#endif /* TTY_TRACE */
		sleep(1);
	}

	return FALSE;		/* Reached, only when killed. */
}

static void
set_signals()
{
#ifdef SIGHUP
	signal(SIGHUP,my_panic);
#endif /* SIGHUP */

#ifdef SIGINT
	signal(SIGINT,term_catch);
#endif /* SIGINT */

#ifdef SIGQUIT
	signal(SIGQUIT,my_panic);
#endif /* SIGQUIT */

#ifdef SIGILL
	signal(SIGILL,my_panic);
#endif /* SIGILL */

#ifdef SIGABRT
	signal(SIGABRT,my_panic);
#endif /* SIGABRT */

#ifdef SIGBUS
	signal(SIGBUS,my_panic);
#endif /* SIGBUS */

#ifdef SIGSEGV
	signal(SIGSEGV,my_panic);
#endif /* SIGSEGV */

#ifdef SIGSYS
	signal(SIGSYS,my_panic);
#endif /* SIGSYS */

#ifdef SIGPIPE
	signal(SIGPIPE,term_catch);
#endif /* SIGPIPE */

#ifdef SIGTERM
	signal(SIGTERM,term_catch);
#endif /* SIGTERM */

#ifdef SIGURG
	signal(SIGURG,term_catch);
#endif /* SIGURG */

#ifdef SIGXCPU
	signal(SIGXCPU,my_panic);
#endif /* SIGXCPU */

#ifdef SIGXFSZ
	signal(SIGXFSZ,my_panic);
#endif /* SIGXFSZ */

#ifdef SIGUSR1
	signal(SIGUSR1,my_panic);
#endif /* SIGUSR1 */

#ifdef SIGUSR2
	signal(SIGUSR2,my_panic);
#endif /* SIGUSR2 */

#ifdef SIGPROF
	signal(SIGPROF,my_panic);
#endif /* SIGPROF */

#ifdef SIGDANGER
	signal(SIGDANGER,my_panic);
#endif /* SIGDANGER */

#ifdef SIGPRE
	signal(SIGPRE,my_panic);
#endif /* SIGPRE */
}

#define	MAX_MACRO_FILES 500
void Paf_Cpp_Cross_Ref_Clean();
int
main(int argc, char **argv)
{
	char	*data;
	char	*key;
	char	*bufp;
	char	save_c;
	long	type;
	int	linenum;
	char	*cache = NULL;
	char	*cross_cache = NULL;
	FILE	*logfp = NULL;
	FILE	*infp = stdin;
	char	*file = NULL;
	int	first_xref = TRUE;
	char	*db_prefix = NULL;
	char	*sn_host = NULL;
	char	*sn_pid = NULL;
	char	*omode = NULL;
	char	lock_file[MAXPATHLEN];
	int	set_sgns = FALSE;
	LongString	buf;
	char *macro_file[MAX_MACRO_FILES];
	int macro_file_num = 0;

	LongStringInit(&buf,0);
	Tcl_FindExecutable(argv[0]);

#if !WIN32 && TTY_TRACE
	tty = fopen("/dev/tty","w");
#endif /* !WIN32 && TTY_TRACE */

	while((type = getopt(argc,argv,"c:C:lf:H:O:P:M:sFm:")) != EOF)
	{
		switch (type)
		{
		case 'c':
			cache = optarg;
			break;

		case 'C':
			cross_cache = optarg;
			break;

		case 'l':
			report_local_vars = TRUE;
			break;

		case 'f':
			file = optarg;
			break;

		case 'H':
			sn_host = optarg;
			break;

		case 'P':
			sn_pid = optarg;
			break;

		case 'M':
			omode = optarg;
			break;

		case 's':
			set_sgns = FALSE;
			break;

		case 'F':
			break;

		case 'm':
			if (macro_file_num < MAX_MACRO_FILES - 1)
			{
				macro_file[macro_file_num++] = optarg;
			}
			MacroReadFile(optarg);
			break;
		}
	}

	if (optind < argc)
	{
		db_prefix = argv[optind];
	}

	if (!db_prefix)
	{
		printf("Usage: %s ?-c cache_size? ?-C cross_cache_size? ?-l? ?-f file? ?-m macrofile? db_prefix\n",
			argv[0]);

		exit(2);
	}

	if (set_sgns)
		set_signals();

	killed = FALSE;

	if (sn_host && sn_pid)
	{
		sprintf(lock_file,"%s.lck",db_prefix);

		switch (create_lock_file(lock_file,sn_host,sn_pid))
		{
		case -1:
			fprintf(stderr,"Could not create lock,%s\n",strerror(errno));
			exit(2);
			break;

		case FALSE:
			fprintf(stderr,"The database is in an inconsistent state.\n");
			exit(2);
			break;
		}
	}
	else
		lock_file[0] = '\0';

	if (file)
	{
		FILE	*in;

		in = fopen(file,"r");
		if (in)
			infp = in;
	}

	/*SN_DBIMP is set interactive in the tcl init procedure!*/
	if (getenv("SN_DBIMP"))
	{
		char    tmp[MAXPATHLEN];
		char    *e;

		e = getenv("TMPDIR");

		sprintf(tmp,"%s/dbimp_%lu.tmp",
			e ? e : "/tmp",
			(unsigned long)getpid());

		logfp = fopen(tmp,"w+");
		if (logfp)
		{
			chmod(tmp,0666);
			fprintf(logfp,"#dbimp (pid: %lu) started database\n#prefix: <%s> cache: %s ",
				(unsigned long)getpid(),
				db_prefix,
				cache ? cache : "#");
			fprintf(logfp,"cross_cache: %s local_vars: %d file: <%s>\n",
				cross_cache ? cross_cache : "#",
				report_local_vars,
				file ? file : "stdin");
			if (macro_file_num)
			{
				int cou;

				for (cou = 0; cou < macro_file_num; cou++)
				{
					fprintf(logfp,"macro file %d: <%s>\n",
						cou + 1,
						macro_file[cou]);
				}
			}
			fflush(logfp);
		}
	}

	Paf_dbimp_running = TRUE;

	Paf_db_init_tables(db_prefix,cache,cross_cache);

	linenum = 0;
	type = -999;

	switch (setjmp(BAD_IMPL_jmp_buf))
	{
	case PAF_PANIC_SOFT:
		killed = TRUE;

		fprintf(stderr,"dbimp (soft) panic");

		if (filename_g)
		{
			fprintf(stderr,", in file: %s, at line: %d column: %d\n",
				filename_g,yylineno,yycharno);
		}
		fprintf(stderr,"\n");
		fflush(stderr);
		break;

	case PAF_PANIC_SIMPLE:
		killed = TRUE;
		break;

	case PAF_PANIC_EMERGENCY:
		killed = -1;

		fprintf(stderr,"Run time error: %s\n",strerror(errno));
		fflush(stderr);
		break;
	}

	while (!killed && (bufp = buf.fgets(&buf,infp)))
	{
		if (logfp)
		{
			fputs(bufp,logfp);
			fputs("\n",logfp);
			fflush(logfp);
		}

		if (bufp[0] == '#' || bufp[0] == '\0')
			continue;

		linenum++;

		key = strchr(bufp,';');
		if (key)
			data = strchr(key + 1,';');
		else
			data = NULL;

		if (!key || !data)
		{
			fprintf(stderr,"dbimp Error: %s\n",bufp);
			fflush(stderr);
			continue;
		}

		save_c = *key;
		*key = '\0';
		type = strtol(bufp,NULL,0);
		*key = save_c;

		if (type < -3)
		{
			fprintf(stderr,"dbimp invalid type: %ld, %s\n",type,bufp);
			fflush(stderr);
			continue;
		}
		else if (type == -1)
		{
			db_remove_file_def(0,data + 1);
		}
		else if (type == -3)
		{
			db_remove_file_xfer_using_keys(0,data + 1);
		}
		else if (type == PAF_CROSS_REF_CPP)
		{
			if (first_xref)
			{
				first_xref = FALSE;

				open_tables_for_cross_ref();
			}
			Paf_insert_cross_ref_qry(bufp);
		}
		else
		{
			*data = '\0';
			db_insert_entry(type,key + 1,data + 1);
		}
	}

#if TTY_TRACE
	if (tty)
	{
		fprintf(tty,"END (pid: %d) killed: %d eof: %d\n",
			(int)getpid(),killed,feof(infp));
	}
#endif /* TTY_TRACE */

	buf.free(&buf);

	if (!first_xref)
	{
		Paf_Cpp_Cross_Ref_Clean();
	}

	if (Paf_db_close_tables() == -1)
	{
		fprintf(stderr,"Database closing error: %s\n",strerror(errno));
		fflush(stderr);

		killed = -1;
	}

	if (logfp)
	{
		fprintf(logfp,"#dbimp (pid: %lu) exited\n",(unsigned long)getpid());
		fclose(logfp);
	}

	if (yyfd >= 0)
	{
		close(yyfd);
		yyfd = -1;
	}

	if (lock_file[0])
	{
		if (killed == -1)
		{
			/*
			 * Mark the project as unusable. We must not open the
			 * file with O_TRUNC, because if the disk is already full,
			 * we will get a zero size file that we cannot write.
			 */
			int	fd = open(lock_file,O_WRONLY,0666);
			if (fd != -1)
			{
				char	status_buf[500];

				/* Now, we just overwrite the file. The 0000000 pid indicates
				 * that the project is unusable.
				 */
				sprintf(status_buf,"%s %s %7d\n",
					sn_pid,
					sn_host,
					0);

				write(fd,status_buf,strlen(status_buf));

				close(fd);
			}
		}
		else
		{
			/* Delete the lock file to indicate, that we did not crash! */
			if (unlink(lock_file) == -1 && access(lock_file,F_OK) != -1)
			{
				/* Under Windows, somebody might have open the file. */
				sleep(1);

#if TTY_TRACE
				if (tty)
				{
					fprintf(tty,"Removing lock file %d\n",(int)getpid());
				}
#endif /* TTY_TRACE */
				unlink(lock_file);
			}
		}
	}

	fclose(infp);	/* It is important to synchronize. */

	exit(killed == -1 ? 2 : 0);

	return 0;
}

