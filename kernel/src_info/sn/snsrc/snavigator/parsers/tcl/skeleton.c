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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <sys/param.h>
#include <tcl.h>

#include "sn.h"

#ifdef WIN32
#define       OPEN_MODE   "rb"
#else
#define       OPEN_MODE   "r"
#endif /* WIN32 */

extern	FILE *yyin;
extern  int   report_local_vars;
extern  FILE *cross_ref_fp;
        FILE *hig_fp;
static  FILE *out_fp;
static  int   highlight;

Tcl_Encoding encoding = NULL;
static  char *group = "tcl";

void start_parser _ANSI_ARGS_ ((char *fname,int cpp,FILE *hfp,int hig));

static int
log_symbol_filename(FILE *fp, char *fname)
{
  char	*outfile = NULL;

  if (fname) {
    if (yyin)
      yyin = freopen(fname,OPEN_MODE,yyin);
    else
      yyin = fopen(fname,OPEN_MODE);
    if (!yyin) {
      printf("Error: unable to open file \"%s\",errno: %d\n", fname,errno);
      fflush(stdout);
      return 1;
    }
  } else 
    yyin = stdin;

  if (fname) {
    if (highlight) {
      if (hig_fp) {
	fclose(hig_fp);
      }

      outfile = Paf_tempnam(NULL,"hj");
      if (fp) {
	fprintf(fp,"%s\n",outfile);
      }
      
      hig_fp = fopen(outfile,"w+");
    }
    printf("%s\n",fname);
    fflush(stdout);
    
    put_file(fname,group,outfile);
  } else {
    if (highlight) {
      if (fp)
	hig_fp = fp;
      else
	hig_fp = stdout;
    }
  }

  return 0;
}

int
main(int argc, char *argv[])
{
  extern int optind;
  extern char *optarg;
  int	opt;
  char	tmp[MAXPATHLEN];
  char	*fname;
  char	*pipe_cmd      = NULL;
  char	*cachesize     = NULL;
  char	*db_prefix     = NULL;
  char	*incl_to_pipe  = NULL;
  int	case_flag      = TRUE;
  FILE	*list_fp       = NULL;
  FILE	*include_fp    = NULL;
  char	*cross_ref_file= NULL;
  char  *hostname      = NULL;
  char  *snpid         = NULL;

  /* Character set encoding (as defined by Tcl). */
  Tcl_FindExecutable(argv[0]);

  while((opt = getopt(argc,argv,"e:H:O:P:I:n:s:hy:g:p:c:x:i:luB:e:tCrDS:")) != EOF) {
    switch (opt) {
    case 's': 
      if ((out_fp = fopen(optarg,"a")) == NULL) {
	fprintf(stderr,"couldn't create \"%s\"\n",optarg);
	exit(1);
      }
      break;
      
    case 'n':
      db_prefix = optarg;
      break;
      
    case 'e':
      if ((encoding = Tcl_GetEncoding(NULL, optarg)) == NULL)
	{
	  printf("Unable to locate `%s' encoding\n", optarg);
	  return 1;
	}
      break;

    case 'h':
      highlight = -1;
      break;
      
    case 'y':
      list_fp = fopen(optarg,"r");
      break;
      
    case 'I':	/* include path ignored */
      include_fp = fopen(optarg,"r");
      break;
      
    case 'p':
      pipe_cmd = optarg;
      break;
      
    case 'c':
      cachesize = optarg;
      break;
      
    case 'i':
      incl_to_pipe = optarg;
      break;
      
    case 'u':
      case_flag = FALSE;
      break;
      
    case 'C': /* Parser *.h *.c as C++ */
      break;
      
    case 'S':
      break;
      
    case 'x': /* cross reference file (ignored) */
      cross_ref_file = optarg;
      break;
      
    case 'l': /* local variables (ignored) */
      report_local_vars = TRUE;
      break;
      
    case 'B': /* Include error report file */
    case 't': /* Drop /usr files. */
    case 'r': /* Comment support. */
    case 'g':
    case 'D':
    case 'H': hostname=optarg; break;
    case 'P': snpid =optarg; break;

      break;
    }
  }

  if (cross_ref_file) {
    if (!(cross_ref_fp = fopen(cross_ref_file,"a"))) {
      printf("Error: (open) \"%s, errno: %d\"\n",cross_ref_file,errno);
      exit(1);
    }
  }

  if (optind < argc || list_fp)	{
    if (pipe_cmd) {
      if (Paf_Pipe_Create(pipe_cmd,db_prefix,incl_to_pipe,cachesize,
		hostname,snpid) < 0)
	  {
		printf("Error: \"%s\",%s\n",pipe_cmd,strerror(errno));
		fflush(stdout);

		exit(2);
	  }
    } else {
      Paf_db_init_tables(db_prefix,cachesize, NULL);
    }
    if (list_fp) {
      /* This part is called when the project is beeing created. */
      while (fgets(tmp,sizeof(tmp) -1,list_fp))	{
	if ((fname = strchr(tmp,'\n')))	{
	  *fname = '\0';
	}
	
	if (log_symbol_filename(out_fp,tmp) == 0) {
	  start_parser(tmp,0,NULL,0);
	}
      }
      fclose(list_fp);
    } else {
      /* This part is called when a file has been saved, thus
       * we parse the file and provide highlighting.
       */
      if (optind == (argc - 1) && highlight != -1)
	highlight = 1;
      
      fname = argv[optind];
      if (!log_symbol_filename(out_fp,fname)) {
	start_parser(fname,0,hig_fp,highlight);
      }
    }
  } else {
    /* We provide only highlighting for stdin. */
    if (log_symbol_filename(out_fp,(char *)NULL) == 0) {
      start_parser(NULL,0,stdout,highlight);
    }
  }
  
  if (yyin)
  {
    fclose(yyin);
  }
  
  if (out_fp)
  {
    fclose(out_fp);
  }

  if (hig_fp && hig_fp != out_fp)
  {
    fclose(hig_fp);
  }

  Paf_Pipe_Close();

  if (cross_ref_fp)
  { 
    fclose(cross_ref_fp);
  }

  if (encoding) {
    Tcl_FreeEncoding(encoding);
    Tcl_Finalize();
  }

  return 0;
}

