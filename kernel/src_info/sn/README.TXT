Thank you for choosing Red Hat Source-Navigator.  This README document
outlines some outstanding issues with this release of Source-Navigator.

DISTRIBUTION:

The most current distribution lives at 
ftp://sourceware.cygnus.com/pub/sourcenav. 

Binary releases may be available for source releases, especially for
the Windows version since the Windows version is hard to build. A UNIX
binary distribution will have a name of the form SNPlatformYYYYMMDD.tar.gz,
where Platform will be descriptive of what the binary was compiled for, and
YYYYMMDD is the source distribution the binary was compiled with. Windows
binaries will be SNWindowsYYYYMMDD.zip or SNWindowsYYYYMMDD.exe, depending
on what happens with installer technology.

BINARY RELEASE INSTALLATION NOTES:

UNIX systems:

Source-Navigator installation requires you to have an X Server running and
the DISPLAY environment variable set. The exact command to set this depends
on the shell you are running; please see your System Administrator if
you cannot figure it out.

To install Source-Navigator, you need to copy the correct .tar.gz file
to your machine, decompress and install. To do this, make a directory in
which you wish to copy the .tar.gz file. This will be a temporary directory,
so /tmp/snavigator is a good choice. Assuming you use /tmp/snavigator, type 
the following:

	% cd /tmp/snavigator
	% cp <path to the correct .tar.gz file> .
	% gzip -d SN*.tar.gz
	% tar xvf SN*.tar

You can then cd into the bin directory and type:

./snavigator

or you can place the bin directory on your path.

Windows systems:

You must have about 25 MB free on your Windows system drive. You must
have about 10 MB free on your installation drive.

Double-click on the file SNWindows20000421.exe (or whatever
the date of the release you are installing is) and following the on-screen
instructions.

BUILD INSTRUCTIONS:

UNIX: 

Source-Navigator is built using gcc, although most OEM compilers should
work as well. It is currently supported on Red Hat Linux x86 6.0, 6.1, 6.2,
SUSE Linux x86 6.0, 6.1, 6.2 and 6.3, Caldera Linux x86 2.2, Solaris SPARC
2.5.1 and 2.6, and HP/UX 10.20 and 11.0. It has been built in the past
for AIX, SunOS, IRIX, and DEC OS/F, although those distributions are no
longer supported and tested. Any problems compiling on these or other
distributions will be noted, but it is unlikely that Red Hat will fix them.
Please feel free to submit patches!

First, download the correct source distribution (see above) Source-Navigator 
requires three distinct directories, one for the source, one for the build, 
and one to install into.

Second, unpack the sources. Copy the source archive into your source
directory, and then type:

gunzip SourceNavSrc20000421.tar.gz
tar xvf SourceNavSrc20000421.tar

Next, create a build directory. Then type:

cd <builddir>
<srcdir>/devo/configure --prefix=<installdir>
make all-snavigator
make install-snavigator

To run the application,

cd <installdir>/bin
./snavigator

If you make changes and have to regenerate configure or any of the Makefile.in
files, you must have autoconf, automake and autoheader installed on your
machine. They are available from a variety of sites, including www.fsf.org.

WINDOWS:

Building Source-Navigator on Windows is pretty complex; it is usually easier
to use the binary distributions. Source-Navigator is supported on Windows NT
4.0, Windows 95, Windows 98, Windows NT 4.0J, Windows 95J and Windows 98J.

If you must rebuild Source-Navigator on Windows, please send mail
to sourcenav@sourceware.cygnus.com, and we will attempt to post
rational build instructions.

MAILING LISTS:

Two mailing lists have been set up for Source-Navigator discussion and
issues:

	sourcenav@sourceware.cygnus.com: A general discussion list and
	the place to submit patches.

	sourcenav-announce@sourceware.cygnus.com: Announcements about
	Source-Navigator releases and third party add-ons should be
	posted here. Moderated.

For subscription information, please see 
http://sourceware.cygnus.com/sourceware.

USING SOURCE-NAVIGATOR

NOTE: If you have projects created by versions of Source-Navigator earlier
than 4.2, you must rebuild your projects.

Projects created by Source-Navigator 4.5.1 are not guaranteed to work
with earlier versions of Source-Navigator.

Online documentation for Source-Navigator can be found in your installation 
folder:

	<installation path>/html/index.html
	
We have removed the license manager from Source-Navigator. You will no
longer need to have a license key to run this product.

Access to Cross-Reference Information 

- Source-Navigator parses and builds a project database in two steps.  
The first step is to build the project's symbol tables.  A progress
bar is displayed during this time, when complete, the application 
will display the browser windows.  Phase 2 is when the cross-
referencing (XRef) info is computed.  Source-Navigator will continue 
to build the cross reference database in the background.  While 
building the cross reference database, the cross reference icon will 
not be accessible in the toolbar, and the application will not be 
very responsive.  Once the cross reference database is built, the
application performance will return to normal.  You can locate such
identifiers with the Grep tool.

C Parser Issues

- C declarations that are generated by a macro are not detected 
correctly by the C and C++ parser.  For example:

	#define EXTERN(type, array) extern type array[]

	EXTERN(char, mybuffer);

In this example, the parser should pick up information about a global 
variable called "mybuffer" whose type is char[].  However, it doesn't.

Tcl Cross-Referencing and Common Tcl Commands

- The following Tcl, Tk, and [incr Tcl] common commands are not put into 
the cross-reference (XRef) database:

append       eval      itcl_class   objects    string
args         exists    itcl_info    parray     switch
array        expr      itk          previous   this
binary       file      iwidgets     private    tkinfo
body         for       join         proc       unset
break        foreach   lappend      protected  uplevel
catch        format    lindex       public     upvar
class        gets      local        rename     variable
common       glob      lrange       return     virtual
concat       global    lreplace     regexp     vwait
configbody   if        lsearch      regsub     while
constructor  incr      lsort        scan       winfo
continue     info      lstat        set        @scope
default      inherit   method       source     #auto
delete       isa       namespace    split
destructor   itcl      object       stat

Changes to the APIs

- In Source-Navigator 4.x and 99r1, the location and syntax of the rc.tcl 
customization script has changed.  The file now exists in the 
.sn directory for user specific customizations.  Procedure names 
are prefixed by "sn_" instead of "paf_".  Please refer to the 
Programmer's Guide for more details.

Windows Menus

In Windows Menus, the '^' symbol means Ctrl and 'x' next to a 
letter means Alt plus that letter.

Multiple Machine Issues

- If you build a project on machine A with sources rooted at /foo 
and real path /disk/user/foo, Source-Navigator will store these 
files in the project as being rooted at /disk/user/foo, not /foo.  
If you try to access these files from machine B, where 
/usr/disk/foo is not visible, you will get an error.  

- When Source-Navigator scans /usr/include for C and C++ projects,
the contents will be machine specific.  It is important to keep
these issues in mind when accessing common projects from different
locations.

- If you have a heterogeneous environment, or if two machines have 
a different perspective on the disk/filesystem layout, there may 
be problems sharing projects between the two machines.

- If Source-Navigator is built on a Digital UNIX system, projects
created by the Digital UNIX system will not be readable on other
operating systems, and projects created by other operating systems
will not be readable on a Digital UNIX system.

Using CVS as a version control system

- In order for CVS to work properly, your project file needs to be
one of two places:

	- In the same directory as the source files. This only works
	if your project has no subdirectories.
	- In the directory where the toplevel of your source tree is.
	Basically, the relative path from your project to each of your
	source files has to be the same as the relative path from the
	CVSROOT to the files on the repository.

Running the generated executable on UNIX

When configuring your executable command line in the Edit Target dialog,
if your executable expects stdin or terminal input, the application will
hang waiting for the input. To fix this, add a command like:

xterm -e myapp

in this dialog. Make sure that xterm is on your path. Now, when you hit
"Run" from the Make window, your application will run.

If you need to debug your application with Insight, however, you will need
to remove "xterm -e" from your command line in the Edit Target window.

This will be addressed in a future version of Source-Navigator.

