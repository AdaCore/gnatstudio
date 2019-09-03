
#include <stdlib.h>
#include <string.h>

#include "parse_cpp_f.hh"
#include "parse_cpp_classes.h"

/** Test for C++'s parsing. This is of course heavily based on C **/

void foo () {}

void bar (int a) {}

void
func_string (char* name) {int a = 1;}

struct u {};
static void 	pa_copyright (int);
static void 	pa_end 		 	(int);
static void pa_enter (int);
static void pa_entry (int);
static void pa_equ (int);
void pa_exit (int);
static struct u pa_struct (int);

int
main () {

  /* Note: the following lines are indented with tabs (ASCII.HT), instead of
     spaces, so that we can easily test that feature.
	<->	The tab in this comment is also intended as a test. */	int t;
  
	int   A	= 1;
	float B  = 2.0;
	char  C  = 'A';
	  short	 Sh	 =	 65;
  long  L  = 32L;
  unsigned Uns = 33;
  unsigned long UL = 33L;

  char* S  = "abcd";
  char* S2 = "ab\nc";
  char* S3 = "ab[\"c";
  char* S4 = (char*) malloc (1024);
  char* S5 = "ab[\"c\"]\n";

  int* Act = &A;

  enum {Blue, Red, Green} My_Enum_Variable = Blue;

  /* Integer array */
  int T [4] = {2, 3, 4, 5};

  /* Empty array */
  int Ea [0];

  /* Array of access */
  int* Aoa [3] = { &A, &A, &A};

  /* Integer array, 2D */
  int U [2][3] = {{2, 3, 4}, {5, 6, 7}};

  /* Integer array, 3D */
  int A3d [2][2][2] = {{{1, 2}, {1, 2}}, {{1,2}, {1,2}}};

  /* Integer array ptr */
  int** Iaa = (int**)&T;

  /* simple struct type */
  /* struct My_Record {
    int i;
    int * field1;
    char * field2;
  }; */
  struct My_Record V = {33, &A, "ab"};

  struct { int a; int b; } Anonymous_Var = {1, 2};

  typedef struct My_Record My_Record_Typedef;
  My_Record_Typedef V2 = {33, &A, "ab"};

  /* Record of record */
/*  struct My_Record_Of_Record {
    struct Field1_Record {
      int a;
      int* b;
    } c;
    int d;
  };
*/
  struct My_Record_Of_Record Mror = {{1, &A}, 2};

  /* Array of record */
  struct My_Record_Of_Record Mrora [2] =
     { {{3, &A}, 4},  Mror};

  /* Array of pointers to records */
  struct My_Record_Of_Record* Mrorpa [2] =
     { &Mror,  &Mror};

  /* Union types */

/*  union My_Union {
    int a;
    float b;
  }; */
  union My_Union Uni = {1};

  /* Union type with embedded struct */
  /* union My_Union2 {
    struct My_Record a;
    int b;
  }; */
  union My_Union2 Uni2;
  union My_Union2 Uni3;

  /* Record with embedded union */
  struct My_Record_With_Union {
    union My_Union field1;
    float field2;
  };
  struct My_Record_With_Union Mrwu = {Uni, 3.4};
  
  /* Access to subprograms */
  void (*as) () = foo;

  /* Array of access to subprograms */
  void (*asa[2]) () = {foo, foo};

  /* Struct containing an access to subprogram */
  /* struct My_Record_With_Subprogram {
    void (*field1) ();
    int field2;
  }; */
  struct My_Record_With_Subprogram Mrws = {foo, 1};

  /*struct My_Record_With_Subprogram2 {
    void (*field1 [2]) (int a);
    int field2;
  }; */
  struct My_Record_With_Subprogram2 Mrws2 = {{bar, bar}, 1};

  /* Class with no field (test 1.9) */
  class No_Field {};
  No_Field NF;

  typedef long int __time_t;

  struct timeval
  {
    __time_t tv_sec;            /* Seconds.  */
    __time_t tv_usec;           /* Microseconds.  */
  } tv;

  struct tree_common {
    union tree_node *chain;
  };
  union tree_node {
    struct tree_common common;
  };

  struct tree_common list;


  // Classes and inheritance
  First_Class FC;
  Second_Class SC;
  Struct_As_Class SAC;
  Multiple_Inheritance MI;

  class Foo : public First_Class {
    int a;
  };

  Foo Foo_Instance;

  class Virtual : public virtual First_Class {
    private:
      int a;
    protected:
      int b;
  };

  Virtual Virtual_Instance;

  CL cl1;
  CL2 cl2;
  CL3 cl3;
  CL4 cl4;

  list.chain = (union tree_node *) malloc (sizeof (union tree_node));
  list.chain->common = list;
  
  tv.tv_sec  = 1100 / 1000;
  tv.tv_usec = (1100 % 1000) * 1000;

  Uni2.a = V;
  Uni3.a = V;
  Uni3.b = 2;

  memset (S4, 'a', 1024);
  memset (S4 + 50, 'b', 100);
  memset (S4 + 100, 'c', 100);
  /* S4 is incorrectly displayed by ddd, but not by odd :-) */
  
  func_string ("aa");
  foo ();
  return 0;
}
