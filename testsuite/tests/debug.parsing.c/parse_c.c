
void
foo () {}

void
case_func (char c)
{
  int found_quote = 0;
  int found_double_quote = 0;

  switch (c)
  {
    case '\'':
      found_quote = 1;
      break;
    case '\"':
      found_double_quote = 1;
      break;
    default:
      break;
  }
}

void
func_string (char* name) {int a = 1;}

#define PARAMS(x) x
struct u {};
static void 	pa_copyright PARAMS ((int));
static void 	pa_end 		 	PARAMS ((int));
static void pa_enter PARAMS ((int));
static void pa_entry PARAMS ((int));
static void pa_equ PARAMS ((int));
void pa_exit PARAMS ((int));
static struct u pa_struct PARAMS ((int));

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
  unsigned char CC = 'B';

  char* S  = "abcd";
  char* S2 = "ab\nc";
  char* S3 = "ab[\"c";
  char* S4 = (char*) malloc (1024);
  char* S5 = "ab[\"c\"]\n";

  int* Act = &A;

  enum colors {Blue, Red, Green} My_Enum_Variable = Blue;

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
  struct My_Record {
    int i;
    int * field1;
    char * field2;
    double field3;
    int field4 [2][2];
  };
  struct My_Record V = {33, &A, "ab", 1.0, {{1, 2}, {3, 4}}};

  struct { int a; int b; int c[2]; } Anonymous_Var = {1, 2, {3, 4}};

  typedef struct My_Record My_Record_Typedef;
  My_Record_Typedef V2 = {33, &A, "ab", 1.0, {{1, 2}, {3, 4}}};

  /* Record of record */
  struct My_Record_Of_Record {
    struct Field1_Record {
      int a;
      int* b;
    } c;
    int d;
  };
  struct My_Record_Of_Record Mror = {{1, &A}, 2};

  /* Record of anonymous record */
  struct My_Record_Of_Record2 {
    struct {
      int a;
      int* b;
    } c;
    int d;
    int e;  /* different number of fields in internal and external structs. */
    int f; 
  };
  struct My_Record_Of_Record2 Mror2 = {{1, &A}, 2, 3, 4};
  
  /* Record of unions */
  struct My_Record_Of_Unions {
    union {
      int a;
      float b;
    } c;
  };
  struct My_Record_Of_Unions mrou = {{1}};

  /* Record of enums */
  struct My_Record_Of_Enum {
    enum colors field;
  };
  struct My_Record_Of_Enum mroe = {Blue};

  /* Array of record */
  struct My_Record_Of_Record Mrora [2] =
     { {{3, &A}, 4},  Mror};

  /* Array of pointers to records */
  struct My_Record_Of_Record* Mrorpa [2] =
     { &Mror,  &Mror};

  /* Union types */

  union My_Union {
    int a;
    float b;
  };
  union My_Union Uni = {1};

  /* Union type with embedded struct */
  union My_Union2 {
    struct My_Record a;
    int b;
  };
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
  struct My_Record_With_Subprogram {
    void (*field1) ();
    int field2;
  };
  struct My_Record_With_Subprogram Mrws = {foo, 1};

  struct My_Record_With_Subprogram2 {
    void (*field1 [2]) (int a);
    int field2;
  };
  struct My_Record_With_Subprogram2 Mrws2 = {{foo, foo}, 1};

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

  struct _test_volatile {
  union{
    volatile struct
    {  
       int xx;
    } x;
      int y;
    } u;
  } test_volatile;

  list.chain = (union tree_node *) malloc (sizeof (union tree_node));
  list.chain->common = list;
  
  tv.tv_sec  = 1100 / 1000;
  tv.tv_usec = (1100 % 1000) * 1000;

  /* Initialize to 0 so that test_parse_c gives reliable results */
  memset (&Uni2, 0, sizeof (Uni2));
  memset (&Uni3, 0, sizeof (Uni3));
  
  Uni2.a = V;
  Uni3.b = 2;

  memset (S4, 'a', 1024);
  memset (S4 + 50, 'b', 100);
  memset (S4 + 100, 'c', 100);
 
  test_volatile.u.y = 12;
  
  func_string ("aa");
  foo ();
  case_func ('\"');
  return 0;
}
