
struct My_Record_Of_Record {
  struct Field1_Record {
    int a;
    int *b;
  } c;
  int d;
};

class My_Class {
public:
  int num;
  char ch;
  My_Record_Of_Record rec;
  /* This is a default constructor of the
   * class, do note that it's name is same as
   * class name and it doesn't have return type.
   */
  My_Class() {
    num = 100;
    ch = 'A';
  }
  int get_x();
};
