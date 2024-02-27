  struct My_Record {
    int i;
    int * field1;
    char * field2;
  };

  /* Record of record */
  struct My_Record_Of_Record {
    struct Field1_Record {
      int a;
      int* b;
    } c;
   int d;
  };

  union My_Union {
   int a;
   float b;
  };

  /* Union type with embedded struct */
  union My_Union2 {
    struct My_Record a;
    int b;
  };

  class CL2 {
    public:
      CL2 (): x (10) {}
      int get_x() {return x;}
      virtual int set_x(int val) {x=val; return x;}
    protected:
      int x;
  };

  class CL3 : public CL2 {
  public:
    int y;
    virtual int set_x(int val) {x=val+1; return x;}
    CL3 (): y (11) {}
  };
                                                                                
  class CL4 : public CL2 {
  public:
    virtual int set_x(int val) {x=val+1; return x;}
  };

  struct My_Record_With_Subprogram {
    void (*field1) ();
    int field2;
  };

  struct My_Record_With_Subprogram2 {
    void (*field1 [2]) (int a);
    int field2;
  };

