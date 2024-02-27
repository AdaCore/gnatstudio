  class First_Class {
  public:
    First_Class (void);  // First constructor
    First_Class (int);   // Second constructor
    void public_func ();
    int public_var;

  protected:
    void protected_func ();
    int protected_var;  // Visible by child

  private:
    void private_func ();
    int private_var;
  };

  class Second_Class : public First_Class {
  public:
    Second_Class (void);
    void second_public_func ();
    int second_public_var;
    
  protected:
    void second_protected_func ();
    int second_protected_var;

  private:
    void second_private_func ();
    int second_private_var;
  };

  struct Struct_As_Class {
  public:
    Struct_As_Class ();
    void foo ();
    int struct_public_var;

  private:
    void bar ();
    int struct_private_var;
  };

  class Multiple_Inheritance : public Second_Class, Struct_As_Class {
  public:
    Multiple_Inheritance ();
    void third_public_func ();
    int third_public_var;
  protected:
    void third_protected_func ();
    int third_protected_var;
  private:
    void third_private_func ();
    int third_private_var;
  };
    
  class CL {
    const static double x;
  };

