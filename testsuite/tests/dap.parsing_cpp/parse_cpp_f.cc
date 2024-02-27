#include "parse_cpp_f.hh"

First_Class::First_Class (void)
  : public_var (1), protected_var (2), private_var (3)
{
}

First_Class::First_Class (int a)
  : public_var (a), protected_var (a + 1), private_var (a + 2)
{
}

void First_Class::public_func () {}
void First_Class::protected_func () {}
void First_Class::private_func () {}

Second_Class::Second_Class (void)
  : First_Class (),
    second_public_var (4),
    second_protected_var (5),
    second_private_var (6)
{
}

void Second_Class::second_public_func () {}
void Second_Class::second_protected_func () {}
void Second_Class::second_private_func () {}

Struct_As_Class::Struct_As_Class ()
  : struct_public_var (10), struct_private_var (11)
{
}


void Struct_As_Class::foo () {}
void Struct_As_Class::bar () {}

Multiple_Inheritance::Multiple_Inheritance (void)
  : Second_Class (),
    third_public_var (7),
    third_protected_var (8),
    third_private_var (9)
{
}

void Multiple_Inheritance::third_public_func () {}
void Multiple_Inheritance::third_protected_func () {}
void Multiple_Inheritance::third_private_func () {}
    
const double CL::x = 5;
