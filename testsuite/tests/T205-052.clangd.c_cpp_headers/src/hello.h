extern "C"
{
#include "common.h"
}

class Hello
{
public:
   Hello();
   virtual ~Hello();

   void DoSomething(int one, int two);
   void DoSomethingElse(int three, int four);
private:
   int MyInt;
   int MyInt2;
   A MyA;
};
