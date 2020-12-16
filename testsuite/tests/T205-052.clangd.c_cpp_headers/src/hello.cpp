#include "hello.h"
#include <stdio.h>

Hello::Hello() : MyInt(8), MyInt2(9), MyA()
{
}

Hello::~Hello()
{
}

void Hello::DoSomething(int one, int two)
{
   printf("this = 0x%08x, one = 0x%08x, two = 0x%08x, MyInt = %d, MyInt2 = %d, MyInd Addr = 0x%08x\n",
          this, one, two, MyInt, MyInt2, &MyInt);
   DoSomethingElse(one+one, two+two);
}

void Hello::DoSomethingElse(int three, int four)
{
   printf("this = 0x%08x, three = 0x%08x, four = 0x%08x, MyInt = %d, MyInt2 = %d, MyInd Addr = 0x%08x\n",
          this, three, four, MyInt, MyInt2, &MyInt);
}
