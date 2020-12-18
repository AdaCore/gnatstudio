#include "hi.h"
#include <stdio.h>

void DoSomething(int one, int two)
{
   A MyA;
   MyA.one = 1;
   MyA.two = 2;
   printf("one = 0x%08x, two = 0x%08x\n", one, two);
   DoSomethingElse(one+MyA.one, two+MyA.two);
}

void DoSomethingElse(int three, int four)
{
   printf("three = 0x%08x, four = 0x%08x\n", three, four);
}
