#include <iostream>
#include "my_class.hh"

using namespace std;

int main()
{
  cout << "Hello, World!\n";
  My_Class obj;

  auto a = obj.ch;
  auto rec = obj.rec;

  return 0;
}
