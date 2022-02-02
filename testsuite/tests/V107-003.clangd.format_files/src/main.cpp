#include <iostream>
#include "my_class.hh"

using namespace std;

int main(   ) {
  int i =         //  VeryVeryVeryVeryVeryLongComment
    longFunction( // Again a long comment
      arg);

  cout << "Hello, World!";
  My_Class obj;

  auto a = obj.ch;
  auto rec = obj.rec;
  int x = obj.get_x();

  return 0;
}
