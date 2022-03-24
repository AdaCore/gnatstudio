#include "my_class.hh"

My_Class::My_Class (int x) 
: x(x) {
}

int My_Class::get_x() {
    return this->x;
}