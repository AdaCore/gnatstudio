#include <vector>
#include <map>

template <class T>
  class bah
  {
  public:
    bah() : b(a){}
    const T &f() const { }
  private:
    T a[2];
    T *b;
  };

class banana { };
class cake : public banana { };

class c
{
public: c();
private:
  typedef std::array<const cake *,2> pouet;
  template <class e>
    class d : public std::vector<const e *> { };
  template <class e>
    class f : public std::map<bool, d<e>> { };
  template <class T>
      std::array<const T*,2> failing(const f<T> &g,double h, double qwerty) const;

  bah<f<cake> > bake;

};

template <class T>
  std::array<const T*,2> c::failing(const f<T> &g,double h, double qwerty) const
{
  if (true)
    {
      return nullptr;
    }
}

bool c::random() const
{
 failing(bake.f(),0.0,0.0);
}
