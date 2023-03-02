#include <cmath>
#include <vector>
#include <iostream>

struct Shape {

  Shape(const std::string& color) : color_(color) {}
  
  virtual double area() const = 0;

  const std::string& color() const { return color_; } 
  
private:
  std::string color_;  
};


struct Circle : public Shape {
  
  Circle(double r, const std::string& color)
    : Shape(color), r_(r) {}

  double area() const override {
    return M_PI * r_ * r_;
  }

  double radius() const { return r_; }

private:
  double r_;
};


struct Rect : public Shape {
  Rect(double a, double b, const std::string& color)
    : Shape(color), a_(a), b_(b) {}

  double area() const override {
    return a_ * b_;
  }

  double side_a() const { return a_; }
  double side_b() const { return b_; }

private:
  double a_;
  double b_;
};




void usage(const std::vector<const Shape*>& n)  {
  for (int i = 0; i < size; ++i)
    std::cout << n[i]->area() << std::endl;
}

int main() {
  Circle c(2.4, "red");
  Rect r(1.0, 2.0, "blue");
  std::vector<const Shape*> s{&c, &r};
  usage(s);
  return 0;
}


