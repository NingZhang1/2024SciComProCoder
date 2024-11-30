#include <iostream>
#include <complex>
struct pos
{
    double x;
    double y;
    double z;

    double norm()
    {
        return x*x+y*y+z*z;
    }
};

bool operator<(pos& pos1, pos& pos2)
{
    return pos1.norm() < pos2.norm();
}

template< typename T>
T max(const T& a, const T& b)
{
    return a < b ? b : a;
}
int main()
{
    int a= 23;
    int b= 102;
    std::cout << "a:" << a << ", b: "<< b << ", max is " << max<int>(a, b) << std::endl;

    float c = 3.14;
    float d = -1.2;
    std::cout << "c:" << c << ", d: "<< d << ", max is " << max(c, d) << std::endl;


    std::cout << max('A', 'B') << std::endl;

    pos p1{1,2,3};
    pos p2{-2,-4,-6};
    auto p3 = max(p1, p2);
    return 0;
}