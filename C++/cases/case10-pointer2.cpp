#include <iostream>

int main()
{
    double b = 3.14;

    // 指针ptr 其值是变量b的地址
    double* ptr = &b;
    std::cout <<"1-> b value:"<< b <<", b address:"<< &b << ", ptr:" << ptr<<std::endl;

    // 解指针赋值，会覆盖该地址存储的值
    *ptr = 0.0;
    std::cout <<"2-> b value:"<< b <<", b address:"<< &b << ", ptr:" << ptr<<std::endl;


}