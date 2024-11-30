#include <iostream>

// 函数声明给定形参默认值
void add(int&a, int value=1);

// 函数定义中不给形参值
void add(int&a, int value)
{
    a += value;
}

int main()
{
    int a=10;

    add(a, 10);
    std::cout << " add 10: a= " << a << std::endl;
    add(a);
    std::cout << " add: a= " << a << std::endl;
}