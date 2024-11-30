#include <iostream>

//值传递，a，b，c本地变量会拷贝一份传入的值
void add(int a, int b, int c)
{
    c = a + b;
}
//引用传递，引用传递copy地址
void add_ref(int& a, int& b, int& c)
{
    c = a + b;
}
//指针传递，指针的值拷贝一份传入本地指针标量
void add_ptr(int* a, int* b, int* c)
{
    *c = *a + *b;
}
// const关键字 防止参数被修改
void add_const(const int a, const int b, int& c)
{
    c = a + b;
}

int main()
{
    int a = 1;
    int b = 2;
    int c = 0;

    add(a,b,c);
    std::cout << "add(a=" << a <<",b=" << b << ") = " << c<< std::endl;
    add_ref(a,b,c);
    std::cout << "add_ref(a=" << a <<",b=" << b << ") = " << c<< std::endl;
    add_ptr(&a, &b, &c);
    std::cout << "add_ptr(a=" << a <<",b=" << b << ") = " << c<< std::endl;
    add_const(a, b, c);
    std::cout << "add_const(a=" << a <<",b=" << b << ") = " << c<< std::endl;
}