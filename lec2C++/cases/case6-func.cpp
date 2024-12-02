#include <iostream>

// 函数定义， （）内定义传参
int main(int argc, char* argv[])
{
    for(int i=0; i<argc; ++i)
    {
        std::cout << "i = " << i << " -> " << argv[i] << std::endl;
    }
    return 0;// 函数返回值
}