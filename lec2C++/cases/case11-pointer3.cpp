#include <iostream>

int main()
{
    int N = 12;
    double* ptr = new double[N];
    // 循环赋值
    for(int i=0; i<N; ++i)
    {
        ptr[i] = i*2.0 + 0.5;
    }
    // 循环取值
    for(int i=0; i<N; ++i)
    {
        std::cout << "i: " << i << ", val:" << ptr[i] << std::endl;
    }
    return 0;
}