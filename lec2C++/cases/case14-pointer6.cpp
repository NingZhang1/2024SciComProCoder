#include <iostream>

double sum(double* ptr, int size)
{
    double s0=0.0;
    // 数组越界
    for(int i = 0; i <= size; ++i)
    {
        s0 += ptr[i];
    }
    return s0;
}

int main()
{
    double* d = new double[100];
    for(int i=0; i<100; ++i)
    {
        d[i] = i*1.0;
    }
    std::cout << sum(d, 100) << std::endl;
    delete[] d;
}