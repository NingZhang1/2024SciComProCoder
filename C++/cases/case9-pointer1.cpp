#include <iostream>

// 指针作为形参传入
void add(double* p1, double* p2, double* p3, int size)
{
    for(int i=0; i<size;++i)
    {
        p3[i] = p1[i] +p2[i];
    }
}

int main()
{
    // 静态分配的指针
    double p_static[128];

    // 指向已有地址
    int a = 10;
    int * p_var = &a;

    // 动态分配的指针

    double* p_dynamic= new double[1024];
    delete [] p_dynamic;

    double* p_malloc = (double*)malloc( 1024 * sizeof(double));
    free(p_malloc);
    return 0;
}