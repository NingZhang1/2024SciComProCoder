#include <iostream>
#include <complex>

struct mstruct
{
    int a = 1;
    double b = 2.0;
    std::complex<double> c = {3.0, 4.0};
};

void mstructPtr(mstruct* ms)
{
    std::cout << "ptr: " << ms << std::endl;
    std::cout << "a ptr: "<< &ms->a << ", val:" << ms->a << std::endl;
    std::cout << "b ptr: "<< &ms->b << ", val:" << ms->b << std::endl;
    std::cout << "c ptr: "<< &ms->c << ", val:" << ms->c << std::endl;
}

int main()
{
    mstruct ms;
    mstructPtr(&ms);
    return 0;
}