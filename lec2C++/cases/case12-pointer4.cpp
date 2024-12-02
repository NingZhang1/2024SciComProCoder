#include <iostream>

void minus_one(double* ptr, int size)
{
    for(int i=0; i<size; ++i)
    {
        ptr[i] -= 1.0;
    }
}
void add_one(double* ptr, int size)
{
    for(int i=0; i<size; ++i)
    {
        ptr[i] += 1.0;
    }
}

int main()
{
    const int N=100;
    double* ptr = new double[N];

    for(int i=0; i<N; ++i)
    {
        ptr[i] = 0.0;
    }

    minus_one(ptr, 50);
    add_one(ptr+50, 50);

    for(int i=0; i<N; ++i)
    {
        std::cout << "ptr:"<< ptr + i<< ", val:" << ptr[i] << std::endl;
    }

    return 0;
}