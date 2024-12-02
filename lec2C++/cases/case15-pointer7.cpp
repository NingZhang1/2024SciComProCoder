#include <iostream>

double sum_(int size)
{
    double s=0.0;
    double* ptr = new double[size];
    for(int i=0; i<size; ++i)
    {
        ptr[i] = i*2.0 +3.14;
        s += ptr[i];
    }

    return s;
}

int main()
{
    double d = sum_(100);
    double dd = sum_(200);
    std::cout << "d: "<< d << ", dd:"<< dd << std::endl;
    return 0.0;
}