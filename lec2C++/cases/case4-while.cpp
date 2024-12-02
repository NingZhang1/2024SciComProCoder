#include <iostream>

int main()
{
    int a = 11;
    // while 循环
    while(a<=20)
    {
        std::cout << "a = " << a << std::endl;
        a++;
        if(a%5==0)
        {
            std::cout << "break, a = " << a << std::endl;
            break; //跳出循环
        }
    }
    return 0;
}