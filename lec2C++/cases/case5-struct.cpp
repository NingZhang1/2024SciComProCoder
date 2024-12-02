#include <iostream>
#include <string>

struct person
{
    std::string name;
    int age=0;
    double height;
};

int main()
{
    // 实例化一个struct 对象
    person pa;

    // 初始化对象的值
    pa.name = "Tom";
    pa.age = 24;
    pa.height = 1.80;

    // 打印对象的值
    std::cout << "name: " << pa.name << std::endl;
    std::cout << "age:  " << pa.age  << std::endl;
    std::cout << "height: " << pa.height << std::endl; 
}