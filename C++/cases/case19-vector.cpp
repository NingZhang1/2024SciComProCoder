#include <iostream>
#include <vector>

void add(int* ptr, int size, int value)
{
    for(int i=0; i<size; ++i)
    {
        ptr[i] += value; 
    }
}

int main()
{
    // 初始化并分配 10 的内存
    std::vector<int> vec(10);
    for(int i=0; i<vec.size();++i)
    {
        vec[i] = i;
    }

    // push_back 往后追加数据
    for(int i=0; i<5; ++i)
    {
        vec.push_back(i);
    }

    // iterator 迭代器循环
    std::vector<int>::iterator iter;
    for(iter=vec.begin(); iter!=vec.end(); iter++)
    {
        std::cout<< "x: " << *iter << std::endl;
    }

    add(vec.data(), vec.size(), 1);

}
