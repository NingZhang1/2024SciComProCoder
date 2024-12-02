#include <iostream>
#include <map>
#include <string>
int main()
{
    // 创建一个键为字符串， 值为整数的map
    std::map<std::string, int> myMap;
    // 添加键值对
    myMap["A"] = 25;myMap["B"] = 30;myMap["C"] = 35;
    std::cout <<"A is: " << myMap["A"] << std::endl;
    // 修改Map
    myMap["C"] = 36;
    // 遍历Map
    for(auto& x: myMap)
    {
        std::cout << x.first << " is " << x.second << std::endl;
    }
    // 查找键
    auto iter= myMap.find("B");
    if (iter != myMap.end())
    {
        std::cout << "Find " << iter->first << " is "<< iter->second << std::endl;
    }
    else
    {
        std::cout << "Cannot Find " << iter->first <<std::endl;
    }
    // 删除键
    myMap.erase("B");
    for(auto& x: myMap)
    {
        std::cout << x.first << " is " << x.second << std::endl;
    }
}