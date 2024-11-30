#include <iostream>
#include <string>
#include <vector>

void copyModify(const std::string& str1, std::string& str2)
{
    str2 = str1;
    str2[0] = 'A';
}
void add(const std::string& str1, const std::string& str2, std::string& str3)
{
    str3 = str1 +str2;
}
/// @brief 删除字符串前后出现的flag
std::string trim(std::string s, std::string flag)
{
    if (s.empty())
    {
        return s;
    }
    s.erase(0, s.find_first_not_of(flag));
    s.erase(s.find_last_not_of(flag) + 1);
    return s;
}


/// @brief 字符串以flag分割处理
std::vector<std::string> split(const std::string& string, const std::string flag)
{
    std::string::size_type pos;
    std::vector<std::string> result;
    std::string str = string + flag;//扩展字符串以方便操作
    size_t size = str.size();

    for (size_t i = 0; i < size; i++)
    {
        pos = str.find(flag, i);
        if (pos < size)
        {
            std::string s = str.substr(i, pos - i);
            if (trim(s, flag) == "")
            {
                continue;
            }
            result.push_back(trim(s, flag));
            i = pos + flag.size() - 1;
        }
    }
    return result;
}


int main()
{
    std::string str = " 12 34 56 7 ";
    std::string str2;
    // copy string
    copyModify(str, str2);
    std::cout << "str2: " << str << std::endl;
    std::cout << "str2: " << str2 << std::endl;

    // add string
    std::string str3 = str2 + "122 123 124";
    auto str4 = trim(str3, " ");
    std::cout << "str3: " << str3 << std::endl;
    auto vec_str = split(str4, " ");

    int a = atoi(vec_str[1].c_str());
    std::cout << "a: " << a << std::endl;
    return 0;
}
