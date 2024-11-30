#include <iostream>

class Atom
{
public:
    // 构造函数
    Atom(){};
    Atom(double pos[3], double m, std::string sname)
    {
        position[0] = pos[0];
        position[1] = pos[1];
        position[2] = pos[2];

        mass = m;
        name = sname;
    };
    // 析构函数
    ~Atom(){};

    // 成员函数
    void setPos(double pos[3])
    {        
        position[0] = pos[0];
        position[1] = pos[1];
        position[2] = pos[2];
    };
    void setMass(double m)
    {
        mass = m;
    };
    void move(const double dpos[3])
    {
        position[0] += dpos[0];
        position[1] += dpos[1];
        position[2] += dpos[2];
    };
    void print()
    {
        std::cout << "Atom name: "<< name
                  << ", mass: " << mass 
                  << ", pos:" << position[0]
                  << ", " << position[1]
                  << ", " << position[2] << std::endl;
    }

private:
    // 成员变量
    std::string name="";
    double position[3];
    double mass;
};
class HAtom: public Atom
{
public:
    HAtom(double pos[3]):
    Atom(pos, 1.000, "H")
    {
    };
    ~HAtom(){};
private:
    int color;
};

int main()
{
    double p1[3] = {1.0, 2.0, 3.0};
    double p2[3] = {-1.0, -2.0, -3.0};
    double m1 = 1.011;
    double m2 = 2.001;
    // 创建类实例
    HAtom atom1(p1);
    Atom atom2;

    // 调用类方法
    atom2.setPos(p2);
    atom2.setMass(m2);

    atom1.move(p1);
    atom2.move(p1);

    atom1.print();
    atom2.print();

    // 离开变量的作用域，会调用类析构函数
}