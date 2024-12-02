#include <iostream>
class Aninmal
{
public:
    virtual void makeSound()
    {
        std::cout << "Animal makes a sound!" << std::endl;
    }
};
class Dog: public Aninmal
{
public:
    virtual void makeSound()
    {
        std::cout << "Dog barks!" << std::endl;
    }
};
class Cat: public Aninmal
{
public:
    virtual void makeSound()
    {
        std::cout << "Cat meows!" << std::endl;
    }
};

void fun(Aninmal& animal)
{
    animal.makeSound();
}

int main()
{
    Dog dog;
    Cat cat;
    fun(dog);
    fun(cat);
}