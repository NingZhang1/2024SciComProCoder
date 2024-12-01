#include <cstdio>
#include <thread>

void thread_function(void)
{
    printf("Thread ID: %d\n", std::this_thread::get_id());
}
int main(int argc, char** argv)
{
    std::thread thread1(thread_function);
    std::thread thread2(thread_function);
    thread1.join();
    thread2.join();
    
    return 0;
}
