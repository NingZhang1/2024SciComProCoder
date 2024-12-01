/*条件编译示例
根据需要分别编译英文版和中文版
by jgao
*/
#include <stdio.h>
int main (int argc, char *argv[]) 
{
printf("example of conditional compilation \n\n");
#ifdef english
 printf(" conditional is englsih \n\n");
 printf (" Wlcome to HPC Class! \n" );
#else
printf(" conditional is 中文 \n\n");
printf ("欢迎加入高性能计算课程!\n" );
#endif
return 1;
}