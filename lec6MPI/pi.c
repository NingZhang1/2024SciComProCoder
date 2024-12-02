/*高性能并行课程算例
    PI 计算的串行程序
 V1
JGao 2023 09
*/ 
#include <stdio.h> 
static long num_steps = 1000000000;  // note the max value of int, long int, longlong 
double step; 
void main () 
{   long  int i; 	 // note int or long int  
	  double x, pi, sum = 0.0; 
	  step = 1.0/(double) num_steps; 
	  for (i=1;i<= num_steps; i++){ 
		  x = (i-0.5)*step; 
		  sum = sum + 4.0/(1.0+x*x); 
	  } 
	  pi = step * sum; 
       printf("pi is %2.20f\n",pi);
}
