
#include<stdio.h>
#include<math.h>
#include<stdlib.h>

float  y_n, e, eps, z, t , k, c, h, k1, k2, k3, k4, y_n1, i, sixieme ; 

int main() {

for(z=1;z<=1000;z++) {

sixieme = 0.16666667 ;
e = 1.0 ;
eps = 0.005 ; 
y_n  = 10.1 ; 
t = 0.0 ;
k = 1.2 ;
c = 100.1 ;
h = 0.1 ;
i = 0.0 ; 

while (e > eps) {
  k1   = k * (c - y_n) * (c - y_n) ;
  k2   = k * (c - (y_n + (0.5 * h * k1))) * (c - (y_n + (0.5 * h * k1))) ;
  k3   = k * (c - (y_n + (0.5 * h * k2))) * (c - (y_n + (0.5 * h * k2))) ;
  k4   = k * (c - (y_n + (h * k3))) * (c - (y_n + (h * k3))) ;
  y_n1 = y_n + (sixieme * h * (k1 + (2.0 * k2) + (2.0 * k3) + k4)) ;
  t    = t + 0.1 ; 
  i    = i + 1.0 ;
  y_n  = y_n1 ;  
  e    = e - eps; 
e=(e>=0)?e:(-e);
printf("\nnb iterations = %f e=%f\n",i, e);
}
} 
}
