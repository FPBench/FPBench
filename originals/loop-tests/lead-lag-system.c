#include<stdio.h>
#include<math.h>
#include<stdlib.h>

float  i, e, eps, y, yc, u, xc0, xc1, t, Ac00, Ac01, Ac10, Ac11, Bc0, Bc1, Cc0, Cc1, Dc, yd ; 
int z;

int main() {

i = 0.0;   
eps = 0.01 ;
e = 1.0 ;
y = 2.5 ; 
xc0 = 0.0 ;
xc1 = 0.0 ;
Ac00 = 0.4990 ;
Ac01 = -0.0500 ;
Ac10 = 0.0100 ;
Ac11 = 1.0000 ;
Bc0 = 1.0 ;
Bc1 = 0.0 ;
Cc0 = 564.48 ;
Cc1 = 0.0 ;
Dc = -1280.0 ;
yd = 5.0 ;

while (e > eps)  {
  yc = y - yd ;
  if (yc < (-1.0)) { yc = (-1.0) ;}
  if (1.0 < yc) { yc = 1.0 ;}  
  u = Cc0 * xc0 + (Cc1 * xc1 + Dc * yc);
  xc0 = Ac00 * xc0 + (Ac01 * xc1 + Bc0 * yc);
  xc1 = Ac10 * xc0 + (Ac11 * xc1 + Bc1 * yc);
  i = i + 1.0 ;
  e = yc - xc1 ;
  e=(e>=0)?e:(-e); 
  printf("\n %f",xc1);
}
}


