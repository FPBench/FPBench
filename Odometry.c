#include<stdio.h>
#include<math.h>
#include<stdlib.h>

volatile float  sr, sl, theta, x, y, inv_l, c, delta_dl, delta_dr, delta_d,  delta_theta, arg, sini, cosi, tmp ;
int t, j; 
int main() {

cosi  = 0.0 ;
sini = 0.0 ; 
arg  = 0.0 ;
delta_d  = 0.0 ;
delta_dl  = 0.0 ;
delta_dr  = 0.0 ;
delta_theta  = 0.0 ;
sr = 0.0785398163397;
sl = 0.0525398163397;
theta = -0.985 ;
t = 0;
x = 0.0 ;
y = 0.0 ; 
inv_l = 0.1;
c = 12.34;
j = 0 ; 

while (t < 1000) {
  delta_dl = c * sl;
  delta_dr = c * sr;
  delta_d = (delta_dl + delta_dr) * 0.5;
  delta_theta = (delta_dr - delta_dl) * inv_l;
  arg = theta + (delta_theta * 0.5);
  cosi = 1.0 -  (arg * arg * 0.5) + ((arg * arg * arg * arg) * 0.0416666666) ; 
  x = x + (delta_d * cosi);
  sini = arg - ((arg * arg * arg) * 0.1666666666) + ((arg * arg * arg * arg * arg) * 0.008333333) ;
  y = y + (delta_d * sini);
  theta = theta + delta_theta  ; 
  t = t + 1;
  if (j == 50) 
  {j = 0; tmp = sl; sl = sr; sr = tmp;  } 
  else 
    {j  = j +1; };  
  printf("%20.18f " "  " "%20.18f\n",x,y);
}
}
