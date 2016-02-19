#include<stdio.h>
#include<math.h>
#include<stdlib.h>

volatile double   kp, ki, kd, p, i, t, d, dt, invdt, m, c , e, eold, r ; 


int main() {

t    = 0.0 ; 
kp   = 9.4514; 
ki   = 0.69006;
kd   = 2.8454;
invdt = 5.0;
dt   = 0.2;
m    = -5.0 ;
c    = 0.0;
eold = 0.0;
i    = 0.0 ; 
 
while (t < 100.0) {
     e = c - m;
     p = kp * e;
     i = i + ki * dt * e;
     d = kd * invdt * (e - eold);
     r = p + i + d;
     m = m + 0.01 * r;	/* computing measure: the plant */
     eold = e;  
     t = t + dt ; 
 
printf("\n%20.18lf",m);
}
}
