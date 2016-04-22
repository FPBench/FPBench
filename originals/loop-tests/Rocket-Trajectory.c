#include<stdio.h>
#include<math.h>
#include<stdlib.h>


int main() {

float R, G, Mt, Mf, A ;
float dt, T, nombrepas ;
float r0, vr0, teta0, viss, vteta0; 
float rf, vrf, tetaf ;
float vl, vlrad, vtetaf ; 
float x, y, s, c ;
float u1_im1, u2_im1, u3_im1, u4_im1, w1_im1, w2_im1, w3_im1, w4_im1 ; 
float i, mf_im1, t_im1 , t_i; 
float mf_i, u1_i, u2_i, u3_i, u4_i, w1_i, w2_i, w3_i, w4_i ; 


R = 6400.0e3 ;  
G = 6.67428e-11 ;   
Mt= 5.9736e24 ; 
Mf = 150000.0 ;   
A = 140.0 ; 

dt = 0.1 ;
T = 24.0 * 3600.0  ; 
nombrepas = T / dt ; 

r0 = 400.0*10e3 + R ;
vr0 = 0.0 ;
teta0 = 0.0 ; 
viss = sqrt(G*Mt/r0) ;
vteta0 = viss / r0 ;


rf = R ;
vrf = 0.0  ; 
tetaf = 0.0 ;

 
vl = sqrt(G*Mt/R) ;  
vlrad = vl / r0 ;
vtetaf = 1.1 * vlrad ; 


u1_im1 = r0 ;
u2_im1 = vr0 ; 
u3_im1 = teta0 ;
u4_im1 = vteta0 ;
w1_im1 = rf ;
w2_im1 = vrf ;
w3_im1 = tetaf ;
w4_im1 = vtetaf ;
i = 1.0 ;

mf_im1 = Mf ;
t_im1 = 0.0 ; 

while (i < 2000000.0) {

if(mf_im1>0.0){
  t_i = t_im1 + dt ; 
  mf_i = mf_im1 - A * t_im1 ;
  u1_i = u2_im1 * dt + u1_im1 ; 
  u3_i = u4_im1 * dt + u3_im1 ;
  w1_i = (w2_im1 * dt) + w1_im1 ;
  w3_i = (w4_im1 * dt) + w3_im1 ;
  u2_i = ((-G) * (Mt / (u1_im1 * u1_im1)) * dt) + (u1_im1 * u4_im1 * u4_im1 * dt) + u2_im1 ; 
  u4_i = ((-2.0) * (u2_im1 * (u4_im1 / u1_im1)) * dt) + u4_im1 ;
  w2_i = ((-G) * (Mt / (w1_im1 * w1_im1)) * dt) + (w1_im1 * w4_im1 * w4_im1 * dt) + (((A * w2_im1) / (Mf - (A * t_im1))) * dt) + w2_im1;
  w4_i = ((-2.0) * (w2_im1 * (w4_im1 / w1_im1)) * dt) + (A * ((w4_im1 / (Mf - (A * t_im1))) * dt) + w4_im1);
}

else

{
 t_i = t_im1 + dt ; 
  mf_i = mf_im1 - A * t_im1 ;
  u1_i = u2_im1 * dt + u1_im1 ; 
  u3_i = u4_im1 * dt + u3_im1 ;
  w1_i = (w2_im1 * dt) + w1_im1 ;
  w3_i = (w4_im1 * dt) + w3_im1 ;
  u2_i = ((-G) * (Mt / (u1_im1 * u1_im1)) * dt) + (u1_im1 * u4_im1 * u4_im1 * dt) + u2_im1 ; 
  u4_i = ((-2.0) * (u2_im1 * (u4_im1 / u1_im1)) * dt) + u4_im1 ;
  w2_i = ((-G) * (Mt / (w1_im1 * w1_im1)) * dt) + (w1_im1 * w4_im1 * w4_im1 * dt)  + w2_im1;
  w4_i = ((-2.0) * (w2_im1 * (w4_im1 / w1_im1)) * dt) +  w4_im1;
}


c = cos(u3_i);
s = sin(u3_i);
x = u1_i * c ;
y = u1_i * s ; 

if (i> 1800000.0) { printf("%f %f\n", x, y ); };
i = i + 1.0 ;

u1_im1 = u1_i ;
u2_im1 = u2_i ;
u3_im1 = u3_i ;
u4_im1 = u4_i ;
w1_im1 = w1_i ;
w2_im1 = w2_i ;
w3_im1 = w3_i ;
w4_im1 = w4_i ;
t_im1  = t_i ;
mf_im1 = mf_i ; 

}

}


