#include <fenv.h>
#include <math_constants.h>
#include <cuda_runtime.h>
#include <math.h>
#include <stdint.h>
__device__ double e = 2.71828182845904523536;

__device__ double ex0(double a, double b) {
	return (((333.75 * pow(b, 6.0)) + (pow(a, 2.0) * (((((11.0 * pow(a, 2.0)) * pow(b, 2.0)) - pow(b, 6.0)) - (121.0 * pow(b, 4.0))) - 2.0))) + (5.5 * pow(b, 8.0))) + (a / (2.0 * b));
}

__device__ double ex1(double a, double b) {
	double b2 = b * b;
	double b4 = b2 * b2;
	double b6 = b4 * b2;
	double b8 = b4 * b4;
	double a2 = a * a;
	double firstexpr = ((((11.0 * a2) * b2) - b6) - (121.0 * b4)) - 2.0;
	return (((333.75 * b6) + (a2 * firstexpr)) + (5.5 * b8)) + (a / (2.0 * b));
}

__device__ double ex2(double a, double b) {
	double b2 = b * b;
	double b4 = b2 * b2;
	double b6 = b4 * b2;
	double b8 = b4 * b4;
	double a2 = a * a;
	double firstexpr = (((11.0 * a2) * b2) - (121.0 * b4)) - 2.0;
	return ((((333.75 - a2) * b6) + (a2 * firstexpr)) + (5.5 * b8)) + (a / (2.0 * b));
}

