#include <fenv.h>
#include <math_constants.h>
#include <cuda_runtime.h>
#include <math.h>
#include <stdint.h>

__device__ double ex0() {
	double tmp;
	if (1.0 < 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex1() {
	double tmp;
	if (0.0 < 1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex2() {
	double tmp;
	if (-1.0 < 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex3() {
	double tmp;
	if (0.0 < -1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex4() {
	double tmp;
	if (1.0 < -1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex5() {
	double tmp;
	if (-1.0 < 1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex6() {
	double tmp;
	if (0.0 < 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex7() {
	double tmp;
	if (((double) CUDART_NAN) < ((double) CUDART_NAN)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex8() {
	double tmp;
	if (0.0 < 0.0 && 0.0 < 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex9() {
	double tmp;
	if (1.0 > 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex10() {
	double tmp;
	if (0.0 > 1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex11() {
	double tmp;
	if (-1.0 > 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex12() {
	double tmp;
	if (0.0 > -1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex13() {
	double tmp;
	if (1.0 > -1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex14() {
	double tmp;
	if (-1.0 > 1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex15() {
	double tmp;
	if (0.0 > 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex16() {
	double tmp;
	if (((double) CUDART_NAN) > ((double) CUDART_NAN)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex17() {
	double tmp;
	if (0.0 > 0.0 && 0.0 > 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex18() {
	double tmp;
	if (1.0 <= 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex19() {
	double tmp;
	if (0.0 <= 1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex20() {
	double tmp;
	if (-1.0 <= 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex21() {
	double tmp;
	if (0.0 <= -1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex22() {
	double tmp;
	if (1.0 <= -1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex23() {
	double tmp;
	if (-1.0 <= 1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex24() {
	double tmp;
	if (0.0 <= 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex25() {
	double tmp;
	if (((double) CUDART_NAN) <= ((double) CUDART_NAN)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex26() {
	double tmp;
	if (0.0 <= 0.0 && 0.0 <= 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex27() {
	double tmp;
	if (1.0 >= 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex28() {
	double tmp;
	if (0.0 >= 1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex29() {
	double tmp;
	if (-1.0 >= 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex30() {
	double tmp;
	if (0.0 >= -1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex31() {
	double tmp;
	if (1.0 >= -1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex32() {
	double tmp;
	if (-1.0 >= 1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex33() {
	double tmp;
	if (0.0 >= 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex34() {
	double tmp;
	if (((double) CUDART_NAN) >= ((double) CUDART_NAN)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex35() {
	double tmp;
	if (0.0 >= 0.0 && 0.0 >= 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex36() {
	double tmp;
	if (1.0 == 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex37() {
	double tmp;
	if (0.0 == 1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex38() {
	double tmp;
	if (-1.0 == 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex39() {
	double tmp;
	if (0.0 == -1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex40() {
	double tmp;
	if (1.0 == -1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex41() {
	double tmp;
	if (-1.0 == 1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex42() {
	double tmp;
	if (0.0 == 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex43() {
	double tmp;
	if (((double) CUDART_NAN) == ((double) CUDART_NAN)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex44() {
	double tmp;
	if (0.0 == 0.0 && 0.0 == 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex45() {
	double tmp;
	if (1.0 != 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex46() {
	double tmp;
	if (0.0 != 1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex47() {
	double tmp;
	if (-1.0 != 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex48() {
	double tmp;
	if (0.0 != -1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex49() {
	double tmp;
	if (1.0 != -1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex50() {
	double tmp;
	if (-1.0 != 1.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex51() {
	double tmp;
	if (0.0 != 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex52() {
	double tmp;
	if (((double) CUDART_NAN) != ((double) CUDART_NAN)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex53() {
	double tmp;
	if (0.0 == 0.0 && 0.0 == 0.0) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex54() {
	double tmp;
	if (TRUE && TRUE) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex55() {
	double tmp;
	if (TRUE && FALSE) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex56() {
	double tmp;
	if (FALSE && TRUE) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex57() {
	double tmp;
	if (FALSE && FALSE) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex58() {
	double tmp;
	if (TRUE && TRUE && TRUE) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex59() {
	double tmp;
	if (TRUE && TRUE && FALSE) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex60() {
	double tmp;
	if (TRUE || TRUE) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex61() {
	double tmp;
	if (TRUE || FALSE) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex62() {
	double tmp;
	if (FALSE || TRUE) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex63() {
	double tmp;
	if (FALSE || FALSE) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex64() {
	double tmp;
	if (FALSE || FALSE || FALSE) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex65() {
	double tmp;
	if (FALSE || FALSE || TRUE) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex66() {
	double tmp;
	if (!TRUE) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex67() {
	double tmp;
	if (!FALSE) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex68() {
	double tmp;
	if (isinf(0.0)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex69() {
	double tmp;
	if (isinf(1.0)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex70() {
	double tmp;
	if (isinf(((double) CUDART_INF))) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex71() {
	double tmp;
	if (isinf(((double) CUDART_NAN))) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex72() {
	double tmp;
	if (isnan(0.0)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex73() {
	double tmp;
	if (isnan(1.0)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex74() {
	double tmp;
	if (isnan(((double) CUDART_INF))) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex75() {
	double tmp;
	if (isnan(((double) CUDART_NAN))) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex76() {
	double tmp;
	if (isfinite(0.0)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex77() {
	double tmp;
	if (isfinite(1.0)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex78() {
	double tmp;
	if (isfinite(((double) CUDART_INF))) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex79() {
	double tmp;
	if (isfinite(((double) CUDART_NAN))) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex80() {
	double tmp;
	if (isnormal(0.0)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex81() {
	double tmp;
	if (isnormal(1.0)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex82() {
	double tmp;
	if (isnormal(((double) CUDART_INF))) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex83() {
	double tmp;
	if (isnormal(((double) CUDART_NAN))) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex84() {
	double tmp;
	if (signbit(0.0)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex85() {
	double tmp;
	if (signbit(1.0)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

__device__ double ex86() {
	double tmp;
	if (signbit(-1.0)) {
		tmp = 1.0;
	} else {
		tmp = 0.0;
	}
	return tmp;
}

