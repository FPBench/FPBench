#include <tgmath.h>

float ex0() {
	float x = 0.0f;
	float y = 0.0f;
	float theta = -0.985f;
	float t = 0.0f;
	float j = 0.0f;
	float tmp = 0.0f;
	float sl = 0.0525398163397f;
	float sr = 0.0785398163397f;
	while ((t < 1000.0f)) {
		float next_x = (x + ((((12.34f * sl) + (12.34f * sr)) * 0.5f) * cos((theta + ((((12.34f * sr) - (12.34f * sl)) * 0.1f) * 0.5f)))));
		float next_y = (y + ((((12.34f * sl) + (12.34f * sr)) * 0.5f) * sin((theta + ((((12.34f * sr) - (12.34f * sl)) * 0.1f) * 0.5f)))));
		float next_theta = (theta + (((12.34f * sr) - (12.34f * sl)) * 0.1f));
		float next_t = (t + 1.0f);
		float next_j = ((j == 50.0f) ? 0.0f : (j + 1.0f));
		float next_tmp = ((j == 50.0f) ? sl : tmp);
		float next_sl = ((j == 50.0f) ? sr : sl);
		float next_sr = ((j == 50.0f) ? sl : sr);
		x = next_x;
		y = next_y;
		theta = next_theta;
		t = next_t;
		j = next_j;
		tmp = next_tmp;
		sl = next_sl;
		sr = next_sr;
	}
	return (x + y);
}

double ex1(double m, double kp, double ki, double kd) {
	double i = 0.0;
	double m = m;
	double eold = 0.0;
	double t = 0.0;
	while ((t < 100.0)) {
		double next_i = (i + ((ki * 0.2) * (0.0 - m)));
		double next_m = (m + (0.01 * (((kp * (0.0 - m)) + (i + ((ki * 0.2) * (0.0 - m)))) + ((kd * (1.0 / 0.2)) * ((0.0 - m) - eold)))));
		double next_eold = (0.0 - m);
		double next_t = (t + 0.2);
		i = next_i;
		m = next_m;
		eold = next_eold;
		t = next_t;
	}
	return m;
}

float ex2(float h, float y_n_, float c) {
	float t = 0.0f;
	float i = 0.0f;
	float y_n = y_n_;
	float e = 1.0f;
	while ((e > 0.005f)) {
		float next_t = (t + 0.1f);
		float next_i = (i + 1.0f);
		float next_y_n = (y_n + (((1.0f / 6.0f) * h) * (((((1.2f * (c - y_n)) * (c - y_n)) + (2.0f * ((1.2f * (c - (y_n + ((0.5f * h) * ((1.2f * (c - y_n)) * (c - y_n)))))) * (c - (y_n + ((0.5f * h) * ((1.2f * (c - y_n)) * (c - y_n)))))))) + (2.0f * ((1.2f * (c - (y_n + ((0.5f * h) * ((1.2f * (c - (y_n + ((0.5f * h) * ((1.2f * (c - y_n)) * (c - y_n)))))) * (c - (y_n + ((0.5f * h) * ((1.2f * (c - y_n)) * (c - y_n)))))))))) * (c - (y_n + ((0.5f * h) * ((1.2f * (c - (y_n + ((0.5f * h) * ((1.2f * (c - y_n)) * (c - y_n)))))) * (c - (y_n + ((0.5f * h) * ((1.2f * (c - y_n)) * (c - y_n)))))))))))) + ((1.2f * (c - (y_n + (h * ((1.2f * (c - (y_n + ((0.5f * h) * ((1.2f * (c - (y_n + ((0.5f * h) * ((1.2f * (c - y_n)) * (c - y_n)))))) * (c - (y_n + ((0.5f * h) * ((1.2f * (c - y_n)) * (c - y_n)))))))))) * (c - (y_n + ((0.5f * h) * ((1.2f * (c - (y_n + ((0.5f * h) * ((1.2f * (c - y_n)) * (c - y_n)))))) * (c - (y_n + ((0.5f * h) * ((1.2f * (c - y_n)) * (c - y_n)))))))))))))) * (c - (y_n + (h * ((1.2f * (c - (y_n + ((0.5f * h) * ((1.2f * (c - (y_n + ((0.5f * h) * ((1.2f * (c - y_n)) * (c - y_n)))))) * (c - (y_n + ((0.5f * h) * ((1.2f * (c - y_n)) * (c - y_n)))))))))) * (c - (y_n + ((0.5f * h) * ((1.2f * (c - (y_n + ((0.5f * h) * ((1.2f * (c - y_n)) * (c - y_n)))))) * (c - (y_n + ((0.5f * h) * ((1.2f * (c - y_n)) * (c - y_n)))))))))))))))));
		float next_e = (e - 0.005f);
		t = next_t;
		i = next_i;
		y_n = next_y_n;
		e = next_e;
	}
	return (i + fabs(e));
}

float ex3(float y, float yd) {
	float xc0 = 0.0f;
	float xc1 = 0.0f;
	float i = 0.0f;
	float e = 1.0f;
	while ((e > 0.01f)) {
		float next_xc0 = ((0.499f * xc0) + ((-0.05f * xc1) + (1.0f * ((1.0f < (((y - yd) < -1.0f) ? -1.0f : (y - yd))) ? 1.0f : (((y - yd) < -1.0f) ? -1.0f : (y - yd))))));
		float next_xc1 = ((0.01f * ((0.499f * xc0) + ((-0.05f * xc1) + (1.0f * ((1.0f < (((y - yd) < -1.0f) ? -1.0f : (y - yd))) ? 1.0f : (((y - yd) < -1.0f) ? -1.0f : (y - yd))))))) + ((1.0f * xc1) + (0.0f * ((1.0f < (((y - yd) < -1.0f) ? -1.0f : (y - yd))) ? 1.0f : (((y - yd) < -1.0f) ? -1.0f : (y - yd))))));
		float next_i = (i + 1.0f);
		float next_e = fabs((((1.0f < (((y - yd) < -1.0f) ? -1.0f : (y - yd))) ? 1.0f : (((y - yd) < -1.0f) ? -1.0f : (y - yd))) - ((0.01f * ((0.499f * xc0) + ((-0.05f * xc1) + (1.0f * ((1.0f < (((y - yd) < -1.0f) ? -1.0f : (y - yd))) ? 1.0f : (((y - yd) < -1.0f) ? -1.0f : (y - yd))))))) + ((1.0f * xc1) + (0.0f * ((1.0f < (((y - yd) < -1.0f) ? -1.0f : (y - yd))) ? 1.0f : (((y - yd) < -1.0f) ? -1.0f : (y - yd))))))));
		xc0 = next_xc0;
		xc1 = next_xc1;
		i = next_i;
		e = next_e;
	}
	return xc1;
}

float ex4(float Mf, float A) {
	float x = 0.0f;
	float y = 0.0f;
	float i = 1.0f;
	float u1_im1 = ((400.0f * 10000.0f) + 6400000.0f);
	float u2_im1 = 0.0f;
	float u3_im1 = 0.0f;
	float u4_im1 = (sqrt(((6.67428e-11f * 5.9736e+24f) / ((400.0f * 10000.0f) + 6400000.0f))) / ((400.0f * 10000.0f) + 6400000.0f));
	float w1_im1 = 6400000.0f;
	float w2_im1 = 0.0f;
	float w3_im1 = 0.0f;
	float w4_im1 = (1.1f * (sqrt(((6.67428e-11f * 5.9736e+24f) / 6400000.0f)) / ((400.0f * 10000.0f) + 6400000.0f)));
	float t_im1 = 0.0f;
	float mf_im1 = Mf;
	while ((i < 2000000.0f)) {
		float next_x = (((u2_im1 * 0.1f) + u1_im1) * cos(((u4_im1 * 0.1f) + u3_im1)));
		float next_y = (((u2_im1 * 0.1f) + u1_im1) * sin(((u4_im1 * 0.1f) + u3_im1)));
		float next_i = (i + 1.0f);
		float next_u1_im1 = ((u2_im1 * 0.1f) + u1_im1);
		float next_u2_im1 = (((6.67428e-11f * (5.9736e+24f / (u1_im1 * u1_im1))) * 0.1f) + (((u1_im1 * u4_im1) * u4_im1) * 0.1f));
		float next_u3_im1 = ((u4_im1 * 0.1f) + u3_im1);
		float next_u4_im1 = (((-2.0f * (u2_im1 * (u4_im1 / u1_im1))) * 0.1f) + u4_im1);
		float next_w1_im1 = ((w2_im1 * 0.1f) + w1_im1);
		float next_w2_im1 = (((((6.67428e-11f * (5.9736e+24f / (w1_im1 * w1_im1))) * 0.1f) + (((w1_im1 * w4_im1) * w4_im1) * 0.1f)) + ((mf_im1 > 0.0f) ? (((A * w2_im1) / (Mf - (A * t_im1))) * 0.1f) : 0.0f)) + w2_im1);
		float next_w3_im1 = ((w4_im1 * 0.1f) + w3_im1);
		float next_w4_im1 = (((-2.0f * (w2_im1 * (w4_im1 / w1_im1))) * 0.1f) + (((mf_im1 > 0.0f) ? (A * ((w4_im1 / (Mf - (A * t_im1))) * 0.1f)) : 0.0f) + w4_im1));
		float next_t_im1 = (t_im1 + 0.1f);
		float next_mf_im1 = (mf_im1 - (A * t_im1));
		x = next_x;
		y = next_y;
		i = next_i;
		u1_im1 = next_u1_im1;
		u2_im1 = next_u2_im1;
		u3_im1 = next_u3_im1;
		u4_im1 = next_u4_im1;
		w1_im1 = next_w1_im1;
		w2_im1 = next_w2_im1;
		w3_im1 = next_w3_im1;
		w4_im1 = next_w4_im1;
		t_im1 = next_t_im1;
		mf_im1 = next_mf_im1;
	}
	return (x + y);
}

float ex5(float a11, float a22, float a33, float a44, float b1, float b2, float b3, float b4) {
	float x_n2 = 0.0f;
	float i = 0.0f;
	float e = 1.0f;
	float x1 = 0.0f;
	float x2 = 0.0f;
	float x3 = 0.0f;
	float x4 = 0.0f;
	while ((e > 1e-17f)) {
		float next_x_n2 = ((((b2 / a22) - ((0.3f / a22) * x1)) + ((0.1f / a22) * x3)) - ((0.2f / a22) * x4));
		float next_i = (i + 1.0f);
		float next_e = (((((b4 / a44) + ((0.1f / a44) * x1)) - ((0.2f / a44) * x2)) - ((0.3f / a44) * x3)) - x4);
		float next_x1 = ((((b1 / a11) - ((0.1f / a11) * x2)) - ((0.2f / a11) * x3)) + ((0.3f / a11) * x4));
		float next_x2 = ((((b2 / a22) - ((0.3f / a22) * x1)) + ((0.1f / a22) * x3)) - ((0.2f / a22) * x4));
		float next_x3 = ((((b3 / a33) - ((0.2f / a33) * x1)) + ((0.3f / a33) * x2)) - ((0.1f / a33) * x4));
		float next_x4 = ((((b4 / a44) + ((0.1f / a44) * x1)) - ((0.2f / a44) * x2)) - ((0.3f / a44) * x3));
		x_n2 = next_x_n2;
		i = next_i;
		e = next_e;
		x1 = next_x1;
		x2 = next_x2;
		x3 = next_x3;
		x4 = next_x4;
	}
	return (((i + x_n2) + x2) + e);
}

float ex6(float x0) {
	float x_n = 0.0f;
	float e = 1.0f;
	float x = 0.0f;
	float i = 0.0f;
	while (((e > 0.0005f) && (i < 100000.0f))) {
		float next_x_n = (x - ((((((((((x * x) * x) * x) * x) - ((((10.0f * x) * x) * x) * x)) + (((40.0f * x) * x) * x)) - ((80.0f * x) * x)) + (80.0f * x)) - 32.0f) / ((((((((5.0f * x) * x) * x) * x) - (((40.0f * x) * x) * x)) + ((120.0f * x) * x)) - (160.0f * x)) + 80.0f)));
		float next_e = fabs((x - (x - ((((((((((x * x) * x) * x) * x) - ((((10.0f * x) * x) * x) * x)) + (((40.0f * x) * x) * x)) - ((80.0f * x) * x)) + (80.0f * x)) - 32.0f) / ((((((((5.0f * x) * x) * x) * x) - (((40.0f * x) * x) * x)) + ((120.0f * x) * x)) - (160.0f * x)) + 80.0f)))));
		float next_x = (x - ((((((((((x * x) * x) * x) * x) - ((((10.0f * x) * x) * x) * x)) + (((40.0f * x) * x) * x)) - ((80.0f * x) * x)) + (80.0f * x)) - 32.0f) / ((((((((5.0f * x) * x) * x) * x) - (((40.0f * x) * x) * x)) + ((120.0f * x) * x)) - (160.0f * x)) + 80.0f)));
		float next_i = (i + 1.0f);
		x_n = next_x_n;
		e = next_e;
		x = next_x;
		i = next_i;
	}
	return (((i + x) + x_n) + e);
}

float ex7(float a11, float a12, float a13, float a14, float a21, float a22, float a23, float a24, float a31, float a32, float a33, float a34, float a41, float a42, float a43, float a44, float v1, float v2, float v3, float v4) {
	float vz = 0.0f;
	float i = 0.0f;
	float v1 = v1;
	float v2 = v2;
	float v3 = v3;
	float v4 = v4;
	float e = 1.0f;
	while ((e > 0.0005f)) {
		float next_vz = ((((a31 * v1) + (a32 * v2)) + (a33 * v3)) + (a34 * v4));
		float next_i = (i + 1.0f);
		float next_v1 = (((((a11 * v1) + (a12 * v2)) + (a13 * v3)) + (a14 * v4)) / ((((a41 * v1) + (a42 * v2)) + (a43 * v3)) + (a44 * v4)));
		float next_v2 = (((((a21 * v1) + (a22 * v2)) + (a23 * v3)) + (a24 * v4)) / ((((a41 * v1) + (a42 * v2)) + (a43 * v3)) + (a44 * v4)));
		float next_v3 = (((((a31 * v1) + (a32 * v2)) + (a33 * v3)) + (a34 * v4)) / ((((a41 * v1) + (a42 * v2)) + (a43 * v3)) + (a44 * v4)));
		float next_v4 = 1.0f;
		float next_e = fabs((1.0f - (((((a11 * v1) + (a12 * v2)) + (a13 * v3)) + (a14 * v4)) / ((((a41 * v1) + (a42 * v2)) + (a43 * v3)) + (a44 * v4)))));
		vz = next_vz;
		i = next_i;
		v1 = next_v1;
		v2 = next_v2;
		v3 = next_v3;
		v4 = next_v4;
		e = next_e;
	}
	return (((i + v1) + vz) + e);
}

float ex8(float Q11, float Q12, float Q13, float Q21, float Q22, float Q23, float Q31, float Q32, float Q33) {
	float qj1 = Q31;
	float qj2 = Q32;
	float qj3 = Q33;
	float r1 = 0.0f;
	float r2 = 0.0f;
	float r3 = 0.0f;
	float e = 10.0f;
	float i = 1.0f;
	float rold = sqrt((((Q31 * Q31) + (Q32 * Q32)) + (Q33 * Q33)));
	while ((e > 5e-06f)) {
		float next_qj1 = (qj1 - (((Q11 * (((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3))) + (Q12 * (((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3)))) + (Q13 * (((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3)))));
		float next_qj2 = (qj2 - (((Q21 * (((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3))) + (Q22 * (((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3)))) + (Q23 * (((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3)))));
		float next_qj3 = (qj3 - (((Q31 * (((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3))) + (Q32 * (((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3)))) + (Q33 * (((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3)))));
		float next_r1 = (r1 + (((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3)));
		float next_r2 = (r2 + (((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3)));
		float next_r3 = (r3 + (((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3)));
		float next_e = fabs((1.0f - (sqrt(((((qj1 - (((Q11 * (((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3))) + (Q12 * (((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3)))) + (Q13 * (((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3))))) * (qj1 - (((Q11 * (((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3))) + (Q12 * (((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3)))) + (Q13 * (((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3)))))) + ((qj2 - (((Q21 * (((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3))) + (Q22 * (((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3)))) + (Q23 * (((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3))))) * (qj2 - (((Q21 * (((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3))) + (Q22 * (((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3)))) + (Q23 * (((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3))))))) + ((qj3 - (((Q31 * (((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3))) + (Q32 * (((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3)))) + (Q33 * (((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3))))) * (qj3 - (((Q31 * (((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3))) + (Q32 * (((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3)))) + (Q33 * (((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3)))))))) / rold)));
		float next_i = (i + 1.0f);
		float next_rold = sqrt(((((qj1 - (((Q11 * (((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3))) + (Q12 * (((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3)))) + (Q13 * (((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3))))) * (qj1 - (((Q11 * (((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3))) + (Q12 * (((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3)))) + (Q13 * (((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3)))))) + ((qj2 - (((Q21 * (((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3))) + (Q22 * (((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3)))) + (Q23 * (((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3))))) * (qj2 - (((Q21 * (((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3))) + (Q22 * (((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3)))) + (Q23 * (((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3))))))) + ((qj3 - (((Q31 * (((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3))) + (Q32 * (((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3)))) + (Q33 * (((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3))))) * (qj3 - (((Q31 * (((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3))) + (Q32 * (((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3)))) + (Q33 * (((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3))))))));
		qj1 = next_qj1;
		qj2 = next_qj2;
		qj3 = next_qj3;
		r1 = next_r1;
		r2 = next_r2;
		r3 = next_r3;
		e = next_e;
		i = next_i;
		rold = next_rold;
	}
	return ((((i + qj1) + qj2) + qj3) + e);
}

