float f(float _c___12_34_, float _j___0_) {
	float x = 0.0f;
	float y = 0.0f;
	float theta = -0.985f;
	float t = 0.0f;
	float sr = 0.0785398163397f;
	float sl = 0.0525398163397f;
	float j = 0.0f;
	while ((t < 1000.0f)) {
		float next_x = (x + ((((c * sl) + (c * sr)) * 0.5f) * cos((theta + ((((c * sr) - (c * sl)) * inv_l) * 0.5f)))));
		float next_y = (y + ((((c * sl) + (c * sr)) * 0.5f) * sin((theta + ((((c * sr) - (c * sl)) * inv_l) * 0.5f)))));
		float next_theta = (theta + (((c * sr) - (c * sl)) * inv_l));
		float next_t = (t + 1.0f);
		float next_sr = ((j == 50.0f) ? sl : sr);
		float next_sl = ((j == 50.0f) ? sr : sl);
		float next_j = ((j == 50.0f) ? 0.0f : (j + 1.0f));
		x = next_x;
		y = next_y;
		theta = next_theta;
		t = next_t;
		sr = next_sr;
		sl = next_sl;
		j = next_j;
	}
	return (x + y);
}

double f(double c, double dt, double kd, double ki, double kp) {
	double i = 0.0;
	double m = -5.0;
	double eold = 0.0;
	double t = 0.0;
	while ((t < 100.0)) {
		double next_i = (i + ((ki * dt) * (c - m)));
		double next_m = (m + (0.01 * (((kp * (c - m)) + (i + ((ki * dt) * (c - m)))) + ((kd * (1.0 / dt)) * ((c - m) - eold)))));
		double next_eold = (c - m);
		double next_t = (t + dt);
		i = next_i;
		m = next_m;
		eold = next_eold;
		t = next_t;
	}
	return m;
}

float f(float h, float c, float k, float eps) {
	float t = 0.0f;
	float i = 0.0f;
	float y_n = 10.1f;
	float e = 1.0f;
	while ((e > eps)) {
		float next_t = (t + 0.1f);
		float next_i = (i + 1.0f);
		float next_y_n = (y_n + (((1.0f / 6.0f) * h) * (((((k * (c - y_n)) * (c - y_n)) + (2.0f * ((k * (c - (y_n + ((0.5f * h) * ((k * (c - y_n)) * (c - y_n)))))) * (c - (y_n + ((0.5f * h) * ((k * (c - y_n)) * (c - y_n)))))))) + (2.0f * ((k * (c - (y_n + ((0.5f * h) * ((k * (c - (y_n + ((0.5f * h) * ((k * (c - y_n)) * (c - y_n)))))) * (c - (y_n + ((0.5f * h) * ((k * (c - y_n)) * (c - y_n)))))))))) * (c - (y_n + ((0.5f * h) * ((k * (c - (y_n + ((0.5f * h) * ((k * (c - y_n)) * (c - y_n)))))) * (c - (y_n + ((0.5f * h) * ((k * (c - y_n)) * (c - y_n)))))))))))) + ((k * (c - (y_n + (h * ((k * (c - (y_n + ((0.5f * h) * ((k * (c - (y_n + ((0.5f * h) * ((k * (c - y_n)) * (c - y_n)))))) * (c - (y_n + ((0.5f * h) * ((k * (c - y_n)) * (c - y_n)))))))))) * (c - (y_n + ((0.5f * h) * ((k * (c - (y_n + ((0.5f * h) * ((k * (c - y_n)) * (c - y_n)))))) * (c - (y_n + ((0.5f * h) * ((k * (c - y_n)) * (c - y_n)))))))))))))) * (c - (y_n + (h * ((k * (c - (y_n + ((0.5f * h) * ((k * (c - (y_n + ((0.5f * h) * ((k * (c - y_n)) * (c - y_n)))))) * (c - (y_n + ((0.5f * h) * ((k * (c - y_n)) * (c - y_n)))))))))) * (c - (y_n + ((0.5f * h) * ((k * (c - (y_n + ((0.5f * h) * ((k * (c - y_n)) * (c - y_n)))))) * (c - (y_n + ((0.5f * h) * ((k * (c - y_n)) * (c - y_n)))))))))))))))));
		float next_e = (e - eps);
		t = next_t;
		i = next_i;
		y_n = next_y_n;
		e = next_e;
	}
	return (i + fabs(e));
}

float f(float yd, float Dc, float Cc1, float Cc0, float Bc1, float Bc0, float Ac11, float Ac10, float Ac01, float Ac00, float y, float eps) {
	float xc0 = 0.0f;
	float xc1 = 0.0f;
	float i = 0.0f;
	float e = 1.0f;
	while ((e > eps)) {
		float next_xc0 = ((Ac00 * xc0) + ((Ac01 * xc1) + (Bc0 * ((1.0f < (((y - yd) < -1.0f) ? -1.0f : (y - yd))) ? 1.0f : (((y - yd) < -1.0f) ? -1.0f : (y - yd))))));
		float next_xc1 = ((Ac10 * ((Ac00 * xc0) + ((Ac01 * xc1) + (Bc0 * ((1.0f < (((y - yd) < -1.0f) ? -1.0f : (y - yd))) ? 1.0f : (((y - yd) < -1.0f) ? -1.0f : (y - yd))))))) + ((Ac11 * xc1) + (Bc1 * ((1.0f < (((y - yd) < -1.0f) ? -1.0f : (y - yd))) ? 1.0f : (((y - yd) < -1.0f) ? -1.0f : (y - yd))))));
		float next_i = (i + 1.0f);
		float next_e = fabs((((1.0f < (((y - yd) < -1.0f) ? -1.0f : (y - yd))) ? 1.0f : (((y - yd) < -1.0f) ? -1.0f : (y - yd))) - ((Ac10 * ((Ac00 * xc0) + ((Ac01 * xc1) + (Bc0 * ((1.0f < (((y - yd) < -1.0f) ? -1.0f : (y - yd))) ? 1.0f : (((y - yd) < -1.0f) ? -1.0f : (y - yd))))))) + ((Ac11 * xc1) + (Bc1 * ((1.0f < (((y - yd) < -1.0f) ? -1.0f : (y - yd))) ? 1.0f : (((y - yd) < -1.0f) ? -1.0f : (y - yd))))))));
		xc0 = next_xc0;
		xc1 = next_xc1;
		i = next_i;
		e = next_e;
	}
	return xc1;
}

float f(float tetaf, float vrf, float teta0, float vr0, float dt, float A, float Mf, float Mt, float G, float R) {
	float x = 0.0f;
	float y = 0.0f;
	float i = 1.0f;
	float u1_im1 = ((400.0f * 10000.0f) + R);
	float u2_im1 = vr0;
	float u3_im1 = teta0;
	float u4_im1 = (sqrt(((G * Mt) / ((400.0f * 10000.0f) + R))) / ((400.0f * 10000.0f) + R));
	float w1_im1 = R;
	float w2_im1 = vrf;
	float w3_im1 = tetaf;
	float w4_im1 = (1.1f * (sqrt(((G * Mt) / R)) / ((400.0f * 10000.0f) + R)));
	float t_im1 = 0.0f;
	float mf_im1 = Mf;
	while ((i < 2000000.0f)) {
		float next_x = (((u2_im1 * dt) + u1_im1) * cos(((u4_im1 * dt) + u3_im1)));
		float next_y = (((u2_im1 * dt) + u1_im1) * sin(((u4_im1 * dt) + u3_im1)));
		float next_i = (i + 1.0f);
		float next_u1_im1 = ((u2_im1 * dt) + u1_im1);
		float next_u2_im1 = (((G * (Mt / (u1_im1 * u1_im1))) * dt) + (((u1_im1 * u4_im1) * u4_im1) * dt));
		float next_u3_im1 = ((u4_im1 * dt) + u3_im1);
		float next_u4_im1 = (((-2.0f * (u2_im1 * (u4_im1 / u1_im1))) * dt) + u4_im1);
		float next_w1_im1 = ((w2_im1 * dt) + w1_im1);
		float next_w2_im1 = (((((G * (Mt / (w1_im1 * w1_im1))) * dt) + (((w1_im1 * w4_im1) * w4_im1) * dt)) + ((mf_im1 > 0.0f) ? (((A * w2_im1) / (Mf - (A * t_im1))) * dt) : 0.0f)) + w2_im1);
		float next_w3_im1 = ((w4_im1 * dt) + w3_im1);
		float next_w4_im1 = (((-2.0f * (w2_im1 * (w4_im1 / w1_im1))) * dt) + (((mf_im1 > 0.0f) ? (A * ((w4_im1 / (Mf - (A * t_im1))) * dt)) : 0.0f) + w4_im1));
		float next_t_im1 = (t_im1 + dt);
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

float f(float b3, float b1, float a44, float a33, float a22, float a11, float eps) {
	float x_n2 = 0.0f;
	float i = 0.0f;
	float e = 1.0f;
	float x1 = 0.0f;
	float x2 = 0.0f;
	float x3 = 0.0f;
	float x4 = 0.0f;
	while ((e > eps)) {
		float next_x_n2 = (((((1.0f / 3.0f) / a22) - ((0.3f / a22) * x1)) + ((0.1f / a22) * x3)) - ((0.2f / a22) * x4));
		float next_i = (i + 1.0f);
		float next_e = ((((((1.0f / 5.0f) / a44) + ((0.1f / a44) * x1)) - ((0.2f / a44) * x2)) - ((0.3f / a44) * x3)) - x4);
		float next_x1 = ((((b1 / a11) - ((0.1f / a11) * x2)) - ((0.2f / a11) * x3)) + ((0.3f / a11) * x4));
		float next_x2 = (((((1.0f / 3.0f) / a22) - ((0.3f / a22) * x1)) + ((0.1f / a22) * x3)) - ((0.2f / a22) * x4));
		float next_x3 = ((((b3 / a33) - ((0.2f / a33) * x1)) + ((0.3f / a33) * x2)) - ((0.1f / a33) * x4));
		float next_x4 = (((((1.0f / 5.0f) / a44) + ((0.1f / a44) * x1)) - ((0.2f / a44) * x2)) - ((0.3f / a44) * x3));
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

float f(float x0, float eps) {
	float x_n = 0.0f;
	float e = 1.0f;
	float x = 0.0f;
	float i = 0.0f;
	while (((e > eps) && (i < 100000.0f))) {
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

float f(float v4, float a43, float a42, float a41, float a34, float a32, float a31, float a24, float a23, float a21, float a14, float a13, float a12, float d, float eps) {
	float i = 0.0f;
	float v1 = 0.0f;
	float v2 = 0.0f;
	float v3 = 0.0f;
	float e = 1.0f;
	while ((e > eps)) {
		float next_i = (i + 1.0f);
		float next_v1 = (((((d * v1) + (a12 * v2)) + (a13 * v3)) + (a14 * v4)) / ((((a41 * v1) + (a42 * v2)) + (a43 * v3)) + (d * v4)));
		float next_v2 = (((((a21 * v1) + (d * v2)) + (a23 * v3)) + (a24 * v4)) / ((((a41 * v1) + (a42 * v2)) + (a43 * v3)) + (d * v4)));
		float next_v3 = (((((a31 * v1) + (a32 * v2)) + (d * v3)) + (a34 * v4)) / ((((a41 * v1) + (a42 * v2)) + (a43 * v3)) + (d * v4)));
		float next_e = fabs((1.0f - (((((d * v1) + (a12 * v2)) + (a13 * v3)) + (a14 * v4)) / ((((a41 * v1) + (a42 * v2)) + (a43 * v3)) + (d * v4)))));
		i = next_i;
		v1 = next_v1;
		v2 = next_v2;
		v3 = next_v3;
		e = next_e;
	}
	return ((i + v1) + e);
}

float f(float eps, float Q23, float Q21, float Q13, float Q12) {
	float qj1 = (1.0f / 2592.0f);
	float qj2 = (1.0f / 2601.0f);
	float qj3 = (1.0f / 2583.0f);
	float r1 = 0.0f;
	float r2 = 0.0f;
	float r3 = 0.0f;
	float e = 10.0f;
	float i = 1.0f;
	float rold = sqrt(((((1.0f / 2592.0f) * (1.0f / 2592.0f)) + ((1.0f / 2601.0f) * (1.0f / 2601.0f))) + ((1.0f / 2583.0f) * (1.0f / 2583.0f))));
	while ((e > eps)) {
		float next_qj1 = (qj1 - ((((1.0f / 63.0f) * ((((1.0f / 63.0f) * qj1) + (Q21 * qj2)) + ((1.0f / 2592.0f) * qj3))) + (Q12 * (((Q12 * qj1) + ((1.0f / 225.0f) * qj2)) + ((1.0f / 2601.0f) * qj3)))) + (Q13 * (((Q13 * qj1) + (Q23 * qj2)) + ((1.0f / 2583.0f) * qj3)))));
		float next_qj2 = (qj2 - (((Q21 * ((((1.0f / 63.0f) * qj1) + (Q21 * qj2)) + ((1.0f / 2592.0f) * qj3))) + ((1.0f / 225.0f) * (((Q12 * qj1) + ((1.0f / 225.0f) * qj2)) + ((1.0f / 2601.0f) * qj3)))) + (Q23 * (((Q13 * qj1) + (Q23 * qj2)) + ((1.0f / 2583.0f) * qj3)))));
		float next_qj3 = (qj3 - ((((1.0f / 2592.0f) * ((((1.0f / 63.0f) * qj1) + (Q21 * qj2)) + ((1.0f / 2592.0f) * qj3))) + ((1.0f / 2601.0f) * (((Q12 * qj1) + ((1.0f / 225.0f) * qj2)) + ((1.0f / 2601.0f) * qj3)))) + ((1.0f / 2583.0f) * (((Q13 * qj1) + (Q23 * qj2)) + ((1.0f / 2583.0f) * qj3)))));
		float next_r1 = (r1 + ((((1.0f / 63.0f) * qj1) + (Q21 * qj2)) + ((1.0f / 2592.0f) * qj3)));
		float next_r2 = (r2 + (((Q12 * qj1) + ((1.0f / 225.0f) * qj2)) + ((1.0f / 2601.0f) * qj3)));
		float next_r3 = (r3 + (((Q13 * qj1) + (Q23 * qj2)) + ((1.0f / 2583.0f) * qj3)));
		float next_e = fabs((1.0f - (sqrt(((((qj1 - ((((1.0f / 63.0f) * ((((1.0f / 63.0f) * qj1) + (Q21 * qj2)) + ((1.0f / 2592.0f) * qj3))) + (Q12 * (((Q12 * qj1) + ((1.0f / 225.0f) * qj2)) + ((1.0f / 2601.0f) * qj3)))) + (Q13 * (((Q13 * qj1) + (Q23 * qj2)) + ((1.0f / 2583.0f) * qj3))))) * (qj1 - ((((1.0f / 63.0f) * ((((1.0f / 63.0f) * qj1) + (Q21 * qj2)) + ((1.0f / 2592.0f) * qj3))) + (Q12 * (((Q12 * qj1) + ((1.0f / 225.0f) * qj2)) + ((1.0f / 2601.0f) * qj3)))) + (Q13 * (((Q13 * qj1) + (Q23 * qj2)) + ((1.0f / 2583.0f) * qj3)))))) + ((qj2 - (((Q21 * ((((1.0f / 63.0f) * qj1) + (Q21 * qj2)) + ((1.0f / 2592.0f) * qj3))) + ((1.0f / 225.0f) * (((Q12 * qj1) + ((1.0f / 225.0f) * qj2)) + ((1.0f / 2601.0f) * qj3)))) + (Q23 * (((Q13 * qj1) + (Q23 * qj2)) + ((1.0f / 2583.0f) * qj3))))) * (qj2 - (((Q21 * ((((1.0f / 63.0f) * qj1) + (Q21 * qj2)) + ((1.0f / 2592.0f) * qj3))) + ((1.0f / 225.0f) * (((Q12 * qj1) + ((1.0f / 225.0f) * qj2)) + ((1.0f / 2601.0f) * qj3)))) + (Q23 * (((Q13 * qj1) + (Q23 * qj2)) + ((1.0f / 2583.0f) * qj3))))))) + ((qj3 - ((((1.0f / 2592.0f) * ((((1.0f / 63.0f) * qj1) + (Q21 * qj2)) + ((1.0f / 2592.0f) * qj3))) + ((1.0f / 2601.0f) * (((Q12 * qj1) + ((1.0f / 225.0f) * qj2)) + ((1.0f / 2601.0f) * qj3)))) + ((1.0f / 2583.0f) * (((Q13 * qj1) + (Q23 * qj2)) + ((1.0f / 2583.0f) * qj3))))) * (qj3 - ((((1.0f / 2592.0f) * ((((1.0f / 63.0f) * qj1) + (Q21 * qj2)) + ((1.0f / 2592.0f) * qj3))) + ((1.0f / 2601.0f) * (((Q12 * qj1) + ((1.0f / 225.0f) * qj2)) + ((1.0f / 2601.0f) * qj3)))) + ((1.0f / 2583.0f) * (((Q13 * qj1) + (Q23 * qj2)) + ((1.0f / 2583.0f) * qj3)))))))) / rold)));
		float next_i = (i + 1.0f);
		float next_rold = sqrt(((((qj1 - ((((1.0f / 63.0f) * ((((1.0f / 63.0f) * qj1) + (Q21 * qj2)) + ((1.0f / 2592.0f) * qj3))) + (Q12 * (((Q12 * qj1) + ((1.0f / 225.0f) * qj2)) + ((1.0f / 2601.0f) * qj3)))) + (Q13 * (((Q13 * qj1) + (Q23 * qj2)) + ((1.0f / 2583.0f) * qj3))))) * (qj1 - ((((1.0f / 63.0f) * ((((1.0f / 63.0f) * qj1) + (Q21 * qj2)) + ((1.0f / 2592.0f) * qj3))) + (Q12 * (((Q12 * qj1) + ((1.0f / 225.0f) * qj2)) + ((1.0f / 2601.0f) * qj3)))) + (Q13 * (((Q13 * qj1) + (Q23 * qj2)) + ((1.0f / 2583.0f) * qj3)))))) + ((qj2 - (((Q21 * ((((1.0f / 63.0f) * qj1) + (Q21 * qj2)) + ((1.0f / 2592.0f) * qj3))) + ((1.0f / 225.0f) * (((Q12 * qj1) + ((1.0f / 225.0f) * qj2)) + ((1.0f / 2601.0f) * qj3)))) + (Q23 * (((Q13 * qj1) + (Q23 * qj2)) + ((1.0f / 2583.0f) * qj3))))) * (qj2 - (((Q21 * ((((1.0f / 63.0f) * qj1) + (Q21 * qj2)) + ((1.0f / 2592.0f) * qj3))) + ((1.0f / 225.0f) * (((Q12 * qj1) + ((1.0f / 225.0f) * qj2)) + ((1.0f / 2601.0f) * qj3)))) + (Q23 * (((Q13 * qj1) + (Q23 * qj2)) + ((1.0f / 2583.0f) * qj3))))))) + ((qj3 - ((((1.0f / 2592.0f) * ((((1.0f / 63.0f) * qj1) + (Q21 * qj2)) + ((1.0f / 2592.0f) * qj3))) + ((1.0f / 2601.0f) * (((Q12 * qj1) + ((1.0f / 225.0f) * qj2)) + ((1.0f / 2601.0f) * qj3)))) + ((1.0f / 2583.0f) * (((Q13 * qj1) + (Q23 * qj2)) + ((1.0f / 2583.0f) * qj3))))) * (qj3 - ((((1.0f / 2592.0f) * ((((1.0f / 63.0f) * qj1) + (Q21 * qj2)) + ((1.0f / 2592.0f) * qj3))) + ((1.0f / 2601.0f) * (((Q12 * qj1) + ((1.0f / 225.0f) * qj2)) + ((1.0f / 2601.0f) * qj3)))) + ((1.0f / 2583.0f) * (((Q13 * qj1) + (Q23 * qj2)) + ((1.0f / 2583.0f) * qj3))))))));
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

