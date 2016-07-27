interpolate_data * interpolate_alloc(size_t type, size_t n, size_t ny,
                                     double *x, double *y);
void interpolate_free(interpolate_data* obj);
int interpolate_0_run(double x, interpolate_data* obj, double *y);
int interpolate_1_run(double x, interpolate_data* obj, double *y);
int interpolate_2_run(double x, interpolate_data* obj, double *y);

// Utility:
int interpolate_search(double target, interpolate_data *obj);

// Splines
void spline_knots(size_t n, double *x, double* y, double *k, double **A);
double spline_eval_i(size_t i, double x, double *xs, double *ys, double *ks);
void gauss_solve(size_t n, double **A, double *x);
void spline_knots_A(size_t n, double *x, double *y, double **A);
