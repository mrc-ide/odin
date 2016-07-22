interpolate_0_data * interpolate_0_alloc(size_t n, size_t ny,
                                         double *x, double *y);
void interpolate_0_free(interpolate_0_data* obj);
int interpolate_0_run(double x, interpolate_0_data* obj, double *y);
int interpolate_1_run(double x, interpolate_0_data* obj, double *y);

// Utility:
int interpolate_search(double target, size_t i, size_t n, double *x);
