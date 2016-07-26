interpolate_data * interpolate_alloc(size_t type, size_t n, size_t ny,
                                     double *x, double *y);
void interpolate_free(interpolate_data* obj);
int interpolate_0_run(double x, interpolate_data* obj, double *y);
int interpolate_1_run(double x, interpolate_data* obj, double *y);

// Utility:
int interpolate_search(double target, interpolate_data *obj);
