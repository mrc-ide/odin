// A little example function that uses a vector.
double f(size_t i, double *x) {
  double n = 0;
  for (size_t j = 0; j < i; ++j) {
    n += x[j];
  }
  return n;
}
