context("support")

test_that("interpolateSearch", {
  ctx <- odin_js_support()

  helper <- c(
    "function testSearch(x, i, target) {",
    '  var obj = interpolateAlloc("constant", x, x, true);',
    "  obj.i = i;",
    "  return interpolateSearch(target, obj);",
    "}")
  ctx$eval(helper)

  test_interpolate_search <- function(x, i, target) {
    ctx$call("testSearch", x, i, target)
  }

  x <- 0:9 + 0.5

  ## off the edges
  expect_equal(test_interpolate_search(x, 0, 0), -1L)
  expect_equal(test_interpolate_search(x, 0, 10), 10L)
  expect_equal(test_interpolate_search(x, 5, 0), -1L)
  expect_equal(test_interpolate_search(x, 5, 10), 10L)
  expect_equal(test_interpolate_search(x, 9, 0), -1L)
  expect_equal(test_interpolate_search(x, 9, 10), 10L)

  x <- I(0.5)
  expect_equal(test_interpolate_search(x, 0, 0), -1)
  expect_equal(test_interpolate_search(x, 0, x), 1)
  expect_equal(test_interpolate_search(x, 0, x - 1e-7), 1)
  expect_equal(test_interpolate_search(x, 0, x + 1e-7), 1)
})


## Was a bug in the interpolation routine.
test_that("interpolateSearch, corner case", {
  ctx <- odin_js_support()
  x <- c(
    0.084133944562211, 0.388214586787225, 0.788885934185673, 1.10933879337353,
    1.26720494581815, 1.29417642265985, 1.33293077275123, 1.66824013118334,
    1.67899697952353, 2.13847581530135, 2.33812341864825, 2.38783145563323,
    2.40261439350479, 2.4133948387136, 2.42602639316569, 3.12713656526426,
    3.59934383578104, 3.95284011716578, 4.09458703214518, 4.15191498077006,
    4.31669185581687, 4.50893006728715, 4.8370562989932, 4.88483239173352,
    5.46442874361944, 5.64474886871073, 5.70643784196122, 5.87292617462855,
    5.93556976775707, 6.23232980114077)
  target <- 4.69528083699613
  expect_equal(
    ctx$call("interpolateSearch",
             to_json_max(jsonlite::unbox(target)),
             to_json_max(list(i = jsonlite::unbox(0L),
                              n = jsonlite::unbox(length(x)),
                              x = x))),
    21)
})


## Ported from cinterpolate - I might move the js code there too?
test_that("interpolation", {
  ctx <- odin_js_support()

  helper <- c(
    "function testInterpolate(x, y, xout, type) {",
    '  var obj = interpolateAlloc(type, x, y, false);',
    "  var ret = [];",
    "  for (var i = 0; i < xout.length; ++i) {",
    "    ret.push(interpolateEval(xout[i], obj));",
    "  }",
    "  return ret;",
    "}")
  ctx$eval(helper)

  test <- function(x, y, xout, type) {
    res <- ctx$call("testInterpolate", to_json_max(x), to_json_max(c(y)),
                    to_json_max(xout), type)
    if (storage.mode(res) == "logical" && all(is.na(res))) {
      storage.mode(res) <- "numeric"
    }
    if (is.matrix(y)) {
      matrix(res, c(length(xout), ncol(y)))
    } else {
      drop(res)
    }
  }

  set.seed(1)
  x <- as.numeric(0:5)
  y <- runif(length(x))
  ## This set of points here has the advantage that it:
  ##   a. is out of order so excercises the search function
  ##   b. includes all original time points
  xout <- sample(seq(0, 5, length.out = 101))
  ## Overshoot:
  xout_over <- c(xout, max(x) + 0.5)
  ## Undershoot
  xout_under <- c(xout, min(x) - 0.5)

  rapprox <- list(
    constant = function(x, y, xout) approx(x, y, xout, "constant"),
    linear = function(x, y, xout) approx(x, y, xout, "linear"),
    spline = function(x, y, xout) spline(x, y, xout = xout, method = "natural"))

  for (type in names(rapprox)) {
    ## We're all good except that the constant interpolation is not
    ## quite correct in the case of identical time matches.
    res_c <- test(x, y, xout, type)
    res_r <- rapprox[[type]](x, y, xout)$y

    expect_equal(res_c, res_r, tolerance = 1e-12)

    res_c <- test(x, cbind(y, deparse.level = 0), xout, type)
    expect_equal(dim(res_c), c(length(xout), 1))
    expect_equal(drop(res_c), res_r, tolerance = 1e-12)

    ## This is where we get messy.
    y2 <- cbind(y, y, deparse.level = 0)
    res_c2 <- test(x, y2, xout, type)
    expect_equal(dim(res_c2), c(length(xout), 2))
    expect_equal(res_c2[, 1], res_r, tolerance = 1e-12)
    expect_equal(res_c2[, 2], res_r, tolerance = 1e-12)

    y3 <- cbind(y, y * 2, deparse.level = 0)
    res_c3 <- test(x, y3, xout, type)
    expect_equal(dim(res_c2), c(length(xout), 2))
    expect_equal(res_c3[, 1], res_r, tolerance = 1e-12)
    expect_equal(res_c3[, 2], res_r * 2, tolerance = 1e-12)

    res_c4 <- test(x, y3, xout_over, type)
    i <- length(xout_over)
    if (type == "constant") {
      expect_equal(res_c4[i, ], y3[nrow(y3),])
    } else {
      expect_equal(res_c4[i, ], rep(NA_real_, ncol(y3)))
    }
    res_c5 <- test(x, y3, xout_under, type)
    i <- length(xout_under)
    expect_equal(res_c5[i, ], rep(NA_real_, ncol(y3)))

    res_c6 <- test(x, y3, xout_over[i], type)
    if (type == "constant") {
      expect_equal(drop(res_c6), y3[nrow(y3),])
    } else {
      expect_equal(drop(res_c6), rep(NA_real_, ncol(y3)))
    }

    expect_equal(drop(test(x, y3, xout_under[i], type)),
                 rep(NA_real_, ncol(y3)))
  }
})


test_that("spline calculations are correct", {
  set.seed(1)
  n <- 30
  x <- sort(runif(n, 0, 2 * pi))
  y <- sin(x)

  ctx <- odin_js_support()

  A <- ctx$call("splineCalcA", to_json_max(x))
  B <- ctx$call("splineCalcB", to_json_max(x), to_json_max(rbind(y)))[1, ]
  k <- ctx$call("solveTridiagonal",
                n, to_json_max(A[1, ]), to_json_max(A[2, ]),
                to_json_max(A[3, ]), to_json_max(B))

  i <- 2:n
  j <- 1:(n - 1)
  m <- matrix(0, n, n)
  m[cbind(i, j)] <- A[1, -1]
  diag(m) <- A[2, ]
  m[cbind(j, i)] <- A[3, -n]

  ## Correct calculation of coefficients:
  expect_equal(drop(m %*% k), B)
  expect_equal(drop(k %*% m), B)
  expect_equal(solve(m, B), k)

  helper <- c(
    "function testInterpolate(x, y, xout) {",
    '  var obj = interpolateAlloc("spline", x, y, false);',
    "  var ret = [];",
    "  for (var i = 0; i < xout.length; ++i) {",
    "    ret.push(interpolateEval(xout[i], obj));",
    "  }",
    "  return ret;",
    "}")
  ctx$eval(helper)

  xout <- sample(seq(min(x), max(x), length.out = 5))

  expected <- spline(x, y, xout = xout, method = "natural")$y
  z <- ctx$call("testInterpolate",
                to_json_max(x), to_json_max(y), to_json_max(xout))
  expect_equal(drop(z), expected)
})


test_that("spline calculations are correct for matrix output", {
  set.seed(1)
  n <- 30
  x <- sort(runif(n, 0, 2 * pi))
  y <- rbind(sin(x), cos(x))

  ctx <- odin_js_support()

  A <- ctx$call("splineCalcA", to_json_max(x))
  B <- ctx$call("splineCalcB", to_json_max(x), to_json_max(y))
  k <- ctx$call("splineCalcK", to_json_max(A), to_json_max(B))

  expect_equal(dim(A), c(3, 30))
  expect_equal(dim(B), c(2, 30))
  expect_equal(dim(k), c(2, 30))

  k1 <- ctx$call("solveTridiagonal",
                 n, to_json_max(A[1, ]), to_json_max(A[2, ]),
                 to_json_max(A[3, ]), to_json_max(B[1, ]))
  k2 <- ctx$call("solveTridiagonal",
                 n, to_json_max(A[1, ]), to_json_max(A[2, ]),
                 to_json_max(A[3, ]), to_json_max(B[2, ]))
  expect_equal(k[1, ], k1)
  expect_equal(k[2, ], k2)

  i <- 2:n
  j <- 1:(n - 1)
  m <- matrix(0, n, n)
  m[cbind(i, j)] <- A[1, -1]
  diag(m) <- A[2, ]
  m[cbind(j, i)] <- A[3, -n]

  ## Correct calculation of coefficients:
  expect_equal(k %*% m, B)
  expect_equal(solve(m, t(B)), t(k))

  helper <- c(
    "function testInterpolate(x, y, xout) {",
    '  var obj = interpolateAlloc("spline", x, y, false);',
    "  var ret = [];",
    "  for (var i = 0; i < xout.length; ++i) {",
    "    ret.push(interpolateEval(xout[i], obj));",
    "  }",
    "  return ret;",
    "}")
  ctx$eval(helper)

  xout <- seq(min(x), max(x), length.out = 30)

  expected1 <- spline(x, y[1, ], xout = xout, method = "natural")$y
  expected2 <- spline(x, y[2, ], xout = xout, method = "natural")$y
  z <- ctx$call("testInterpolate",
                to_json_max(x), to_json_max(c(t(y))), to_json_max(xout))
  expect_equal(z[, 1], expected1)
  expect_equal(z[, 2], expected2)
})
