context("run: %TARGET%: interpolation")

test_that("constant", {
  gen <- odin({
    deriv(y) <- pulse
    initial(y) <- 0
    ##
    pulse <- interpolate(tp, zp, "constant")
    ##
    tp[] <- user()
    zp[] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
    output(p) <- pulse
  })

  ## NOTE: when doing the checks for spanning, the only thing that
  ## matters for constant interpolation is that the *minimum* time
  ## matches.  The minimum match should be that t[1] is not *smaller*
  ## than the smallest interpolation time (equality being OK).
  ##
  ## TODO: I want this to work with tp[1] = 0 but that requires some
  ## tweakery with the interpolation functions;
  ##
  ## TODO: need to check that at least 2 times are provided for each
  ## variable used as an interpolation variable.  Could do that in the
  ## interpolation creation itself but that will give obscure error
  ## messages.  Probably best to do this in the user stage as there's
  ## already some checking there.
  tp <- c(0, 1, 2)
  zp <- c(0, 1, 0)
  expect_error(gen(tp = tp, zp = zp[1:2]), "Expected zp to have length 3")
  expect_error(gen(tp = tp, zp = rep(zp, 2)), "Expected zp to have length 3")

  mod <- gen(tp = tp, zp = zp)

  tt <- seq(0, 3, length.out = 301)
  expect_error(mod$run(tt - 0.1),
               "Integration times do not span interpolation")

  yy <- mod$run(tt)
  zz <- ifelse(tt < 1, 0, ifelse(tt > 2, 1, tt - 1))
  expect_equal(yy[, 2], zz, tolerance = 1e-5)
})


test_that("constant array", {
  gen <- odin({
    deriv(y[]) <- pulse[i]
    initial(y[]) <- 0
    ##
    pulse[] <- interpolate(tp, zp, "constant")
    ##
    tp[] <- user()
    zp[, ] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
    dim(pulse) <- 2
    dim(y) <- 2
  })

  tp <- c(0, 1, 2)
  zp <- cbind(c(0, 1, 0),
              c(0, 2, 0))
  ## Two dimensions to check here:
  expect_error(gen(tp = tp, zp = zp[1:2, ]), "zp to have size 3")
  expect_error(gen(tp = tp, zp = zp[c(1:3, 1:3), ]), "zp to have size 3")
  expect_error(gen(tp = tp, zp = zp[, 1, drop = FALSE]), "zp to have size 2")
  expect_error(gen(tp = tp, zp = zp[, c(1:2, 1)]), "zp to have size 2")

  mod <- gen(tp = tp, zp = zp)

  tt <- seq(0, 3, length.out = 301)
  expect_error(mod$run(tt - 0.1),
               "Integration times do not span interpolation")

  yy <- mod$run(tt)
  zz1 <- ifelse(tt < 1, 0, ifelse(tt > 2, 1, tt - 1))
  zz2 <- ifelse(tt < 1, 0, ifelse(tt > 2, 2, 2 * (tt - 1)))
  expect_equal(yy[, 2], zz1, tolerance = 1e-5)
  expect_equal(yy[, 3], zz2, tolerance = 1e-5)
})


test_that("constant 3d array", {
  gen <- odin({
    deriv(y[, ]) <- pulse[i, j]
    initial(y[, ]) <- 0
    ##
    pulse[, ] <- interpolate(tp, zp, "constant")
    ##
    tp[] <- user()
    zp[, , ] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
    dim(pulse) <- c(2, 2)
    dim(y) <- c(2, 2)
    config(base) <- "ic2"
  })

  ## This is really challenging to even build the 'z' matrix here.
  ## When we go up one more dimension the user is going to enter a
  ## world of pain
  tp <- c(0, 1, 2)
  zp <- array(c(c(0, 1, 0),
                c(0, 2, 0),
                c(0, 3, 0),
                c(0, 4, 0)), c(length(tp), 2, 2))
  stopifnot(isTRUE(all.equal(zp[1, , ], matrix(0, 2, 2))))
  stopifnot(isTRUE(all.equal(zp[2, , ], cbind(1:2, 3:4))))
  stopifnot(isTRUE(all.equal(zp[3, , ], matrix(0, 2, 2))))

  ## Three dimensions to check here:
  expect_error(gen(tp = tp, zp = zp[1:2, , ]), "zp to have size 3")
  expect_error(gen(tp = tp, zp = zp[c(1:3, 1:3), , ]), "zp to have size 3")
  expect_error(gen(tp = tp, zp = zp[, 1, , drop = FALSE]), "zp to have size 2")
  expect_error(gen(tp = tp, zp = zp[, c(1:2, 1), ]), "zp to have size 2")
  expect_error(gen(tp = tp, zp = zp[, , 1, drop = FALSE]), "zp to have size 2")
  expect_error(gen(tp = tp, zp = zp[, , c(1:2, 1)]), "zp to have size 2")

  mod <- gen(tp = tp, zp = zp)

  tt <- seq(0, 3, length.out = 301)
  expect_error(mod$run(tt - 0.1),
               "Integration times do not span interpolation")

  yy <- mod$run(tt)
  cmp <- sapply(1:4, function(i)
    ifelse(tt < 1, 0, ifelse(tt > 2, i, i * (tt - 1))))
  expect_equal(unname(yy[, -1]), cmp, tolerance = 1e-5)
})


test_that("linear", {
  gen <- odin({
    deriv(y) <- pulse
    initial(y) <- 0
    ##
    pulse <- interpolate(tp, zp, "linear")
    ##
    tp[] <- user()
    zp[] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
  })

  tp <- c(0, 1, 2)
  zp <- c(0, 1, 0)
  mod <- gen(tp = tp, zp = zp)

  tt <- seq(0, 2, length.out = 101)
  yy <- mod$run(tt)

  f <- approxfun(tp, zp, "linear")
  target <- function(t, x, .) list(f(t))
  cmp <- deSolve::lsoda(mod$initial(), tt, target, tcrit = 2)
  expect_equal(yy[, 2], cmp[, 2])

  expect_error(mod$run(c(tt, max(tp) + 1)),
               "Integration times do not span interpolation range")

  expect_error(mod$run(tt, tcrit = 3),
               "Interpolation failed as .+ is out of range")
})


test_that("spline", {
  gen <- odin({
    deriv(y) <- pulse
    initial(y) <- 0
    ##
    pulse <- interpolate(tp, zp, "spline")
    ##
    tp[] <- user()
    zp[] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
  })

  tp <- seq(0, pi, length.out = 31)
  zp <- sin(tp)
  mod <- gen(tp = tp, zp = zp)

  tt <- seq(0, pi, length.out = 101)
  yy <- mod$run(tt, tcrit = tt[length(tt)])

  f <- splinefun(tp, zp, "natural")
  target <- function(t, x, .) list(f(t))
  cmp <- deSolve::lsoda(mod$initial(), tt, target, tcrit = tt[length(tt)])
  expect_equal(yy[, 2], cmp[, 2])
})


test_that("interpolation with two variables", {
  for (type in INTERPOLATION_TYPES) {
    gen <- odin_(
      bquote({
        deriv(y) <- pulse1 + pulse2
        initial(y) <- 0

        pulse1 <- interpolate(tp1, zp1, "linear")
        tp1[] <- user()
        zp1[] <- user()
        dim(tp1) <- user()
        dim(zp1) <- length(tp1)

        pulse2 <- interpolate(tp2, zp2, .(type))
        tp2[] <- user()
        zp2[] <- user()
        dim(tp2) <- user()
        dim(zp2) <- length(tp2)
      }, list(type = type)))

    tp1 <- c(-1, 3)
    zp1 <- c(0, 1)
    tp2 <- c(0, 1, 2)
    zp2 <- c(0, 1, 0)
    mod <- gen(tp1 = tp1, zp1 = zp1, tp2 = tp2, zp2 = zp2)

    t1 <- if (type == "constant") max(tp1) else max(tp2)
    expect_equal(r6_private(mod)$interpolate_t$min, 0)
    expect_equal(r6_private(mod)$interpolate_t$max, t1)

    tt <- seq(0, t1, length.out = 101)
    res <- mod$run(tt)

    ## and compare with deSolve:
    pulse1 <- approxfun(tp1, zp1, "linear")
    if (type == "spline") {
      pulse2 <- splinefun(tp2, zp2, "natural")
    } else {
      pulse2 <- approxfun(tp2, zp2, type,
                          rule = if (type == "constant") 2 else 1)
    }
    p <- list(a = pulse1, b = pulse2)
    deriv <- function(t, y, p) {
      list(p[[1]](t) + p[[2]](t))
    }
    cmp <- deSolve::lsoda(0, tt, deriv, p, tcrit = t1)
    expect_equal(res[, 2], cmp[, 2])

    expect_error(mod$run(tt + 1),
                 "Integration times do not span interpolation range")
    expect_error(mod$run(tt - 1),
                 "Integration times do not span interpolation range")
  }
})


test_that("interpolation in a delay", {
  gen <- odin({
    deriv(y) <- ud
    initial(y) <- 0
    deriv(z) <- u
    initial(z) <- 0
    output(u) <- TRUE
    output(ud) <- TRUE

    u <- interpolate(ut, uy, "linear")

    ud <- delay(u, 2)

    ut[] <- user()
    uy[] <- user()
    dim(ut) <- user()
    dim(uy) <- length(ut)
  })

  tt <- seq(0, 10, length.out = 11)
  u <- seq(-10, 20, length.out = 301)
  mod <- gen(ut = u, uy = u)
  yy <- mod$run(tt)

  expect_equal(yy[, "ud"], seq(-2, 8))
  expect_equal(yy[, "u"], seq(0, 10))
})


test_that("interpolation in a delay, with default", {
  gen <- odin({
    deriv(y) <- ud
    initial(y) <- 0
    deriv(z) <- u
    initial(z) <- 0
    output(u) <- TRUE
    output(ud) <- TRUE

    u <- interpolate(ut, uy, "linear")

    ud <- delay(u, 2, 3)

    ut[] <- user()
    uy[] <- user()
    dim(ut) <- user()
    dim(uy) <- length(ut)
  })

  tt <- seq(0, 10, length.out = 11)
  u <- seq(-10, 20, length.out = 301)
  mod <- gen(ut = u, uy = u)
  yy <- mod$run(tt)

  expect_equal(yy[, "ud"], c(3, 3, 3, seq(1, 8)))
  expect_equal(yy[, "u"], seq(0, 10))
})


test_that("critical times", {
  ## this is only done for the R generation so far:
  skip_for_target("c")
  gen <- odin({
    deriv(y) <- pulse1 + pulse2
    initial(y) <- 0

    pulse1 <- interpolate(tp1, zp1, "constant")
    tp1[] <- user()
    zp1[] <- user()
    dim(tp1) <- user()
    dim(zp1) <- length(tp1)

    pulse2 <- interpolate(tp2, zp2, "constant")
    tp2[] <- user()
    zp2[] <- user()
    dim(tp2) <- user()
    dim(zp2) <- length(tp2)
  })

  tp1 <- c(-1, 3)
  zp1 <- c(0, 1)
  tp2 <- c(-1, 1, 2)
  zp2 <- c(0, 1, 0)
  mod <- gen(tp1 = tp1, zp1 = zp1, tp2 = tp2, zp2 = zp2)

  expect_equal(r6_private(mod)$interpolate_t$critical, c(-1, 1, 2, 3))
})


test_that("user sized interpolation, 1d", {
  gen <- odin({
    deriv(y[]) <- pulse[i]
    initial(y[]) <- 0
    ##
    pulse[] <- interpolate(tp, zp, "constant")
    ##
    tp[] <- user()
    zp[, ] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
    dim(pulse) <- user()
    dim(y) <- 2
    output(time) <- t
    output(pulse) <- TRUE
    output(zp) <- TRUE
  })

  tp <- c(0, 1, 2)
  zp <- cbind(c(0, 1, 0),
              c(0, 2, 0))
  mod <- gen(tp = tp, zp = zp)

  tt <- seq(0, 3, length.out = 301)
  yy <- mod$run(tt)
  zz1 <- ifelse(tt < 1, 0, ifelse(tt > 2, 1, tt - 1))
  zz2 <- ifelse(tt < 1, 0, ifelse(tt > 2, 2, 2 * (tt - 1)))
  expect_equal(yy[, 2], zz1, tolerance = 1e-5)
  expect_equal(yy[, 3], zz2, tolerance = 1e-5)
})


test_that("user sized interpolation, 2d", {
  gen <- odin({
    deriv(y[, ]) <- pulse[i, j]
    initial(y[, ]) <- 0
    ##
    pulse[, ] <- interpolate(tp, zp, "constant")
    ##
    tp[] <- user()
    zp[, , ] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
    dim(pulse) <- user()
    dim(y) <- c(2, 2)
  })

  tp <- c(0, 1, 2)
  zp <- array(c(c(0, 1, 0),
                c(0, 2, 0),
                c(0, 3, 0),
                c(0, 4, 0)), c(length(tp), 2, 2))
  mod <- gen(tp = tp, zp = zp)
  dat <- mod$contents()

  tt <- seq(0, 3, length.out = 301)
  yy <- mod$run(tt)
  cmp <- sapply(1:4, function(i)
    ifelse(tt < 1, 0, ifelse(tt > 2, i, i * (tt - 1))))
  expect_equal(unname(yy[, -1]), cmp, tolerance = 1e-5)
})


test_that("double delayed interpolation function", {
  gen <- odin({
    deriv(y) <- ud
    initial(y) <- 0
    deriv(z) <- u
    initial(z) <- 0
    output(u) <- TRUE
    output(ud) <- TRUE
    output(udd) <- TRUE

    u <- interpolate(ut, uy, "constant")

    ud <- delay(u, 2)
    udd <- delay(ud * 0.5, 1)

    ut[] <- user()
    uy[] <- user()
    dim(ut) <- user()
    dim(uy) <- length(ut)
  })

  tt <- seq(0, 10, length.out = 1001)
  u <- seq(-10, 20, length.out = 301)

  ut <- c(-20, 2)
  uy <- c(0, 1)

  mod <- gen(ut = ut, uy = uy)
  yy <- mod$run(tt)

  expect_equal(yy[, "udd"], ifelse(tt < 5, 0, 0.5))

  expect_error(gen(ut = c(0, 2), uy = uy)$run(tt),
               "Interpolation failed as -2.* is out of range")
  expect_error(gen(ut = c(-2, 2), uy = uy)$run(tt),
               "Interpolation failed as -3.* is out of range")
  expect_equal(gen(ut = c(-3, 2), uy = uy)$run(tt), yy)
})
