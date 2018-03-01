context("graph")

## This can't actually be tested apart from "does it fail" because we
## can't see what the graph looks like...
test_that("generate graph", {
  lorenz <- odin::odin({
    ## Derivatives
    deriv(y1) <- sigma * (y2 - y1)
    deriv(y2) <- R * y1 - y2 - y1 * y3
    deriv(y3) <- -b * y3 + y1 * y2

    ## Initial conditions
    initial(y1) <- 10.0
    initial(y2) <- 1.0
    initial(y3) <- 1.0

    ## parameters
    sigma <- 10.0
    R     <- 28.0
    b     <-  8.0 / 3.0

    config(base) <- "lorenz"
  })

  graph <- plot(lorenz)
  expect_is(graph, "visNetwork")
})


test_that("discrete", {
  gen <- odin::odin({
    initial(x) <- 1
    update(x) <- x + 1
  }, verbose = TEST_VERBOSE)
  mod <- gen()

  graph <- plot(mod)
  expect_is(graph, "visNetwork")
})
