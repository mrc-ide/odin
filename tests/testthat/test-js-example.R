context("example")

## Full tests here would require selinium I believe
test_that("create example bundle", {
  model <- odin_file("examples/logistic.R")
  path <- odin_js_example(model)
  expect_setequal(dir(path), c("index.html", "odin.js"))
  ## The include process worked:
  js <- readLines(file.path(path, "odin.js"))
  expect_true(any(grepl("odinParameterId", js)))
  ct <- V8::v8()
  expect_true(ct$validate(js))
})
