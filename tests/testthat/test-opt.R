expect_equal(static_eval(quote(1 + 2)), 3)
expect_equal(static_eval(quote(1 + 2 + 3)), 6)

expect_equal(static_eval(quote(a + 1 + 2)), quote(a + 3))
expect_equal(static_eval(quote(1 + a + 2)), quote(a + 3))
expect_equal(static_eval(quote(1 + 2 + a)), quote(a + 3))

expect_equal(static_eval(quote(a + 1 + b + 2 + c + 3)),
             quote(a + b + c + 6))

expect_equal(collect_assoc(quote(a + b + c), quote(`+`)),
             list(quote(a), quote(b), quote(c)))
expect_equal(collect_assoc(quote(a + 1 + b + 2 + c + 3), quote(`+`)),
             list(quote(a), 1, quote(b), 2, quote(c), 3))

expect_equal(static_eval(quote(1 + (2) + 3)), 6)

static_eval(quote(1 + (a + 2) + 3))

static_eval(quote(1 + (a + 2)))

expect_equal(static_eval(quote(1 + (a + 2) + 3)),
             quote(a + 6))

expect_equal(static_eval(quote((a + 2 * 3) + 4 * 5)),
             quote(a + 26))
expect_equal(static_eval(quote((a + 2 * 3) + 4 * b)),
             quote(b * 4 + a + 6))
expect_equal(static_eval(quote((1 + 4) * (b + 3))),
             quote((b + 3) * 5))
