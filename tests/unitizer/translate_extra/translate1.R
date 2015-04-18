# for translate unitizer tests

expect_equal(fun0(a), 1:10)   # blah blah
expect_is(obj, "something")   # don't translate

# a test for errors

expect_error(stop("hello"))

random_function()
