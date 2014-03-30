library(functools)
fun <- function(a=1, bravo, card=25, ..., xar=list("aurochs", 1), z) {}

# Need to add tests:
# - with complex objects? (did I mean in the definition? Or the call??)
(NULL)
# These should be identical to match.call()

body(fun) <- parse(text="{print(match_call()); print(match.call())}")

calls <- c(
  'fun(54, "hello", "wowo", "blergh", 8, 9)',
  'fun(54, "hello", "wowo", "blergh", a=8, z=9)',
  'fun(54, "hello", z="wowo", "blergh", 8, 9)',
  'fun(54, "hello", z="wowo", x="blergh", 8, 9)',
  'fun(54, c="hello", z="wowo", xar=3, 8, 9)'
)
invisible(lapply(calls, function(x){cat("-- New Call --", x, sep="\n"); eval(parse(text=x))}))
