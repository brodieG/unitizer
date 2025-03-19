source(file.path("_helper", "init.R"))

# - "Test Error Diffs" ---------------------------------------------------------

diffs <- new(
  "unitizerItemTestsErrorsDiffs",
  value = new("unitizerItemTestsErrorsDiff",
  txt = "value", err = TRUE, diff = diffobj::diffChr(1, 2))
)
diffs$value@diff@target
diffs$value@diff@current
try(diffs$values)
try(diffs[[NA]])
err <- new(
  "unitizerItemTestsErrors", 
  value = new(
    "unitizerItemTestError", compare.err = TRUE, value = c("compare", "error")
) )
# - "Show Test Error" ----------------------------------------------------------

is(unitizer:::as.Diffs(err)@value, "unitizerItemTestsErrorsDiff")
