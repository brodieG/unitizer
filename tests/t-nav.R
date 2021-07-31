source(file.path("_helper", "init.R"))
nav <- file.path("_helper", "unitizers", "nav.R")

# Simple navigation tests that don't require complex unitizers

# - "Re-run bookmark" -----=----------------------------------------------------

# Relates to #278.  Tests both Review and Browse

unitizer:::read_line_set_vals(
  c("R", "Y", "B", "7", "R", "Y", "B", "9", "R", "Y", "Q")
)
unitize(nav, interactive.mode=TRUE)

