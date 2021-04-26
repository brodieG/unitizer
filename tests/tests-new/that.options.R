source(file.path("_helper", "init.R"))

old.opts <- options()
new.opts <- unitizer:::options_zero()
options(old.opts)

# - "options" ------------------------------------------------------------------

# not great tests...

all(names(new.opts) %in% names(old.opts))
length(new.opts) <= length(old.opts)

