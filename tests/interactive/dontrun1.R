# We will add an ignored statement to section 1; want to make sure it doesn't
# show up for review

unitize("tests/interactive/unitizer/assign.new.R")
unitize("tests/interactive/unitizer/assign.new.2.R", "tests/interactive/unitizer/assign.new.unitizer")
