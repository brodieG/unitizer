# We will add an ignored statement to section 1; want to make sure it doesn't
# show up for review

unitizer_sect("Section 1", {
  TRUE
  runif(10)
} )
