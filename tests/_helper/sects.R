# Tests with sections with funs that produce output

unitizer_sect("stdout", compare=unitizer:::comp_stdout, {
  1 + 1
})
unitizer_sect("stderr", compare=unitizer:::comp_stderr, {
  1 + 2
})
unitizer_sect("stdboth", compare=unitizer:::comp_stdboth, {
  1 + 3
})
