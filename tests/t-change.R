source(file.path("_helper", "init.R"))

# - "Construction works" -------------------------------------------------------

# invalid slot
try(new("unitizerChanges", removed = 1:3))
# invalid/ got character
try(new("unitizerChanges", failed = letters[1:2]))

# - "Output as expected" -------------------------------------------------------

my.changes <- new("unitizerChanges", failed = c(1L, 10L), new = c(1L, 
    5L), removed = c(2L, 4L), corrupted = c(3L, 8L))
show(my.changes)

# - "Length Works" -------------------------------------------------------------

length(my.changes) # 7
