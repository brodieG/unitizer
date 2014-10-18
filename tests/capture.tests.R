# Due to how these tests mess with output and message streams this is not within
# a unit testing framework

# Simple test

unitizer:::release_sinks()
test.file <- tempfile()
cat("hello", "goodbye", "theres", file=test.file, sep="\n")

std.out <- tempfile()
std.err <- tempfile()

con <- unitizer:::set_text_capture(std.out, "output")
message(class(con))    # File Connection

print(readLines(test.file))
sink()

print(readLines(std.out))      # should see contents of test.file
close(con)

# Pre-existing sink

pre.sink <- tempfile()
sink(pre.sink)

con <- unitizer:::set_text_capture(std.out, "output")
message(typeof(con))    # should be logical

print("oogabooga")
sink()

print(readLines(std.out))      # should still see the same unchanged contents
readLines(pre.sink)     # oogabooga

# Cause errors

try(con <- unitizer:::set_text_capture(std.out, 1))
Sys.chmod(std.out, "0000")
try(con <- unitizer:::set_text_capture(std.out, "output"))
Sys.chmod(std.out, "0700")

# Test std.err

con <- unitizer:::set_text_capture(std.err, "message")
print(class(con))       # file connection

message("blargh")      # no output to screen
sink(type="message")

print(readLines(std.err))     # blargh
close(con)

# Pre-existing sink

pre.sink.con <- file(pre.sink, "wt")
sink(pre.sink.con, type="message")

con <- unitizer:::set_text_capture(std.err, "message")
print(con)         # FALSE

message("marshmallows")  # no output to screen
sink(type="message")
close(pre.sink.con)

print(readLines(pre.sink))   # marshmallows
print(readLines(std.err))    # still blargh

# Test full cycle with output

con <- unitizer:::set_text_capture(std.out, "output")
cat("The cat ate the rat")
print(unitizer:::get_text_capture(con, std.out, "output"))

con <- unitizer:::set_text_capture(std.out, "output")
cat("The cat ate the bat")
try(unitizer:::get_text_capture(con, std.err, "output"))  # mismatch connection and file
sink()  # this is handled by logic in `test_eval`
close(con)

pre.sink.con <- file(pre.sink, "wt")
con <- unitizer:::set_text_capture(std.out, "output")
cat("The cat barfed hats")
sink(pre.sink.con, "output")                            # Add connection to sink buffer
try(unitizer:::get_text_capture(con, std.out, "output"))
invisible(replicate(sink.number(), sink()))
close(con)
close(pre.sink.con)

pre.sink.con <- file(pre.sink, "wt")
con <- unitizer:::set_text_capture(std.out, "output")
sink()
sink(pre.sink.con, "output")                            # Subvert the sink
cat("Dragon barfed hats")
try(unitizer:::get_text_capture(con, std.out, "output"))
invisible(replicate(sink.number(), sink()))
try(close(con))  # should have been closed already
close(pre.sink.con)

con <- unitizer:::set_text_capture(std.out, "output")
cat("Test Errors")
try(unitizer:::get_text_capture("causeerror", std.out, "output"))
try(unitizer:::get_text_capture(con, 333, "output"))
try(unitizer:::get_text_capture(con, std.out, list()))
invisible(replicate(sink.number(), sink()))
close(con)

# Test full cycle with message

con <- unitizer:::set_text_capture(std.err, "message")
message("The cat ate the racoon")
print(unitizer:::get_text_capture(con, std.err, "message"))

con <- unitizer:::set_text_capture(std.err, "message")
message("The cat ate the bat", sample(1:100, 1))
try(unitizer:::get_text_capture(con, std.out, "message"))  # mismatch connection and file
sink(type="message")  # this is handled by logic in `test_eval`
print(readLines(std.err))
close(con)

pre.sink.con <- file(pre.sink, "wt")
con <- unitizer:::set_text_capture(std.err, "message")
sink(pre.sink.con, type="message")                            # Change connection
message("The cat barfed hats", sample(1:100, 1))
print(unitizer:::get_text_capture(con, std.err, "message"))    # Empty
sink(type="message")
close(pre.sink.con)
print(readLines(pre.sink))

pre.sink.con <- file(pre.sink, "wt")
con <- unitizer:::set_text_capture(std.err, "message")
sink(type="message")
sink(pre.sink.con, "message")                            # Subvert the sink, but this is not an error with message
message("Dragon barfed hats")
print(unitizer:::get_text_capture(con, std.err, "message")) # should be fine
sink(type="message")
try(close(con))  # should have been closed already
close(pre.sink.con)

# Pre-existing sink, full cycle

pre.sink.con <- file(pre.sink, "wt")
sink(pre.sink.con)
con <- unitizer:::set_text_capture(std.out, "output")
cat("Cat the Cat says hi")
message(class(con))  # should be logical (FALSE)
sink()
print(unitizer:::get_text_capture(con, std.out, "output")) # should work fine, but show nothing
print(readLines(pre.sink))
close(pre.sink.con)

# Clean up

unitizer:::release_sinks()
file.remove(std.out)
file.remove(std.err)
file.remove(pre.sink)
