\name{testor_ls}
\alias{testor_ls}
\title{An `ls` Like Function}
\usage{
testor_ls(name, pos = -1L, envir = as.environment(pos), all.names = FALSE,
  pattern)
}
\value{
list of object names, or a list with environments
containing the objects
}
\description{
Much like `ls`, except that it will list all the objects
recorded in the test items
}
\examples{
env1 <- new.env()
assign("var1", "25", env1)
env2 <- new.env(parent=env1)
assign("var2", "hello", env2)
env3 <- new.env(parent=env2)
assign("var3", 3.1415, env3)

ls <- testor:::make_ls(globalenv())
evalq(ls(), env3)
evalq(base::ls(), env3)
rm(ls)
}
\seealso{
\code{`\link{print.testor_ls}`}
}

