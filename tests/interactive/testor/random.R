
i.calls <- lapply(testor:::as.list(items.sorted2), function(x) x@call)  # FOR DEBUGGING
i.ls <- lapply(testor:::as.list(items.sorted2), function(x) x@ls)
names(i.ls) <- paste0(i.calls, ifelse(sapply(testor:::as.list(items.sorted2), function(x) x@reference), " REFERENCE", ""))
i.ls.bind <- do.call(rbind, i.ls)
cbind(i.ls.bind, comp=c("*", "'", "", "", "", "", "", "", "", "'", "**", "*", "*", "", "", "", "", "", "", "'", "", "**", "*", "*"))

my.testor3 <- new("testor", id=1, zero.env=new.env())
my.testor3 <- my.testor3 + ref.exps2   # add ref.exps as new items
my.testor4 <- new("testor", id=2, zero.env=new.env())  
my.testor4 <- my.testor4 + my.testor3@items.new    # now convert them to reference items
my.testor4 <- my.testor4 + new.exps2   # now test against new.exps
items.mixed2 <- my.testor4@items.ref[c(8, 10, 3, 5)] + my.testor4@items.new[c(1, 4, 5, 9)]
items.sorted2 <- testor:::healEnvs(items.mixed2, my.testor4)


i.calls <- lapply(testor:::as.list(items.sorted2), function(x) x@call)  # FOR DEBUGGING
i.ls <- lapply(testor:::as.list(items.sorted2), function(x) x@ls)
names(i.ls) <- paste0(i.calls, ifelse(sapply(testor:::as.list(items.sorted2), function(x) x@reference), " REFERENCE", ""))
i.ls

x[itemsType(x) == "reference"][[i]]