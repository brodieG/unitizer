setMethod(
  "diffObj", c("conditionList", "conditionList"),
  function(target, current, ...) {
    dots <- match.call(expand.dots=FALSE)[["..."]]
    if("mode" %in% names(dots))
      callNextMethod()
    else
      callNextMethod(target=target, current=current, ..., mode="unified")
  }
)
