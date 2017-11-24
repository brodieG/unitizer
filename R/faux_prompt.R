# Emulate the R Console Prompt
#
# @keywords internal
# @param prompt what character to use as the prompt character
# @param continue what character to use as the second and onward prompt line character
# @return the expression typed in by the user

faux_prompt <- function(
  prompt=getOption("prompt"), continue=getOption("continue")
) {
  res <- character()
  repeat {
    res.parse <- tryCatch({
        res <- paste0(res, read_line(prompt), if(length(res)) '\n' else "")
        parsed <- parse(text=res)
      },
      error=function(e) {
        if(!isTRUE(grepl(" unexpected end of input\n", conditionMessage(e)))) {
          e$call <- if(length(res)) res else NULL
          stop(e)
        }
    } )
    prompt <- `continue`
    if(is.expression(res.parse)) {
      return(res.parse)
  } }
  NULL
}
