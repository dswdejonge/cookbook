find_missing <- function(sequence){
  diffs <- diff(sequence)
  off <- which(!duplicated(diffs) & !duplicated(diffs, fromLast = T))
  progression <- unique(diffs[-(off)])
  missing_value <- sequence[off]+progression
  return(missing_value)
}
