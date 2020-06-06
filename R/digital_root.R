digital_root <- function(n){
  result <- sum(as.numeric(strsplit(as.character(n), "")[[1]]))
  if(nchar(result) == 1){
    return(result)
  }else{
    return(digital_root(result))
  }
}
