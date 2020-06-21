# highest common factor
hcf <- function(x, y) {
  if(x > y) {
    smaller <- y
  } else {
    smaller <- x
  }
  for(i in 1:smaller) {
    if((x %% i == 0) && (y %% i == 0)) {
      hcf <- i
    }
  }
  return(hcf)
}

# lowest common multiple
lcm <- function(x, y) {
  if(x > y) {
    greater <- x
  } else {
    greater <- y
  }
  while(TRUE) {
    if((greater %% x == 0) && (greater %% y == 0)) {
      lcm <- greater
      break
    }
    greater <- greater + 1
  }
  return(lcm)
}
