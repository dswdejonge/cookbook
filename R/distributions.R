# TODO: werkt niet voor (0,1)
# TODO: check p >= n
sum_Pascal_diagonal <- function(n, p) {
  choose(n + 1, p + 1)
}

sum_Pascal_diagonal_old <- function(n, p) {
  return(sum(choose(n, 0:n)[p+1:2]))
}
