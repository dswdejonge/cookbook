# speed_of_A = speed A (distance per hour)
# speed_of_B = speed B (distance per hour)
# lead_of_A = lead of A over B (distance)
# return = time for B to catch up with A c(hh, mm, ss) - round down to the nearest second.
# #If speed_of_A >= speed_of_B then return NULL.
race <- function(speed_of_A, speed_of_B, lead_of_A){
  if(speed_of_A >= speed_of_B){return(NULL)}
  seconds_to_victory <- lead_of_A / (speed_of_B - speed_of_A) * 3600
  hh <- seconds_to_victory %/% 3600
  mm <- (seconds_to_victory-hh*3600) %/% 60
  ss <- floor((seconds_to_victory-hh*3600-mm*60))
  return(c(hh, mm, ss))
}
