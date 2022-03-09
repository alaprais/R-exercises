#1) Using the functions runif and round, simulate the results of 100 dice rolls.
dice_roller <- function(n){
  # n: number of 6 sided dice rolls
  u <- runif(n,min = 0.5, max = 6.5)
  outcomes <- round(u)
  return (outcomes)
}


result <- table(dice_roller(1000000))
barplot(result)
