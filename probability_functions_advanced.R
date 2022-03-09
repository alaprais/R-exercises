#1 
#Using the functions runif and round, simulate the results of 100 dice rolls.
dice_roller <- function(n){
  # n: number of 6 sided dice rolls
  u <- runif(n,min = 0.5, max = 6.5)
  outcomes <- round(u)
  return (outcomes)
}


result <- table(dice_roller(1000000))
barplot(result)


#2 
#Let's assume that we want to simulate a game in which we throw an unfair coin 
# (success probability is 0.48) 10 times and you win 10 every time the result is tails and
# lose 10 every time the result is heads. 
#
# Simulate this game 1000 times using rbinom, and find the expected amount of 
# money you will gain or lose in this game using the simulated values

set.seed(5412)
unfair_coin_games <- function(n) {
  # n is number of times we play the game (one game tosses 10 times)
  n_games <- rbinom(n, size=10, prob = 0.48)
  scores <- (10-n_games)*10 - n_games*10 
  return(scores)
}

# expected winnings 
mean(unfair_coin_games(10000))



#3



