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
# Simulate an experiment of throwing one dice 
# 30 times using the function rmultinom, and find out how many 6's are in the simulated sample
throws <- rmultinom(1,size=30, prob = rep(1/6,6))
throws[6]



#4
# Obtain a vector that shows how many 1's, 2's,..6's were obtained in the previous simulation
throws



#5
# Simulate normal distribution values. Imagine a population 
# in which the average height is 1.70 m with a standard deviation of 0.1. 
# Use rnorm to simulate the height of 1000 people and save it in an object called heights.

heights <- rnorm(n=1000,mean=1.7,sd=0.1)

# a) Plot the density of the simulated values.
plot(density(heights))

# b) Generate 10000 values with the same parameters and plot the respective density
# function on top of the previous plot in red to differentiate it.

heights_2 <- rnorm(n=10000,mean=1.7,sd=0.1)
lines(density(heights_2), col = "red")
