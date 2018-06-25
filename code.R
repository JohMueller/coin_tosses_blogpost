
### Write a function to simulate one coin toss
coin_toss <- function(){
  rbinom(1,1, 0.5)
}

### Write a function that simulates the game 
# Bob: condition = c(1,1); Alice: condition = c(1,0)

simulate_game <- function(condition = c(1,1)){ 
  condition_met <- F
  tosses <- c()
  while(condition_met != T){
    tosses <- c(tosses, rbinom(1,1, 0.5))
    condition_met <- all.equal(tail(tosses, 2), condition)
  }
  return(tosses)
}

### Simulate game for 10000 times and save the number of tosses each one needed

games_bob <- c(); games_alice <- c()

for(i in 1:10000){
 games_bob[i] <- length(simulate_game(condition = c(1,1)))
 games_alice[i] <- length(simulate_game(condition = c(1,0)))
}

hist(games_bob,
     main= "Number of Coin Tosses to end the game for Bob (10000 times)",
     xlim= c(0,40),
     breaks = 20,
     xlab="")
hist(games_alice,
     main= "Number of Coin Tosses to end the game for Bob (10000 times)",
     xlim= c(0,40),
     breaks = 20,
     xlab="")

mean(games_bob)


##### Merkov Chain
library(markovchain)

states_bob <-c("H","T","HH")
states_alice <- c("H", "T", "HT")

#Transition matric for Bob
tm_bob <- matrix(c(0,.5,.5,
                   0.5,0.5,0,
                   0,0,1),
             nrow=3, byrow=TRUE)
row.names(tm_bob) <- states_bob; colnames(tm_bob) <- states_bob
tm_bob

#Transition Matrix for Alice
tm_alice <- matrix(c(.5,0,0.5,
                     0.5,.5,0,
                     0,0,1),
                 nrow=3, byrow=TRUE)
row.names(tm_alice) <- states_alice; colnames(tm_alice) <- states_alice
tm_alice

# Plot transition for Bob
mc_bob <- new("markovchain", states=states_bob, transitionMatrix=tm_bob)
plot(mc_bob,package="diagram", 
     layout = matrix(c(0,0,0,1,1,1), ncol = 2, byrow = TRUE),
     edge.curved = -0.1, main = "Transition Plot Bob")

#Plot transition for Alice
mc_alice <- new("markovchain", states=states_alice, transitionMatrix=tm_alice)
plot(mc_alice,package="diagram", 
     layout = matrix(c(0,0,0,1,1,1), ncol = 2, byrow = TRUE),
     edge.curved = -0.1, main = "Transition Plot Alice")



#####


simulate_game <- function(condition = c(1,1)){ 
  condition_met <- F
  tosses <- c()
  
  while(condition_met == F){
    tosses <- c(tosses, rbinom(1,1, 0.5))
    condition_met <- all.equal(tail(tosses, 2), condition)
  }
  
  return(tosses)
}

### Simulate game 10000 times and save the number of tosses nedded to end the game

games_bob <- c()
games_alice <- c()

for(i in 1:10000){
  games_bob[i] <- length(simulate_game(condition = c(1,1)))
  games_alice[i] <- length(simulate_game(condition = c(1,0)))
}

# Mean and Distribution for Heads, Heads to end the game
mean(games_bob)
hist(games_bob)

# Mean and Distribution for Heads, Tails to end the game
mean(games_alice)
hist(games_alice)

