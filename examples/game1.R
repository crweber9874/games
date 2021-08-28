N = 100
pop = array(0, dim = c(N, 2))
pop[1:N,2] = as.integer(1 + 7*runif(N)) ### Fill in integer 1:7
#if (pop[x,2] == 1) coop = 1
#for((x in 1:N)): something

# b is the benefit of altruism
# r is number of rounds
# N - pop, G = number of generations, mr = mutation
# np - no play option payoff (mutual defection payoff is 0)
# c is cost of altruism
b = 1; c = 4; R= 50; N = 1000; G = 1000; mr = 0.005; np = 1;

Lottery = array(0, dim = c(N,7))
Tot     = array(0, dim = c(7))
P       = array(0, dim = c(7))
AF      = array(0, dim = c(7))

pop = array(0, dim = c(N, 3)) ## 3 columns
data = array(0, dim = c(G,7))
# types: 1 = cooperators; 2=defectors; 3 = coequals; 
# 4=anti-coequals; 5=cooperate with poor only; 6=cooperate with
# rich only; 7=no play.

## Starting fitness
pop[1:N,1] = 1 + R*c ## Fitness is positive, it is just 1 + number of rounds times cost
pop[1:N,3] = 0
head(pop)

### Different permutations
# 100 percent coequal, just fill in type 3.
pop[1:N, 2] = 3
head(pop)


pop[1:N, 2] = 2


pop[1:N, 2] = 3

pop[1:N, 2] = as.integer(1 + 7*runif(N)) # A random integer

pop[1:N, 2] = 1


for(g in 1:G){
  for (r in 1:R){
    for(x in 1:N){
      # Cycle of individuals in the population?
      NoPlay = 0; Coop1 = 0; Coop2 = 0;
      y = as.integer(1 + N*runif(1))  ## a random number from 1 through N
      ## This is pure cooperation
      if (pop[x,2] == 1) Coop1=1  
      if (pop[y,2]==  1) Coop2=1
      ## This is pure defection, by default [WHY?]
      ## This is classic coeqauls
      if (pop[x,2] == 3 &  pop[x,3] == pop[y, 3]) Coop1 =1  ### cooperate with those of same type, equal fitness
      if (pop[y,2] == 3 &  pop[y,3] == pop[x, 3]) Coop2 =1
      
      ## This is anti-co
      if (pop[x,2] == 4 &  pop[x,3] != pop[y, 3]) Coop1 =1  ### cooperate with same type, uneqaul fitness
      if (pop[y,2] == 4 &  pop[y,3] != pop[x, 3]) Coop2 =1
      
      ## Cooperate with poor
      if (pop[x,2] == 5 &  pop[x,3] >  pop[y, 3]) Coop1 =1  ### person x cooperates with poor y
      if (pop[y,2] == 5 &  pop[y,3] >  pop[x, 3]) Coop2 =1
      
      ## Cooperate with rich
      if (pop[x,2] == 6 &  pop[x,3] <  pop[y, 3]) Coop1 =1  ### person x cooperates with poor y
      if (pop[y,2] == 6 &  pop[y,3] <  pop[x, 3]) Coop2 =1
      
      # No play
      if (pop[x,2] ==7) NoPlay =1
      if (pop[y,2] ==7) NoPlay =1
      # b is benefit of cooperating with y, c is cost of cooperating with y.
      # This is just the payoff for the player interactions. I think:
      # randomly comparing an x and y value, determining fitness of each,
      # choosing whether to cooperate. If person y cooperates you receive a benefit
      # if person x cooperates, there is a cost. no play gives a slight regturn.
      pop[x,3] = pop[x,1] + (1 - NoPlay)*(Coop2*b - Coop1*c) + NoPlay*np
  }
  
    AFP = sum(pop[1:N, 3]) / N  ## The average payoff function
  for (i in 1:7){
      Tot[i] =  sum(pop[1:N, 2] == i) ##  Total population of each type. 
      P[i]   =  data[g,i] = Tot[i]/N   ## Fill in each generation of the payoff matrix for each type.
      if(Tot[i] > 0) AF[i] = sum(pop[pop[1:N,2] == i, 3])/Tot[i]  ## the average fitness of each type
      # discrete RP
      P[i] = P[i] * (AF[i]/AFP) # P is just the proportion of each type * 
      # [Average fitness of each type/Average fitness in the population ]
    }
    ### Individual types given aggregate level proportions (probabilites)
    # stcochastic replicator dynamics via transposed multinomial random number generator
    Lottery = t(rmultinom(N, size = 1, prob = c(P[1], P[2], P[3], P[4], P[5], P[6], P[7])))
    # For each "type" we're deteremining whether they replicate. These are based on the proprotion of the payoff -- i.e, fitness
  for (i in 1:N){
      pop[i,2] = which.max(Lottery[i, 1:7]) #Now replace the population with its own value, or that from the random dra.
  }
    # Mutants in the population (uniform from 1 to N*Mr*2)
    # Pick randomly among these and mutate
    mutants = round(N*mr*runif(1)*2)
    for(i in 1:mutants){
      mutant = as.integer(1+N*runif(1))
      pop[mutant, 2] = as.integer(1 +7*runif(1))
    }
  } 
# No mutations
    pop[1:N, 1] = 1 + R*c
    pop[1:N, 3] = 0
} 

# = cooperators; 2=defectors; 3 = coequals; 
# 4=anti-coequals; 5=cooperate with poor only; 6=cooperate with
# rich only; 7=no play.

library(dplyr)

dat = reshape2::melt(data) %>% mutate(Strategy = rep(c("Leslie Knope (Cooperate)", 
                                                 "Jeremy Jamm (Defect)", "Jean-Ralphio (Like Me)",
                                                 "April Ludgate (Not Like Me)",
                                                 "Ann Perkins (Poor)", 
                                                 "Tom Haverford(Rich)", 
                                                 "Ron Swanson(Abstain)"), each = G)) %>%
  mutate(generation = rep(c(1:G), times = 7))
                          
#melted<-subset(melted, Year==2016)
ggplot(data = dat,
               aes(x = generation,
                   y = value))+
  facet_wrap(~Strategy) + 
  geom_line()
  
 N = 200; G = 1000; mr = 0.02

Lottery = array(0, dim = c(N,3))
Tot     = array(0, dim = c(3))
P       = array(0, dim = c(3))
AF      = array(0, dim = c(3))
pop = array(0, dim = c(N, 3)) ## 3 columns

PSR =t(array(c(3,3,2,2,1,3,1,2,3), dim = c(3,3)))
data = array(0, dim = c(G,3))

# types: 1 = Paper, 2 = Scisoors, 3= Rock
pop[1:N,1] = 10
pop[1:N,2] = as.integer(1 + 3*runif(N)) ### Fill in integer 1:7


for(g in 1:G){
  for (r in 1:R){
    for(x in 1:N){
      # Cycle of individuals in the population?
      y = as.integer(1 + N*runif(1))  ## a random number from 1 through N
      pop[x,1] = pop[x,1] + PSR[pop[x,2], pop[y,2]] ### play a random draw, update payoff 
      pop[y,1] = pop[y,1] + PSR[pop[y,2], pop[y,2]] ### Sam here
    }
    AFP = sum(pop[1:N, 1]) / N  ## The average payoff function
    for (i in 1:3){
      Tot[i] =  sum(pop[1:N, 2] == i) ##  Total population of each type. 
      P[i]   =  data[g,i] = Tot[i]/N   ## Fill in each generation of the payoff matrix for each type.
      if(Tot[i] > 0) AF[i] = sum(pop[pop[1:N,2] == i, 1])/Tot[i]  ## the average fitness of each type
      # discrete RP
      P[i] = P[i] * (AF[i]/AFP) # P is just the proportion of each type * 
      # [Average fitness of each type/Average fitness in the population ]
    }
    ### Individual types given aggregate level proportions (probabilites)
    # stcochastic replicator dynamics via transposed multinomial random number generator
    Lottery = t(rmultinom(N, size = 1, prob = c(P[1], P[2], P[3])))
    # For each "type" we're deteremining whether they replicate. These are based on the proprotion of the payoff -- i.e, fitness
    pop[1:N,2] = max.col(Lottery) #Now replace the population with its own value, or that from the random dra.
    
    # Mutants in the population (uniform from 1 to N*Mr*2)
    pop[1:N, 3] = runif(N)
    for(i in 1:N){
      if (pop[i,3] < mr) pop[i,2] = as.integer(1 + 3*runif(1))
    }
    # Pick randomly among these and mutate

  } 
  # No mutations
  pop[1:N, 1] = 1 + R*c
  pop[1:N, 3] = 0
} 

matplot(1:G, cbind(data[1:G,1],data[1:G,2], data[1:G,3]))



dat = reshape2::melt(data) %>% mutate(Strategy = rep(c("Rock", "Paper", "Scissors"), each = G)) %>%
  mutate(generation = rep(c(1:G), times = 3))

#melted<-subset(melted, Year==2016)
ggplot(data = dat,
       aes(x = generation,
           y = value))+
  facet_wrap(~Strategy) + 
  geom_line()
