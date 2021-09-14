PSR =t(array(c(5,0,4,1), dim = c(2,2)))  ### PD 
#cats = nrow(PSR)
#1-ADD, 2=ACC, 3 =TFT
types = 3
Lottery = array(0, dim = c(N,types))
Tot     = array(0, dim = c(types))
P       = array(0, dim = c(types))
AF      = array(0, dim = c(types))
pop = array(0, dim = c(N, 3)) ## 3 columns, placeholds
data = array(0, dim = c(G, types))
pop[1:N,1] = initial.fitness
#pop[1:N,2] = as.integer(1 + cats*runif(N)) 
pop[1:N,2] = sample(c(1,2, 3), N, prob = c(0.33, 0.33, 0.33), replace = TRUE)

strategy_matrix = data.frame("strategy" = NA, "obs_i" = NA, "obs_j" = NA)

strategy_matrix = data.frame()

## Always view from perspective of row player; just swap to calculate y###
payoff_counter = PSR =t(array(c(4,0,5,1), dim = c(2,2)))  ### PD 

##Column player is column players behavior
tit.for.tat      <-   function(strategy_matrix, column_player = 1,  t = 1, .....){
  if(t==1) i = 1
  if(t>=2) i  = column_player
  tmp = data.frame("i" = i, "j" = column_player, time = t)
  nam = ifelse(i ==1, "C", "D")
  return(list(tmp, nam))
}
always.defect    <-   function(strategy_matrix, column_player = 1,  t = 1, .....){
  i = 2
  tmp = data.frame("i" = i, "j" = column_player, time = t)
  nam = ifelse(i ==1, "C", "D")
  return(list(tmp, nam))
}
always.cooperate <-   function(strategy_matrix, column_player = 2,  t = 1, .....){
  i = 1
  tmp = data.frame("i" = i, "j" = column_player, time = t)
  nam = ifelse(i ==1, "C", "D")
  return(list(tmp, nam))
}

### It's lagging j for the pure strategies. This isn't right. Deal with the if, else in the strategies ###


tournament = function(length = 10, 
                      stx = always.defect, 
                      sty = tit.for.tat,
                      payoff = PSR, .....){
  strategy_x = data.frame(i= NA, j=NA, time=NA) 
  strategy_y = data.frame(i= NA, j=NA, time=NA) 
  for(time in 1:5){
    if(time ==1){
      strategy_x =  stx(t = time)[[1]]
      strategy_y =  sty(t = time)[[1]]
      names(strategy_x) <- names(strategy_y) <- c("i", "j", "time") 
      # true strategy in t1
      strategy_x$j <- strategy_y$i
      strategy_y$j <- strategy_x$i
    }
    else{
    strategy_x = rbind(strategy_x,
                       stx(column_player= strategy_y[time-1,1], t = time)[[1]])
    strategy_y = rbind(strategy_y,
                       sty(column_player= strategy_x[time-1,1], t = time)[[1]])
    }
  } 
    output = data.frame(i = strategy_x$i, j = strategy_y$i) ### actual payoff
    ### Calculate final payoffs.
  payoff_y = payoff_x = data.frame()
  for(time in 1:5){
    payoff_y =  rbind(payoff_y, 
                     PSR[as.numeric(output[time,2]), as.numeric(output[time,1])])
    payoff_x = rbind(payoff_x, 
                     PSR[as.numeric(output[time,1]), as.numeric(output[time,2])])
    
  }
  # returns payoff to row then column player
  
  return(data.frame(sum(payoff_x), sum(payoff_y)))
}





pairs = 
  tournament(length = 5, stx = tit.for.tat, sty = always.cooperate)



  
payoff_counter() 

simulate_1 = function(N=1000, G=1000, mr = 0.025, 
                      initial.fitness = 10, rounds = 5,....){
  cats = nrow(PSR)
  Lottery = array(0, dim = c(N,cats))
  Tot     = array(0, dim = c(cats))
  P       = array(0, dim = c(cats))
  AF      = array(0, dim = c(cats))
  pop = array(0, dim = c(N, 3)) ## 3 columns, placeholds
  data = array(0, dim = c(G, cats))
  pop[1:N,1] = initial.fitness
  #pop[1:N,2] = as.integer(1 + cats*runif(N)) 
  pop[1:N,2] = sample(c(1,2), N, prob = c(0.50, 0.50), replace = TRUE)
  for(g in 1:G){
    for (r in 1:rounds){
      for(x in 1:N){
        y = as.integer(1 + N*runif(1))
        ### This transition, then it should work ####
        row = c()
        row    =         car::recode(pop[x,2], "3='tit.for.tat'; 2='always.cooperate'; 1='always.defect'")
        column = c()
        column =         car::recode(pop[x,2], "3='tit.for.tat'; 2='always.cooperate'; 1='always.defect'")

        ######################################3
        outcome = payoff_counter(tournament_matrix = tournament( stx = tit.for.tat, 
                                                                 sty = always.cooperate)) 
        row  =     payoff_counter(tournament_matrix = tournament( stx = tit.for.tat, 
                                                                  sty = always.cooperate)) 
        pop[x,1] = pop[x,1] + row
        pop[y,1] = pop[y,1] + row
      }
      # We're also looping over rounds -- do this R number of times
      AFP = sum(pop[1:N, 1]) / N  ## In each round calculate the total fitness of the population
      for (i in 1:cats){
        Tot[i] =  sum(pop[1:N, 2] == i) ##  In each round, also calculate the total fitness for each category profile.
        P[i]   =  data[g,i] = Tot[i]/N   ## update the generation holder with the relative fitness of each category
        if(Tot[i] > 0) AF[i] = sum(pop[pop[1:N,2] == i, 1])/Tot[i]  ## the proportion of totral fitness of each type
        # discrete RP
        P[i] = P[i] * (AF[i]/AFP) # Multiply this proportion by the relative fitness in the total population. This forms the "eta" in the multinomial
      }
      ### This is known as the replicator dynamics. Just use the multinomial
      Lottery = t(rmultinom(N, size = 1, prob = P))
      pop[1:N,2] = max.col(Lottery) 
    }
    # Select those into the reproduction pool with maximum score. W're just replacing existing values ,based on the fitness...
    
    # Mutants in the population (uniform from 1 to N*Mr*2).
    # Change over scores by smome mutation rate. I believe we're doing everything with "float" values, so, it's also "flote replace, 
    # with a probability 
    
    
    pop[1:N, 1] = initial.fitness
    pop[1:N, 3] = 0
    
  } 
  
  pop[1:N, 3] = runif(N) ## Draw from uniform, length N, replace those values with a random new 'type"
  mutants = round(N*mr*runif(1)*2)
  for(i in 1:mutants){
    mutant = as.integer(1 + N*runif(1))
    pop[mutant, 2] = as.integer(1+cats*runif(1))
  }
  # Pick randomly among these and mutate
  
  # Updatre the population matrix -- i.e., reset
  return(data)
} 

