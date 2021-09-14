

strategies = list(
##Column player is column players behavior
tit.for.tat      <-   function(strategy_matrix, column_player = 1,  t = 1, .....){
  if(t==1) i = 1
  if(t>=2) i  = column_player
  tmp = data.frame("i" = i, "j" = column_player, time = t)
  nam = ifelse(i ==1, "C", "D")
  return(list(tmp, nam))
},
always.defect    <-   function(strategy_matrix, column_player = 1,  t = 1, .....){
  i = 2
  tmp = data.frame("i" = i, "j" = column_player, time = t)
  nam = ifelse(i ==1, "C", "D")
  return(list(tmp, nam))
},

always.cooperate <-   function(strategy_matrix, column_player = 2,  t = 1, .....){
  i = 1
  tmp = data.frame("i" = i, "j" = column_player, time = t)
  nam = ifelse(i ==1, "C", "D")
  return(list(tmp, nam))
}
)


### Simple simulation
simulate_1 = function(N=100, G=100, mr = 0.025, 
         initial.fitness = 10, rounds = 5
         ){
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
          pop[x,1] = pop[x,1] + PSR[pop[x,2], pop[y,2]] 
          pop[y,1] = pop[y,1] + PSR[pop[y,2], pop[x,2]] 
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

PSR =t(array(c(4,0,5,1), dim = c(2,2)))  ### PD 

### Iterated three type PD.
simulation_tourn = function(N=3, G=10, mr = 0.025, 
                      initial.fitness = 10, rounds = 5, 
                      cats = 3, PSR, strategies = strategies){
  Lottery = array(0, dim = c(N,cats))
  Tot     = array(0, dim = c(cats))
  P       = array(0, dim = c(cats))
  AF      = array(0, dim = c(cats))
  pop     = array(0, dim = c(N, 3)) ## 3 columns, placeholds
  data = array(0, dim = c(G, cats))
  pop[1:N,1] = initial.fitness
  #pop[1:N,2] = as.integer(1 + cats*runif(N)) 
  pop[1:N,2] = sample(c(1:cats), N, prob = c(0.25, 0.50, 0.25), replace = TRUE)
  for(g in 1:G){
    for (r in 1:rounds){
     for(x in 1:N){
        y = as.integer(1 + N*runif(1))
        tmp1 = tmp2 = c()
        if(pop[x,2] == 1) tmp1 = strategies[[1]]
        if(pop[x,2] == 2) tmp1 = strategies[[2]]
        if(pop[x,2] == 3) tmp1 = strategies[[3]]
        
        if(pop[x,2] == 1) tmp2 = strategies[[1]]
        if(pop[x,2] == 2) tmp2 = strategies[[2]]
        if(pop[x,2] == 3) tmp2 = strategies[[3]]
        
        pop[x,1] = pop[x,1] + as.numeric(tournament(length = 3 , stx = tmp1,
                                          sty = tmp2, payoff = PSR)[1])

        pop[y,1] = pop[y,1] + as.numeric(tournament(length = 3 , stx = tmp2,
                                          sty = tmp1, payoff = PSR)[1])
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
    print(paste("Generation is:", g, "of", G))
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

library(dplyr)
round1 = simulation_tourn(N=50, G=24, mr = 0.25, 
                         initial.fitness = 1000, rounds = 3, 
                         cats = 3, PSR, strategies = strategies)  %>% data.frame() 






ggplot(plot_dat, aes(x = Generation, y = value , group = as.character(variable))) +
  geom_line()

ggplot(data = dat,
       aes(x = generation,
           y = value))+
  facet_wrap(~Strategy) + 
  geom_line()




simulate_3 = function(N=100, G=100, mr = 0.025, 
                      initial.fitness = 10, rounds = 5,
                      cats = 3
){

  LN = RN = 0;
  i = 1
  PSR = t(array(c(2,1,3,3,2,1,1,3,2), dim = c(3,3)))
  pop = array(0, dim = c(N, 3)) ## 3 columns, placeholds
  pop[1:N,1] = initial.fitness
  Lottery = array(0, dim = c(N,cats))
  probs= c(0.33, 0.33, 0.33)
  Lottery = t(rmultinom(N, size = 1, prob = probs))  
  pop[1:N, 2] = max.col(Lottery)
  pop[1:N, 3] = rbinom(N, 1, 0.5) + 1  ### Initial behavior, random
  data = array(0, dim = c(G, 3)) ### F?
  for(g in 1:G){
    for (r in 1:rounds){
      for(x in 1:N){
        LN = x - 1; if(LN== 0) LN = N
        RN = x + 1; if(RN == N + 1) RN = 1
        ### Now program the interactions
        pop[x,1]  = pop[x,1]  + PSR[pop[x,2], pop[LN,2]] 
        pop[LN,1] = pop[LN,1] + PSR[pop[LN,2], pop[x,2]] 
        pop[x,1]  = pop[x,1]  + PSR[pop[x,2], pop[RN,2]] 
        pop[RN,1] = pop[RN,1] + PSR[pop[RN,2], pop[x,2]] 
      }
      for(i in 1:N){
        LN = i - 1; 
        if(LN== 0) LN = N
        RN = i + 1;
        if(RN == N +1) RN = 1
        ### Local interaction update rather than replicator dynamic
        if(pop[i,1]  < pop[LN,1] | pop[i,1]  < pop[RN,1]){
          if(pop[i,1]  > pop[RN,1])  
            pop[i,2]  = pop[LN,2]
          else 
              pop[i,2] = pop[RN,2]
          }
        }
      
           
           
        pop[LN,1] = pop[LN,1] + PSR[pop[LN,2], pop[x,2]] 
        pop[x,1]  = pop[x,1]  + PSR[pop[x,2], pop[RN,2]] 
        pop[RN,1] = pop[RN,1] + PSR[pop[RN,2], pop[x,2]] 
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






simulate_1() %>% data.frame() %>% mutate(time = seq(1:n())) %>%
  reshape2::melt( id.vars = "time", value = "Prop in Popultation", measure.vars = c("X1", "X2")) %>% 
  mutate(type = rep(c("Hawk", "Dove"), each = 1000)) %>% 
  ggplot(aes(x=time, y = value, group = type)) + 
  geom_line() +
  scale_colour_manual(name="Type", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() 
  
  
melt(
  data,
  id.vars,
  measure.vars,
  variable.name = "variable",
  ...,
  na.rm = FALSE,
  value.name = "value",
  factorsAsStrings = TRUE
)
Arg


library(ggtern)
set.seed(1)

plot <- ggtern(round1*100,
               aes(x=round1[,1], y =round1[,2], round1[,3]))
plot + stat_density_tern(geom = 'polygon',
                         n         = 1000,
                         aes(fill  = ..level..,
                             alpha = ..level..)) +
  geom_line() +
  theme_rgbw() +
  labs(title = "Example Density/Contour Plot")    +
  scale_fill_gradient(low = "blue",high = "red")  +
  guides(color = "none", fill = "none", alpha = "none")
data = simulate_1()


matplot(1:G, cbind(data[1:G,1],data[1:G,2], data[1:G,3]))



dat = reshape2::melt(data) %>% mutate(Strategy = rep(c("Rock", "Paper", "Scissors"), each = G)) %>%
  mutate(generation = rep(c(1:G), times = 3))

#melted<-subset(melted, Year==2016)
ggplot(data = dat,
       aes(x = generation,
           y = value))+
  facet_wrap(~Strategy) + 
  geom_line()

