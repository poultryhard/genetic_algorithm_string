#Written By Allan Macdonald 24/12/2022
#Genetic Algorithm to determine contents of a String

GENES <- ' !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~'
TARGET <- "porn"
population_size = 20 #ensure that the population size is dividable evenly by the survivor size
survivor_size = 10
target_length = nchar(TARGET)
mutation_rate = 0.8
NO_OF_MUTATIONS = 1
NO_OF_GENERATIONS = 100000

#function to create random word of target strings length from string 
rand_word <- function(TARGET,GENES){
  string <- ""                        
  for (i in 1:nchar(TARGET)){
    letter <- rand_char(GENES)
    string <- paste(string,letter,sep="")
  }
  return(string)
}

#function to return random char from input string 
rand_char <- function(string){
  index = sample.int(n=nchar(string),size = 1, replace = T)
  return(substr(string, index, index))
} 

#function to generate population of words
generate_population <- function() {
population <- data.frame(individual=NA,fitness=NA)
for (i in 1:population_size){
  frame <- data.frame(individual=rand_word(TARGET,GENES),fitness=NA)
  population <- rbind(population,frame)
}
  #clean up and return population
  population = population[-c(1),] 
  row.names(population) <- 1:population_size 
  return(population)
}

#function to evaluate fitness of population of words, returns fittest individuals
evaluate_fitness <- function(population){
  for(i in 1:population_size){
  fitness <- 0
  for (k in 1:target_length){
    if (substr(population$individual[i],k,k) != substr(TARGET,k,k)){
      fitness <- fitness+1
      }
    population$fitness[i] <- fitness
    }
  }
  top_ten_fit <- order(population$fitness, decreasing=F)[1:survivor_size]
  survivors <- population[top_ten_fit,]
  row.names(survivors) <- 1:survivor_size #rename rows
  return(survivors)
}

#function to mate population of survivors and return a new population of words
mate <- function(survivors){
  
  #create dataframe to store offspring in
  offspring <- data.frame(individual = NA,fitness= NA)
  
  #loop through survivors dataframes to create a new population
  for (j in 1:(population_size/survivor_size)) {
    
    #create index that determines which individuals will mate
    frame <- data.frame(individual = survivors$individual,fitness= NA)
    index = sample.int((survivor_size),(survivor_size), replace=F)
    parents <- survivors[index,]
    row.names(parents) <- 1:survivor_size
    
    #loop through each mating pair and randomly assign half of each parents chromosomes to the offspring
    for (i in 1:survivor_size){
      chrom_index = sample.int(target_length, round(target_length/2), replace=F)
      for (k in 1:round(target_length/2)){
        substr(frame$individual[i],chrom_index[k],chrom_index[k]) <- substr(parents$individual[i],chrom_index[k],chrom_index[k])
      }
    }
    offspring <- rbind(offspring,frame)
  }
  #cleaning up offspring population and returning
  offspring = offspring[-c(1),] 
  row.names(offspring) <- 1:population_size
  
  offspring <- mutate(offspring)
  return(offspring)
}


mutate <- function(offspring){
  #create dataframe to store offspring in
  mutated_offspring <- data.frame(individual = offspring$individual, fitness= NA)
  total_mutations <- round(nrow(offspring)*mutation_rate)
  # create index for mutations
  to_mutate <- sample.int(nrow(offspring), total_mutations, replace = F)
  for (i in 1:total_mutations){
    
    mutate_chrom <- sample.int(target_length,NO_OF_MUTATIONS,replace = F)

    substr(mutated_offspring$individual[to_mutate[i]],mutate_chrom, mutate_chrom) <- rand_char(GENES)
    
  }
  return(mutated_offspring)
} 




population <- generate_population()
survivors <- evaluate_fitness(population)

output <- data.frame(individual = NA, fitness = NA)
for (i in 1:NO_OF_GENERATIONS)
{
  offspring <<- mate(survivors)
  survivors <<- evaluate_fitness(offspring)
  output[i,] <- head(survivors,1)
  print(output[i,])
  if (output$fitness[i] == 0){
    break
  }
}

