#problema de la mochila
items <- data.frame(item = c("pocketnife", "beans", "potatoes", "unions", "sleeping bag", "rope", "compass"),
                    survivalpoints = c(10, 20, 15, 2, 30, 10, 30),
                    weight = c(1, 5, 10, 1, 7, 5, 1))
items
weight.limit <- 20         

especimen1 <- c(1,0,1,0,1,0,1)
especimen1 %*% items$weight

fitness.three <- function(x)
{
  items.weight <- x %*% items$weight
  items.survival.points <- x %*% items$survivalpoints
  
  if(items.weight > weight.limit){
    return (0)
  }
  else
  {
    return (-items.survival.points)
  }
}

fitness.three(especimen1)
rep(1, 7)
greedy <- (rep(1, 7))
greedy
fitness.three(greedy)

ga.three <- rbga.bin(size = 7, popSize = 200, iters = 200, mutationChance = .01, 
                   elitism = 100, evalFunc = fitness.three)

summary(ga.three, echo=T)

bestSolution <- c(1, 1, 0, 1, 1, 1, 1)
bestSolution %*% items$survivalpoints

items[bestSolution==1,]
bestSolution==1
