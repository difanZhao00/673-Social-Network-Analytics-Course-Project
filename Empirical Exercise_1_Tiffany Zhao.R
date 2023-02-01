library(igraph)
library(dplyr)
library(gtools)
setwd("D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #1")
data <- read.csv('social_and_task_network.csv')

### Question 1 -- A

# social networks plot

social <- data %>% filter(data$social_tie >0)
socialm <- cbind(social$ego,social$alter)
sg <- graph.data.frame(socialm)
E(sg)$weight <- social$social_tie
plot.igraph

# task networks plot

task <- data %>% filter(data$task_tie >0)
taskm <- cbind(task$ego,task$alter)
tg <- graph.data.frame(taskm)
E(tg)$weight <- task$task_tie
plot(tg, edge.arrow.size=0.2)

## Statistics for networks

# Calculate indegree, outdegree, closeness, betweenness and eigenvector centrality of social ties. 

sgindegree <- degree(sg, v = V(sg), mode = "in")
sgindegree

sgoutdegree <- degree(sg, v = V(sg), mode = "out")
sgoutdegree

sgclose<- closeness(sg, vids = V(sg), mode ="total", normalized = TRUE)
sgclose

sgbetween <- betweenness(sg, v = V(sg), directed = TRUE, normalized = TRUE)
sgbetween

sgeigen <- eigen_centrality(sg, directed = TRUE)
sgeigen <- sgeigen[["vector"]]
sgeigen

# Calculate indegree, outdegree, closeness, betweenness and eigenvector centrality of task ties.

tgindegree <- degree(tg, v = V(tg), mode = "in")
tgindegree

tgoutdegree <- degree(tg, v = V(tg), mode = "out")
tgoutdegree

tgclose<- closeness(tg, vids = V(tg), mode ="total", normalized = TRUE)
tgclose

tgbetween <- betweenness(tg, v = V(tg), directed = TRUE, normalized = TRUE)
tgbetween

tgeigen <- eigen_centrality(tg, directed = TRUE)
tgeigen <- tgeigen[["vector"]]

### Question 1 -- B

index <- c()
for (x in 1:22) {
  if ((x %in% names(tgeigen)) & (x %in% names(sgeigen))) {
    index <- append(index, x)
  }
}
index <- as.character(index)
index

# Compute pearson correlation

pcin <- cor(sgindegree[index], tgindegree[index])
pcin
pcout <- cor(sgoutdegree[index], tgoutdegree[index])
pcout
pcclose <- cor(sgclose[index], tgclose[index])
pcclose
pcbetween <- cor(sgbetween[index], tgbetween[index])
pcbetween
pceigen <- cor(sgeigen[index], tgeigen[index])
pceigen
which.max(c(pcin,pcout,pcclose,pcbetween,pceigen))

# Compute kendall correlation

kcin <- cor(sgindegree[index], tgindegree[index], method = "kendall")
kcin
kcout <- cor(sgoutdegree[index], tgoutdegree[index], method = "kendall")
kcout
kcclose <- cor(sgclose[index], tgclose[index], method = "kendall")
kcclose
kcbetween <- cor(sgbetween[index], tgbetween[index], method = "kendall")
kcbetween
kceigen <- cor(sgeigen[index], tgeigen[index], method = "kendall")
kceigen
which.max(c(kcin,kcout,kcclose,kcbetween,kceigen))

# Compute spearman correlation

scin <- cor(sgindegree[index], tgindegree[index], method = "spearman")
scin
scout <- cor(sgoutdegree[index], tgoutdegree[index], method = "spearman")
scout
scclose <- cor(sgclose[index], tgclose[index], method = "spearman")
scclose
scbetween <- cor(sgbetween[index], tgbetween[index], method = "spearman")
scbetween
sceigen <- cor(sgeigen[index], tgeigen[index], method = "spearman")
sceigen
which.max(c(scin,scout,scclose,scbetween,sceigen))


### Question 2 -- A

# Create a new table with seperate edges of two ties.

both <- data %>% filter(data$social_tie >0 | data$task_tie>0)
library(tidyr)
both <- gather(both, key = 'type', value = 'value', social_tie:task_tie)
both <- both %>% filter(both$value>0)

# Calculate mean and median of two types of ties.

meansocial <- mean(both[both$type == "social_tie",]$value)
meantask <- mean(both[both$type == "task_tie",]$value)
mediansocial <- median(both[both$type == "social_tie",]$value)
mediantask <- median(both[both$type == "task_tie",]$value)

# See which tie is strong based on the mean of weights.

sstrong <- both %>% filter(type == "social_tie") %>% mutate(strong = (value > meansocial))
tstrong <- both %>% filter(type == "task_tie") %>% mutate(strong = (value > meantask))

### (1) Visually:

#Plot the netwotk and set strong edges as blue, weak edges as grey.

sw <- c(sstrong$strong, tstrong$strong)
sw[sw == TRUE] = "blue"
sw[sw == FALSE] = "grey"
bothm <- cbind(both$ego,both$alter)
bothg <- graph.data.frame(bothm,directed = TRUE)
E(bothg)$weight <- both$value
plot.igraph(bothg, edge.arrow.size=0.2, edge.color=sw, vertex.size = 15, edge.curved = TRUE)


### (2) Programmatically

# Combine two types of ties together, and build a matrix with all types of ties.

combine <- rbind(sstrong, tstrong)
strongmean <- combine$strong # extract values for question 3
combinesort <- combine[order(combine$ego),]
all <- cbind(combine$ego,combine$alter)
allg <- graph_from_edgelist(all)
allgm <-  as_adj(allg)

# Extract strong ties and make an matrix only with strong ties.

combines <- combinesort %>% filter(combinesort$strong == TRUE)
combinese <- cbind(combines$ego,combines$alter)
combineg <- graph_from_edgelist(combinese)
combinegm <-  as_adj(combineg)

number <- 0
stc <- data.frame()
for (i in 1:22) {
  if (sum(combinegm[i,] > 0) > 1) {
    n <- which((combinegm[i,]) > 0)
    agent <- stc
    stc <- combinations(n=length(n), r=2, v=n)
    stc <- rbind(agent, stc)
  } 
}
for (i in 1:22) {
  if (sum(combinegm[,i] > 0) > 1) {
    n <- which((combinegm[,i]) > 0)
    agent <- stc
    stc <- combinations(n=length(n), r=2, v=n)
    stc <- rbind(agent, stc)
  } 
}
stc <- stc[order(stc$V1,stc$V2),]
stc <- stc[!duplicated(stc[,1:2]),]
for (j in 1:(length(stc$V1))) {
  if ((allgm[stc[j,1],stc[j,2]] == 0) | (allgm[stc[j,2],stc[j,1]] == 0)) {
    number <- number +1
  }
}
number


## Question 2 -- B

# See which tie is strong based on the median of weights.

sstrong <- both %>% filter(type == "social_tie") %>% mutate(strong = (value > mediansocial))
tstrong <- both %>% filter(type == "task_tie") %>% mutate(strong = (value > mediantask))


combine <- rbind(sstrong, tstrong)
strongmedian <- combine$strong # extract values for question 3
combinesort <- combine[order(combine$ego),]
all <- cbind(combine$ego,combine$alter)
allg <- graph_from_edgelist(all)
allgm <-  as_adj(allg)
combines <- combinesort %>% filter(combinesort$strong == TRUE)
combinese <- cbind(combines$ego,combines$alter)
combineg <- graph_from_edgelist(combinese)
combinegm <-  as_adj(combineg)
number <- 0
stc <- data.frame()
for (i in 1:22) {
  if (sum(combinegm[i,] > 0) > 1) {
    n <- which((combinegm[i,]) > 0)
    agent <- stc
    stc <- combinations(n=length(n), r=2, v=n)
    stc <- rbind(agent, stc)
  } 
}
for (i in 1:22) {
  if (sum(combinegm[,i] > 0) > 1) {
    n <- which((combinegm[,i]) > 0)
    agent <- stc
    stc <- combinations(n=length(n), r=2, v=n)
    stc <- rbind(agent, stc)
  } 
}

stc <- stc[order(stc$V1,stc$V2),]
stc <- stc[!duplicated(stc[,1:2]),]
for (j in 1:(length(stc$V1))) {
  if ((allgm[stc[j,1],stc[j,2]] == 0) | (allgm[stc[j,2],stc[j,1]] == 0)) {
    number <- number +1
  }
}
number > 0



### Question 3 -- A

# Calculate the edge-level betweenness for the 2 types of tie.


socialeb <- edge_betweenness(sg, e = E(sg), directed = TRUE)
socialeb

# Edge-level betweenness of task ties:
  
taskeb <- edge_betweenness(tg, e = E(tg), directed = TRUE)
taskeb



## Question 3 -- B

# Compute the edge betweenness of social ties and task ties.

strongmean[strongmean == TRUE] = "Strong"
strongmean[strongmean == FALSE] = "Weak"
strongmedian[strongmedian == TRUE] = "Strong"
strongmedian[strongmedian == FALSE] = "Weak"
compare <- cbind(strongmean, strongmedian, edge_betweenness = c(socialeb, taskeb))

# The mean of edge betweenness when ties are strong

mean(as.integer(compare[,'edge_betweenness'][compare[,'strongmean'] == 'Strong']))

# The mean of edge betweenness when ties are weak

mean(as.integer(compare[,'edge_betweenness'][compare[,'strongmean'] == 'Weak']))

# The mean of edge betweenness when ties are strong

mean(as.integer(compare[,'edge_betweenness'][compare[,'strongmedian'] == 'Strong']))

# The mean of edge betweenness when ties are weak

mean(as.integer(compare[,'edge_betweenness'][compare[,'strongmedian'] == 'Weak']))


### Qeustion 4

adjmatrix <- as_adjacency_matrix(bothg, type = "both", names = TRUE, sparse = FALSE)
library(expm)
count <- c()
for (i in 1:(nrow(adjmatrix)-1)) {
  matrix <- adjmatrix %^% i
  for (j in 1:nrow(adjmatrix)) {
    for (k in 1:nrow(adjmatrix)) {
      if ((matrix[j,k] == 0) & (matrix[k,j] == 0)) {
        count <- rbind(count,c(j,k))
      }
    }
  }
}
count <- count[order(count[,1], count[,2]),]
colnames(count) <- c("a","b")
count <- as.data.frame(count)
number <-summarise(group_by(count,a,b),length(b))
sum(number$`length(b)` == (nrow(adjmatrix)-1))


### Question 5

one <- graph_from_literal(A---B, A---C, A---D, A---E)
plot(one)

zero <- graph_from_literal(A---B, A---C, A---D,A---E,B---C,B---D,B---E,C---D,C---E,D---E)
plot(zero)


par(mfrow=c(1,2))
hist(degree(one))
hist(degree(zero))

# Plot the closeness distribution of 1 network-level degree centrality and the closeness distribution

par(mfrow=c(1,2))
hist(closeness(one))
hist(closeness(zero))

# Plot the betweenness distribution of 1 network-level degree centrality and the betweenness distribution of 0 network-level degree centrality, respectively.

par(mfrow=c(1,2))
hist(betweenness(one))
hist(betweenness(zero))


