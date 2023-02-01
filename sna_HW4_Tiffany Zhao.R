library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(igraph)
library(proxy)
library(MASS)
library(stats)

rm(list = ls())
setwd("D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #4")

# Load data
pro_film <- read.csv("producers_and_films.csv", header = T)
kw_film <- read.csv("film_keywords.csv", header = T)
box_film <- read.csv("box_office_revenues.csv", header = T)
pro_subsid <- read.csv("production_subsidiaries.csv", header = T)
head(pro_film)
head(kw_film)
head(box_film)
head(pro_subsid)

# Data preprocess
## Extract the production of US
pf <- pro_film[which(pro_film$country == "us"),]
pf <- pf[,c(1,2,4)]
head(pf)

# 1.A
## Construct the network
years <- sort(unique(pf$year))
net_film <- NULL
for (i in 1:length(years)) {
  a1 <- pf[which(pf$year == years[i]),]
  a2 <- unique(a1$pindex)
  a3 <- NULL
  for (j in 1:length(a2)) {
    a4 <- a1[which(a1$pindex == a2[j]),]
    id1 <- NULL
    id2 <- NULL
    if(nrow(a4) == 1){
      id1 <- a4$pcindex
      id2 <- 'isolate'
    }else{
      for (k in 1:nrow(a4)) {
        id1 <- c(id1, rep(a4$pcindex[k], nrow(a4)-k))
        id2 <- c(id2, a4$pcindex[-c(1:k)])
      }
    }
    a5 <- data.frame(rep(a2[j], length(id1)),
                         rep(years[i], length(id1)), id1, id2)
    a3 <- rbind(a3, a5)
  }
  net_film <- rbind(net_film, a3)
}
colnames(net_film) <- c("pindex", "year", "id1", "id2")
## Find the generalist and specialist
generalist <- NULL
for (i in 10:length(years)) {
  a6 <- which(net_film$year == years[i] - 9)
  a7 <- which(net_film$year == years[i])
  eg1 <- net_film[a6[1]:a7[length(a7)],]
  eg1 <- eg1[,-c(1,2)]
  eg1 <- distinct(eg1)
  iso <- which(eg1$id2 == "isolate")
  vg1 <- eg1$id1[iso]
  eg2 <- eg1[-iso,]
  vg2 <- unique(c(eg2$id1, eg2$id2))
  g <- graph_from_data_frame(eg2, directed = F, vertices = vg2)
  g <- add_vertices(g, length(vg1), name = vg1)
#  plot(g, layout = layout.fruchterman.reingold(g), 
#       vertex.color = 'steelblue', vertex.shape = 'circle',
#       vertex.size = 5, vertex.label = NA,
#       main = paste("The Network from ",(years[i]-9), " to ", 
#                    years[i], sep = ""))
  concentrate <- as.data.frame((page.rank(g)$vector))
  concentrate <- data.frame(c(vg2, vg1), concentrate)
  colnames(concentrate) <- c("pcindex", "pr")
  concentrate <- concentrate[order(concentrate$pr, decreasing = T),]
  threshold <- concentrate[round(nrow(concentrate)/4),2]
  a8 <- concentrate[which(concentrate$pr >= threshold),1]
  generalist <- c(generalist, a8)
}
generalist <- unique(generalist)

if_generalist <- NULL
for (i in 1:nrow(pf)) {
  if(pf$pcindex[i] %in% generalist){
    a9 <- 1
  }else{
    a9 <- 0
  }
  if_generalist <- c(if_generalist, a9)
}
pf <- cbind(pf, if_generalist)
pf <- pf[order(pf$year),]

## Classify the film in 5 types
index_film <- unique(pf$pindex)
pf_1 <- NULL
for (i in 1:length(index_film)) {
  a10 <- pf[which(pf$pindex == index_film[i]),]
  a10.1 <- sum(unique(a10$if_generalist) + 1)
  if(nrow(a10) == 1 && a10$if_generalist == 0){
    types_film <- 1
  }else if(nrow(a10) == 1 && a10$if_generalist == 1){
    types_film <- 2
  }else if(nrow(a10) != 1 && a10.1 == 2){
    types_film <- 3
  }else if(nrow(a10) != 1 && a10.1 == 1){
    types_film <- 4
  }else{
    types_film <- 5
  }
  a11 <- data.frame(unique(a10$pindex), unique(a10$year), types_film)
  pf_1 <- rbind(pf_1, a11)
}
colnames(pf_1) <- c("pindex", "year", "type")
pf_1 <- pf_1[order(pf_1$year),]

## Count the number of new keywords of each film
## New keywords and combination
kw_1 <- kw_film[which(str_detect(kw_film$keyword, "-") == F), ]
kw_2 <- kw_film[which(str_detect(kw_film$keyword, "-") == T), ]
kw_1_year <- NULL
for (i in 1:length(years)) {
  if(years[i] == 1985 || years[i] == 1986){
    a12 <- pf_1[which(pf_1$year == 1985),]
    a13 <- unique(a12$pindex)
    a14 <- NULL
    for (j in 1:length(a13)) {
      a15 <- length(which(kw_1$pindex == a13[j]))
      if(a15 != 0){
        a16 <- kw_1$keyword[which(kw_1$pindex == a13[j])]
        a14 <- c(a14, a16)
      }
    }
    a14 <- unique(a14)
    a17 <- list(years[i], a14)
  }else if(years[i] == 1987){
    a12.1 <- which(pf_1$year == 1986)
    a12 <- pf_1[1:a12.1[length(a12.1)],]
    a13 <- unique(a12$pindex)
    a14 <- NULL
    for (j in 1:length(a13)) {
      a15 <- length(which(kw_1$pindex == a13[j]))
      if(a15 != 0){
        a16 <- kw_1$keyword[which(kw_1$pindex == a13[j])]
        a14 <- c(a14, a16)
      }
    }
    a14 <- unique(a14)
    a17 <- list(years[i], a14)
  }else{
    a12.1 <- which(pf_1$year == years[i] - 3)
    a12.2 <- which(pf_1$year == years[i] - 1)
    a12 <- pf_1[a12.1[1]:a12.2[length(a12.2)],]
    a13 <- unique(a12$pindex)
    a14 <- NULL
    for (j in 1:length(a13)) {
      a15 <- length(which(kw_1$pindex == a13[j]))
      if(a15 != 0){
        a16 <- kw_1$keyword[which(kw_1$pindex == a13[j])]
        a14 <- c(a14, a16)
      }
    }
    a14 <- unique(a14)
    a17 <- list(years[i], a14)
  }
  kw_1_year <- c(kw_1_year, a17)
}
kw_2_year <- NULL
for (i in 1:length(years)) {
  if(years[i] == 1985 || years[i] == 1986){
    a12 <- pf_1[which(pf_1$year == 1985),]
    a13 <- unique(a12$pindex)
    a14 <- NULL
    for (j in 1:length(a13)) {
      a15 <- length(which(kw_2$pindex == a13[j]))
      if(a15 != 0){
        a16 <- kw_2$keyword[which(kw_2$pindex == a13[j])]
        a14 <- c(a14, a16)
      }
    }
    a14 <- unique(a14)
    a17 <- list(years[i], a14)
  }else if(years[i] == 1987){
    a12.1 <- which(pf_1$year == 1986)
    a12 <- pf_1[1:a12.1[length(a12.1)],]
    a13 <- unique(a12$pindex)
    a14 <- NULL
    for (j in 1:length(a13)) {
      a15 <- length(which(kw_2$pindex == a13[j]))
      if(a15 != 0){
        a16 <- kw_2$keyword[which(kw_2$pindex == a13[j])]
        a14 <- c(a14, a16)
      }
    }
    a14 <- unique(a14)
    a17 <- list(years[i], a14)
  }else{
    a12.1 <- which(pf_1$year == years[i] - 3)
    a12.2 <- which(pf_1$year == years[i] - 1)
    a12 <- pf_1[a12.1[1]:a12.2[length(a12.2)],]
    a13 <- unique(a12$pindex)
    a14 <- NULL
    for (j in 1:length(a13)) {
      a15 <- length(which(kw_2$pindex == a13[j]))
      if(a15 != 0){
        a16 <- kw_2$keyword[which(kw_2$pindex == a13[j])]
        a14 <- c(a14, a16)
      }
    }
    a14 <- unique(a14)
    a17 <- list(years[i], a14)
  }
  kw_2_year <- c(kw_2_year, a17)
}
## Count the number of new keywords and combinations
num_kw_1 <- NULL
for (i in 1:nrow(pf_1)) {
  a18 <- kw_1$keyword[which(kw_1$pindex == pf_1$pindex[i])]
  if(length(a18) == 0){
    new_kw <- 0
  }else if(length(a18) != 0 && pf_1$year[i] == 1985){
    new_kw <- 0
    a20 <- kw_1_year[[2]]
    for (j in 1:length(a18)) {
      if(length(which(a20 == a18[j])) < 2){
        new_kw <- new_kw + 1
      }
    }
  }else{
    a19 <- which(years == pf_1$year[i])
    a20 <- kw_1_year[[2*a19]]
    new_kw <- 0
    for (j in 1:length(a18)) {
      if((a18[j] %in% a20) == F){
        new_kw <- new_kw + 1
      }
    }
  }
  num_kw_1 <- rbind(num_kw_1, new_kw)
}
num_kw_2 <- NULL
for (i in 1:nrow(pf_1)) {
  a18 <- kw_2$keyword[which(kw_2$pindex == pf_1$pindex[i])]
  if(length(a18) == 0){
    new_cm <- 0
  }else if(length(a18) != 0 && pf_1$year[i] == 1985){
    new_cm <- 0
    a20 <- kw_2_year[[2]]
    for (j in 1:length(a18)) {
      if(length(which(a20 == a18[j])) < 2){
        new_cm <- new_cm + 1
      }
    }
  }else{
    a19 <- which(years == pf_1$year[i])
    a20 <- kw_2_year[[2*a19]]
    new_cm <- 0
    for (j in 1:length(a18)) {
      if((a18[j] %in% a20) == F){
        new_cm <- new_cm + 1
      }
    }
  }
  num_kw_2 <- rbind(num_kw_2, new_cm)
}
pf_1 <- data.frame(pf_1, num_kw_1, num_kw_2, (num_kw_1+num_kw_2))
colnames(pf_1) <- c("pindex", "year", "type",
                    "num_kw", "num_cm", "num_total")
write.csv(pf_1, "D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #4/pf_1.csv", 
          row.names = F)

type_kw <- NULL
for (i in 1:5) {
  a22 <- pf_1[which(pf_1$type == i),]
  a23 <- unique(a22$year)
  a24 <- NULL
  for (j in 1:length(a23)) {
    a25 <- sum(a22$num_cm[which(a22$year == a23[j])])
    a26 <- data.frame(a22$type[i], a23[j], a25)
    a24 <- rbind(a24, a26)
  }
  type_kw <- rbind(type_kw, a24)
}
colnames(type_kw) <- c("type", "year", "num_type_kw")
ggplot(data = type_kw, aes(x = year)) + 
  geom_line(aes(y = num_type_kw, color = factor(type)), size = 0.8) +
  scale_colour_discrete(breaks = c("1","2","3","4","5"),
                        labels = c("Peripheral solo productions",
                                   "Central solo productions",
                                   "Central co-productions",
                                   "Peripheral co-productions",
                                   "Hybrid co-productions")) +
  labs(title = "Plot of the Number of New Combinations per Type",
       x = "Year", y = "Number of New Combinations", 
       color = "Type")
rm(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a10.1,a11,a12,a12.1,a12.2,a13,a14,
   a15,a16,a17,a18,a19,a20,a22,a23,a24,a25,id1,id2,if_generalist,
   iso,new_cm,new_kw,threshold,types_film,vg1,vg2,num_kw_1,num_kw_2,
   eg1,eg2,a26,concentrate,g,generalist)

# 1.B  
b1 <- left_join(pf, pf_1, by = "pindex")
b1 <- left_join(b1, box_film, by = "pindex")
b1 <- b1[,-c(4,5,9,11)]
b1 <- na.omit(b1)
colnames(b1)[2] <- c("year")

jaccard1 = function(x,y){
  jac1 <- (length(union(x, y))-length(intersect(x, y)))/length(union(x, y)) 
  return(jac1)
}

num_cph <- NULL
num_ope <- NULL
is_subsi <- NULL
num_films <- NULL
num_same <- NULL
for (i in 1:nrow(b1)) {
  b2 <- b1[which(b1$pcindex == b1$pcindex[i]),]
  ## Count the number of cen-co-pro and per-co-pro and hy-co-pro
  b3 <- b2[which(b2$year == b1$year[i]),]
  num_ccp <- length(which(b3$type == 3))
  num_pcp <- length(which(b3$type == 4))
  num_hcp <- length(which(b3$type == 5))
  num_cph <- rbind(num_cph, c(num_ccp, num_pcp, num_hcp))
  ## Number of years in operations
  num_ope <- c(num_ope, length(unique(b2$year)))
  ## If_subsidiary
  b4 <- which(pro_subsid == b1$pcindex[i])
  if(length(b4) != 0){
    firstyear <- pro_subsid[b4, 2]
    lastyear <- pro_subsid[b4, 3]
    if(b1$year[i] >= firstyear && b1$year[i] <= lastyear){
      if_subsi <- 1
    }else{
      if_subsi <- 0
    }
  }else{
    if_subsi <- 0
  }
  is_subsi <- c(is_subsi, if_subsi)
  ## Total films made that year
  num_films <- c(num_films, nrow(b3))
  ## Similarities
  b5 <- which(b2$year == (b1$year[i]-1))
  b6 <- which(b2$year == (b1$year[i]-2))
  if(length(b5) == 0 && length(b6) == 0){
    ja <- 0
  }else if(length(b5) == 0 && length(b6) != 0){
    b7 <- b2[b6,]
    b8 <- unique(b7$pindex)
    kw_2years <- NULL
    for (j in 1:length(b8)) {
      b9 <- kw_film$keyword[which(kw_film == b8[j])]
      kw_2years <- c(kw_2years, b9)
    }
    kw_2years <- unique(kw_2years)
    kw_now <- kw_film$keyword[which(kw_film == b1$pindex[i])]
    ja <- length(intersect(kw_now, kw_2years))
  }else if(length(b5) != 0 && length(b6) == 0){
    b7 <- b2[b5,]
    b8 <- unique(b7$pindex)
    kw_2years <- NULL
    for (j in 1:length(b8)) {
      b9 <- kw_film$keyword[which(kw_film == b8[j])]
      kw_2years <- c(kw_2years, b9)
    }
    kw_2years <- unique(kw_2years)
    kw_now <- kw_film$keyword[which(kw_film == b1$pindex[i])]
    ja <- length(intersect(kw_now, kw_2years))
  }else{
    b7 <- b2[b6[1]:b5[length(b5)],]
    b8 <- unique(b7$pindex)
    kw_2years <- NULL
    for (j in 1:length(b8)) {
      b9 <- kw_film$keyword[which(kw_film == b8[j])]
      kw_2years <- c(kw_2years, b9)
    }
    kw_2years <- unique(kw_2years)
    kw_now <- kw_film$keyword[which(kw_film == b1$pindex[i])]
    ja <- length(intersect(kw_now, kw_2years))
  }
  num_same <- c(num_same, ja)
}
b10 <- dist(num_same, method = "jaccard")
b11 <- cmdscale(b10, k = 2)
cordd1 <- b11[1]
cordd2 <- b11[2]

pf_2 <- data.frame(b1, num_cph, cordd1, cordd2,
                   num_ope, is_subsi, num_films)
colnames(pf_2) <- c("pindex", "year", "pcindex", "type", "num_kw",
                    "num_cm","total_box","release_coverage",
                    "cen_co_pro","per_co_pro","hyb_co_pro",
                    "cord1","cord2","num_operation",
                    "is_subsidiary","num_films")
write.csv(pf_2, "D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #4/pf_2.csv", 
          row.names = F)


m1 <- glm.nb(num_kw~cen_co_pro+per_co_pro+hyb_co_pro+cord1+cord2+total_box+num_operation+is_subsidiary+factor(year),
             data = pf_2, offset(num_films))
summary(m1)
m2 <- glm.nb(num_cm~cen_co_pro+per_co_pro+hyb_co_pro+cord1+cord2+total_box+num_operation+is_subsidiary+factor(year),
             data = pf_2, offset(num_films))
summary(m2)

# 2.
c1 <- c(which(pf_1$type == 3),
        which(pf_1$type == 4), 
        which(pf_1$type == 5))
c2 <- pf_1[c1,]

avg_ja <- NULL
for (i in 1:nrow(c2)) {
  kw_now <- kw_film$keyword[which(kw_film$pindex == c2$pindex[i])]
  if(length(kw_now) == 0){kw_now <- NULL}
  c3 <- pf$pcindex[which(pf$pindex == c2$pindex[i])]
  if(length(c3) == 0){
    ja <- 0
  }else if(length(c3) > 0){
    kw_2years <- NULL
    for (j in 1:length(c3)) {
      c4 <- pf[which(pf$pcindex == c3[j]),]
      c5 <- c(c4$pindex[which(c4$year == c2$year[i]-1)],
              c4$pindex[which(c4$year == c2$year[i]-2)])
      c5 <- unique(c5)
      if(length(c5) != 0){
        c7 <- NULL
        for (k in 1:length(c5)) {
          c6 <- kw_film$keyword[which(kw_film$pindex == c5[k])]
          c7 <- c(c7,c6)
        }
        kw_2years <- c(kw_2years, unique(c7))
      }
    }
    ja <- jaccard1(kw_now, kw_2years)
  }
  c8 <- ja/length(c3)
  avg_ja <- c(avg_ja, c8)
}
pf_3 <- data.frame(c2, avg_ja)
c9 <- order(pf_3$num_total, decreasing = T)
pf_3 <- pf_3[-c9[1:15],]
write.csv(pf_3, "D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #4/pf_3.csv", 
          row.names = F)
ggplot(data = pf_3, aes(x = avg_ja, y = num_total)) + 
  geom_line(aes(color = factor(type)), size = 0.8) +
  geom_smooth(method = "loess", se = T) + 
  scale_colour_discrete(breaks = c("3","4","5"),
                        labels = c("Central co-productions",
                                   "Peripheral co-productions",
                                   "Hybrid co-productions")) +
  labs(title = "Plot of Relationship between the Number of Keywords and Average Jaccard Distance",
       x = "Average Jaccard Distance", y = "Number of New Keywords", 
       color = "Type")

# 3.
return_film <- b1$total_box/(b1$release_coverage)
pf_4 <- data.frame(pf_2, return_film)
d1 <- which(pf_4$return_film == Inf)
pf_4 <- pf_4[-d1,]
std_return <- (pf_4$return_film - mean(pf_4$return_film))/sd(pf_4$return_film)
pf_4 <- data.frame(pf_4, std_return)

m3 <- lm(std_return~cen_co_pro+per_co_pro+hyb_co_pro+total_box+num_operation+is_subsidiary+factor(year),
         data = pf_4)
summary(m3)