library(igraph)
library(readxl)
library(lubridate)
library(stringr)
library(mice)
library(dplyr)
library(proxy)
library(cluster)
library(factoextra)

rm(list = ls())
setwd("D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #2")

# Load data
fe1 <- read.csv("Funding_events_7.14.csv", header = T)
fe2 <- read_xlsx("Funding_events_7.14_page2.xlsx")
fe2 <- data.frame(fe2)
fe1$Deal.Date <- as.Date(mdy(fe1$Deal.Date))
fe <- rbind(fe1,fe2)
head(fe)

# Data preprocess
## Delete missing values
fe_sdi <- fe[,c(1,4,11)]
md.pattern(fe_sdi)
fe_sdi <- na.omit(fe_sdi)
md.pattern(fe_sdi)
## Delete decay data 
startup <- unique(fe_sdi$Portfolio.Company.Name)
renew_win <- NULL
for (i in 1:length(startup)) {
  a <- fe_sdi[which(fe_sdi$Portfolio.Company.Name == startup[i]),]
  if(nrow(a) != 1){
    a <- a[order(a$Deal.Date),]
    temp1 <- NULL
    for (j in 1:nrow(a)) {
      temp1 <- c(temp1, 
                 as.double(difftime(a$Deal.Date[j+1],a$Deal.Date[j])))
    }
    temp1 <- temp1[-length(temp1)]
    renew_win <- c(renew_win, temp1)
  }
}
threshold_renew_win <- (sort(renew_win))[length(renew_win)*0.9]  #2191
## Get data after deleting decay data
df <- NULL
for (i in 1:length(startup)) {
  a <- fe_sdi[which(fe_sdi$Portfolio.Company.Name == startup[i]),]
  if(nrow(a) == 1){
    df <- rbind(df, a)
  }else{
    a <- a[order(a$Deal.Date),]
    temp2 <- NULL
    for (j in 1:nrow(a)) {
      b <- as.double(difftime(a$Deal.Date[j+1],a$Deal.Date[j]))
      temp2 <- c(temp2, b)
    }
    temp2 <- temp2[-length(temp2)] - threshold_renew_win
    temp3 <- NULL
    for (k in 1:length(temp2)) {
      if(temp2[k] < 0){
        temp3 <- c(temp3, k)
      }
    }
    temp3 <- a[c(temp3, nrow(a)),]
    df <- rbind(df, temp3)
  }
}
## Delete data without CV
df <- df[-which(df$Investors == ""),]
write.csv(df, "D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #2/df.csv", 
          row.names = F)
rm(fe,fe1,fe2,fe_sdi,temp1,temp2,temp3,a,b,i,j,k,
   renew_win,threshold_renew_win,startup)

# 1.
## Compute the edge
d1 <- df[which(str_detect(df$Investors, ",") == T),]
id1 <- NULL
id2 <- NULL
for (i in 1:nrow(d1)) {
  d2 <- unlist(strsplit(d1$Investors[i], ", "))
  for (j in 1:(length(d2)-1)) {
    id1 <- c(id1, rep(d2[j],length(d2)-j))
    id2 <- c(id2, d2[-c(1:j)])
  }
}
eg1 <- data.frame(id1,id2)
eg1 <- eg1[-which(duplicated(eg1) == T), ]
vg1 <- unique(c(eg1$id1, eg1$id2))
g1 <- graph_from_data_frame(eg1, directed = F,vertices = vg1)
plot(g1, layout = layout.fruchterman.reingold(g1), 
     vertex.color = 'steelblue', vertex.shape = 'circle',
     vertex.size = 5, vertex.label = NA)
max(closeness(g1))  # 1
which((closeness(g1)) == max(closeness(g1))) # Salida Capital
rm(eg1,vg1,d1,d2,id1,id2,i,j,c,startup_df)

# 2.
e1 <- df[which(str_detect(df$Investors, ",") == T),]
e1 <- e1[order(e1$Deal.Date),]
e1$Deal.Date <- str_c(str_sub(e1$Deal.Date,1,7),"-01")
e1$Deal.Date <- ymd(e1$Deal.Date)
## Get the same date
date_same <- unique(e1$Deal.Date[which(duplicated(e1$Deal.Date) == T)])
kcore <- NULL
for (i in 1:nrow(e1)) {
  if(i == 1){
    e2 <- e1[i,]
    id3 <- NULL
    id4 <- NULL
    e3 <- unlist(strsplit(e2$Investors, ", "))
    for (k in 1:(length(e3)-1)) {
      id3 <- c(id3, rep(e3[k],length(e3)-k))
      id4 <- c(id4, e3[-c(1:k)])
    }
    eg2 <- data.frame(id3,id4)
    g2 <- graph_from_data_frame(eg2, directed = F, vertices = e3)
    kcore <- c(kcore, round(mean(coreness(g2)), digits = 2))
  }else{
    e2 <- e1[1:i,]
    id3 <- NULL
    id4 <- NULL
    for (j in 1:nrow(e2)) {
      e3 <- unlist(strsplit(e2$Investors[j], ", "))
      for (k in 1:(length(e3)-1)) {
        id3 <- c(id3, rep(e3[k],length(e3)-k))
        id4 <- c(id4, e3[-c(1:k)])
      }
    }
    eg2 <- distinct(data.frame(id3,id4))
    vg2 <- unique(c(eg2$id3, eg2$id4))
    g2 <- graph_from_data_frame(eg2, directed = F, vertices = vg2)
    kcore <- c(kcore, round(mean(coreness(g2)), digits = 2))
  }
}
kcore_date <- data.frame(e1$Deal.Date, kcore)
colnames(kcore_date) <- c("Deal.Date","kcore")

kcore_same <- NULL
e4 <- NULL
for (i in 1:length(date_same)) {
  e4 <- c(e4, which(kcore_date$Deal.Date == date_same[i]))
  e5 <- mean(kcore_date$kcore[which(kcore_date$Deal.Date == date_same[i])])
  kcore_same <- c(kcore_same, round(e5, digits = 2))
}
kcore_same <- data.frame(date_same, kcore_same)
colnames(kcore_same) <- c("Deal.Date","kcore")

kcore_date <- kcore_date[-e4,]
kcore_date <- rbind(kcore_date, kcore_same)
kcore_date <- kcore_date[order(kcore_date$Deal.Date),]
write.csv(kcore_date, "D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #2/kcore_date.csv", 
          row.names = F)
month <- 0
kcore_month <- NULL
for (i in 1:(nrow(kcore_date)-1)) {
  e6 <- round((as.double(difftime(kcore_date$Deal.Date[i+1],
                                  kcore_date$Deal.Date[i])))/30, 
              digits = 0)
  if(month == 0){
    for (j in 1:e6) {
      month <- j
      value <- kcore_date$kcore[i]
      kcore_month <- rbind(kcore_month, c(month, value))
    }
  }else{
    for (j in 1:e6) {
      month <- month + 1
      value <- kcore_date$kcore[i]
      kcore_month <- rbind(kcore_month, c(month, value))
    }
  }
}
colnames(kcore_month) <- c("Months", "AverageCoreness")
plot(kcore_month, type = 'l', 
     main = "Plot of Coreness in Co-investment Network over Time")
rm(e2,e3,e4,e5,e6,i,j,k,id3,id4,kcore_same,kcore_date,eg2,vg2,
   date_same,month,value)

# 3.1
# Pick data in June of each year, after 1995
f1 <- unique((e1[which(str_detect(e1$Deal.Date, "-06-") == T),])$Deal.Date)
f1 <- f1[7:26]
f2 <- e1[-(1:((which(e1$Deal.Date == f1[1]))[1]-1)),]

result_list <- NULL
for (i in 1:length(f1)) {
  f3 <- which(f2$Deal.Date == f1[i],)
  f4 <- f2[1:f3[length(f3)],]
  id5 <- NULL
  id6 <- NULL
  for (j in 1:nrow(f4)) {
    f5 <- unlist(strsplit(f4$Investors[j], ", "))
    for (k in 1:(length(f5)-1)) {
      id5 <- c(id5, rep(f5[k],length(f5)-k))
      id6 <- c(id6, f5[-c(1:k)])
    }
  }
  eg3 <- distinct(data.frame(id5,id6))
  vg3 <- unique(c(eg3$id5, eg3$id6))
  g3 <- graph_from_data_frame(eg3, directed = F, vertices = vg3)
  concentrate <- page.rank(g3)$vector
  result <- list(g3, vg3, concentrate)
  result_list <- c(result_list, result)
}

par(mfrow = c(3, 7),mar = c(5,4,4,2.1))
for (i in 1:length(f1)) {
  plot(result_list[[3*i]], type = 'l',
       main = str_sub(f1[i],3,7), 
       xlab = "p", ylab = "Concentrate Score")
}
par(mfrow = c(1, 1),mar = c(5.1,4.1,4.1,2.1))

# 3.2
for (i in 1:length(f1)) {
  plot(result_list[[3*(i-1)+1]], vertex.color = 'steelblue', 
       vertex.shape = 'circle', vertex.size = 5, vertex.label = NA,
       main = f1[i])
}
rm(f2,f3,f4,f5,i,j,k,id5,id6,eg3,vg3,result,concentrate)

# 4.
## Extract data of June in 1996
h1 <- e1[which(str_sub(e1$Deal.Date, 1,7) == "1996-06"),]
id7 <- NULL
id8 <- NULL
for (i in 1:nrow(h1)) {
  h2 <- unlist(strsplit(h1$Investors[i], ", "))
  for (j in 1:(length(h2)-1)) {
    id7 <- c(id7, rep(h2[j], length(h2)-j))
    id8 <- c(id8, h2[-c(1:j)])
  }
}
eg4 <- distinct(data.frame(id7,id8))
vg4 <- unique(c(eg4$id7, eg4$id8))
g4 <- graph_from_data_frame(eg4, directed = F, vertices = vg4)
plot(g4, vertex.color = 'steelblue', 
     vertex.shape = 'circle', vertex.size = 5,
     vertex.label.cex = 0.8,
     main = "1996-06")

h3 <- shortest.paths(g4)
for (i in 1:nrow(h3)) {
  h3[which(h3[,i] == "Inf"), i] <- 0
}
fviz_nbclust(h3, kmeans, method = "silhouette") +
  geom_vline(xintercept = 3, linetype = 2)
h4 <- pam(h3, k = 3)
fviz_cluster(h4, h3)
rm(h1,h2,h3,h4,eg4,vg4,id7,id8)

# Extra A
vcfo <- read.csv("Venture_capital_firm_outcomes.csv")
vcfo_1 <- vcfo[which(vcfo$year == "2014"),c(1:4)]
g5 <- result_list[[58]]
vg5 <- result_list[[59]]

a <- NULL
for (i in 1:length(vg5)) {
  a <- c(a, which(vcfo_1$firm_name == vg5[i]))
}
vcfo_1 <- vcfo_1[a,]

temp1 <- data.frame(degree(g5))
temp2 <- data.frame(closeness(g5))
temp3 <- data.frame(betweenness(g5))
temp4 <- data.frame(page.rank(g5)$vector)

degree_1 <- NULL
closeness_1 <- NULL
between_1 <- NULL
pagerank_1 <- NULL
for (i in 1:nrow(vcfo_1)) {
  degree_1 <- c(degree_1, 
                temp1[which(rownames(temp1) == vcfo_1$firm_name[i]),])
  closeness_1 <- c(closeness_1, 
                   temp2[which(rownames(temp2) == vcfo_1$firm_name[i]),])
  between_1 <- c(between_1, 
                 temp3[which(rownames(temp3) == vcfo_1$firm_name[i]),])
  pagerank_1 <- c(pagerank_1, 
                  temp4[which(rownames(temp4) == vcfo_1$firm_name[i]),])
}
result_1 <- data.frame(degree_1, closeness_1, between_1, 
                       pagerank_1, vcfo_1$successful_investments)
colnames(result_1) <- c("degree", "closeness","betweenness", 
                        "pagerank","successful_investments")
reg1 <- lm(successful_investments ~ ., data = result_1)
summary(reg1)
plot(reg1)

# Extra B
vcfo_2 <- vcfo[which(vcfo$year == "2011"),c(1:4)]

g6 <- result_list[[49]]
vg6 <- result_list[[50]]

a <- NULL
for (i in 1:length(vg6)) {
  a <- c(a, which(vcfo_2$firm_name == vg6[i]))
}
vcfo_2 <- vcfo_2[a,]

temp5 <- data.frame(degree(g6))
temp6 <- data.frame(closeness(g6))
temp7 <- data.frame(betweenness(g6))
temp8 <- data.frame(page.rank(g6)$vector)

degree_2 <- NULL
closeness_2 <- NULL
between_2 <- NULL
pagerank_2 <- NULL
for (i in 1:nrow(vcfo_2)) {
  degree_2 <- c(degree_2, 
                temp1[which(rownames(temp5) == vcfo_2$firm_name[i]),])
  closeness_2 <- c(closeness_2, 
                   temp2[which(rownames(temp6) == vcfo_2$firm_name[i]),])
  between_2 <- c(between_2, 
                 temp3[which(rownames(temp7) == vcfo_2$firm_name[i]),])
  pagerank_2 <- c(pagerank_2, 
                  temp4[which(rownames(temp8) == vcfo_2$firm_name[i]),])
}
result_2 <- data.frame(degree_2, closeness_2, between_2, 
                       pagerank_2, vcfo_2$out_of_business)
colnames(result_2) <- c("degree", "closeness","betweenness", 
                        "pagerank","out_of_business")
reg2 <- glm(out_of_business ~ ., data = result_2)
summary(reg2)
plot(reg2)







