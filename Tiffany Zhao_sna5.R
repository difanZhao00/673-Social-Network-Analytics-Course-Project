library(lubridate)
library(stringr)
library(mice)
library(dplyr)
library(igraph)
library(hhi)
library(plm)
library(pglm)
library(sqldf)
library(ggplot2)
library(plot3D)

rm(list = ls())
setwd("D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #5")

# Load data
comp_details <- read.csv("company_details.csv", header = T)
deal_details <- read.csv("deal_details.csv", header = T)
inve_details <- read.csv("investor_details.csv", header = T)
inve_deal <- read.csv("investors_and_deals.csv", header = T)
comp_details <- comp_details[,c(1,6,8)]
deal_details <- deal_details[,c(1,2,3,4,9)]
inve_deal <- inve_deal[,c(1,2,4)]
colnames(deal_details)[1:2] <- c("Deal_Id", "CompanyID")
head(comp_details)
head(deal_details)
head(inve_details)
head(inve_deal)

# Data preprocess
## Extract deals after 1990
deal_details$Deal_Date <- dmy(deal_details$Deal_Date)
deal_details <- deal_details[order(deal_details$Deal_Date),]
a1 <- which(str_sub(deal_details$Deal_Date, 1, 4) == "1990")
deal_details <- deal_details[-(1:a1[length(a1)]),]
md.pattern(deal_details)
a2 <- which(is.na(deal_details$Deal_Date) == T)
deal_details <- deal_details[-a2,]
md.pattern(deal_details)
deal_details$Deal_Date <- str_sub(deal_details$Deal_Date,1,4)
## Extract the investor with type Venture Capital
inve_details <- inve_details[which(inve_details$Investor_Type == "Venture Capital"),]
a3 <- NULL
for (i in 1:nrow(inve_details)) {
  a4 <- which(inve_deal$Investor_Id == inve_details$InvestorId[i])
  if(length(a4) != 0){
    a3 <- c(a3, a4)
  }
}
inve_deal <- inve_deal[a3,]
## Extract the deal data with co-invested
a5 <- unique(inve_deal$Deal_Id)
a6 <- NULL
for (i in 1:length(a5)) {
  a7 <- which(inve_deal$Deal_Id == a5[i])
  a8 <- inve_deal[a7,]
  if(length(a7) != 1 && length(unique(a8$Lead_Investor)) != 1){
    a6 <- c(a6, a7)
  }
}
inve_deal <- inve_deal[a6,]
## Extract the invest_deal data after 1990
a5 <- unique(inve_deal$Deal_Id)
a9 <- NULL
for (i in 1:length(a5)) {
  a10 <- which(deal_details$Deal_Id == a5[i])
  if(length(a10) != 0){
    a11 <- which(inve_deal$Deal_Id == a5[i])
    a9 <- c(a9, a11)
  }
}
inve_deal <- inve_deal[a9,]
df <- left_join(inve_deal, deal_details, by = "Deal_Id")
df <- left_join(df, comp_details, by = "CompanyID")
df <- df[-which(is.na(df$Deal_Size) == T),]
df <- df[order(df$Investor_Id),]
write.csv(df, "D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #5/result/1.csv",
          row.names = F)

tmp2 <- df
a12.1 <- sort(unique(tmp2$Deal_Date))
df_result <- NULL

df <- tmp2[which(tmp2$Deal_Date == a12.1[28]),]
a12 <- unique(df$Investor_Id)
inve_count_deal <- NULL
for (i in 1:length(a12)) {
  a13 <- length(which(df$Investor_Id == a12[i]))
  inve_count_deal <- c(inve_count_deal, a13)
}
inve_count_deal <- data.frame(Investor_Id = a12, 
                              count = inve_count_deal)
adj_wei <- NULL
for (i in 1:length(a12)) {
  a13 <- df[which(df$Investor_Id == a12[i]),]
  a14 <- a13$Deal_Id[which(a13$Lead_Investor == 1)]
  if(length(a14) == 0){
    a19 <- rep(0, length(a12))
  }else{
    a15 <- NULL
    for (j in 1:length(a14)) {
      a16 <- df[which(df$Deal_Id == a14[j]),]
    a16 <- a16$Investor_Id[which(a16$Lead_Investor == 0)]
    a15 <- c(a15, a16)
    }
    a17 <- as.data.frame(table(a15))
    a17$a15 <- as.character(a17$a15)
    colnames(a17) <- c("Investor_Id", "num")
    a18 <- left_join(inve_count_deal, a17, by = "Investor_Id")
    a18$num[which(is.na(a18$num) == T)] <- 0
    a19 <- a18$num/a18$count
  }
  adj_wei <- rbind(adj_wei, a19)
}
colnames(adj_wei) <- a12
rownames(adj_wei) <- a12
g1 <- graph_from_adjacency_matrix(adj_wei, 
                                  mode = c("directed"), 
                                  weighted = T)
plot(g1, layout = layout.fruchterman.reingold(g1), 
     vertex.color = 'steelblue', vertex.shape = 'circle',
     vertex.size = 5, vertex.label = NA,
     edge.arrow.size = 0.3, main = "2018")
a20 <- as.data.frame(power_centrality(g1, exponent = 0.75))
status <- data.frame(Investor_Id = rownames(a20), 
                     status = round(a20$`power_centrality(g1, exponent = 0.75)`,
                                    digits = 4))
df <- left_join(df, status, by = "Investor_Id")
df_result <- rbind(df_result, df)
write.csv(df_result, "D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #5/result/2.csv",
          row.names = F)

a12 <- unique(df_result$Investor_Id)
a21 <- NULL
for (i in 1:length(a12)) {
  a22 <- df_result[which(df_result$Investor_Id == a12[i]),]
  a23 <- sort(unique(a22$Deal_Date))
  a25 <- NULL
  for (j in 1:length(a23)) {
    a26 <- which(a22$Deal_Date == a23[j])
    if(j == 1){
      a27 <- a22[a26,]
    }else if(j > 1 && j <= 5){
      a27 <- a22[1:a26[length(a26)],]
    }else{
      a28 <- which(a22$Deal_Date == a23[j-5])
      a27 <- a22[(a28[length(a28)]+1):a26[length(a26)],]
    }
    a29 <- data.frame(Investor_Id = a12[i], 
                      Deal_Date = a23[j],
                      status = round(sum(a27$status), digits = 3))
    a25 <- rbind(a25, a29)
  }
  a21 <- rbind(a21, a25)
}
a30 <- sqldf("select Investor_Id, Deal_Date,count(1) from a21 group by Investor_Id, Deal_Date")
unique(a30$`count(1)`)
df_status <- a21
rm(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,
   a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,adj_wei,a12.1,tmp,
   inve_count_deal,df,status)

# 1.A
b1 <- unique(df_result$Investor_Id)
b2 <- NULL
for (i in 1:length(b1)) {
  b3 <- df_result[which(df_result$Investor_Id == b1[i]),]
  b4 <- sort(unique(b3$Deal_Date))
  b5 <- NULL
  for (j in 1:length(b4)) {
    ## Compute the hhi
    b6 <- b3[which(b3$Deal_Date == b4[j]),]
    b7 <- 0
    for (k in 1:nrow(b6)) {
      b8 <- (b6$Deal_Size[k]/sum(b3$Deal_Size))^2
      b7 <- b7 + b8
    }
    ## Whether a venture capital is originate/IT sector/early-stage
    b9 <- length(which(b6$Deal_Number == 1))
    if(b9/nrow(b6) >= 0.5){b10 <- 1}else{b10 <- 0}
    b11 <- length(which(b6$Primary_Industry_Sector == "Information Technology"))
    if(b11/nrow(b6) >= 0.5){b12 <- 1}else{b12 <- 0}
    b13 <- length(which(b6$Deal_Type_1 == "Early Stage VC")) +
      length(which(b6$Deal_Type_1 == "Accelerator/Incubator")) +
      length(which(b6$Deal_Type_1 == "Seed Round")) +
      length(which(b6$Deal_Type_1 == "Angel (individual)"))
    if(b13/nrow(b6) >= 0.5){b14 <- 1}else{b14 <- 0}
    b15 <- data.frame(Investor_Id = b1[i], Deal_Date = b4[j], 
                      hhi = round(b7, digits = 5), b10, b12, b14)
    b5 <- rbind(b5, b15)
  }
  b2 <- rbind(b2, b5)
}
colnames(b2) <- c("Investor_Id", "Deal_Date","hhi","is_originate",
                  "is_IT", "is_earlystage")
df1 <- data.frame(df_status, b2[,3:6])
plm_data_1 <- pdata.frame(df1, index = c("Investor_Id","Deal_Date"))
plm_model_1 <- plm(hhi~status+I(status^2)+is_originate+is_IT+is_earlystage,
                   data = plm_data_1, effect = "individual",
                   model = "within", 
                   index = c("Investor_Id", "Deal_Date"))
summary(plm_model_1)
rm(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15)

# 1.B
jacc = function(x,y){
  ja <- (length(union(x,y))-length(intersect(x,y)))/length(union(x,y))
  return(ja)
}
c1 <- unique(df_result$Investor_Id)
niche_width <- NULL
for (i in 1:length(c1)) {
  c2 <- df_result[which(df_result$Investor_Id == c1[i]),]
  c3 <- sort(unique(c2$Deal_Date))
  c8 <- NULL
  for (j in 1:length(c3)) {
    c4 <- c2[which(c2$Deal_Date == c3[j]),]
    c5 <- unique(c4$Primary_Industry_Code)
    if(nrow(c4) == 1){
      nw <- 0
    }else if(nrow(c4) != 1 && length(c5) == 1){
      nw <- 0
    }else if(nrow(c4) != 1 && length(c5) != 1){
      c7 <- 0
      for (k in 1:nrow(c4)) {
        c6 <- jacc(c4$Primary_Industry_Code[k], 
                   c4$Primary_Industry_Code)
        c7 <- c7 + c6
      }
      nw <- round(c7/(length(unique(c2$Primary_Industry_Code))+c7-1),
                  digits = 5)
    }
    c9 <- data.frame(Investor_Id = c1[i],
                     Deal_Date = c3[j],
                     nw = nw)
    c8 <- rbind(c8, c9)
  }
  niche_width <- rbind(niche_width, c8)
}
df2 <- data.frame(df1, nw = niche_width[,3])
plm_data_2 <- pdata.frame(df2, index = c("Investor_Id","Deal_Date"))
plm_model_2 <- plm(nw~status+I(status^2)+is_originate+is_IT+is_earlystage,
                   data = plm_data_2, effect = "individual",
                   model = "within", 
                   family = quasibinomial(link = "logit"),
                   index = c("Investor_Id", "Deal_Date"))
summary(plm_model_2)
rm(c1,c2,c3,c4,c5,c6,c7,c8,c9,niche_width)

# 1.C
plm_model_3 <-  plm(nw~status+I(status^2),
                    data = plm_data_2, effect = "individual",
                    model = "within", 
                    family = quasibinomial(link = "logit"),
                    index = c("Investor_Id", "Deal_Date"))
summary(plm_model_3)
set.seed(1234)
new_status <- data.frame(status = runif(100, 
                                        min = min(df2$status),
                                        max = max(df2$status)))
fit_value <- predict(plm_model_3, new_status)
new_df <- data.frame(new_status, fit_value)
ggplot(new_df, aes(x = status, y = fit_value)) +
  geom_ribbon(aes(ymin = fit_value-(sd(fit_value)*(1.96)), 
                  ymax = fit_value+(sd(fit_value)*1.96)),
              alpha = 0.2) +
  geom_line(colour = "steelblue", lwd = 1) +
  labs(title = "Plot of the Relationship between Status and Diversification",
       x = "Status", y = "Diversification")

# 2.A
d1 <- df_result[which(df_result$Deal_Type_1 == "Buyout/LBO"),]
d2 <- unique(d1$Investor_Id)
d7 <- NULL
for (i in 1:length(d2)) {
  d3 <- df2[which(df2$Investor_Id == d2[i]),]
  d4 <- d1[which(d1$Investor_Id == d2[i]),]
  if(nrow(d4) == 1){
    d5 <- d3[which(d3$Deal_Date == d4$Deal_Date),]
    d6 <- data.frame(d5, dn = d4$Deal_Number)
  }else{
    d5 <- d3[which(d3$Deal_Date == d4$Deal_Date[1]),]
    d6 <- data.frame(d5, dn = sum(d4$Deal_Number))
  }
  d7 <- rbind(d7, d6)
}
d11 <- NULL
for (i in 1:nrow(d7)) {
  d8 <- df2[which(df2$Investor_Id == d7$Investor_Id[i]),]
  d9 <- d8[which(d8$Deal_Date == d7$Deal_Date[i]),]
  d10 <- as.numeric(rownames(d9))
  d11 <- c(d11, d10)
}
dn <- rep(0, nrow(df2))
for (i in 1:length(d11)) {
  dn[d11[i]] <- d7$dn[i]
}
df3 <- data.frame(df2, dn = dn)
plm_data_4 <- pdata.frame(df3, index = c("Investor_Id","Deal_Date"))
plm_model_4 <- pglm(dn~status+nw+status:nw+is_originate+is_IT+is_earlystage,
                    data = plm_data_4, effect = "individual",
                    model = "within", 
                    family = poisson(link = "logit"),
                    index = c("Investor_Id", "Deal_Date"))
summary(plm_model_4)
rm(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,nw,dn)

# 2.B
plm_model_5 <- glm(dn~status+nw+status:nw, data = df3,
                    family = "poisson")
summary(plm_model_5)
set.seed(1234)
new_status_nw <- data.frame(status = runif(100, 
                                           min = min(df3$status),
                                           max = max(df3$status)),
                            nw = runif(100,
                                       min = min(df3$nw),
                                       max = max(df3$nw)))
fit_value <- predict(plm_model_5, new_status_nw)
new_df <- data.frame(new_status_nw, fit_value)
scatter3D(new_df$status, new_df$nw, new_df$fit_value,
          pch = 16)

# 3.A
e1 <- unique(df_result$Investor_Id)
cordinate <- NULL
for (i in 1:length(e1)) {
  e2 <- df_result[which(df_result$Investor_Id == e1[i]),]
  e3 <- sort(unique(e2$Deal_Date))
  e8 <- NULL
  for (j in 1:length(e3)) {
    e4 <- e2[which(e2$Deal_Date == e3[j]),]
    e6 <- NULL
    for (k in 1:nrow(e4)) {
      e5 <- jacc(e4$Primary_Industry_Sector[k], 
                 e2$Primary_Industry_Sector)
      e5 <- round(e5, digits = 4)
      e6 <- c(e6, e5)
    }
    e6 <- round(sum(e6), digits = 4)
    e7 <- data.frame(Investor_Id = e1[i],
                     Deal_Date = e3[j],
                     names = e4$Primary_Industry_Sector[1],
                     jdis = e6)
    e8 <- rbind(e8, e7)
  }
  if(nrow(e8) == 1){
    e8 <- rbind(e8,e8,e8)
    set.seed(1234)
    e8$jdis <- e8$jdis + runif(3, 0, 0.01)
    e9 <- cmdscale(dist(e8$jdis))
    e9 <- t(e9[1,])
    e8 <- e8[1,]
  }else if(nrow(e8) < 3 || length(unique(e8$jdis)) == 1 || length(unique(e8$jdis))<nrow(e8)){
    e11 <- NULL
    for (k in 1:nrow(e8)) {
      e10 <- rbind(e8[k,], e8[k,])
      set.seed(1234)
      e10$jdis <- e10$jdis + runif(2, 0, 0.01)
      e11 <- rbind(e11, e10)
    }
    e9 <- cmdscale(dist(e11$jdis))
    e13 <- NULL
    for (k in 1:nrow(e8)) {
      e12 <- e9[2*k,]
      e13 <- rbind(e13,e12)
    }
    e9 <- e13
  }else{
    e9 <- cmdscale(dist(e8$jdis))
  }
  e10 <- data.frame(e8[,1:3], e9)
  cordinate <- rbind(cordinate, e10)
}

e14 <- unique(df_result$Primary_Industry_Sector)
e19 <- NULL
for (i in 1:length(e14)) {
  e15 <- df_result[df_result$Primary_Industry_Sector == e14[i],]
  e16 <- sqldf("select Investor_Id, Deal_Date, count(1) from e15 group by Investor_Id,Deal_Date")
  e17 <- e16[which(e16$`count(1)` == max(e16$`count(1)`)),]
  e18 <- data.frame(e17[1,], e14[i])
  e19 <- rbind(e19, e18)
}
e23 <- NULL
for (i in 1:nrow(e19)) {
  e20 <- cordinate[which(cordinate$Investor_Id == e19$Investor_Id[i]),]
  e21 <- e20[which(e20$Deal_Date == e19$Deal_Date[i]),3:4]
  e22 <- data.frame(e21, e19$e14.i.[i])
  colnames(e22) <- c("X1","X2","names")
  e23 <- rbind(e23, e22)
}
e23$X1 <- round(e23$X1, digits = 4)
cordinate$X1 <- round(cordinate$X1, digits = 4)
e23$X2 <- round(e23$X2, digits = 9)
cordinate$X2 <- round(cordinate$X2, digits = 9)
e24 <- left_join(cordinate, e23, by = "names")
e25 <- sqrt((e24$X1.x-e24$X1.y)^2 + (e24$X2.x-e24$X2.y)^2)
e26 <- NULL
for (i in 1:length(e1)) {
  e2 <- df_result[which(df_result$Investor_Id == e1[i]),]
  e3 <- sort(unique(e2$Deal_Date))
  e8 <- NULL
  for (j in 1:length(e3)) {
    e4 <- e2[which(e2$Deal_Date == e3[j]),]
    e6 <- NULL
    for (k in 1:nrow(e4)) {
      e5 <- jacc(e4$Primary_Industry_Sector[k], 
                 e2$Primary_Industry_Sector)
      e5 <- round(e5, digits = 4)
      e6 <- c(e6, e5)
    }
    e6 <- round(sum(e6), digits = 4)
    e7 <- data.frame(Investor_Id = e1[i],
                     Deal_Date = e3[j],
                     names = e4$Primary_Industry_Sector[1],
                     jdis = e6)
    e8 <- rbind(e8, e7)
  }
  e26 <- rbind(e26, e8)
}
df4 <- data.frame(df3, dis = e25,jdis = e26$jdis)
plm_data_6 <- pdata.frame(df4, index = c("Investor_Id","Deal_Date"))
plm_model_6 <- plm(dis~status+jdis+status:jdis+is_originate+is_IT+is_earlystage,
                   data = plm_data_6, effect = "individual",
                   model = "within", 
                   index = c("Investor_Id", "Deal_Date"))
summary(plm_model_6)
