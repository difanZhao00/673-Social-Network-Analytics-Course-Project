library(PearsonDS)
library(sqldf)
library(ggplot2)
library(stringr)
library(plm)
library(pglm)

rm(list = ls())
setwd("D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #3")

# Load data
election <- read.csv("election_results.csv", header = T)
rainfall <- read.csv("monthly_rainfall.csv", header = T)
border <- read.csv("border_information.csv", header = T)
head(election)
head(rainfall)
head(border)

# 1.
## Data preprocess
## Delete the data in rainfall out of 1951~1999
year_set <- as.character(1951:1999)
rainfall_1 <- NULL
for (i in 1:length(year_set)) {
  a1 <- rainfall[which(str_sub(rainfall$time, 1, 4) == year_set[i]),]
  rainfall_1 <- rbind(rainfall_1, a1)
}
unique(str_sub(rainfall_1$time,1,4))

## Delete the different district between rainfall and election
a2 <- c(unique(election$district), unique(rainfall_1$district))
a2 <- data.frame(a2)
colnames(a2) <- "district"
a3 <- sqldf("select district,count(1) from a2 group by district")
a4 <- a3$district[which(a3$`count(1)` == 2)]
election_1 <- NULL
for (i in 1:length(a4)) {
  a5 <- election[which(election$district == a4[i]),]
  election_1 <- rbind(election_1, a5)
}
rainfall_2 <- NULL
for (i in 1:length(a4)) {
  a6 <- rainfall_1[which(rainfall_1$district == a4[i]),]
  rainfall_2 <- rbind(rainfall_2, a6)
}

## Calculate the SPI
ggplot(rainfall_2, aes(rainfall)) +
  geom_histogram(bins = 30, fill = 'steelblue') +
  labs(x = 'Rainfall', y = 'Count',
       title = 'The Histogram of Rainfall from 1951 to 1999')

rainfall_2 <- rainfall_2[,2:4]
rainfall_2$time <- str_sub(as.character(rainfall_2$time), 1, 4)
district_names <- unique(rainfall_2$district)
SPI_it <- NULL
for (i in 1:length(district_names)) {
  a7 <- rainfall_2[which(rainfall_2$district == district_names[i]),]
  a8 <- unique(a7$time)
  a9 <- NULL
  for (j in 1:length(a8)) {
    a10 <- mean((a7[which(a7$time == a8[j]),])$rainfall)
    a9 <- c(a9, a10)
  }
  scale_1 <- var(a9)/mean(a9)
  shape_1 <- (mean(a9))^2/var(a9)
  prod_1 <- cumsum(a9)/sum(a9)
  prod_1[which(prod_1 == 1)] <- 0.9999
  a11 <- qpearsonIII(prod_1, scale = scale_1, 
                     location = 0, shape = shape_1)
  spi_1 <- round((a11-mean(a11))/sd(a11), digits = 3)
  a12 <- data.frame(rep(district_names[i], length(a10)), a8, spi_1)
  SPI_it <- rbind(SPI_it, a12)
}
colnames(SPI_it) <- c("district", "year", "SPI")
SPI_it <- data.frame(SPI_it)

# 1.A
election_1 <- election_1[,c(2,3,7,8,10)]
avg_spi_num_par <- NULL
for (i in 1:length(district_names)) {
  a13 <- election_1[which(election_1$district == district_names[i]),]
  a14 <- SPI_it[which(SPI_it$district == district_names[i]),]
  a15 <- sort(unique(a13$year))
  a16 <- NULL
  for (j in 1:length(a15)) {
    avg_spi <- round(mean((a14[1:which(a14$year == a15[j]),])$SPI),
                     digits = 3)
    num_par <- length(unique((a13[which(a13$year == a15[j]),])$party_name))
    avg_num <- data.frame(district_names[i], a15[j], avg_spi, num_par)
    a16 <- rbind(a16, avg_num)
  }
  avg_spi_num_par <- rbind(avg_spi_num_par, a16)
}
colnames(avg_spi_num_par) <- c("district", "year", "avg_spi", "num_par")

ggplot(avg_spi_num_par, aes(avg_spi, num_par)) +
  geom_point(color = "steelblue") +
  labs(x = 'Average SPI', y = 'Number of Political Parties',
       title = 'Scatter Plot of Average SPI and Number of Political Parties')

# 1.B
## Border Process
tmp1 <- c(unique(election_1$district), unique(border$focal_district))
tmp1 <- data.frame(tmp1)
colnames(tmp1) <- "district"
tmp2 <- sqldf("select district,count(1) from tmp1 group by district")
tmp3 <- tmp2$district[which(tmp2$`count(1)` == 2)]
tmp4 <- NULL
for (i in 1:length(tmp3)) {
  tmp5 <- border[which(border$focal_district == tmp3[i]),]
  tmp4 <- rbind(tmp4,tmp5)
}
tmp6 <- unique(tmp4$district)
tmp7 <- NULL
for (i in 1:length(tmp6)) {
  tmp8 <- length(which(SPI_it$district == tmp6[i]))
  if(tmp8 == 0){
    tmp7 <- c(tmp7, tmp6[i])
  }
}
tmp9 <- NULL
for (i in 1:length(tmp7)) {
  tmp10 <- which(tmp4$district == tmp7[i])
  tmp9 <- c(tmp9,tmp10)
}
tmp4 <- tmp4[-tmp9,]
border_1 <- tmp4
colnames(border_1) <- c("district1", "district2")

spi_lag_nei <- NULL
for (i in 1:length(district_names)) {
  a19 <- election_1[which(election_1$district == district_names[i]),]
  a20 <- SPI_it[SPI_it$district == district_names[i],]
  a21 <- as.character(unique(a19$year))
  lag_spi_result <- NULL
  for (j in 1:length(a21)) {
    a22 <- border_1$district2[which(border_1$district1 == district_names[i])]
    lag1 <- round(mean(a20$SPI[1:which(a20$year == a21[j])]),
                  digits = 3)
    if(length(a22) == 0){
      lag2 <- 0
    }else{
      lag2 <- NULL
      for (k in 1:length(a22)) {
        a23 <- SPI_it[SPI_it$district == a22[k],]
        a24 <- round(mean(a23$SPI[1:which(a23$year == a21[j])]),
                     digits = 3)
        lag2 <- c(lag2, a24)
      }
    }
    lags <- data.frame(district_names[i], a21[j],
                       a20$SPI[which(a20$year == a21[j])],
                       lag1, round(mean(lag2),digits = 3))
    lag_spi_result <- rbind(lag_spi_result, lags)
  }
  spi_lag_nei <- rbind(spi_lag_nei, lag_spi_result)
}
colnames(spi_lag_nei) <- c("district", "year", "SPI", 
                              "lag_SPI", "lag_nei_avg_SPI")

plm_data_1 <- pdata.frame(spi_lag_nei, index = c("district","year"))
plm_model_1 <- plm(SPI~lag_SPI+lag_nei_avg_SPI,
                   data = spi_lag_nei, effect = "twoways",
                   model = "within", index = "district")
summary(plm_model_1)

# 1.C
num_extreme <- NULL
for (i in 1:length(district_names)) {
  a25 <- election_1[which(election_1$district == district_names[i]),]
  a26 <- SPI_it[SPI_it$district == district_names[i],]
  a27 <- as.character(unique(a25$year))
  num_lag_result <- NULL
  for (j in 1:length(a27)) {
    a28 <- border_1$district2[which(border_1$district1 == district_names[i])]
    a29 <- a26$SPI[1:which(a26$year == a27[j])]
    num_lag1 <- length(which(a29 >1)) + length(which(a29 < -1))
    if(length(a28) == 0){
      num_lag2 <- 0
    }else{
      num_lag2 <- NULL
      for (k in 1:length(a28)) {
        a30 <- SPI_it[SPI_it$district == a28[k],]
        a31 <- a30$SPI[1:which(a30$year == a27[j])]
        a32 <- length(which(a31 > 1)) + length(which(a31 < -1))
        num_lag2 <- c(num_lag2, a32)
      }
    }
    if(j == 1){
      a33 <- a26$SPI[which(a26$year == a27[j])]
    }else{
      a33 <- a26$SPI[which(a26$year == a27[j-1]):which(a26$year == a27[j])]
    }
    num_lags <- data.frame(district_names[i], a27[j],
                           length(which(a33 > 1)) + length(which(a33 < -1)),
                           num_lag1, round(mean(num_lag2),digits = 0))
    num_lag_result <- rbind(num_lag_result, num_lags)
  }
  num_extreme <- rbind(num_extreme, num_lag_result)
}
colnames(num_extreme) <- c("district", "year", "num_extreme", 
                           "lag_num", "lag_nei_avg_num")

plm_data_2 <- pdata.frame(num_extreme, index = c("district","year"))
plm_model_2 <- pglm(num_extreme ~ lag_num + lag_nei_avg_num,
                   data = num_extreme, effect = "twoways",
                   model = "within", index = "district", 
                   family = "poisson")
summary(plm_model_2)
rm(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a19,a20,a21,
   a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32,a33,tmp1,tmp2,tmp3,tmp4,
   tmp5,tmp6,tmp7,tmp8,tmp9,tmp10,avg_spi,lag_spi_result,lag1,lag2,
   num_lag1,num_lag2,num_par,prod_1,scale_1,shape_1,year_set,spi_1,
   num_lags,num_lag_result,lags,avg_num)

# 2.A
num_par_extreme <- data.frame(avg_spi_num_par$district,
                              avg_spi_num_par$year,
                              avg_spi_num_par$num_par,
                              num_extreme$num_extreme)
colnames(num_par_extreme) <- c("district", "year", 
                               "num_par", "num_extreme")
plm_data_3 <- pdata.frame(num_par_extreme, index = c("district","year"))
plm_model_3 <- pglm(num_par ~ num_extreme,
                    data = num_par_extreme, effect = "twoways",
                    model = "within", index = "district", 
                    family = "poisson")
summary(plm_model_3)

# 2.B
num_lag_two <- NULL
for (i in 1:length(district_names)) {
  b1 <- election_1[which(election_1$district == district_names[i]),]
  b2 <- SPI_it[SPI_it$district == district_names[i],]
  b3 <- as.character(unique(b1$year))
  num_lag_result <- NULL
  for (j in 1:length(b3)) {
    b4 <- border_1$district2[which(border_1$district1 == district_names[i])]
    if(length(b4) == 0){
      lag_nei_two <- 0
    }else{
      num_lag_2 <- NULL
      for (k in 1:length(b4)) {
        b5 <- SPI_it[SPI_it$district == b4[k],]
        if(j == 1){
          b6 <- b5$SPI[which(b5$year == b3[j])]
          b7 <- length(which(b6 > 1)) + length(which(b6 < -1))
          num_lag_2 <- c(num_lag_2, b7)
        }else if(j == 2){
          b6 <- b5$SPI[1:which(b5$year == b3[j])]
          b7 <- length(which(b6 > 1)) + length(which(b6 < -1))
          num_lag_2 <- c(num_lag_2, b7)
        }else{
          b6 <- b5$SPI[which(b5$year == b3[j-1]):which(b5$year == b3[j])]
          b7 <- length(which(b6 > 1)) + length(which(b6 < -1))
          num_lag_2 <- c(num_lag_2, b7)
        }
      }
    }
    num_lags_2 <- data.frame(district_names[i], b3[j], 
                             
                             round(mean(num_lag_2), digits = 0))
    num_lag_result <- rbind(num_lag_result, num_lags_2)
  }
  num_lag_two <- rbind(num_lag_two, num_lag_result)
}
colnames(num_lag_two) <- c("district", "year", "lag_2_nei_num")

num_par_extreme_2 <- data.frame(avg_spi_num_par$district,
                                avg_spi_num_par$year,
                                avg_spi_num_par$num_par,
                                num_extreme$num_extreme,
                                num_lag_two$lag_2_nei_num)
colnames(num_par_extreme_2) <- c("district", "year", "num_par", 
                                 "num_extreme", "lag_2_nei_num")
plm_data_4 <- pdata.frame(num_par_extreme_2, 
                          index = c("district","year"))
plm_model_4 <- pglm(num_par ~ num_extreme + lag_2_nei_num,
                    data = num_par_extreme_2, effect = "twoways",
                    model = "within", index = "district", 
                    family = "poisson")
summary(plm_model_4)
rm(b1,b2,b3,b4,b5,b6,b7,lag_nei_two,num_lag_2)

# 3.
hhi_result <- NULL
for (i in 1:length(district_names)) {
  c1 <- election_1[which(election_1$district == district_names[i]),]
  c2 <- as.character(unique(c1$year))
  hhi_3 <- NULL
  for (j in 1:length(c2)) {
    c3 <- c1[which(c1$year == c2[j]),]
    vote_share <- prop.table(c3$vote_count)
    hhi_1 <- round(sum((vote_share)^2), digits = 3)
    hhi_2 <- data.frame(district_names[i], c2[j], hhi_1)
    hhi_3 <- rbind(hhi_3,hhi_2)
  }
  hhi_result <- rbind(hhi_result, hhi_3)
}
colnames(hhi_result) <- c("district", "year", "hhi")

hhi_extreme <- data.frame(hhi_result$district,
                          hhi_result$year,
                          hhi_result$hhi,
                          num_extreme$num_extreme,
                          num_lag_two$lag_2_nei_num)
colnames(hhi_extreme) <- c("district", "year", "hhi", 
                           "num_extreme", "lag_2_nei_num")
plm_data_5 <- pdata.frame(hhi_extreme, index = c("district","year"))
plm_model_5 <- plm(hhi ~ num_extreme + lag_2_nei_num,
                   data = hhi_extreme, effect = "twoways",
                   model = "within", index = "district")
summary(plm_model_5)
rm(c1,c2,c3,hhi_1,hhi_2,hhi_3,vote_share)

