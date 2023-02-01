library(RSiena)
library(lubridate)
library(stringr)
library(dplyr)
library(sqldf)
library(mice)
library(geosphere)
library(survival)
library(nnet)
library(plm)

rm(list = ls())

# 1.
# Load data
setwd("D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #6")

execs <- read.csv("execs.csv", header = T)
ind_inv <- read.csv("individual_investors.csv", header = T)
deal_details <- read.csv("deal_details.csv", header = T)
inv_details <- read.csv("investor_details.csv", header = T)
comp_details <- read.csv("company_details.csv", header = T)
people <- read.csv("people.csv", header = T)
rep_name <- read.csv("representative_names.csv", header = T)
load("edges_dist.Rdata")
head(execs)
head(ind_inv)
head(deal_details)
head(inv_details)
head(comp_details)
head(people)
head(rep_name)
head(edges_dist)
colnames(comp_details)[1] <- "CompanyId"

# Data preprocess
## Extra the company data with type "Venture Capital"
inv_details <- inv_details[which(inv_details$Investor_Type == "Venture Capital"),]
ind_inv <- filter(ind_inv, 
                  ind_inv$InvestorId %in% inv_details$InvestorId)
## Extra the execs data with full title "Founder","CEO" and "Managing Director"
a1 <- c(which(execs$Full.Title == "Founder"),
        which(execs$Full.Title == "Chief Executive Officer"),
        which(execs$Full.Title == "Managing Director"))
execs <- execs[a1,]
ind_inv <- filter(ind_inv, ind_inv$PersonId %in% execs$PersonId)
## Extra the deal data after 2000s
deal_details$Deal_Date <- dmy(deal_details$Deal_Date)
deal_details <- deal_details[order(deal_details$Deal_Date),]
a2 <- which(str_sub(deal_details$Deal_Date, 1, 4) == "2000")
deal_details <- deal_details[-(1:a2[1]),]
deal_details <- deal_details[,c(1,2,4)]
md.pattern(deal_details)
deal_details <- deal_details[-which(is.na(deal_details$Deal_Date) == T),]
md.pattern(deal_details)
ind_inv <- left_join(ind_inv, deal_details, by = "DealId")
md.pattern(ind_inv)
ind_inv <- ind_inv[-which(is.na(ind_inv$Deal_Date) == T),]
all.equal(ind_inv$CompanyId.x, ind_inv$CompanyId.y)
ind_inv <- ind_inv[,c(1,3,4,5,8)]
colnames(ind_inv)[2] <- "CompanyId"
## Check the if Investor.Id is same as Person.Id
length(which(ind_inv$PersonId %in% ind_inv$InvestorId))
length(which(ind_inv$InvestorId %in% ind_inv$PersonId))
## Add information of city and industry group
comp_details <- comp_details[,c(1,7,11)]
ind_inv <- left_join(ind_inv, comp_details, by = "CompanyId")
## Add information of gender of Investors and Entrepreneurs
a3 <- as.data.frame(str_c(ind_inv$InvestorId, "P"))
colnames(a3) <- "PersonId"
a4 <- ind_inv
a3 <- left_join(a3, people[,c(1,14)], by = "PersonId")
a3$Gender[which(is.na(a3$Gender) == T)] <- "Male"
a3$Gender[which(a3$Gender == "Male")] <- 1
a3$Gender[which(a3$Gender == "Female")] <- 0
a4 <- left_join(a4, people[,c(1,14)], by = "PersonId")
a4$Gender[which(a4$Gender == "")] <- 0
a4$Gender[which(a4$Gender == "Male")] <- 1
a4$Gender[which(a4$Gender == "Female")] <- 0
## Add information of top school of Investors and Entrepreneurs
a3 <- left_join(a3, people[,c(1,16)], by = "PersonId")
a4 <- left_join(a4, people[,c(1,16)], by = "PersonId")
MBA.i <- str_detect(a3$Education, "MBA")
MBA.i[which(MBA.i == T)] <- 1
MBA.i[-which(MBA.i == T)] <- 0
MBA.p <- str_detect(a4$Education, "MBA")
MBA.p[which(MBA.p == T)] <- 1
MBA.p[-which(MBA.p == T)] <- 0
PHD.i <- str_detect(a3$Education, "Ph.D")
ENG.i <- str_detect(a3$Education, "Engineering")
a5 <- c(which(PHD.i == T), which(ENG.i == T))
a5 <- unique(a5)
PHD.i[a5] <- 1
PHD.i[-a5] <- 0
PHD.p <- str_detect(a4$Education, "Ph.D")
ENG.p <- str_detect(a4$Education, "Engineering")
a5 <- c(which(PHD.p == T), which(ENG.p == T))
a5 <- unique(a5)
PHD.p[a5] <- 1
PHD.p[-a5] <- 0
topschoolnames <- c("Harvard", "Yale", "Princeton", "Columbia",
                    "University of Pennsylvania", "Dartmouth College",
                    "Brown", "Cornell", "Caltech","Chicago","MIT",
                    "Stanford", "Cambridge", "Oxford")
a7 <- NULL
for (i in 1:length(topschoolnames)) {
  a6 <- which(str_detect(a3$Education, topschoolnames[i]) == T)
  a7 <- c(a7, a6)
}
a7 <- unique(a7)
topschool.i <- rep(0, nrow(a3))
topschool.i[a7] <- 1
a7 <- NULL
for (i in 1:length(topschoolnames)) {
  a6 <- which(str_detect(a4$Education, topschoolnames[i]) == T)
  a7 <- c(a7, a6)
}
a7 <- unique(a7)
topschool.p <- rep(0, nrow(a4))
topschool.p[a7] <- 1
a3 <- data.frame(a3[,1:2], MBA.i = MBA.i, PHD.i = PHD.i, 
                 topschool.i = topschool.i)
a4 <- data.frame(a4[,1:8], MBA.p = MBA.p, PHD.p = PHD.p,
                 topschool.p = topschool.p)
## Add information of geo
cord.inv <- NULL
for (i in 1:nrow(a4)) {
  a8 <- which(edges_dist$InvestorId == a4$InvestorId[i])
  if(length(a8) == 0){
    cord.inv <- rbind(cord.inv, c(0,0))
    colnames(cord.inv) <- colnames(edges_dist)[5:6]
  }else{
    cord.inv <- rbind(cord.inv, edges_dist[a8[1],5:6])
  }
}
cord.com <- NULL
for (i in 1:nrow(a4)) {
  a9 <- which(edges_dist$CompanyId == a4$CompanyId[i])
  if(length(a9) == 0){
    cord.com <- rbind(cord.com, c(0,0))
    colnames(cord.com) <- colnames(edges_dist)[3:4]
  }else{
    cord.com <- rbind(cord.com, edges_dist[a9[1],3:4])
  }
}
cordd <- data.frame(cord.com, cord.inv)
a4 <- data.frame(a4, cordd)
## Add information of Experience
exep.i <- NULL
exep.p <- NULL
for (i in 1:nrow(a4)) {
  a10 <- deal_details$Deal_Date[which(deal_details$CompanyId == a4$CompanyId[i])]
  a10 <- 2018-(as.numeric(sort(str_sub(a10, 1, 4))))[1]
  exep.i <- c(exep.i, a10)
  a11 <- execs$CompanyId[which(execs$PersonId == a4$PersonId[i])]
  a11 <- deal_details$Deal_Date[which(deal_details$CompanyId == a11)]
  a11 <- 2018-as.numeric(sort(str_sub(a11, 1, 4)))[1]
  exep.p <- c(exep.p, a11)
}
exep.p[which(is.na(exep.p))] <- round(mean(exep.p, na.rm = T), 
                                      digits = 0)
a4 <- data.frame(a4, exep.i, exep.p)
## Add information of the city of firms
a12 <- sort(table(comp_details$City), decreasing = T)[1:11]
a12 <- as.data.frame(a12)
a12 <- as.character(a12$Var1[2:11])
a4$City[which(a4$City %in% a12)] <- 1
a4$City[which(a4$City != 1)] <- 0
## Add information of the invested number of each entrepreneur
numinv <- NULL
for (i in 1:nrow(a4)) {
  a13 <- execs$CompanyId[which(execs$PersonId == a4$PersonId[i])]
  a13 <- length(deal_details$Deal_Date[which(deal_details$CompanyId == a13)])
  numinv <- c(numinv, a13)
}

result_ind_inv <- data.frame(a4[,c(1,3,5,6)],
                             city = as.numeric(a4$City),
                             gender.i = as.numeric(a3$Gender),
                             gender.p = as.numeric(a4$Gender),
                             MBA.i = a3$MBA.i, MBA.p = a4$MBA.p,
                             PHD.i = a3$PHD.i, PHD.p = a4$PHD.p,
                             topschool.i = a3$topschool.i,
                             topschool.p = a4$topschool.p,
                             a4[,12:17], numinv = numinv)
write.csv(result_ind_inv, 
          "D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #6/result_q1.csv",
          row.names = F)
rm(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,MBA.i,MBA.p,PHD.i,PHD.p,
   topschool.i,topschool.p,topschoolnames,exep.i,exep.p,ENG.i,ENG.p,
   cordd,cord.com,cord.inv,numinv)

## Choose the industry
industryname <- unique(result_ind_inv$Primary_Industry_Group)
SieData <- result_ind_inv[which(result_ind_inv$Primary_Industry_Group %in% industryname[25]),]
SieData$Deal_Date <- str_sub(SieData$Deal_Date, 1, 4)
SieData <- SieData[order(SieData$Deal_Date),]

numinvstor <- unique(SieData$InvestorId)
b1 <- NULL
for (i in 1:length(numinvstor)) {
  b2 <- which(SieData$InvestorId == numinvstor[i])
  b1 <- c(b1, b2[1])
}
numinvstor <- data.frame(numinvstor, b1)
numpersons <- unique(SieData$PersonId)
b1 <- NULL
for (i in 1:length(numpersons)) {
  b2 <- which(SieData$PersonId == numpersons[i])
  b1 <- c(b1, b2[1])
}
numpersons <- data.frame(numpersons, b1)

## Define node sets
investors <- sienaNodeSet(nrow(numinvstor), nodeSetName = "investors")
persons <- sienaNodeSet(nrow(numpersons), nodeSetName = "persons")

## Define coDyaCovar
### Gender2
gender <- outer(SieData$gender.i[numinvstor$b1], 
                SieData$gender.p[numpersons$b1], FUN = "+")
gender[which(gender == 1)] <- 3
gender[which(gender == 0)] <- 1
gender[which(gender == 2)] <- 1
gender[which(gender == 3)] <- 0
genderCor <- coDyadCovar(gender, nodeSets = c("investors", "persons"),
                         type = "bipartite")
### Top School2
topschool <- outer(SieData$topschool.i[numinvstor$b1],
                   SieData$topschool.p[numpersons$b1], FUN = "*")
topschoolCor <- coDyadCovar(topschool, 
                            nodeSets = c("investors", "persons"),
                            type = "bipartite")
### Geodist2
b3 <- SieData[numinvstor$b1, 16:17]
b4 <- SieData[numpersons$b1, 14:15]
geodist <- distm(as.matrix(cbind(b3$inv_lon, b3$inv_lat)), 
                 as.matrix(cbind(b4$comp_lon,b4$comp_lat)), 
                 fun = distGeo)
geodistCor <- coDyadCovar(geodist, nodeSets = c("investors", "persons"),
                          type = "bipartite")
### Experience2
experience <- outer(SieData$exep.i[numinvstor$b1],
                    SieData$exep.p[numpersons$b1], FUN = "-")
experience <- experience - max(experience)
experience <- abs(experience)
experience <- round(scale(experience), digits = 2)
experienceCor <- coDyadCovar(experience, 
                             nodeSets = c("investors", "persons"),
                             type = "bipartite")
### comskill2
comskill <- outer(SieData$MBA.i[numinvstor$b1],
                  SieData$PHD.p[numpersons$b1], FUN = "*")
comskillCor <- coDyadCovar(comskill, 
                           nodeSets = c("investors", "persons"),
                           type = "bipartite")
### Gender1
gender.p <- SieData$gender.p[numpersons$b1]
genderpCor <- coCovar(gender.p, nodeSet = "persons")
### Top school1
topschool.p <- SieData$topschool.p[numpersons$b1]
topschoolpCor <- coCovar(topschool.p, nodeSet = "persons")
### city
city <- SieData$city[numpersons$b1]
citypCor <- coCovar(city, nodeSet = "persons")
### experience
experience.p <- SieData$exep.p[numpersons$b1]
expepCor <- coCovar(experience.p, nodeSet = "persons")
### is MBA
MBA.p <- SieData$MBA.p[numpersons$b1]
MBApCor <- coCovar(MBA.p, nodeSet = "persons")
### is PHD
PHD.p <- SieData$PHD.p[numpersons$b1]
PHDpCor <- coCovar(PHD.p, nodeSet = "persons")
### num of invests
numinv.p <- SieData$numinv[numpersons$b1]
numinvpCor <- coCovar(numinv.p, nodeSet = "persons")

## Define dependent data
numwave <- unique(SieData$Deal_Date)
for (i in 1:length(numwave)) {
  b5 <- SieData[which(SieData$Deal_Date == numwave[i]),]
  adj <- matrix(0, nrow(numinvstor), nrow(numpersons))
  rownames(adj) <- numinvstor$numinvstor
  colnames(adj) <- numpersons$numpersons
  b8 <- NULL
  for (j in 1:nrow(numinvstor)) {
    b6 <- which(rownames(adj) == b5$InvestorId[j])
    b7 <- which(colnames(adj) == b5$PersonId[j])
    adj[b6, b7] <- 1
    b8 <- c(b8, b6)
  }
  b8 <- unique(b8)
  adj[-b8,] <- 0
  adj.1 <- matrix(0, nrow(numinvstor), nrow(numpersons))
  rownames(adj) <- rownames(adj.1)
  colnames(adj) <- colnames(adj.1)
  assign(paste("SieData.w",i,sep = ""), adj)
}

SieData.w1 <- read.csv("D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #6/w1_q1_9.csv")
SieData.w2 <- read.csv("D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #6/w2_q1_9.csv")

SieDataD <- sienaDependent(array(c(SieData.w1,SieData.w2),
                                 dim = c(nrow(numinvstor), 
                                         nrow(numpersons), 2)),
                           "bipartite",
                           nodeSet = c("investors", "persons"))
bipData <- sienaDataCreate(SieDataD, genderCor, topschoolCor,
                           experienceCor, comskillCor, genderpCor,
                           topschoolpCor, citypCor, expepCor, 
                           MBApCor, PHDpCor, numinvpCor,
                           nodeSets = list(investors, persons))
bipEffects <- getEffects(bipData)
print01Report(bipData, 
              modelname = "D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #6/result/IT")
# Specify the model
bipEffects <- includeEffects(bipEffects, cycle4, outActSqrt, 
                             inPopSqrt, outInAss, name = "SieDataD")
bipEffects <- includeEffects(bipEffects, X, name = "SieDataD",
                             interaction1 = "genderCor")
bipEffects <- includeEffects(bipEffects, X, name = "SieDataD",
                             interaction1 = "topschoolCor")
#bipEffects <- includeEffects(bipEffects, X, name = "SieDataD",
#                             interaction1 = "geodistCor")
bipEffects <- includeEffects(bipEffects, X, name = "SieDataD",
                             interaction1 = "experienceCor")
bipEffects <- includeEffects(bipEffects, X, name = "SieDataD",
                             interaction1 = "comskillCor")

bipEffects <- includeEffects(bipEffects, altX, name = "SieDataD",
                             interaction1 = "genderpCor")
bipEffects <- includeEffects(bipEffects, altX, name = "SieDataD",
                             interaction1 = "topschoolpCor")
bipEffects <- includeEffects(bipEffects, altX, name = "SieDataD",
                             interaction1 = "citypCor")
bipEffects <- includeEffects(bipEffects, altX, name = "SieDataD",
                             interaction1 = "expepCor")
bipEffects <- includeEffects(bipEffects, altX, name = "SieDataD",
                             interaction1 = "MBApCor")
bipEffects <- includeEffects(bipEffects, altX, name = "SieDataD",
                             interaction1 = "PHDpCor")
bipEffects <- includeEffects(bipEffects, altX, name = "SieDataD",
                             interaction1 = "numinvpCor")
# create algorithm object:
bipModel <- sienaAlgorithmCreate(
  projname = 'D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #6/result/IT_outcomes')
# estimate model:
bipResults <- siena07(bipModel, data = bipData, effects = bipEffects)
bipResults
bipResults <- siena07(bipModel, data = bipData, effects = bipEffects,
                      prevAns = bipResults, returnDeps = TRUE)
bipResults

# 2.
rm(list = ls())
setwd("D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #6/data/Q2")

# 2.1
ind_inv_out <- read.csv("individual_investor_outcomes.csv", header = T)
survobj1 <- with(ind_inv_out, Surv(year, out_of_business))
fit1 <- coxph(survobj1 ~ l4c_scaled + gender + ethnicity + age_diff +
                geo_dist + ivyplus + complementarity + male_exec +
                nonwhite_exec + ivyplus_exec, data = ind_inv_out)
summary(fit1)

c1 <- which(ind_inv_out$successful_investments == 0)
ind_inv_out$successful_investments[-c1] <- 1
survobj2 <- with(ind_inv_out, Surv(year, successful_investments))
fit2 <- coxph(survobj2 ~ l4c_scaled + gender + ethnicity + age_diff +
                geo_dist + ivyplus + complementarity + male_exec +
                nonwhite_exec + ivyplus_exec, data = ind_inv_out)
summary(fit2)

# 2.2
status_out <- read.csv("startup_states.csv", header = T)
c2 <- which(status_out$company_state == "")
status_out <- status_out[-c2,]
status_out$company_state[which(status_out$company_state == "Exit")] <- 1
status_out$company_state[which(status_out$company_state == "Generating revenue")] <- 2
status_out$company_state[which(status_out$company_state == "Profitable")] <- 3
status_out$company_state[which(status_out$company_state == "Not profitable")] <- 4
status_out$company_state[which(status_out$company_state == "Startup")] <- 5
status_out$company_state[which(status_out$company_state == "Out of business")] <- 6

fit3 <- multinom(company_state ~ l4c_scaled + gender + ethnicity + 
                   age_diff + geo_dist + ivyplus + complementarity + 
                   male_exec + nonwhite_exec + ivyplus_exec, 
                 data = status_out)
summary(fit3)
z_value <- summary(fit3)$coefficients/summary(fit3)$standard.errors
p_value <- (1 - pnorm(abs(z_value), 0, 1)) * 2
summary(fit3)$coefficients

# 2.3
indus_out <- read.csv("industry_outcomes.csv", header = T)
plm_data <- pdata.frame(indus_out, index = c("industry", "year"))
fit4 <- plm(technical_exec ~ l4c_scaled + gender + ethnicity + 
              age_diff + geo_dist + ivyplus + outdegree + complementarity,
            data = plm_data, effect = "individual",
            model = "within", 
            index = c("industry", "year"))
summary(fit4)
fit5 <- plm(mba_exec ~ l4c_scaled + gender + ethnicity + age_diff + 
              geo_dist + ivyplus + outdegree + complementarity,
            data = plm_data, effect = "individual",
            model = "within", 
            index = c("industry", "year"))
summary(fit5)

# Extra
## Choose the industry
SieData <- result_ind_inv[which(result_ind_inv$Primary_Industry_Group %in% industryname[25]),]
SieData$Deal_Date <- str_sub(SieData$Deal_Date, 1, 4)
SieData <- SieData[order(SieData$Deal_Date),]

numinvstor <- unique(SieData$InvestorId)
b1 <- NULL
for (i in 1:length(numinvstor)) {
  b2 <- which(SieData$InvestorId == numinvstor[i])
  b1 <- c(b1, b2[1])
}
numinvstor <- data.frame(numinvstor, b1)
numpersons <- unique(SieData$PersonId)
b1 <- NULL
for (i in 1:length(numpersons)) {
  b2 <- which(SieData$PersonId == numpersons[i])
  b1 <- c(b1, b2[1])
}
numpersons <- data.frame(numpersons, b1)

## Define node sets
investors <- sienaNodeSet(nrow(numinvstor), nodeSetName = "investors")
persons <- sienaNodeSet(nrow(numpersons), nodeSetName = "persons")

## Define coDyaCovar
### Gender2
gender <- outer(SieData$gender.i[numinvstor$b1], 
                SieData$gender.p[numpersons$b1], FUN = "+")
gender[which(gender == 1)] <- 3
gender[which(gender == 0)] <- 1
gender[which(gender == 2)] <- 1
gender[which(gender == 3)] <- 0
genderCor <- coDyadCovar(gender, nodeSets = c("investors", "persons"),
                         type = "bipartite")
### Top School2
topschool <- outer(SieData$topschool.i[numinvstor$b1],
                   SieData$topschool.p[numpersons$b1], FUN = "*")
topschoolCor <- coDyadCovar(topschool, 
                            nodeSets = c("investors", "persons"),
                            type = "bipartite")
### Geodist2
b3 <- SieData[numinvstor$b1, 16:17]
b4 <- SieData[numpersons$b1, 14:15]
geodist <- distm(as.matrix(cbind(b3$inv_lon, b3$inv_lat)), 
                 as.matrix(cbind(b4$comp_lon,b4$comp_lat)), 
                 fun = distGeo)
geodistCor <- coDyadCovar(geodist, nodeSets = c("investors", "persons"),
                          type = "bipartite")
### Experience2
experience <- outer(SieData$exep.i[numinvstor$b1],
                    SieData$exep.p[numpersons$b1], FUN = "-")
experience <- experience - max(experience)
experience <- abs(experience)
experience <- round(scale(experience), digits = 2)
experienceCor <- coDyadCovar(experience, 
                             nodeSets = c("investors", "persons"),
                             type = "bipartite")
### comskill2
comskill <- outer(SieData$MBA.i[numinvstor$b1],
                  SieData$PHD.p[numpersons$b1], FUN = "*")
comskillCor <- coDyadCovar(comskill, 
                           nodeSets = c("investors", "persons"),
                           type = "bipartite")
### Gender1
gender.p <- SieData$gender.p[numpersons$b1]
genderpCor <- coCovar(gender.p, nodeSet = "persons")
### Top school1
topschool.p <- SieData$topschool.p[numpersons$b1]
topschoolpCor <- coCovar(topschool.p, nodeSet = "persons")
### city
city <- SieData$city[numpersons$b1]
citypCor <- coCovar(city, nodeSet = "persons")
### experience
experience.p <- SieData$exep.p[numpersons$b1]
expepCor <- coCovar(experience.p, nodeSet = "persons")
### is MBA
MBA.p <- SieData$MBA.p[numpersons$b1]
MBApCor <- coCovar(MBA.p, nodeSet = "persons")
### is PHD
PHD.p <- SieData$PHD.p[numpersons$b1]
PHDpCor <- coCovar(PHD.p, nodeSet = "persons")
### num of invests
numinv.p <- SieData$numinv[numpersons$b1]
numinvpCor <- coCovar(numinv.p, nodeSet = "persons")

## Define dependent data
SieData.w1 <- read.csv("D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #6/result/w1_q1_9.csv")
SieData.w2 <- read.csv("D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #6/result/w2_q1_9.csv")
SieData.w1 <- as.matrix(SieData.w1[,-1])
SieData.w2 <- as.matrix(SieData.w2[,-1])

SieDataD <- sienaDependent(array(c(SieData.w1,SieData.w2),
                                 dim = c(nrow(numinvstor), 
                                         nrow(numpersons), 2)),
                           "bipartite",
                           nodeSet = c("investors", "persons"))
bipData <- sienaDataCreate(SieDataD, genderCor, topschoolCor,
                           experienceCor, comskillCor,
                           nodeSets = list(investors, persons))
bipEffects <- getEffects(bipData)
print01Report(bipData, 
              modelname = "D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #6/result/IT")
# Specify the model
bipEffects <- includeEffects(bipEffects, cycle4, outActSqrt, 
                             inPopSqrt, outInAss, name = "SieDataD")
bipEffects <- includeEffects(bipEffects, X, name = "SieDataD",
                             interaction1 = "genderCor")
bipEffects <- includeEffects(bipEffects, X, name = "SieDataD",
                             interaction1 = "topschoolCor")
#bipEffects <- includeEffects(bipEffects, X, name = "SieDataD",
#                             interaction1 = "geodistCor")
bipEffects <- includeEffects(bipEffects, X, name = "SieDataD",
                             interaction1 = "experienceCor")
bipEffects <- includeEffects(bipEffects, X, name = "SieDataD",
                             interaction1 = "comskillCor")

bipEffects <- includeEffects(bipEffects, altX, name = "SieDataD",
                             interaction1 = "genderpCor")
bipEffects <- includeEffects(bipEffects, altX, name = "SieDataD",
                             interaction1 = "topschoolpCor")
bipEffects <- includeEffects(bipEffects, altX, name = "SieDataD",
                             interaction1 = "citypCor")
bipEffects <- includeEffects(bipEffects, altX, name = "SieDataD",
                             interaction1 = "expepCor")
bipEffects <- includeEffects(bipEffects, altX, name = "SieDataD",
                             interaction1 = "MBApCor")
bipEffects <- includeEffects(bipEffects, altX, name = "SieDataD",
                             interaction1 = "PHDpCor")
bipEffects <- includeEffects(bipEffects, altX, name = "SieDataD",
                             interaction1 = "numinvpCor")
# create algorithm object:
bipModel <- sienaAlgorithmCreate(
  projname = 'D:/PRIORITY/Graduate/Emory MSBA/673 Social Network Analytics/Empirical Exercise #6/result/IT_outcomes_extra')
# estimate model:
bipResults <- siena07(bipModel, data = bipData, effects = bipEffects)
bipResults
bipResults <- siena07(bipModel, data = bipData, effects = bipEffects,
                      prevAns = bipResults, returnDeps = TRUE)
bipResults





