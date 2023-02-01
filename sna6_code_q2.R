
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




