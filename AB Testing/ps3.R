rm(list = ls())
graphics.off()

library(haven)
library(stargazer)
library(tidyverse)
library(ivpack)


re_d = read_dta("./STAR.dta")

#1

summary(re_d$girl)

re_d %>% 
  group_by(sck) %>% 
  summarise(avg=mean(tscorek))

re_d %>% 
  summarise( mean_all = mean(tscorek),
             median_all = median(tscorek),
             max_experience = max(totexpk_m)/12) 

#2
model1 <- lm(tscorek ~ sck, re_d)
sd(re_d$tscorek)

library("lfe")
model2 <- felm(tscorek ~ sck + as.factor(schidkn), re_d)
stargazer(model1,model2, type = "latex",
          keep = c("sck"))

#3
library(sandwich)
compute_rob_se = function(lm) {
  vcov = vcovCL(lm, type="HC1")
  se = sqrt(diag(vcov))
}

compute_clu_se = function(lm) {
  vcov = vcovCL(lm, type="HC1", cluster=~schidkn)
  se = sqrt(diag(vcov))
}

rob_se = compute_rob_se(model2)
clu_se = compute_clu_se(model2)
stargazer(model2,model2,model2,
          se = list(NULL,rob_se,clu_se),
          type = "latex",
          keep = c("sck"),
          column.labels = c("Wild", 
                            "Robust",
                            "Cluster"))

#4

#(a)
ts_g <- lm(girl ~ sck , re_d)
ts_f <- lm(freelunk ~ sck  ,re_d)
ts_t <- lm(totexpk_m ~ sck  ,re_d)
stargazer(ts_g,ts_f,ts_t,
          type = "latex")

#(b)
ts_girl <- lm(sck ~ girl, re_d)
ts_freelunk <- lm(sck ~ freelunk,re_d)
ts_totexpk_m <- lm(sck ~ totexpk_m,re_d)
stargazer(ts_girl,ts_freelunk,ts_totexpk_m,
          type = "latex")

#(c)

model_add_cov <- lm(tscorek ~ sck 
                    + girl + freelunk 
                    + totexpk_m, re_d)

stargazer(model_add_cov,
          type = "latex",
          keep = c("sck","girl","totexpk_m",
                   "freelunk"))

#5

#(a)
re_dm <- re_d %>% 
    mutate(sck_girl = sck*girl,
           sck_freelunk = sck*freelunk,
         sck_totexpk_m = sck*totexpk_m)
library(plm)
model_inter_d <- plm(tscorek ~ sck 
                    + girl + sck_girl
                    + freelunk + sck_freelunk
                    + totexpk_m + sck_totexpk_m
                    , re_dm, index = c("schidkn"), model = "within")
stargazer(model_inter_d,
          type = "latex",
          keep = c("sck","girl","sck_girl",
                   "freelunk","sck_freelunk",
                   "totexpk_m",  "sck_totexpk_m"))

linearHypothesis(model_inter_d, c("sck_freelunk=0", "sck_girl=0","sck_totexpk_m=0"),
                 vcov. = vcovHC, type = "HC1")

#(b)
coe <- model_inter_d$coefficients
ATE <- coe[1:1] + coe[3:3]*re_dm$girl + coe[5:5]*re_dm$freelunk + coe[7:7]*re_dm$totexpk_m
summary(ATE)
