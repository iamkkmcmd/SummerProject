# Clear environment
rm(list = ls())
graphics.off()

# Load the libraries
library(tidyverse)
library(lfe)
library(stargazer)

# Set working directory
# setwd("D:/Project/SummerProject/notebook/")
setwd("G:/Other computers/HP/SummerProject/notebook")
# Load the data
data <- read.csv('./../data/final_result.csv')
# Change the variable type
data$banker_name <- factor(data$banker_name, )
data$vulnerable <- factor(data$vulnerable)
data$type <- factor(data$type)
data$company_name <- factor(data$company_name)
data$time <- factor(data$time)
data$bank_prod_code <- factor(as.numeric(data$banker_name)*data$nic_prod_code)

# t-test

unique_company_name <- data %>% 
  group_by(company_name) %>% 
  count() %>% 
  filter(n == 2) %>% 
  select(company_name)
unique_company_name$company_name

df <- data %>% 
  filter(company_name %in% unique_company_name$company_name)

df_nonvul <- df %>% filter(vulnerable == 0)
NSKEW_Before <- df_nonvul$nskew[df_nonvul$time == 0]
NSKEW_After <- df_nonvul$nskew[df_nonvul$time == 1]
t.test(NSKEW_Before, NSKEW_After, paired = TRUE)
DUVOL_Before <- df_nonvul$duvol[df_nonvul$time == 0]
DUVOL_After <- df_nonvul$duvol[df_nonvul$time == 1]
t.test(DUVOL_Before, DUVOL_After, paired = TRUE)

df_vul <- df %>% filter(vulnerable == 1)
NSKEW_Before <- df_vul$nskew[df_vul$time == 0]
NSKEW_After <- df_vul$nskew[df_vul$time == 1]
t.test(NSKEW_Before, NSKEW_After, paired = TRUE)
DUVOL_Before <- df_vul$duvol[df_vul$time == 0]
DUVOL_After <- df_vul$duvol[df_vul$time == 1]
t.test(DUVOL_Before, DUVOL_After, paired = TRUE)

DUVOL_Before <- df_vul$roa[df_vul$time == 0]
DUVOL_After <- df_vul$roa[df_vul$time == 1]
t.test(DUVOL_Before, DUVOL_After, paired = TRUE)


t.test(unique(NSKEW_After), unique(NSKEW_Before), paired = T)


sapply(data, function(x){length(unique(x))})

# COUNT
model1 <- felm(count ~ mes + time + mes*time | banker_name | 0 | banker_name + nic_prod_code, data = data)
model2 <- felm(count ~ mes + time + mes*time | nic_prod_code | 0 | banker_name + nic_prod_code, data = data)
model3 <- felm(count ~ mes + time + mes*time | bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model4 <- felm(count ~ mes + time + mes*time | banker_name + nic_prod_code | 0 | banker_name + nic_prod_code, data = data)
model5 <- felm(count ~ mes + time + mes*time | banker_name + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model6 <- felm(count ~ mes + time + mes*time | nic_prod_code + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model7 <- felm(count ~ mes + time + mes*time | banker_name + nic_prod_code + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)

stargazer(model1, model2, model3, model4, model5, model6, model7, type = 'text',
          dep.var.labels = c('COUNT: Crash Risk Measure'), title = 'Table 5: Result for COUNT measure (Without controls)',
          covariate.labels = c('MES', 'Post', 'MES:Post'), out = './../report/result_model/count_wo_control.txt')

model1 <- felm(count ~ mes + time + mes*time + roa + leverage + ic_count | banker_name | 0 | banker_name + nic_prod_code, data = data)
model2 <- felm(count ~ mes + time + mes*time + roa + leverage + ic_count | nic_prod_code | 0 | banker_name + nic_prod_code, data = data)
model3 <- felm(count ~ mes + time + mes*time + roa + leverage + ic_count | bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model4 <- felm(count ~ mes + time + mes*time + roa + leverage + ic_count | banker_name + nic_prod_code | 0 | banker_name + nic_prod_code, data = data)
model5 <- felm(count ~ mes + time + mes*time + roa + leverage + ic_count | banker_name + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model6 <- felm(count ~ mes + time + mes*time + roa + leverage + ic_count | nic_prod_code + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model7 <- felm(count ~ mes + time + mes*time + roa + leverage + ic_count | banker_name + nic_prod_code + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)

stargazer(model1, model2, model3, model4, model5, model6, model7, type = 'text',
          dep.var.labels = c('COUNT: Crash Risk Measure'), title = 'Table 6: Result for COUNT measure (With controls)',
          covariate.labels = c('MES', 'Post', 'ROA', 'Leverage', 'NSKEW (t-1)', 'MES:Post'), out = './../report/result_model/count_wo_control.txt')

# NSKEW
model1 <- felm(nskew ~ mes + time + mes*time | banker_name | 0 | banker_name + nic_prod_code, data = data)
model2 <- felm(nskew ~ mes + time + mes*time | nic_prod_code | 0 | banker_name + nic_prod_code, data = data)
model3 <- felm(nskew ~ mes + time + mes*time | bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model4 <- felm(nskew ~ mes + time + mes*time | banker_name + nic_prod_code | 0 | banker_name + nic_prod_code, data = data)
model5 <- felm(nskew ~ mes + time + mes*time | banker_name + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model6 <- felm(nskew ~ mes + time + mes*time | nic_prod_code + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model7 <- felm(nskew ~ mes + time + mes*time | banker_name + nic_prod_code + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)

stargazer(model1, model2, model3, model4, model5, model6, model7, type = 'text',
          dep.var.labels = c('NSKEW: Crash Risk Measure'), title = 'Table 1: Result for NSKEW measure (Without controls)',
          covariate.labels = c('MES', 'Post', 'MES:Post'), out = './../report/result_model/nskew_wo_control.txt')

model1 <- felm(nskew ~ mes + time + mes*time + roa + leverage + ic_nskew | banker_name | 0 | banker_name + nic_prod_code, data = data)
model2 <- felm(nskew ~ mes + time + mes*time + roa + leverage + ic_nskew | nic_prod_code | 0 | banker_name + nic_prod_code, data = data)
model3 <- felm(nskew ~ mes + time + mes*time + roa + leverage + ic_nskew | bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model4 <- felm(nskew ~ mes + time + mes*time + roa + leverage + ic_nskew | banker_name + nic_prod_code | 0 | banker_name + nic_prod_code, data = data)
model5 <- felm(nskew ~ mes + time + mes*time + roa + leverage + ic_nskew | banker_name + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model6 <- felm(nskew ~ mes + time + mes*time + roa + leverage + ic_nskew | nic_prod_code + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model7 <- felm(nskew ~ mes + time + mes*time + roa + leverage + ic_nskew | banker_name + nic_prod_code + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)

stargazer(model1, model2, model3, model4, model5, model6, model7, type = 'text',
          dep.var.labels = c('NSKEW: Crash Risk Measure'), title = 'Table 2: Result for NSKEW measure (With controls)',
          covariate.labels = c('MES', 'Post', 'ROA', 'Leverage', 'NSKEW (t-1)', 'MES:Post'), out = './../report/result_model/nskew_w_control.txt')

# DUVOL
model1 <- felm(duvol ~ mes + time + mes*time | banker_name | 0 | banker_name + nic_prod_code, data = data)
model2 <- felm(duvol ~ mes + time + mes*time | nic_prod_code | 0 | banker_name + nic_prod_code, data = data)
model3 <- felm(duvol ~ mes + time + mes*time | bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model4 <- felm(duvol ~ mes + time + mes*time | banker_name + nic_prod_code | 0 | banker_name + nic_prod_code, data = data)
model5 <- felm(duvol ~ mes + time + mes*time | banker_name + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model6 <- felm(duvol ~ mes + time + mes*time | nic_prod_code + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model7 <- felm(duvol ~ mes + time + mes*time | banker_name + nic_prod_code + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)

stargazer(model1, model2, model3, model4, model5, model6, model7, type = 'text',
          dep.var.labels = c('DUVOL: Crash Risk Measure'), title = 'Table 3: Result for DUVOL measure (Without controls)',
          covariate.labels = c('MES', 'Post', 'MES:Post'), out = './../report/result_model/duvol_wo_control.txt')

model1 <- felm(duvol ~ mes + time + mes*time + roa + leverage + ic_duvol | banker_name | 0 | banker_name + nic_prod_code, data = data)
model2 <- felm(duvol ~ mes + time + mes*time + roa + leverage + ic_duvol | nic_prod_code | 0 | banker_name + nic_prod_code, data = data)
model3 <- felm(duvol ~ mes + time + mes*time + roa + leverage + ic_duvol | bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model4 <- felm(duvol ~ mes + time + mes*time + roa + leverage + ic_duvol | banker_name + nic_prod_code | 0 | banker_name + nic_prod_code, data = data)
model5 <- felm(duvol ~ mes + time + mes*time + roa + leverage + ic_duvol | banker_name + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model6 <- felm(duvol ~ mes + time + mes*time + roa + leverage + ic_duvol | nic_prod_code + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)
model7 <- felm(duvol ~ mes + time + mes*time + roa + leverage + ic_duvol | banker_name + nic_prod_code + bank_prod_code | 0 | banker_name + nic_prod_code, data = data)

stargazer(model1, model2, model3, model4, model5, model6, model7, type = 'text',
          dep.var.labels = c('DUVOL: Crash Risk Measure'), title = 'Table 4: Result for DUVOL measure (With controls)',
          covariate.labels = c('MES', 'Post', 'ROA', 'Leverage', 'DUVOL (t-1)', 'MES:Post'), out = './../report/result_model/duvol_w_control.txt')
