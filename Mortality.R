#######data_process#######
Mortality = fread('death_participant.csv')
colnames(Mortality)
Mortality = Mortality[,-c(2,3,6)]
apply(Mortality,2, pMiss)
table(Mortality$`Date of death | Instance 1`)
Mortality = Mortality[,-c(3,4,6,8)]
saveRDS(Mortality, 'Mortality.rds')
colnames(isolation3)
colnames(Mortality) = c('eid', 'cause_of_death','date_of_death1', 'death_age')
isolation3 = isolation3[,-c(55:57)]
isolation3 = merge(isolation3, Mortality, by = 'eid')
str(isolation3)
isolation3$status = if_else(is.na(isolation3$date_of_death1),0,1)
table(isolation3$status)

isolation3$date_of_death1[is.na(isolation3$date_of_death1)] <- as.Date("2023-07-01")
isolation3$date_of_death1 = as.integer(isolation3$date_of_death1)
isolation3 = isolation3 %>%
  mutate(end_time = pmap_dbl(list(date_of_death1, Date_lost_to_follow_up), ~min(c(...), na.rm = TRUE)),
         time = as.numeric(end_time - date_of_attended))
summary(isolation3)
isolation3$time = isolation3$time/365

###########model1##########
results_list <- list()
colnames(isolation3)
# 创建包含所有协变量的字符串
covariates <- paste(colnames(isolation3)[c(20,21,23,24,31,30,52)], collapse=" + ")

for (i in c(8:12,48)) {
  # 多变量Cox回归模型，包含协变量
  formula_str <- paste("Surv(time, status) ~", colnames(isolation3)[i], "+", covariates)
  cox_model <- coxph(as.formula(formula_str), data = isolation3)
  a = summary(cox_model)
  
  # 存储结果
  results_list[[colnames(isolation3)[i]]] <- data.frame(
    Variable = colnames(isolation3)[i],
    HR=a$conf.int[,"exp(coef)"],
    L95CI=a$conf.int[,"lower .95"],
    H95CI=a$conf.int[,"upper .95"],
    pvalue=a$coefficients[,"Pr(>|z|)"])
}

# 将结果汇总到一个数据框中
death_model1_isolation3 <- do.call(rbind, results_list)
death_model1_isolation3$Variable = row.names(death_model1_isolation3)
death_model1_isolation3$Variable <- gsub(".*\\.", "", death_model1_isolation3$Variable)
# 定义要筛选的字符
characters_to_include <- colnames(isolation3)[c(8:12,48)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
death_model1_isolation3 <- death_model1_isolation3 %>%
  filter(grepl(pattern, Variable))
###########model2##########
results_list <- list()
colnames(isolation3)
# 创建包含所有协变量的字符串
covariates <- paste(colnames(isolation3)[c(18:25,29:31,36,37,44,46,52)], collapse=" + ")

for (i in c(8:12,48)) {
  # 多变量Cox回归模型，包含协变量
  formula_str <- paste("Surv(time, status) ~", colnames(isolation3)[i], "+", covariates)
  cox_model <- coxph(as.formula(formula_str), data = isolation3)
  a = summary(cox_model)
  
  # 存储结果
  results_list[[colnames(isolation3)[i]]] <- data.frame(
    Variable = colnames(isolation3)[i],
    HR=a$conf.int[,"exp(coef)"],
    L95CI=a$conf.int[,"lower .95"],
    H95CI=a$conf.int[,"upper .95"],
    pvalue=a$coefficients[,"Pr(>|z|)"])
}

# 将结果汇总到一个数据框中
death_model2_isolation3 <- do.call(rbind, results_list)
death_model2_isolation3$Variable = row.names(death_model2_isolation3)
death_model2_isolation3$Variable <- gsub(".*\\.", "", death_model2_isolation3$Variable)
# 定义要筛选的字符
characters_to_include <- colnames(isolation3)[c(8:12,48)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
death_model2_isolation3 <- death_model2_isolation3 %>%
  filter(grepl(pattern, Variable))
###########model3##########
results_list <- list()
colnames(isolation3)
# 创建包含所有协变量的字符串
covariates <- paste(colnames(isolation3)[c(18:25,29:37,44,46,47,52)], collapse=" + ")

for (i in c(8:12,48)) {
  # 多变量Cox回归模型，包含协变量
  formula_str <- paste("Surv(time, status) ~", colnames(isolation3)[i], "+", covariates)
  cox_model <- coxph(as.formula(formula_str), data = isolation3)
  a = summary(cox_model)
  
  # 存储结果
  results_list[[colnames(isolation3)[i]]] <- data.frame(
    Variable = colnames(isolation3)[i],
    HR=a$conf.int[,"exp(coef)"],
    L95CI=a$conf.int[,"lower .95"],
    H95CI=a$conf.int[,"upper .95"],
    pvalue=a$coefficients[,"Pr(>|z|)"])
}

# 将结果汇总到一个数据框中
death_model3_isolation3 <- do.call(rbind, results_list)
death_model3_isolation3$Variable = row.names(death_model3_isolation3)
death_model3_isolation3$Variable <- gsub(".*\\.", "", death_model3_isolation3$Variable)
# 定义要筛选的字符
characters_to_include <- colnames(isolation3)[c(8:12,48)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
death_model3_isolation3 <- death_model3_isolation3 %>%
  filter(grepl(pattern, Variable))







###########direct_effect##########
results_list <- list()
colnames(isolation3)
# 创建包含所有协变量的字符串
covariates <- paste(colnames(isolation3)[c(8,18:25,29:37,44,46,47,52)], collapse=" + ")

for (i in c(9,11)) {
  # 多变量Cox回归模型，包含协变量
  formula_str <- paste("Surv(time, status) ~", colnames(isolation3)[i], "+", covariates)
  cox_model <- coxph(as.formula(formula_str), data = isolation3)
  a = summary(cox_model)
  
  # 存储结果
  results_list[[colnames(isolation3)[i]]] <- data.frame(
    Variable = colnames(isolation3)[i],
    HR=a$conf.int[,"exp(coef)"],
    L95CI=a$conf.int[,"lower .95"],
    H95CI=a$conf.int[,"upper .95"],
    pvalue=a$coefficients[,"Pr(>|z|)"])
}

# 将结果汇总到一个数据框中
death_DE_isolation3 <- do.call(rbind, results_list)
death_DE_isolation3$Variable = row.names(death_DE_isolation3)
death_DE_isolation3$Variable <- gsub(".*\\.", "", death_DE_isolation3$Variable)
# 定义要筛选的字符
characters_to_include <- colnames(isolation3)[c(9,11)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
death_DE_isolation3 <- death_DE_isolation3 %>%
  filter(grepl(pattern, Variable))







##########cause-specific death######

isolation3 <- isolation3 %>%
  mutate(status_ca = if_else(grepl("C[0-8][0-9]|C9[0-7]", cause_of_death), 1, 0))
isolation3<- isolation3%>%
  mutate(date_of_death_ca = if_else(str_detect(cause_of_death, "C[0-8][0-9]|C9[0-7]"),
                             date_of_death1,
                               19539))

isolation3 = isolation3 %>%
  mutate(end_time = pmap_dbl(list(date_of_death_ca, Date_lost_to_follow_up), ~min(c(...), na.rm = TRUE)),
         time_ca = as.numeric(end_time - date_of_attended))
summary(isolation3)
isolation3$time_ca = isolation3$time_ca/365


isolation3 <- isolation3 %>%
  mutate(status_cad = if_else(grepl("I[0-9][0-9]", cause_of_death), 1, 0))
isolation3<- isolation3%>%
  mutate(date_of_death_cad = if_else(str_detect(cause_of_death, "I[0-9][0-9]"),
                                    date_of_death1,
                                    19539))

isolation3 = isolation3 %>%
  mutate(end_time = pmap_dbl(list(date_of_death_cad, Date_lost_to_follow_up), ~min(c(...), na.rm = TRUE)),
         time_cad = as.numeric(end_time - date_of_attended))
summary(isolation3)
isolation3$time_cad = isolation3$time_cad/365


######cancer#####
###########model1##########
results_list <- list()
colnames(isolation3)
# 创建包含所有协变量的字符串
covariates <- paste(colnames(isolation3)[c(20,21,23,24,31,30,52)], collapse=" + ")

for (i in c(8:12,48)) {
  # 多变量Cox回归模型，包含协变量
  formula_str <- paste("Surv(time_ca, status_ca) ~", colnames(isolation3)[i], "+", covariates)
  cox_model <- coxph(as.formula(formula_str), data = isolation3)
  a = summary(cox_model)
  
  # 存储结果
  results_list[[colnames(isolation3)[i]]] <- data.frame(
    Variable = colnames(isolation3)[i],
    HR=a$conf.int[,"exp(coef)"],
    L95CI=a$conf.int[,"lower .95"],
    H95CI=a$conf.int[,"upper .95"],
    pvalue=a$coefficients[,"Pr(>|z|)"])
}

# 将结果汇总到一个数据框中
ca_death_model1_isolation3 <- do.call(rbind, results_list)
ca_death_model1_isolation3$Variable = row.names(ca_death_model1_isolation3)
ca_death_model1_isolation3$Variable <- gsub(".*\\.", "", ca_death_model1_isolation3$Variable)
# 定义要筛选的字符
characters_to_include <- colnames(isolation3)[c(8:12,48)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
ca_death_model1_isolation3 <- ca_death_model1_isolation3 %>%
  filter(grepl(pattern, Variable))
###########model2##########
results_list <- list()
colnames(isolation3)
# 创建包含所有协变量的字符串
covariates <- paste(colnames(isolation3)[c(18:25,29:31,36,37,44,46,52)], collapse=" + ")

for (i in c(8:12,48)) {
  # 多变量Cox回归模型，包含协变量
  formula_str <- paste("Surv(time_ca, status_ca) ~", colnames(isolation3)[i], "+", covariates)
  cox_model <- coxph(as.formula(formula_str), data = isolation3)
  a = summary(cox_model)
  
  # 存储结果
  results_list[[colnames(isolation3)[i]]] <- data.frame(
    Variable = colnames(isolation3)[i],
    HR=a$conf.int[,"exp(coef)"],
    L95CI=a$conf.int[,"lower .95"],
    H95CI=a$conf.int[,"upper .95"],
    pvalue=a$coefficients[,"Pr(>|z|)"])
}

# 将结果汇总到一个数据框中
ca_death_model2_isolation3 <- do.call(rbind, results_list)
ca_death_model2_isolation3$Variable = row.names(ca_death_model2_isolation3)
ca_death_model2_isolation3$Variable <- gsub(".*\\.", "", ca_death_model2_isolation3$Variable)
# 定义要筛选的字符
characters_to_include <- colnames(isolation3)[c(8:12,48)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
ca_death_model2_isolation3 <- ca_death_model2_isolation3 %>%
  filter(grepl(pattern, Variable))
###########model3##########
results_list <- list()
colnames(isolation3)
# 创建包含所有协变量的字符串
covariates <- paste(colnames(isolation3)[c(18:25,29:37,44,46,47,52)], collapse=" + ")

for (i in c(8:12,48)) {
  # 多变量Cox回归模型，包含协变量
  formula_str <- paste("Surv(time_ca, status_ca) ~", colnames(isolation3)[i], "+", covariates)
  cox_model <- coxph(as.formula(formula_str), data = isolation3)
  a = summary(cox_model)
  
  # 存储结果
  results_list[[colnames(isolation3)[i]]] <- data.frame(
    Variable = colnames(isolation3)[i],
    HR=a$conf.int[,"exp(coef)"],
    L95CI=a$conf.int[,"lower .95"],
    H95CI=a$conf.int[,"upper .95"],
    pvalue=a$coefficients[,"Pr(>|z|)"])
}

# 将结果汇总到一个数据框中
ca_death_model3_isolation3 <- do.call(rbind, results_list)
ca_death_model3_isolation3$Variable = row.names(ca_death_model3_isolation3)
ca_death_model3_isolation3$Variable <- gsub(".*\\.", "", ca_death_model3_isolation3$Variable)
# 定义要筛选的字符
characters_to_include <- colnames(isolation3)[c(8:12,48)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
ca_death_model3_isolation3 <- ca_death_model3_isolation3 %>%
  filter(grepl(pattern, Variable))







###########DIRECT effect##########
results_list <- list()
colnames(isolation3)
# 创建包含所有协变量的字符串
covariates <- paste(colnames(isolation3)[c(8,18:25,29:37,44,46,47,52)], collapse=" + ")

for (i in c(9,11)) {
  # 多变量Cox回归模型，包含协变量
  formula_str <- paste("Surv(time_ca, status_ca) ~", colnames(isolation3)[i], "+", covariates)
  cox_model <- coxph(as.formula(formula_str), data = isolation3)
  a = summary(cox_model)
  
  # 存储结果
  results_list[[colnames(isolation3)[i]]] <- data.frame(
    Variable = colnames(isolation3)[i],
    HR=a$conf.int[,"exp(coef)"],
    L95CI=a$conf.int[,"lower .95"],
    H95CI=a$conf.int[,"upper .95"],
    pvalue=a$coefficients[,"Pr(>|z|)"])
}

# 将结果汇总到一个数据框中
ca_death_DE_isolation3 <- do.call(rbind, results_list)
ca_death_DE_isolation3$Variable = row.names(ca_death_DE_isolation3)
ca_death_DE_isolation3$Variable <- gsub(".*\\.", "", ca_death_DE_isolation3$Variable)
# 定义要筛选的字符
characters_to_include <- colnames(isolation3)[c(9,11)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
ca_death_DE_isolation3 <- ca_death_DE_isolation3 %>%
  filter(grepl(pattern, Variable))







##########cad###########
###########model1##########
results_list <- list()
colnames(isolation3)
# 创建包含所有协变量的字符串
covariates <- paste(colnames(isolation3)[c(20,21,23,24,31,30,52)], collapse=" + ")

for (i in c(8:12,48)) {
  # 多变量Cox回归模型，包含协变量
  formula_str <- paste("Surv(time_cad, status_cad) ~", colnames(isolation3)[i], "+", covariates)
  cox_model <- coxph(as.formula(formula_str), data = isolation3)
  a = summary(cox_model)
  
  # 存储结果
  results_list[[colnames(isolation3)[i]]] <- data.frame(
    Variable = colnames(isolation3)[i],
    HR=a$conf.int[,"exp(coef)"],
    L95CI=a$conf.int[,"lower .95"],
    H95CI=a$conf.int[,"upper .95"],
    pvalue=a$coefficients[,"Pr(>|z|)"])
}

# 将结果汇总到一个数据框中
cad_death_model1_isolation3 <- do.call(rbind, results_list)
cad_death_model1_isolation3$Variable = row.names(cad_death_model1_isolation3)
cad_death_model1_isolation3$Variable <- gsub(".*\\.", "", cad_death_model1_isolation3$Variable)
# 定义要筛选的字符
characters_to_include <- colnames(isolation3)[c(8:12,48)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
cad_death_model1_isolation3 <- cad_death_model1_isolation3 %>%
  filter(grepl(pattern, Variable))
###########model2##########
results_list <- list()
colnames(isolation3)
# 创建包含所有协变量的字符串
covariates <- paste(colnames(isolation3)[c(18:25,29:31,36,37,44,46,52)], collapse=" + ")

for (i in c(8:12,48)) {
  # 多变量Cox回归模型，包含协变量
  formula_str <- paste("Surv(time_cad, status_cad) ~", colnames(isolation3)[i], "+", covariates)
  cox_model <- coxph(as.formula(formula_str), data = isolation3)
  a = summary(cox_model)
  
  # 存储结果
  results_list[[colnames(isolation3)[i]]] <- data.frame(
    Variable = colnames(isolation3)[i],
    HR=a$conf.int[,"exp(coef)"],
    L95CI=a$conf.int[,"lower .95"],
    H95CI=a$conf.int[,"upper .95"],
    pvalue=a$coefficients[,"Pr(>|z|)"])
}

# 将结果汇总到一个数据框中
cad_death_model2_isolation3 <- do.call(rbind, results_list)
cad_death_model2_isolation3$Variable = row.names(cad_death_model2_isolation3)
cad_death_model2_isolation3$Variable <- gsub(".*\\.", "", cad_death_model2_isolation3$Variable)
# 定义要筛选的字符
characters_to_include <- colnames(isolation3)[c(8:12,48)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
cad_death_model2_isolation3 <- cad_death_model2_isolation3 %>%
  filter(grepl(pattern, Variable))
###########model3##########
results_list <- list()
colnames(isolation3)
# 创建包含所有协变量的字符串
covariates <- paste(colnames(isolation3)[c(18:25,29:37,44,46,47,52)], collapse=" + ")

for (i in c(8:12,48)) {
  # 多变量Cox回归模型，包含协变量
  formula_str <- paste("Surv(time_cad, status_cad) ~", colnames(isolation3)[i], "+", covariates)
  cox_model <- coxph(as.formula(formula_str), data = isolation3)
  a = summary(cox_model)
  
  # 存储结果
  results_list[[colnames(isolation3)[i]]] <- data.frame(
    Variable = colnames(isolation3)[i],
    HR=a$conf.int[,"exp(coef)"],
    L95CI=a$conf.int[,"lower .95"],
    H95CI=a$conf.int[,"upper .95"],
    pvalue=a$coefficients[,"Pr(>|z|)"])
}

# 将结果汇总到一个数据框中
cad_death_model3_isolation3 <- do.call(rbind, results_list)
cad_death_model3_isolation3$Variable = row.names(cad_death_model3_isolation3)
cad_death_model3_isolation3$Variable <- gsub(".*\\.", "", cad_death_model3_isolation3$Variable)
# 定义要筛选的字符
characters_to_include <- colnames(isolation3)[c(8:12,48)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
cad_death_model3_isolation3 <- cad_death_model3_isolation3 %>%
  filter(grepl(pattern, Variable))







###########direct effect##########
results_list <- list()
colnames(isolation3)
# 创建包含所有协变量的字符串
covariates <- paste(colnames(isolation3)[c(8,18:25,29:37,44,46,47,52)], collapse=" + ")

for (i in c(9,11)) {
  # 多变量Cox回归模型，包含协变量
  formula_str <- paste("Surv(time_cad, status_cad) ~", colnames(isolation3)[i], "+", covariates)
  cox_model <- coxph(as.formula(formula_str), data = isolation3)
  a = summary(cox_model)
  
  # 存储结果
  results_list[[colnames(isolation3)[i]]] <- data.frame(
    Variable = colnames(isolation3)[i],
    HR=a$conf.int[,"exp(coef)"],
    L95CI=a$conf.int[,"lower .95"],
    H95CI=a$conf.int[,"upper .95"],
    pvalue=a$coefficients[,"Pr(>|z|)"])
}

# 将结果汇总到一个数据框中
cad_death_DE_isolation3 <- do.call(rbind, results_list)
cad_death_DE_isolation3$Variable = row.names(cad_death_DE_isolation3)
cad_death_DE_isolation3$Variable <- gsub(".*\\.", "", cad_death_DE_isolation3$Variable)
# 定义要筛选的字符
characters_to_include <- colnames(isolation3)[c(9,11)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
cad_death_DE_isolation3 <- cad_death_DE_isolation3 %>%
  filter(grepl(pattern, Variable))








##########mediate#######


# 已知值
a <- 0.167   # X对M的效应
SE_a <- 0.012 # X对M效应的标准误
b <- log(1.028)   # M对Y的效应
SE_b <- (log(1.028) - log(1.025))/1.96  # M对Y效应的标准误

# 计算间接效应
ab <- a * b

# 计算间接效应的标准误
SE_ab <- sqrt((b^2 * SE_a^2) + (a^2 * SE_b^2))
Z_score <- ab / SE_ab

# 计算两尾P值
P_value <- 2 * (1 - pnorm(abs(Z_score)))
P_value
# 输出结果
cat("间接效应: ", ab, "\n",
    "间接效应的标准误: ", SE_ab, "\n")
exp(ab)
exp(ab-1.96*SE_ab)
exp(ab+1.96*SE_ab)


# 假设已知直接效应、间接效应及其标准误
c_prime <- log(1.093)  # 直接效应
SE_c_prime <- (log(1.093) - log(1.067))/1.96  # 直接效应的标准误
ab <- 0.004611733   # 间接效应
SE_ab <- 0.0004145139  # 间接效应的标准误

# 计算总效应和介导比例
c <- c_prime + ab
PM <- ab / c

# 计算介导比例的方差和标准误
var_PM <- (1/c * SE_ab)^2 + (-ab/c^2 * SE_c_prime)^2
SE_PM <- sqrt(var_PM)

# 计算95%可信区间
lower_CI <- PM - 1.96 * SE_PM
upper_CI <- PM + 1.96 * SE_PM

# 输出结果
cat("Proportion Mediated: ", PM, "\n",
    "95% CI: [", lower_CI, ", ", upper_CI, "]\n")


gc()


