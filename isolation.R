##########data_process############
library(data.table)
library(survival)
library(tidyverse)
bioage_data = fread('bioage.csv')
summary(bioage_data)
colnames(bioage_data)

isolation = fread('isolation_participant.csv')
colnames(isolation)
isolation = isolation[,-8]
colnames(isolation) = c('eid', 'visit_freq', 'leisure_activity', 'confide', 'household_num', 'loneliness', 'sleep_duration',
                        'chronotype', 'insomnia', 'snoring', 'daytime_dozing', 'handgrip_l', 'handgrip_r')
saveRDS(isolation, 'isolation_sleep.rds')
isolation1 = merge(isolation, bioage_data[,c(1,44,45)])
colnames(isolation1)
pMiss = function(x){sum(is.na(x))/length(x)*100}
apply(isolation1,2, pMiss)


# 定义需要排除的值
values_to_remove <- c("", "Prefer not to answer", "Do not know")

# 筛选数据，排除第二列到第六列中包含特定值的行
isolation2 <- isolation1 %>%
  filter(across(.cols = 2:6, .fns = ~ !(. %in% values_to_remove)))
table(isolation2$visit_freq)
table(isolation2$leisure_activity)
table(isolation2$confide)
table(isolation2$household_num)
table(isolation2$loneliness)

# 修改confide列，将指定的文本值转换为1，其他转换为0
isolation2 <- isolation2 %>%
  mutate(confide = case_when(
    confide == "Never or almost never" ~ 1,
    TRUE ~ 0  # 其他所有情况
  ))

isolation2 <- isolation2 %>%
  mutate(leisure_activity = case_when(
    leisure_activity == "None of the above" ~ 1,
    TRUE ~ 0  # 其他所有情况
  ))
isolation2 <- isolation2 %>%
  mutate(household_num = case_when(
    household_num == "1" ~ 1,
    TRUE ~ 0  # 其他所有情况
  ))

isolation2 <- isolation2 %>%
  mutate(visit_freq = case_when(
    visit_freq %in% c("About once a month", "Never or almost never", "Once every few months", 'No friends/family outside household') ~ 1,
    TRUE ~ 0  # 其他所有情况
  ))

isolation2 <- isolation2 %>%
  mutate(loneliness = case_when(
    loneliness == "Yes" ~ 1,
    TRUE ~ 0  # 其他所有情况
  ))

isolation2 <- isolation2 %>%
  mutate(social_isolation_score = visit_freq + household_num + leisure_activity
  )
table(isolation2$social_isolation_score)
# 修改 isolation2 数据框，添加 social_isolation_level 列
isolation2 <- isolation2 %>%
  mutate(social_isolation_level = case_when(
    social_isolation_score == 0 ~ "least_isolated",
    social_isolation_score == 1 ~ "moderate_isolated",
    social_isolation_score %in% 2:3 ~ "most_isolated",
    TRUE ~ "undefined"  # 处理可能存在的其他情况
  ))
table(isolation2$social_isolation_level)

isolation2 <- isolation2 %>%
  mutate(loneliness_score = loneliness + confide
  )
table(isolation2$loneliness_score)
isolation2 <- isolation2 %>%
  mutate(loneliness_level = case_when(
    loneliness_score <= 1 ~ "no-loneliness",
    loneliness_score == 2 ~ "loneliness",
    TRUE ~ "undefined"  # 处理可能存在的其他情况
  ))
table(isolation2$loneliness_level)

table(isolation2$sleep_duration)
table(isolation2$chronotype)
table(isolation2$insomnia)
table(isolation2$snoring)
table(isolation2$daytime_dozing)
colnames(isolation2)

isolation2 <- isolation2 %>%
  mutate(across(.cols = 7:11, .fns = ~na_if(.x, "Do not know"))) %>%
  mutate(across(.cols = 7:11, .fns = ~na_if(.x, "Prefer not to answer")))
apply(isolation2,2, pMiss)
str(isolation2)
isolation2 = isolation3

isolation2 <- isolation2 %>%
  mutate(sleep_duration = case_when(
    sleep_duration %in% c("7", "8") ~ 1,   # 将 "7" 和 "8" 转换为 1
    is.na(sleep_duration) ~ NA_real_,      # 保持 NA 值不变
    TRUE ~ 0                               # 其他所有值转换为 0
  ))

isolation2 <- isolation2 %>%
  mutate(chronotype = case_when(
    chronotype %in% c("Definitely a 'morning' person", "More a 'morning' than 'evening' person") ~ 1,   # 将 "7" 和 "8" 转换为 1
    is.na(chronotype) ~ NA_real_,      # 保持 NA 值不变
    TRUE ~ 0                               # 其他所有值转换为 0
  ))

isolation2 <- isolation2 %>%
  mutate(insomnia = case_when(
    insomnia %in% c("Never/rarely", "Sometimes") ~ 1,   # 将 "7" 和 "8" 转换为 1
    is.na(insomnia) ~ NA_real_,      # 保持 NA 值不变
    TRUE ~ 0                               # 其他所有值转换为 0
  ))

isolation2 <- isolation2 %>%
  mutate(daytime_dozing = case_when(
    daytime_dozing %in% c("Never/rarely", "Sometimes") ~ 1,   # 将 "7" 和 "8" 转换为 1
    is.na(daytime_dozing) ~ NA_real_,      # 保持 NA 值不变
    TRUE ~ 0                               # 其他所有值转换为 0
  ))

isolation2 <- isolation2 %>%
  mutate(snoring = case_when(
    snoring %in% c("No") ~ 1,   # 将 "7" 和 "8" 转换为 1
    is.na(snoring) ~ NA_real_,      # 保持 NA 值不变
    TRUE ~ 0                               # 其他所有值转换为 0
  ))


miss_data = isolation2[,c(7:13)]
library(mice)
library(VIM)
colnames(miss_data)
str(miss_data)
imputed_data <- mice(miss_data, method = c('pmm', 'pmm','pmm','pmm', 'pmm','pmm', 'pmm'), m = 5)
completed_data <- complete(imputed_data, 3)  # 获取第一个插补数据集
apply(completed_data,2, pMiss)
isolation2 = isolation2[,-c(7:13)]
isolation2 = cbind(isolation2, completed_data)

colnames(all_cov)
all_cov = all_cov[,-c(24:27)]
apply(all_cov,2, pMiss)
isolation3 = merge(isolation2, all_cov, by = 'eid')
str(isolation3)
isolation3$social_isolation_level = factor(isolation3$social_isolation_level, levels = c("least_isolated", "moderate_isolated", "most_isolated"))
isolation3$loneliness_level = factor(isolation3$loneliness_level, levels = c("no-loneliness", "loneliness"))
isolation3 = isolation3 %>%
  mutate(sleep_score = sleep_duration + chronotype + insomnia + snoring + daytime_dozing)
table(isolation3$sleep_score)
isolation3$sleep_level = if_else(isolation3$sleep_score >= 4, 'health', 'unhealth')
table(isolation3$sleep_level)
isolation3$sleep_level = factor(isolation3$sleep_level, levels = c('unhealth', 'health'))
isolation3 <- isolation3 %>%
  mutate(across(.cols = 2:6, .fns = ~ factor(.x, levels = c(0, 1))))
isolation3$caner_history = if_else(isolation3$type_cancer == '', 'no', 'yes')
table(isolation3$caner_history)
isolation3$caner_history = factor(isolation3$caner_history, levels = c('yes', 'no'))
table(isolation3$social_isolation_level)
table(isolation3$isolation_loneliness_level)
isolation3 <- isolation3 %>%
  mutate(isolation_loneliness_level = paste(social_isolation_level, loneliness_level, sep="_"))
isolation3$isolation_loneliness_level = factor(isolation3$isolation_loneliness_level, levels = c('least_isolated_no-loneliness', 'moderate_isolated_no-loneliness',
                           'most_isolated_no-loneliness', 'least_isolated_loneliness', 'moderate_isolated_loneliness', 'most_isolated_loneliness'))

##########model1########
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(7:8)]
independent_vars <- colnames(isolation3)[c(2:6,9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(20,21,23,24,31,30,28)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3)
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model1_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(2:6,9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model1_df <- model1_df %>%
  filter(grepl(pattern, Term))
model1_df$adjusted_pvalue <- p.adjust(model1_df$Pr, method = "BH")
colnames(model1_df)

model1_df$LowerCI <- model1_df$Estimate - 1.96 * model1_df$StdError
model1_df$UpperCI <- model1_df$Estimate + 1.96 * model1_df$StdError

model1_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model1_df$Estimate, model1_df$LowerCI, model1_df$UpperCI)
write.csv(model1_df, './multi/model1_df.csv')

##########model2########
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(7:8)]
independent_vars <- colnames(isolation3)[c(2:6,9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,28:31,37,44,46)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3)
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model2_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(2:6,9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model2_df <- model2_df %>%
  filter(grepl(pattern, Term))
model2_df$adjusted_pvalue <- p.adjust(model2_df$Pr, method = "BH")
colnames(model2_df)

model2_df$LowerCI <- model2_df$Estimate - 1.96 * model2_df$StdError
model2_df$UpperCI <- model2_df$Estimate + 1.96 * model2_df$StdError

model2_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model2_df$Estimate, model2_df$LowerCI, model2_df$UpperCI)
write.csv(model2_df, './multi/model2_df.csv')

##########model3########
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(7:8)]
independent_vars <- colnames(isolation3)[c(2:6,9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,28:35,37,44,46,47)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3)
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(2:6,9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_df <- model3_df %>%
  filter(grepl(pattern, Term))
model3_df$adjusted_pvalue <- p.adjust(model3_df$Pr, method = "BH")
colnames(model3_df)

model3_df$LowerCI <- model3_df$Estimate - 1.96 * model3_df$StdError
model3_df$UpperCI <- model3_df$Estimate + 1.96 * model3_df$StdError

model3_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_df$Estimate, model3_df$LowerCI, model3_df$UpperCI)
write.csv(model3_df, './multi/model3_df.csv')

########isolation_loneliness#######
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(7:8)]
independent_vars <- colnames(isolation3)[48] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,28:35,37,44,46,47)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3)
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
isolation_loneliness_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[48]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
isolation_loneliness_df <- isolation_loneliness_df %>%
  filter(grepl(pattern, Term))
isolation_loneliness_df$adjusted_pvalue <- p.adjust(isolation_loneliness_df$Pr, method = "BH")
colnames(isolation_loneliness_df)

isolation_loneliness_df$LowerCI <- isolation_loneliness_df$Estimate - 1.96 * isolation_loneliness_df$StdError
isolation_loneliness_df$UpperCI <- isolation_loneliness_df$Estimate + 1.96 * isolation_loneliness_df$StdError

isolation_loneliness_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", isolation_loneliness_df$Estimate, isolation_loneliness_df$LowerCI, isolation_loneliness_df$UpperCI)
write.csv(isolation_loneliness_df, './multi/isolation_loneliness_df.csv')

##########PRS#######
pheno_prs = fread('phenoage.best')
pheno_prs = pheno_prs[,c(1,4)]
colnames(pheno_prs) = c('eid', 'prs_ph')
isolation3 = merge(isolation3, pheno_prs, by = 'eid')
quantiles <- isolation3 %>%
  summarise(
    Q1 = quantile(prs_ph, 0.25, na.rm = TRUE),
    Q2 = quantile(prs_ph, 0.5, na.rm = TRUE),
    Q3 = quantile(prs_ph, 0.75, na.rm = TRUE)
  )

# 修改 isolation3 数据框，添加 prs_ph_level 列，划分为三个等级
isolation3 <- isolation3 %>%
  mutate(prs_ph_level = case_when(
    prs_ph <= quantiles$Q1 ~ "Low",
    prs_ph > quantiles$Q1 & prs_ph <= quantiles$Q3 ~ "Medium",
    prs_ph > quantiles$Q3 ~ "High",
    TRUE ~ "NA or undefined"  # 处理可能的NA值或未定义情况
  ))
table(isolation3$prs_ph_level)
isolation3$prs_ph_level = factor(isolation3$prs_ph_level, levels = c('Low', 'Medium', 'High'))
z_score <- function(col) {
  (col - mean(col, na.rm = TRUE)) / sd(col, na.rm = TRUE)
}

# 批量应用此函数计算z得分
perSD_columns <- as.data.frame(lapply(isolation3 %>% select(49), z_score))

# 将新列名设置为原列名后加上_perSD
colnames(perSD_columns) <- paste0(colnames(perSD_columns), "_perSD")

# 将新列添加到原始数据框中
isolation3 <- cbind(isolation3, perSD_columns)
#########model1######
colnames(isolation3)
dependent_vars <- colnames(isolation3)[8]
independent_vars <- colnames(isolation3)[c(50:51)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(20,21,23,24,31,30,28)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3)
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
prs_model1_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(50:51)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
prs_model1_df <- prs_model1_df %>%
  filter(grepl(pattern, Term))
prs_model1_df$adjusted_pvalue <- p.adjust(prs_model1_df$Pr, method = "BH")
colnames(prs_model1_df)

prs_model1_df$LowerCI <- prs_model1_df$Estimate - 1.96 * prs_model1_df$StdError
prs_model1_df$UpperCI <- prs_model1_df$Estimate + 1.96 * prs_model1_df$StdError

prs_model1_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", prs_model1_df$Estimate, prs_model1_df$LowerCI, prs_model1_df$UpperCI)
write.csv(prs_model1_df, './multi/prs_model1_df.csv')

#########model2######
colnames(isolation3)
dependent_vars <- colnames(isolation3)[8]
independent_vars <- colnames(isolation3)[c(50:51)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,28:31,37,44,46)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3)
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
prs_model2_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(50:51)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
prs_model2_df <- prs_model2_df %>%
  filter(grepl(pattern, Term))
prs_model2_df$adjusted_pvalue <- p.adjust(prs_model2_df$Pr, method = "BH")
colnames(prs_model2_df)

prs_model2_df$LowerCI <- prs_model2_df$Estimate - 1.96 * prs_model2_df$StdError
prs_model2_df$UpperCI <- prs_model2_df$Estimate + 1.96 * prs_model2_df$StdError

prs_model2_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", prs_model2_df$Estimate, prs_model2_df$LowerCI, prs_model2_df$UpperCI)
write.csv(prs_model2_df, './multi/prs_model2_df.csv')

#########model3######
colnames(isolation3)
dependent_vars <- colnames(isolation3)[8]
independent_vars <- colnames(isolation3)[c(50:51)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,28:35,37,44,46,47)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3)
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
prs_model3_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(50:51)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
prs_model3_df <- prs_model3_df %>%
  filter(grepl(pattern, Term))
prs_model3_df$adjusted_pvalue <- p.adjust(prs_model3_df$Pr, method = "BH")
colnames(prs_model3_df)

prs_model3_df$LowerCI <- prs_model3_df$Estimate - 1.96 * prs_model3_df$StdError
prs_model3_df$UpperCI <- prs_model3_df$Estimate + 1.96 * prs_model3_df$StdError

prs_model3_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", prs_model3_df$Estimate, prs_model3_df$LowerCI, prs_model3_df$UpperCI)
write.csv(prs_model3_df, './multi/prs_model3_df.csv')

#######interaction###########
str(isolation3)
table(isolation3$education_level)
isolation3$education_level = if_else(isolation3$education == 'College or University degree', 'College', 'other')
isolation3$education_level = factor(isolation3$education_level, levels = c('College', 'other'))
colnames(isolation3)
table(isolation3$race)
isolation3$race_c = if_else(isolation3$race == 'White', 'White', 'other')
isolation3$race_c = factor(isolation3$race_c, levels = c('White', 'other'))
table(isolation3$income)
isolation3 = isolation3 %>%
  mutate(income_c = 
    case_when(
      income == 'Less_than_18,000' ~'low',
      income == '18,000_to_30,999'|income == '31,000_to_51,999' ~'medium',
      income == '52,000_to_100,000'|income == 'Greater_than_100,000' ~'high'
    )
  )
isolation3$income_c = factor(isolation3$income_c, levels = c('low', 'medium', 'high'))


results_table <- data.frame()
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量
covariates <- colnames(isolation3)[c(18:20,22,23,25,29:35,37,44,46,47,50,52,67,68)]
# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归分析，考虑每个自变量与每个协变量之间的交互作用
for (iv in independent_vars) {
  for (cv in covariates) {
    # 构建包含一个交互作用和所有其他协变量的模型
    interaction_formula <- paste("phenoage_residual~", iv, "*", cv, "+", paste(setdiff(covariates, cv), collapse = "+"))
    model <- lm(as.formula(interaction_formula), data = isolation3)
    results[[paste(iv, "interact", cv)]] <- model
  }
}
# 提取和保存模型结果
model_data <- list()

for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  data <- data.frame(
    Response = key,
    Term = rownames(coefficients),
    Estimate = coefficients[, "Estimate"],
    StdError = coefficients[, "Std. Error"],
    tValue = coefficients[, "t value"],
    Pr = coefficients[, "Pr(>|t|)"]
  )
  model_data[[key]] <- data
}

# 合并所有模型数据
all_results_df_PRS <- bind_rows(model_data)
all_results_df_PRS = all_results_df_PRS %>%
  filter(grepl(":", Term))
all_results_df_PRS$FDR = p.adjust(all_results_df_PRS$Pr, method = 'fdr')
write.csv(all_results_df_PRS, './multi/interaction_cov.csv')

#######JOINT########
colnames(isolation3)
isolation3 <- isolation3 %>%
  mutate(isolation_prs_level = paste(social_isolation_level, prs_ph_level, sep="_"))
table(isolation3$isolation_prs_level)
isolation3$isolation_prs_level = factor(isolation3$isolation_prs_level, levels = c('least_isolated_Low', 'moderate_isolated_Low','most_isolated_Low',
    'least_isolated_Medium', 'moderate_isolated_Medium', 'most_isolated_Medium', 'least_isolated_High', 'moderate_isolated_High', 'most_isolated_High'))

isolation3 <- isolation3 %>%
  mutate(loneliness_prs_level = paste(loneliness_level, prs_ph_level, sep="_"))
table(isolation3$loneliness_prs_level)
isolation3$loneliness_prs_level = factor(isolation3$loneliness_prs_level, levels = c('no-loneliness_Low', 'loneliness_Low','no-loneliness_Medium',
                                                                                   'loneliness_Medium', 'no-loneliness_High', 'loneliness_High'))

colnames(isolation3)
dependent_vars <- colnames(isolation3)[8]
independent_vars <- colnames(isolation3)[c(53:54)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,29:35,37,44,46,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3)
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
prs_loneliness_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(53:54)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
prs_loneliness_df <- prs_loneliness_df %>%
  filter(grepl(pattern, Term))
prs_loneliness_df$adjusted_pvalue <- p.adjust(prs_loneliness_df$Pr, method = "BH")
colnames(prs_loneliness_df)

prs_loneliness_df$LowerCI <- prs_loneliness_df$Estimate - 1.96 * prs_loneliness_df$StdError
prs_loneliness_df$UpperCI <- prs_loneliness_df$Estimate + 1.96 * prs_loneliness_df$StdError

prs_loneliness_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", prs_loneliness_df$Estimate, prs_loneliness_df$LowerCI, prs_loneliness_df$UpperCI)
write.csv(prs_loneliness_df, './multi/prs_loneliness_df.csv')


str(isolation3)


###########table1##########
library(gtsummary)
?tbl_summary
colnames(isolation3)
table1_isolation <- isolation3 %>%
  select(c(23,20,21,24,52,30,22,25,29,46,31,32:35,37,47,18:19,44,10)) %>%
  tbl_summary(
    by = social_isolation_level,  # 使用LEVF_DR作为分类变量
    statistic = list(
      all_continuous() ~ '{mean} ({sd})',  # 连续变量的统计方式：平均值和标准差
      all_categorical() ~ '{n} ({p}%)'     # 分类变量的统计方式：频数和百分比
    ),
    digits = list(all_continuous() ~ 2),  # 设置数字精度
    missing = "no"  # 处理缺失值选项
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ 'kruskal.test',  # 对所有连续变量使用Kruskal-Wallis Test
      all_categorical() ~ "chisq.test"    # 对所有分类变量使用卡方检验
    )
  ) %>%
  add_overall() %>%  # 在表格中添加包含所有数据的“总体”列
  modify_header(label = "**Variable**") %>%
  bold_labels()

# 输出表格到Word文档
table1_isolation %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = './table/table1_isolation.docx')

table1_loneliness <- isolation3 %>% 
  select(c(23,20,21,24,52,30,22,25,29,46,31,32:35,37,47,18:19,44,12)) %>%
  #select(c(81:112,21)) %>%
  tbl_summary(by = loneliness_level,
              statistic = list(all_continuous() ~  "{mean} ({sd})"),
              digits = list(all_continuous() ~ 2)) %>%
  #add_p(test = list(all_continuous() ~ 't.test')) %>%
  add_p() %>%
  add_overall() %>%
  add_stat_label() %>%
  modify_header(label = "**Variable**") %>%
  bold_labels()
table1_loneliness %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = './table/table1_loneliness.docx')

##########subset####
#########sex#######
colnames(isolation3)
dependent_vars <- colnames(isolation3)[8]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:19,21:25,28:35,37,44,46,47)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(sex == 'female'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_female_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_female_df <- model3_female_df %>%
  filter(grepl(pattern, Term))
model3_female_df$adjusted_pvalue <- p.adjust(model3_female_df$Pr, method = "BH")
colnames(model3_female_df)

model3_female_df$LowerCI <- model3_female_df$Estimate - 1.96 * model3_female_df$StdError
model3_female_df$UpperCI <- model3_female_df$Estimate + 1.96 * model3_female_df$StdError

model3_female_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_female_df$Estimate, model3_female_df$LowerCI, model3_female_df$UpperCI)
write.csv(model3_female_df, './multi/model3_female_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(sex == 'male'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_male_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_male_df <- model3_male_df %>%
  filter(grepl(pattern, Term))
model3_male_df$adjusted_pvalue <- p.adjust(model3_male_df$Pr, method = "BH")
colnames(model3_male_df)

model3_male_df$LowerCI <- model3_male_df$Estimate - 1.96 * model3_male_df$StdError
model3_male_df$UpperCI <- model3_male_df$Estimate + 1.96 * model3_male_df$StdError

model3_male_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_male_df$Estimate, model3_male_df$LowerCI, model3_male_df$UpperCI)
write.csv(model3_male_df, './multi/model3_male_df.csv')


#######age#########
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:22,24:25,29:35,37,44,46,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Age_at_recruitment < 60))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_young_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_young_df <- model3_young_df %>%
  filter(grepl(pattern, Term))
model3_young_df$adjusted_pvalue <- p.adjust(model3_young_df$Pr, method = "BH")
colnames(model3_young_df)

model3_young_df$LowerCI <- model3_young_df$Estimate - 1.96 * model3_young_df$StdError
model3_young_df$UpperCI <- model3_young_df$Estimate + 1.96 * model3_young_df$StdError

model3_young_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_young_df$Estimate, model3_young_df$LowerCI, model3_young_df$UpperCI)
write.csv(model3_young_df, './multi/model3_young_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Age_at_recruitment >= 60))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_old_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_old_df <- model3_old_df %>%
  filter(grepl(pattern, Term))
model3_old_df$adjusted_pvalue <- p.adjust(model3_old_df$Pr, method = "BH")
colnames(model3_old_df)

model3_old_df$LowerCI <- model3_old_df$Estimate - 1.96 * model3_old_df$StdError
model3_old_df$UpperCI <- model3_old_df$Estimate + 1.96 * model3_old_df$StdError

model3_old_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_old_df$Estimate, model3_old_df$LowerCI, model3_old_df$UpperCI)
write.csv(model3_old_df, './multi/model3_old_df.csv')

#######BMI#########
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:21,23:25,29:35,37,44,46,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(BMI < 30))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_noobesity_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_noobesity_df <- model3_noobesity_df %>%
  filter(grepl(pattern, Term))
model3_noobesity_df$adjusted_pvalue <- p.adjust(model3_noobesity_df$Pr, method = "BH")
colnames(model3_noobesity_df)

model3_noobesity_df$LowerCI <- model3_noobesity_df$Estimate - 1.96 * model3_noobesity_df$StdError
model3_noobesity_df$UpperCI <- model3_noobesity_df$Estimate + 1.96 * model3_noobesity_df$StdError

model3_noobesity_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_noobesity_df$Estimate, model3_noobesity_df$LowerCI, model3_noobesity_df$UpperCI)
write.csv(model3_noobesity_df, './multi/model3_noobesity_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(BMI >= 30))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_obesity_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_obesity_df <- model3_obesity_df %>%
  filter(grepl(pattern, Term))
model3_obesity_df$adjusted_pvalue <- p.adjust(model3_obesity_df$Pr, method = "BH")
colnames(model3_obesity_df)

model3_obesity_df$LowerCI <- model3_obesity_df$Estimate - 1.96 * model3_obesity_df$StdError
model3_obesity_df$UpperCI <- model3_obesity_df$Estimate + 1.96 * model3_obesity_df$StdError

model3_obesity_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_obesity_df$Estimate, model3_obesity_df$LowerCI, model3_obesity_df$UpperCI)
write.csv(model3_obesity_df, './multi/model3_obesity_df.csv')

#######diet#########
table(isolation3$health_diet_count)
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,29:35,44,46,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(health_diet_count == 'yes'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_health_diet_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_health_diet_df <- model3_health_diet_df %>%
  filter(grepl(pattern, Term))
model3_health_diet_df$adjusted_pvalue <- p.adjust(model3_health_diet_df$Pr, method = "BH")
colnames(model3_health_diet_df)

model3_health_diet_df$LowerCI <- model3_health_diet_df$Estimate - 1.96 * model3_health_diet_df$StdError
model3_health_diet_df$UpperCI <- model3_health_diet_df$Estimate + 1.96 * model3_health_diet_df$StdError

model3_health_diet_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_health_diet_df$Estimate, model3_health_diet_df$LowerCI, model3_health_diet_df$UpperCI)
write.csv(model3_health_diet_df, './multi/model3_health_diet_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(health_diet_count == 'no'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_unhealth_diet_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_unhealth_diet_df <- model3_unhealth_diet_df %>%
  filter(grepl(pattern, Term))
model3_unhealth_diet_df$adjusted_pvalue <- p.adjust(model3_unhealth_diet_df$Pr, method = "BH")
colnames(model3_unhealth_diet_df)

model3_unhealth_diet_df$LowerCI <- model3_unhealth_diet_df$Estimate - 1.96 * model3_unhealth_diet_df$StdError
model3_unhealth_diet_df$UpperCI <- model3_unhealth_diet_df$Estimate + 1.96 * model3_unhealth_diet_df$StdError

model3_unhealth_diet_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_unhealth_diet_df$Estimate, model3_unhealth_diet_df$LowerCI, model3_unhealth_diet_df$UpperCI)
write.csv(model3_unhealth_diet_df, './multi/model3_unhealth_diet_df.csv')

########sleep########
table(isolation3$sleep_level)
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,29:35,37,44,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(sleep_level == 'health'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_health_sleep_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_health_sleep_df <- model3_health_sleep_df %>%
  filter(grepl(pattern, Term))
model3_health_sleep_df$adjusted_pvalue <- p.adjust(model3_health_sleep_df$Pr, method = "BH")
colnames(model3_health_sleep_df)

model3_health_sleep_df$LowerCI <- model3_health_sleep_df$Estimate - 1.96 * model3_health_sleep_df$StdError
model3_health_sleep_df$UpperCI <- model3_health_sleep_df$Estimate + 1.96 * model3_health_sleep_df$StdError

model3_health_sleep_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_health_sleep_df$Estimate, model3_health_sleep_df$LowerCI, model3_health_sleep_df$UpperCI)
write.csv(model3_health_sleep_df, './multi/model3_health_sleep_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(sleep_level == 'unhealth'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_unhealth_sleep_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_unhealth_sleep_df <- model3_unhealth_sleep_df %>%
  filter(grepl(pattern, Term))
model3_unhealth_sleep_df$adjusted_pvalue <- p.adjust(model3_unhealth_sleep_df$Pr, method = "BH")
colnames(model3_unhealth_sleep_df)

model3_unhealth_sleep_df$LowerCI <- model3_unhealth_sleep_df$Estimate - 1.96 * model3_unhealth_sleep_df$StdError
model3_unhealth_sleep_df$UpperCI <- model3_unhealth_sleep_df$Estimate + 1.96 * model3_unhealth_sleep_df$StdError

model3_unhealth_sleep_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_unhealth_sleep_df$Estimate, model3_unhealth_sleep_df$LowerCI, model3_unhealth_sleep_df$UpperCI)
write.csv(model3_unhealth_sleep_df, './multi/model3_unhealth_sleep_df.csv')
########education########
table(isolation3$education_level)
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,29:35,37,44,46,47)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(education_level == 'College'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_education_college_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_education_college_df <- model3_education_college_df %>%
  filter(grepl(pattern, Term))
model3_education_college_df$adjusted_pvalue <- p.adjust(model3_education_college_df$Pr, method = "BH")
colnames(model3_education_college_df)

model3_education_college_df$LowerCI <- model3_education_college_df$Estimate - 1.96 * model3_education_college_df$StdError
model3_education_college_df$UpperCI <- model3_education_college_df$Estimate + 1.96 * model3_education_college_df$StdError

model3_education_college_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_education_college_df$Estimate, model3_education_college_df$LowerCI, model3_education_college_df$UpperCI)
write.csv(model3_education_college_df, './multi/model3_education_college_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(education_level == 'other'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_education_other_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_education_other_df <- model3_education_other_df %>%
  filter(grepl(pattern, Term))
model3_education_other_df$adjusted_pvalue <- p.adjust(model3_education_other_df$Pr, method = "BH")
colnames(model3_education_other_df)

model3_education_other_df$LowerCI <- model3_education_other_df$Estimate - 1.96 * model3_education_other_df$StdError
model3_education_other_df$UpperCI <- model3_education_other_df$Estimate + 1.96 * model3_education_other_df$StdError

model3_education_other_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_education_other_df$Estimate, model3_education_other_df$LowerCI, model3_education_other_df$UpperCI)
write.csv(model3_education_other_df, './multi/model3_education_other_df.csv')

########met########
table(isolation3$Summed_MET_cat)
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,29:30,32:35,37,44,46,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Summed_MET_cat == 'Low'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_met_low_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_met_low_df <- model3_met_low_df %>%
  filter(grepl(pattern, Term))
model3_met_low_df$adjusted_pvalue <- p.adjust(model3_met_low_df$Pr, method = "BH")
colnames(model3_met_low_df)

model3_met_low_df$LowerCI <- model3_met_low_df$Estimate - 1.96 * model3_met_low_df$StdError
model3_met_low_df$UpperCI <- model3_met_low_df$Estimate + 1.96 * model3_met_low_df$StdError

model3_met_low_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_met_low_df$Estimate, model3_met_low_df$LowerCI, model3_met_low_df$UpperCI)
write.csv(model3_met_low_df, './multi/model3_met_low_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Summed_MET_cat == 'Medium'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_met_medium_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_met_medium_df <- model3_met_medium_df %>%
  filter(grepl(pattern, Term))
model3_met_medium_df$adjusted_pvalue <- p.adjust(model3_met_medium_df$Pr, method = "BH")
colnames(model3_met_medium_df)

model3_met_medium_df$LowerCI <- model3_met_medium_df$Estimate - 1.96 * model3_met_medium_df$StdError
model3_met_medium_df$UpperCI <- model3_met_medium_df$Estimate + 1.96 * model3_met_medium_df$StdError

model3_met_medium_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_met_medium_df$Estimate, model3_met_medium_df$LowerCI, model3_met_medium_df$UpperCI)
write.csv(model3_met_medium_df, './multi/model3_met_medium_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Summed_MET_cat == 'High'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_met_high_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_met_high_df <- model3_met_high_df %>%
  filter(grepl(pattern, Term))
model3_met_high_df$adjusted_pvalue <- p.adjust(model3_met_high_df$Pr, method = "BH")
colnames(model3_met_high_df)

model3_met_high_df$LowerCI <- model3_met_high_df$Estimate - 1.96 * model3_met_high_df$StdError
model3_met_high_df$UpperCI <- model3_met_high_df$Estimate + 1.96 * model3_met_high_df$StdError

model3_met_high_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_met_high_df$Estimate, model3_met_high_df$LowerCI, model3_met_high_df$UpperCI)
write.csv(model3_met_high_df, './multi/model3_met_high_df.csv')
########Townsend########
table(isolation3$Townsend_cat)
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,29,31:35,37,44,46,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Townsend_cat == 'Low'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_town_low_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_town_low_df <- model3_town_low_df %>%
  filter(grepl(pattern, Term))
model3_town_low_df$adjusted_pvalue <- p.adjust(model3_town_low_df$Pr, method = "BH")
colnames(model3_town_low_df)

model3_town_low_df$LowerCI <- model3_town_low_df$Estimate - 1.96 * model3_town_low_df$StdError
model3_town_low_df$UpperCI <- model3_town_low_df$Estimate + 1.96 * model3_town_low_df$StdError

model3_town_low_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_town_low_df$Estimate, model3_town_low_df$LowerCI, model3_town_low_df$UpperCI)
write.csv(model3_town_low_df, './multi/model3_town_low_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Townsend_cat == 'Medium'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_town_medium_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_town_medium_df <- model3_town_medium_df %>%
  filter(grepl(pattern, Term))
model3_town_medium_df$adjusted_pvalue <- p.adjust(model3_town_medium_df$Pr, method = "BH")
colnames(model3_town_medium_df)

model3_town_medium_df$LowerCI <- model3_town_medium_df$Estimate - 1.96 * model3_town_medium_df$StdError
model3_town_medium_df$UpperCI <- model3_town_medium_df$Estimate + 1.96 * model3_town_medium_df$StdError

model3_town_medium_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_town_medium_df$Estimate, model3_town_medium_df$LowerCI, model3_town_medium_df$UpperCI)
write.csv(model3_town_medium_df, './multi/model3_town_medium_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Townsend_cat == 'High'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_town_high_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_town_high_df <- model3_town_high_df %>%
  filter(grepl(pattern, Term))
model3_town_high_df$adjusted_pvalue <- p.adjust(model3_town_high_df$Pr, method = "BH")
colnames(model3_town_high_df)

model3_town_high_df$LowerCI <- model3_town_high_df$Estimate - 1.96 * model3_town_high_df$StdError
model3_town_high_df$UpperCI <- model3_town_high_df$Estimate + 1.96 * model3_town_high_df$StdError

model3_town_high_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_town_high_df$Estimate, model3_town_high_df$LowerCI, model3_town_high_df$UpperCI)
write.csv(model3_town_high_df, './multi/model3_town_high_df.csv')
########heart_disease########
table(isolation3$heart_disease)
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,29:31,33:35,37,44,46,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(heart_disease == 'no'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_heart_no_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_heart_no_df <- model3_heart_no_df %>%
  filter(grepl(pattern, Term))
model3_heart_no_df$adjusted_pvalue <- p.adjust(model3_heart_no_df$Pr, method = "BH")
colnames(model3_heart_no_df)

model3_heart_no_df$LowerCI <- model3_heart_no_df$Estimate - 1.96 * model3_heart_no_df$StdError
model3_heart_no_df$UpperCI <- model3_heart_no_df$Estimate + 1.96 * model3_heart_no_df$StdError

model3_heart_no_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_heart_no_df$Estimate, model3_heart_no_df$LowerCI, model3_heart_no_df$UpperCI)
write.csv(model3_heart_no_df, './multi/model3_heart_no_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(heart_disease == 'yes'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_heart_yes_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_heart_yes_df <- model3_heart_yes_df %>%
  filter(grepl(pattern, Term))
model3_heart_yes_df$adjusted_pvalue <- p.adjust(model3_heart_yes_df$Pr, method = "BH")
colnames(model3_heart_yes_df)

model3_heart_yes_df$LowerCI <- model3_heart_yes_df$Estimate - 1.96 * model3_heart_yes_df$StdError
model3_heart_yes_df$UpperCI <- model3_heart_yes_df$Estimate + 1.96 * model3_heart_yes_df$StdError

model3_heart_yes_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_heart_yes_df$Estimate, model3_heart_yes_df$LowerCI, model3_heart_yes_df$UpperCI)
write.csv(model3_heart_yes_df, './multi/model3_heart_yes_df.csv')

########strok########
table(isolation3$strok)
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,29:32,34:35,37,44,46,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(strok == 'no'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_strok_no_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_strok_no_df <- model3_strok_no_df %>%
  filter(grepl(pattern, Term))
model3_strok_no_df$adjusted_pvalue <- p.adjust(model3_strok_no_df$Pr, method = "BH")
colnames(model3_strok_no_df)

model3_strok_no_df$LowerCI <- model3_strok_no_df$Estimate - 1.96 * model3_strok_no_df$StdError
model3_strok_no_df$UpperCI <- model3_strok_no_df$Estimate + 1.96 * model3_strok_no_df$StdError

model3_strok_no_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_strok_no_df$Estimate, model3_strok_no_df$LowerCI, model3_strok_no_df$UpperCI)
write.csv(model3_strok_no_df, './multi/model3_strok_no_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(strok == 'yes'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_strok_yes_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_strok_yes_df <- model3_strok_yes_df %>%
  filter(grepl(pattern, Term))
model3_strok_yes_df$adjusted_pvalue <- p.adjust(model3_strok_yes_df$Pr, method = "BH")
colnames(model3_strok_yes_df)

model3_strok_yes_df$LowerCI <- model3_strok_yes_df$Estimate - 1.96 * model3_strok_yes_df$StdError
model3_strok_yes_df$UpperCI <- model3_strok_yes_df$Estimate + 1.96 * model3_strok_yes_df$StdError

model3_strok_yes_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_strok_yes_df$Estimate, model3_strok_yes_df$LowerCI, model3_strok_yes_df$UpperCI)
write.csv(model3_strok_yes_df, './multi/model3_strok_yes_df.csv')

########Diabetes_mellitus########
table(isolation3$Diabetes_mellitus)
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,29:33,35,37,44,46,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Diabetes_mellitus == 'no'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_Diabetes_no_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_Diabetes_no_df <- model3_Diabetes_no_df %>%
  filter(grepl(pattern, Term))
model3_Diabetes_no_df$adjusted_pvalue <- p.adjust(model3_Diabetes_no_df$Pr, method = "BH")
colnames(model3_Diabetes_no_df)

model3_Diabetes_no_df$LowerCI <- model3_Diabetes_no_df$Estimate - 1.96 * model3_Diabetes_no_df$StdError
model3_Diabetes_no_df$UpperCI <- model3_Diabetes_no_df$Estimate + 1.96 * model3_Diabetes_no_df$StdError

model3_Diabetes_no_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_Diabetes_no_df$Estimate, model3_Diabetes_no_df$LowerCI, model3_Diabetes_no_df$UpperCI)
write.csv(model3_Diabetes_no_df, './multi/model3_Diabetes_no_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Diabetes_mellitus == 'yes'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_Diabetes_yes_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_Diabetes_yes_df <- model3_Diabetes_yes_df %>%
  filter(grepl(pattern, Term))
model3_Diabetes_yes_df$adjusted_pvalue <- p.adjust(model3_Diabetes_yes_df$Pr, method = "BH")
colnames(model3_Diabetes_yes_df)

model3_Diabetes_yes_df$LowerCI <- model3_Diabetes_yes_df$Estimate - 1.96 * model3_Diabetes_yes_df$StdError
model3_Diabetes_yes_df$UpperCI <- model3_Diabetes_yes_df$Estimate + 1.96 * model3_Diabetes_yes_df$StdError

model3_Diabetes_yes_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_Diabetes_yes_df$Estimate, model3_Diabetes_yes_df$LowerCI, model3_Diabetes_yes_df$UpperCI)
write.csv(model3_Diabetes_yes_df, './multi/model3_Diabetes_yes_df.csv')

########Hypertension########
table(isolation3$Hypertension)
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,29:34,37,44,46,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Hypertension == 'no'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_Hypertension_no_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_Hypertension_no_df <- model3_Hypertension_no_df %>%
  filter(grepl(pattern, Term))
model3_Hypertension_no_df$adjusted_pvalue <- p.adjust(model3_Hypertension_no_df$Pr, method = "BH")
colnames(model3_Hypertension_no_df)

model3_Hypertension_no_df$LowerCI <- model3_Hypertension_no_df$Estimate - 1.96 * model3_Hypertension_no_df$StdError
model3_Hypertension_no_df$UpperCI <- model3_Hypertension_no_df$Estimate + 1.96 * model3_Hypertension_no_df$StdError

model3_Hypertension_no_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_Hypertension_no_df$Estimate, model3_Hypertension_no_df$LowerCI, model3_Hypertension_no_df$UpperCI)
write.csv(model3_Hypertension_no_df, './multi/model3_Hypertension_no_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Hypertension == 'yes'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_Hypertension_yes_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_Hypertension_yes_df <- model3_Hypertension_yes_df %>%
  filter(grepl(pattern, Term))
model3_Hypertension_yes_df$adjusted_pvalue <- p.adjust(model3_Hypertension_yes_df$Pr, method = "BH")
colnames(model3_Hypertension_yes_df)

model3_Hypertension_yes_df$LowerCI <- model3_Hypertension_yes_df$Estimate - 1.96 * model3_Hypertension_yes_df$StdError
model3_Hypertension_yes_df$UpperCI <- model3_Hypertension_yes_df$Estimate + 1.96 * model3_Hypertension_yes_df$StdError

model3_Hypertension_yes_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_Hypertension_yes_df$Estimate, model3_Hypertension_yes_df$LowerCI, model3_Hypertension_yes_df$UpperCI)
write.csv(model3_Hypertension_yes_df, './multi/model3_Hypertension_yes_df.csv')


########cancer_history########
table(isolation3$caner_history)
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,29:35,37,44,46,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(caner_history == 'no'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_cancer_no_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_cancer_no_df <- model3_cancer_no_df %>%
  filter(grepl(pattern, Term))
model3_cancer_no_df$adjusted_pvalue <- p.adjust(model3_cancer_no_df$Pr, method = "BH")
colnames(model3_cancer_no_df)

model3_cancer_no_df$LowerCI <- model3_cancer_no_df$Estimate - 1.96 * model3_cancer_no_df$StdError
model3_cancer_no_df$UpperCI <- model3_cancer_no_df$Estimate + 1.96 * model3_cancer_no_df$StdError

model3_cancer_no_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_cancer_no_df$Estimate, model3_cancer_no_df$LowerCI, model3_cancer_no_df$UpperCI)
write.csv(model3_cancer_no_df, './multi/model3_cancer_no_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(caner_history == 'yes'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_cancer_yes_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_cancer_yes_df <- model3_cancer_yes_df %>%
  filter(grepl(pattern, Term))
model3_cancer_yes_df$adjusted_pvalue <- p.adjust(model3_cancer_yes_df$Pr, method = "BH")
colnames(model3_cancer_yes_df)

model3_cancer_yes_df$LowerCI <- model3_cancer_yes_df$Estimate - 1.96 * model3_cancer_yes_df$StdError
model3_cancer_yes_df$UpperCI <- model3_cancer_yes_df$Estimate + 1.96 * model3_cancer_yes_df$StdError

model3_cancer_yes_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_cancer_yes_df$Estimate, model3_cancer_yes_df$LowerCI, model3_cancer_yes_df$UpperCI)
write.csv(model3_cancer_yes_df, './multi/model3_cancer_yes_df.csv')



########smoking########
table(isolation3$smoking_status)
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,30:35,37,44,46,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(smoking_status == 'current'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_smoking_current_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_smoking_current_df <- model3_smoking_current_df %>%
  filter(grepl(pattern, Term))
model3_smoking_current_df$adjusted_pvalue <- p.adjust(model3_smoking_current_df$Pr, method = "BH")
colnames(model3_smoking_current_df)

model3_smoking_current_df$LowerCI <- model3_smoking_current_df$Estimate - 1.96 * model3_smoking_current_df$StdError
model3_smoking_current_df$UpperCI <- model3_smoking_current_df$Estimate + 1.96 * model3_smoking_current_df$StdError

model3_smoking_current_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_smoking_current_df$Estimate, model3_smoking_current_df$LowerCI, model3_smoking_current_df$UpperCI)
write.csv(model3_smoking_current_df, './multi/model3_smoking_current_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(smoking_status == 'previous'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_smoking_previous_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_smoking_previous_df <- model3_smoking_previous_df %>%
  filter(grepl(pattern, Term))
model3_smoking_previous_df$adjusted_pvalue <- p.adjust(model3_smoking_previous_df$Pr, method = "BH")
colnames(model3_smoking_previous_df)

model3_smoking_previous_df$LowerCI <- model3_smoking_previous_df$Estimate - 1.96 * model3_smoking_previous_df$StdError
model3_smoking_previous_df$UpperCI <- model3_smoking_previous_df$Estimate + 1.96 * model3_smoking_previous_df$StdError

model3_smoking_previous_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_smoking_previous_df$Estimate, model3_smoking_previous_df$LowerCI, model3_smoking_previous_df$UpperCI)
write.csv(model3_smoking_previous_df, './multi/model3_smoking_previous_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(smoking_status == 'never'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_smoking_never_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_smoking_never_df <- model3_smoking_never_df %>%
  filter(grepl(pattern, Term))
model3_smoking_never_df$adjusted_pvalue <- p.adjust(model3_smoking_never_df$Pr, method = "BH")
colnames(model3_smoking_never_df)

model3_smoking_never_df$LowerCI <- model3_smoking_never_df$Estimate - 1.96 * model3_smoking_never_df$StdError
model3_smoking_never_df$UpperCI <- model3_smoking_never_df$Estimate + 1.96 * model3_smoking_never_df$StdError

model3_smoking_never_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_smoking_never_df$Estimate, model3_smoking_never_df$LowerCI, model3_smoking_never_df$UpperCI)
write.csv(model3_smoking_never_df, './multi/model3_smoking_never_df.csv')



########drinking########
table(isolation3$Alcohol_intake_frequency)
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:24,29:35,37,44,46,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Alcohol_intake_frequency == 'never'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_drinking_never_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_drinking_never_df <- model3_drinking_never_df %>%
  filter(grepl(pattern, Term))
model3_drinking_never_df$adjusted_pvalue <- p.adjust(model3_drinking_never_df$Pr, method = "BH")
colnames(model3_drinking_never_df)

model3_drinking_never_df$LowerCI <- model3_drinking_never_df$Estimate - 1.96 * model3_drinking_never_df$StdError
model3_drinking_never_df$UpperCI <- model3_drinking_never_df$Estimate + 1.96 * model3_drinking_never_df$StdError

model3_drinking_never_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_drinking_never_df$Estimate, model3_drinking_never_df$LowerCI, model3_drinking_never_df$UpperCI)
write.csv(model3_drinking_never_df, './multi/model3_drinking_never_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Alcohol_intake_frequency == 'occasionally'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_drinking_occasionally_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_drinking_occasionally_df <- model3_drinking_occasionally_df %>%
  filter(grepl(pattern, Term))
model3_drinking_occasionally_df$adjusted_pvalue <- p.adjust(model3_drinking_occasionally_df$Pr, method = "BH")
colnames(model3_drinking_occasionally_df)

model3_drinking_occasionally_df$LowerCI <- model3_drinking_occasionally_df$Estimate - 1.96 * model3_drinking_occasionally_df$StdError
model3_drinking_occasionally_df$UpperCI <- model3_drinking_occasionally_df$Estimate + 1.96 * model3_drinking_occasionally_df$StdError

model3_drinking_occasionally_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_drinking_occasionally_df$Estimate, model3_drinking_occasionally_df$LowerCI, model3_drinking_occasionally_df$UpperCI)
write.csv(model3_drinking_occasionally_df, './multi/model3_drinking_occasionally_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Alcohol_intake_frequency == 'sometimes'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_drinking_sometimes_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_drinking_sometimes_df <- model3_drinking_sometimes_df %>%
  filter(grepl(pattern, Term))
model3_drinking_sometimes_df$adjusted_pvalue <- p.adjust(model3_drinking_sometimes_df$Pr, method = "BH")
colnames(model3_drinking_sometimes_df)

model3_drinking_sometimes_df$LowerCI <- model3_drinking_sometimes_df$Estimate - 1.96 * model3_drinking_sometimes_df$StdError
model3_drinking_sometimes_df$UpperCI <- model3_drinking_sometimes_df$Estimate + 1.96 * model3_drinking_sometimes_df$StdError

model3_drinking_sometimes_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_drinking_sometimes_df$Estimate, model3_drinking_sometimes_df$LowerCI, model3_drinking_sometimes_df$UpperCI)
write.csv(model3_drinking_sometimes_df, './multi/model3_drinking_sometimes_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Alcohol_intake_frequency == 'Daily'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_drinking_daily_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_drinking_daily_df <- model3_drinking_daily_df %>%
  filter(grepl(pattern, Term))
model3_drinking_daily_df$adjusted_pvalue <- p.adjust(model3_drinking_daily_df$Pr, method = "BH")
colnames(model3_drinking_daily_df)

model3_drinking_daily_df$LowerCI <- model3_drinking_daily_df$Estimate - 1.96 * model3_drinking_daily_df$StdError
model3_drinking_daily_df$UpperCI <- model3_drinking_daily_df$Estimate + 1.96 * model3_drinking_daily_df$StdError

model3_drinking_daily_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_drinking_daily_df$Estimate, model3_drinking_daily_df$LowerCI, model3_drinking_daily_df$UpperCI)
write.csv(model3_drinking_daily_df, './multi/model3_drinking_daily_df.csv')

########sun_exposure########
table(isolation3$Time_outdoors_in_summer)
median(isolation3$Time_outdoors_in_summer)
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,29:35,37,46,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Time_outdoors_in_summer < 3))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_sun_low_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_sun_low_df <- model3_sun_low_df %>%
  filter(grepl(pattern, Term))
model3_sun_low_df$adjusted_pvalue <- p.adjust(model3_sun_low_df$Pr, method = "BH")
colnames(model3_sun_low_df)

model3_sun_low_df$LowerCI <- model3_sun_low_df$Estimate - 1.96 * model3_sun_low_df$StdError
model3_sun_low_df$UpperCI <- model3_sun_low_df$Estimate + 1.96 * model3_sun_low_df$StdError

model3_sun_low_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_sun_low_df$Estimate, model3_sun_low_df$LowerCI, model3_sun_low_df$UpperCI)
write.csv(model3_sun_low_df, './multi/model3_sun_low_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(Time_outdoors_in_summer >= 3))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_sun_long_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_sun_long_df <- model3_sun_long_df %>%
  filter(grepl(pattern, Term))
model3_sun_long_df$adjusted_pvalue <- p.adjust(model3_sun_long_df$Pr, method = "BH")
colnames(model3_sun_long_df)

model3_sun_long_df$LowerCI <- model3_sun_long_df$Estimate - 1.96 * model3_sun_long_df$StdError
model3_sun_long_df$UpperCI <- model3_sun_long_df$Estimate + 1.96 * model3_sun_long_df$StdError

model3_sun_long_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_sun_long_df$Estimate, model3_sun_long_df$LowerCI, model3_sun_long_df$UpperCI)
write.csv(model3_sun_long_df, './multi/model3_sun_long_df.csv')

########race########
table(isolation3$race)
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:20,22:25,29:35,37,44,46,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(race == 'White'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_race_white_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_race_white_df <- model3_race_white_df %>%
  filter(grepl(pattern, Term))
model3_race_white_df$adjusted_pvalue <- p.adjust(model3_race_white_df$Pr, method = "BH")
colnames(model3_race_white_df)

model3_race_white_df$LowerCI <- model3_race_white_df$Estimate - 1.96 * model3_race_white_df$StdError
model3_race_white_df$UpperCI <- model3_race_white_df$Estimate + 1.96 * model3_race_white_df$StdError

model3_race_white_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_race_white_df$Estimate, model3_race_white_df$LowerCI, model3_race_white_df$UpperCI)
write.csv(model3_race_white_df, './multi/model3_race_white_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(race != 'White'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_race_nowhite_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_race_nowhite_df <- model3_race_nowhite_df %>%
  filter(grepl(pattern, Term))
model3_race_nowhite_df$adjusted_pvalue <- p.adjust(model3_race_nowhite_df$Pr, method = "BH")
colnames(model3_race_nowhite_df)

model3_race_nowhite_df$LowerCI <- model3_race_nowhite_df$Estimate - 1.96 * model3_race_nowhite_df$StdError
model3_race_nowhite_df$UpperCI <- model3_race_nowhite_df$Estimate + 1.96 * model3_race_nowhite_df$StdError

model3_race_nowhite_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_race_nowhite_df$Estimate, model3_race_nowhite_df$LowerCI, model3_race_nowhite_df$UpperCI)
write.csv(model3_race_nowhite_df, './multi/model3_race_nowhite_df.csv')


##########income########
table(isolation3$income)
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:23,25,29:35,37,44,46,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(income == 'Less_than_18,000'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_income_low_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_income_low_df <- model3_income_low_df %>%
  filter(grepl(pattern, Term))
model3_income_low_df$adjusted_pvalue <- p.adjust(model3_income_low_df$Pr, method = "BH")
colnames(model3_income_low_df)

model3_income_low_df$LowerCI <- model3_income_low_df$Estimate - 1.96 * model3_income_low_df$StdError
model3_income_low_df$UpperCI <- model3_income_low_df$Estimate + 1.96 * model3_income_low_df$StdError

model3_income_low_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_income_low_df$Estimate, model3_income_low_df$LowerCI, model3_income_low_df$UpperCI)
write.csv(model3_income_low_df, './multi/model3_income_low_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(income %in% c('18,000_to_30,999', '31,000_to_51,999')))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_income_medium_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_income_medium_df <- model3_income_medium_df %>%
  filter(grepl(pattern, Term))
model3_income_medium_df$adjusted_pvalue <- p.adjust(model3_income_medium_df$Pr, method = "BH")
colnames(model3_income_medium_df)

model3_income_medium_df$LowerCI <- model3_income_medium_df$Estimate - 1.96 * model3_income_medium_df$StdError
model3_income_medium_df$UpperCI <- model3_income_medium_df$Estimate + 1.96 * model3_income_medium_df$StdError

model3_income_medium_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_income_medium_df$Estimate, model3_income_medium_df$LowerCI, model3_income_medium_df$UpperCI)
write.csv(model3_income_medium_df, './multi/model3_income_medium_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(income %in% c('52,000_to_100,000', 'Greater_than_100,000')))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_income_high_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_income_high_df <- model3_income_high_df %>%
  filter(grepl(pattern, Term))
model3_income_high_df$adjusted_pvalue <- p.adjust(model3_income_high_df$Pr, method = "BH")
colnames(model3_income_high_df)

model3_income_high_df$LowerCI <- model3_income_high_df$Estimate - 1.96 * model3_income_high_df$StdError
model3_income_high_df$UpperCI <- model3_income_high_df$Estimate + 1.96 * model3_income_high_df$StdError

model3_income_high_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_income_high_df$Estimate, model3_income_high_df$LowerCI, model3_income_high_df$UpperCI)
write.csv(model3_income_high_df, './multi/model3_income_high_df.csv')



########PRS########
table(isolation3$prs_ph_level)
colnames(isolation3)
dependent_vars <- colnames(isolation3)[c(8)]
independent_vars <- colnames(isolation3)[c(9:12)] ####自变量

# 附加协变量
covariates <- colnames(isolation3)[c(18:25,29:35,37,44,46,47,52)]

# 初始化一个列表来存储模型结果
results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(prs_ph_level == 'Low'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_prs_low_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_prs_low_df <- model3_prs_low_df %>%
  filter(grepl(pattern, Term))
model3_prs_low_df$adjusted_pvalue <- p.adjust(model3_prs_low_df$Pr, method = "BH")
colnames(model3_prs_low_df)

model3_prs_low_df$LowerCI <- model3_prs_low_df$Estimate - 1.96 * model3_prs_low_df$StdError
model3_prs_low_df$UpperCI <- model3_prs_low_df$Estimate + 1.96 * model3_prs_low_df$StdError

model3_prs_low_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_prs_low_df$Estimate, model3_prs_low_df$LowerCI, model3_prs_low_df$UpperCI)
write.csv(model3_prs_low_df, './multi/model3_prs_low_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(prs_ph_level == 'Medium'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_prs_medium_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_prs_medium_df <- model3_prs_medium_df %>%
  filter(grepl(pattern, Term))
model3_prs_medium_df$adjusted_pvalue <- p.adjust(model3_prs_medium_df$Pr, method = "BH")
colnames(model3_prs_medium_df)

model3_prs_medium_df$LowerCI <- model3_prs_medium_df$Estimate - 1.96 * model3_prs_medium_df$StdError
model3_prs_medium_df$UpperCI <- model3_prs_medium_df$Estimate + 1.96 * model3_prs_medium_df$StdError

model3_prs_medium_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_prs_medium_df$Estimate, model3_prs_medium_df$LowerCI, model3_prs_medium_df$UpperCI)
write.csv(model3_prs_medium_df, './multi/model3_prs_medium_df.csv')


results <- list()

# 执行批量回归
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    formula_str <- paste(dv, "~", iv, "+", paste(covariates, collapse = "+"))
    model <- lm(as.formula(formula_str), data = isolation3, subset = c(prs_ph_level == 'High'))
    results[[paste(dv, iv)]] <- model
  }
}

# 提取关键信息并将其保存到数据框中
model_data <- list()


for (key in names(results)) {
  coefficients <- summary(results[[key]])$coefficients
  if ("t value" %in% colnames(coefficients)) {
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = coefficients[, "t value"],
      Pr = coefficients[, "Pr(>|t|)"]
    )
  } else {
    # Handle the case where the column does not exist
    # For example, you might set tValue to NA or some other value
    data <- data.frame(
      Response = key,
      Term = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      StdError = coefficients[, "Std. Error"],
      tValue = NA,
      Pr = coefficients[, "Pr(>|t|)"]  # Assuming this column exists
    )
  }
  model_data[[key]] <- data
}


# 使用bind_rows合并所有的数据框
model3_prs_high_df <- bind_rows(model_data)
characters_to_include <- colnames(isolation3)[c(9:12)]

# 创建正则表达式，匹配任何包含上述字符的字符串
pattern <- paste(characters_to_include, collapse = "|")

# 使用dplyr的filter和grepl函数筛选包含特定字符的行
model3_prs_high_df <- model3_prs_high_df %>%
  filter(grepl(pattern, Term))
model3_prs_high_df$adjusted_pvalue <- p.adjust(model3_prs_high_df$Pr, method = "BH")
colnames(model3_prs_high_df)

model3_prs_high_df$LowerCI <- model3_prs_high_df$Estimate - 1.96 * model3_prs_high_df$StdError
model3_prs_high_df$UpperCI <- model3_prs_high_df$Estimate + 1.96 * model3_prs_high_df$StdError

model3_prs_high_df$BetaCI <- sprintf("%.2f (%.2f, %.2f)", model3_prs_high_df$Estimate, model3_prs_high_df$LowerCI, model3_prs_high_df$UpperCI)
write.csv(model3_prs_high_df, './multi/model3_prs_high_df.csv')