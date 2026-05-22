# ---- Load features (already contains unit_faculty & demographic_sex) ----
ind <- read.csv("TaskD_Dataset/forum_ind_train.csv", stringsAsFactors = FALSE)
ind$unit_faculty    <- factor(ind$unit_faculty)
ind$demographic_sex <- factor(ind$demographic_sex)

# The 93 numeric LIWC variables = everything except ID and the 2 categoricals
liwc_vars <- setdiff(names(ind), c("Unique_ID", "unit_faculty", "demographic_sex"))

# ============ SEX: F vs M ============
sex_df <- ind[ind$demographic_sex %in% c("F", "M"), ]   # drop NA and the 2 'X'
sex_df$demographic_sex <- droplevels(sex_df$demographic_sex)

sex_results <- data.frame()
for (v in liwc_vars) {
  tt <- t.test(sex_df[[v]] ~ sex_df$demographic_sex)     # Welch two-sample t-test
  sex_results <- rbind(sex_results, data.frame(
    variable = v,
    mean_F = tt$estimate[1], mean_M = tt$estimate[2],
    diff = tt$estimate[1] - tt$estimate[2], p_value = tt$p.value))
}
sex_results$p_adj <- p.adjust(sex_results$p_value, method = "BH")  # correct for 93 tests
sex_results <- sex_results[order(sex_results$p_adj), ]            # most significant first
head(sex_results, 10)

# ============ FACULTY: 10 groups ============
fac_df <- ind[!is.na(ind$unit_faculty), ]
fac_df$unit_faculty <- droplevels(fac_df$unit_faculty)

fac_results <- data.frame()
for (v in liwc_vars) {
  fit <- aov(fac_df[[v]] ~ fac_df$unit_faculty)          # one-way ANOVA
  p   <- summary(fit)[[1]][["Pr(>F)"]][1]                # pull the p-value out
  fac_results <- rbind(fac_results, data.frame(variable = v, p_value = p))
}
fac_results$p_adj <- p.adjust(fac_results$p_value, method = "BH")
fac_results <- fac_results[order(fac_results$p_adj), ]
head(fac_results, 10)

# Group-means table for the standout faculty variable
tapply(fac_df$money, fac_df$unit_faculty, mean)

# ============ PLOTS ============
library(ggplot2)
ggplot(sex_df, aes(demographic_sex, social, fill = demographic_sex)) +
  geom_boxplot() + labs(title = "Social words by sex", x = "Sex", y = "social") +
  theme_minimal()

ggplot(fac_df, aes(unit_faculty, money)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Money words by faculty", x = NULL, y = "money") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))