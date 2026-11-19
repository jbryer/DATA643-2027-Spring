library(ggplot2)
library(estimatr)

ggplot2::theme_set(ggplot2::theme_minimal())

daacs <- readRDS('data/daacs.rds')

t_out <- t.test(Term1GPA ~ Treat, data = daacs)
t_out
diff(t_out$estimate) |> unname() # GPA difference
# Cohen's d
unname(diff(t_out$estimate)) / sd(daacs$Term1GPA)

psych::describeBy(daacs$Term1GPA, group = daacs$Treat, mat = TRUE)

# Multilevel model
# library(lme4)
# library(performance)
# daacs2 <- daacs[complete.cases(daacs[,c('Term1GPA', 'Treat', 'HSGPA', 'USCITIZEN', 'Course')]),]
# null_model <- lmer(Term1GPA ~ 1 + (1 | Course), data = daacs2)
# icc(null_model)
# model_1 <- lmer(Term1GPA ~ Treat + HSGPA + USCITIZEN + (1 | Course), data = daacs2)
# summary(model_1)
# confint(model_1)
# m1_ml <- update(null_model, REML = FALSE)
# m2_ml <- update(model_1, REML = FALSE)
# anova(m1_ml, m2_ml)

##### compiler average causal effect ############################################
# Y = Outcome
# D = Actual treatment received (1 = treated, 0 = control)
# Z = Random treatment assignment / instrument (1 = assigned treatment, 0 = assigned control)

itt_fit_t <- t.test(Term1GPA ~ Treat, data = daacs)
itt_fit_t
diff(itt_fit_t$estimate) |> unname()

itt_fit <- difference_in_means(Term1GPA ~ Treat, data = daacs)
itt_fit
# first_stage_fit <- difference_in_means(Complier ~ Treat, data = daacs2)

# cace_model <- iv_robust(Term1GPA ~ Complier | Treat, data = daacs2)
cace_model <- iv_robust(
	Term1GPA ~ Complier + Ethnicity + Gender + USCitizen + Pell + Age + HSGPA | Treat + Ethnicity + Gender + USCitizen + Pell + Age + HSGPA,
	data = daacs)
case_table <- summary(cace_model)
case_table
case_table$coefficients['ComplierTRUE','Estimate'] # Effect in GPA unites
case_table$coefficients['ComplierTRUE','Estimate'] / sd(daacs2$Term1GPA) # Cohen's d


