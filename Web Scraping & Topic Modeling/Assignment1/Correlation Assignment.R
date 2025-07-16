
install.packages("dplyr")
library(dplyr)


data <- read.csv("C:/Users/jinia/Desktop/CSE/Semester 10/Data Science/Final Term/Assignment/student_depression_dataset.csv", header = TRUE, sep = ',')
View(data)
str(data)

no_of_col <- ncol(data)
no_of_row <- nrow(data)
cat("No of rows in the dataset: ", no_of_row, "\n")
cat("No of columns in the dataset: ", no_of_col, "\n")

total_missing <- sum(is.na(mydata))
total_missing
mydata <- na.omit(mydata)



pearson_age <- cor.test(data$Age, data$Depression, method = "pearson")
cat("Age: r =", pearson_age$estimate, ", p-value =", pearson_age$p.value, "\n")

pearson_cgpa <- cor.test(data$CGPA, data$Depression, method = "pearson")
cat("CGPA: r =", pearson_cgpa$estimate, ", p-value =", pearson_cgpa$p.value, "\n")

pearson_study <- cor.test(data$Work.Study.Hours, data$Depression, method = "pearson")
cat("Work/Study Hours: r =", pearson_study$estimate, ", p-value =", pearson_study$p.value, "\n")




spearman_age <- cor.test(data$Age, data$Depression, method = "spearman")
cat("Age: rho =", spearman_age$estimate, ", p-value =", spearman_age$p.value, "\n")

spearman_cgpa <- cor.test(data$CGPA, data$Depression, method = "spearman")
cat("CGPA: rho =", spearman_cgpa$estimate, ", p-value =", spearman_cgpa$p.value, "\n")

spearman_study <- cor.test(data$Work.Study.Hours, data$Depression, method = "spearman")
cat("Work/Study Hours: rho =", spearman_study$estimate, ", p-value =", spearman_study$p.value, "\n")




data$Gender <- as.factor(data$Gender)
anova_gender <- aov(Depression ~ Gender, data = data)
summary_gender <- summary(anova_gender)
cat("Gender: F =", summary_gender[[1]][["F value"]][1], ", p-value =", summary_gender[[1]][["Pr(>F)"]][1], "\n")

data$Degree <- as.factor(data$Degree)
anova_degree <- aov(Depression ~ Degree, data = data)
summary_degree <- summary(anova_degree)
cat("Degree: F =", summary_degree[[1]][["F value"]][1], ", p-value =", summary_degree[[1]][["Pr(>F)"]][1], "\n")

data$Profession <- as.factor(data$Profession)
anova_profession <- aov(Depression ~ Profession, data = data)
summary_profession <- summary(anova_profession)
cat("Profession: F =", summary_profession[[1]][["F value"]][1], ", p-value =", summary_profession[[1]][["Pr(>F)"]][1], "\n")

data$Family.History.of.Mental.Illness <- as.factor(data$Family.History.of.Mental.Illness)
anova_history <- aov(Depression ~ Family.History.of.Mental.Illness, data = data)
summary_history <- summary(anova_history)
cat("Family History: F =", summary_history[[1]][["F value"]][1], ", p-value =", summary_history[[1]][["Pr(>F)"]][1], "\n")





chisq1 <- chisq.test(table(data$Gender, data$Degree))
cat("Gender vs Degree: Chi-Square =", chisq1$statistic, ", p-value =", chisq1$p.value, "\n")

chisq2 <- chisq.test(table(data$Gender, data$Family.History.of.Mental.Illness))
cat("Gender vs Family History: Chi-Square =", chisq2$statistic, ", p-value =", chisq2$p.value, "\n")

chisq3 <- chisq.test(table(data$Degree, data$Family.History.of.Mental.Illness))
cat("Degree vs Family History: Chi-Square =", chisq3$statistic, ", p-value =", chisq3$p.value, "\n")


