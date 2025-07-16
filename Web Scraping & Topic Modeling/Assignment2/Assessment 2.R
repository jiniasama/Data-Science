library(ggplot2)
library(dplyr)
library(moments) 
library(modeest)  

data <- read.csv("C:/Users/jinia/Desktop/CSE/Semester 10/Data Science/Final Term/Assignment2/student_depression_dataset.csv", header = TRUE, sep = ',')
View(data)
str(data)

hist_age <- hist(data$Age,
                 col = "skyblue",
                 main = "Histogram of Age with Curve",
                 xlab = "Age",
                 ylab = "Frequency",
                 freq = FALSE)

x <- seq(min(data$Age), max(data$Age), length = 100)
fit <- dnorm(x, mean = mean(data$Age), sd = sd(data$Age))

lines(x, fit, col = "#ADD8E6", lwd = 2)
polygon(c(min(x), x, max(x)), c(0, fit, 0), col = rgb(65/255, 105/255, 225/255, 0.4))


plot(hist_age$mids, hist_age$counts,
     type = "l",
     col = "blue",
     lwd = 2,
     xlab = "Age (Midpoints of bar)",
     ylab = "Frequency",
     main = "Line Histogram of Age")


age_skew <- skewness(data$Age, na.rm = TRUE)
cat("Skewness of Age:", age_skew, "\n")


if (age_skew > 0) {
  cat("The distribution of Age is positively skewed (tail on the right).\n")
} else if (age_skew < 0) {
  cat("The distribution of Age is negatively skewed (tail on the left).\n")
} else {
  cat("The distribution of Age is Symmetric.\n")
}

age_mean <- mean(data$Age, na.rm = TRUE)
age_median <- median(data$Age, na.rm = TRUE)
age_mode <- mfv(data$Age, na_rm = TRUE)

cat("Mean:", age_mean, "\n")
cat("Median:", age_median, "\n")
cat("Mode:", age_mode, "\n")



ggplot(data, aes(x = Gender)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Graph of Gender", x = "Gender", y = "Count")



ggplot(data, aes(y = Age)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Age", y = "Age")



#.......................................... 


ggplot(data, aes(x = Age, y = Work.Study.Hours)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Age vs Work/Study Hours",
       x = "Age", y = "Work/Study Hours")

ggplot(data, aes(x = Gender, y = Age, fill = Gender)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title = "Violin Plot with Boxplot of Age by Gender", x = "Gender", y = "Age")



