# Installing the necessary libraries
install.packages("e1071")
library(e1071)

#student scores data frame
student_scores <- data.frame(
  student = 1:17,
  no_visual_aids = c(50, 60, 58, 72, 36, 51, 49, 49, 25, 52, 41, 32, 58, 39, 25, 40, 61),
  with_visual_aids = c(58, 70, 60, 73, 40, 63, 54, 60, 29, 57, 66, 37, 50, 48, 80, 65, 70)
)

# Displaying structure and view of the data
str(student_scores)
View(student_scores)

# Creating a boxplot to compare scores
boxplot(
  student_scores$no_visual_aids, student_scores$with_visual_aids,
  names = c("No Visual Aids", "With Visual Aids"),
  main = "Scores Comparison",
  ylab = "Scores",
  col = c("violet", "darkgreen")
)

# Calculating the differences between scores with and without visual aids
differences <- student_scores$with_visual_aids - student_scores$no_visual_aids

# Q-Q plot for the differences
qqnorm(differences, main = "Q-Q Plot for Differences Between Visual Aids and No Visual Aids")
qqline(differences, col = "black", lwd = 2)

# Shapiro-Wilk normality tests for both groups
shapiro_result_no_aids <- shapiro.test(student_scores$no_visual_aids)
shapiro_result_with_aids <- shapiro.test(student_scores$with_visual_aids)
shapiro_result_combined <- shapiro.test(c(student_scores$no_visual_aids, student_scores$with_visual_aids))

# Printing the results of normality tests
print(shapiro_result_no_aids)
print(shapiro_result_with_aids)
print(shapiro_result_combined)

# Calculating descriptive statistics for both groups
mean_no_aids <- mean(student_scores$no_visual_aids)
mean_with_aids <- mean(student_scores$with_visual_aids)
sd_no_aids <- sd(student_scores$no_visual_aids)
sd_with_aids <- sd(student_scores$with_visual_aids)

# Printing descriptive statistics
print(paste("Mean (No Visual Aids):", mean_no_aids))
print(paste("Mean (With Visual Aids):", mean_with_aids))
print(paste("Standard Deviation (No Visual Aids):", sd_no_aids))
print(paste("Standard Deviation (With Visual Aids):", sd_with_aids))

# Calculating the paired t-test
t_test_result <- t.test(student_scores$no_visual_aids, student_scores$with_visual_aids, paired = TRUE)

# Printing the results of the paired t-test
print(t_test_result)
