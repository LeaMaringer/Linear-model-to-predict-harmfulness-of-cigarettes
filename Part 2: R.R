# Import data
library(readxl)
cig <- read_excel("cigarettes.xlsx", sheet = 1, col_names = TRUE,)
colnames(cig) <- c("cigarette", "tar", "nicotine", "weight", "CO")

# Simple descriptive statistics
data_summary <- summary(cig)
print(data_summary)

# Point cloud 2 variables by 2 variables
pairs(cig[,2:5], col = "red", main = "Point clouds")

# General linear model
model <- lm(CO ~ tar+nicotine+weight, data = cig)
print(model)

# Model summary
model_summary <- summary(model)
print(model_summary)

# Standard errors of estimated coefficients
print(model_summary$coefficients[,2])

# Residuals
e <- model$residuals
print(mean(e)) 
# mean(e) = -6e-17 -> very small value close to 0, residuals are well centered

plot(cig$CO, e, ylab = "Residuals", xlab = "CO quantity", abline(h=0, col="red"))

# Studentized residual
student_residuals <- rstudent(model)

# Critical threshold (Student quantile)
alpha <- 0.1
student_threshold <- qt(1-alpha/2, 24 - 3 - 2)
print(student_threshold)
rstudent_outliers <- (student_residuals < -student_threshold| student_residuals > + student_threshold)
student_outliers <- cig[rstudent_outliers,]
print(student_outliers)
plot(cig$CO, student_residuals, xlab = "CO quantity", ylab = "Student residuals", cex = 0.75)
abline(h=-student_threshold, col = "red")
abline(h=+student_threshold, col = "red")
abline(h=0, col = "red")
text(cig$CO[rstudent_outliers], student_residuals[rstudent_outliers], rownames(cig)[rstudent_outliers])

# Leverage
indicators <- influence.measures(model)
residuals_hat <- indicators$infmat[,"hat"]
print(residuals_hat)

# Threshold : 2(p+1)/n
threshold_hat <- 2*4/24
print(threshold_hat)

# Outliers (in the sens of the leverage)
leverage_outliers <- (residuals_hat > threshold_hat)
outliers_hat <- cig[leverage_outliers,]
print(outliers_hat)

# Removing the 3 outliers we found
new_cig <- cig[-c(13, 15, 24),]

# New model
model2 <- lm(CO ~ tar+nicotine+weight, data = new_cig)

# Coefficient of determination
new_model_summary <- summary(model2)
print(new_model_summary)
