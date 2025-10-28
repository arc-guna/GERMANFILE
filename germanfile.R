
# 1. Display the first 6 rows
head(germanfile,6)

# 2. Give the dimensions (rows, columns)
dim(germanfile)

# 3. Select rows with Duration < 30
selected_rows = germanfile[germanfile$Duration < 30,]
print(selected_rows)

# 4. Select the subset concerning Purpose == "education" OR Purpose == "car"
Purpose_edu_or_car <- subset(germanfile, Purpose == "education" | Purpose == "car")
print(Purpose_edu_or_car)

# 5. Display the number of loans with Duration equal to 24
Duration_24 = sum(germanfile$Duration == 24, na.rm = TRUE)
print(Duration_24)

# 6. Select loans with Credit.amount < 5000 OR > 10000
loans_credit_range <- subset(germanfile, Credit.amount < 5000 | Credit.amount > 10000)
head(loans_credit_range)

# 7. Extract the first and third row of the dataset
germanfile[c(1,3),]

# 8. Scatter plot: Credit Amount as a function of Duration, add regression line
plot(germanfile$Duration, germanfile$Credit.amount,
     xlab = "Duration (units in dataset)",
     ylab = "Credit amount",
     main = "Credit Amount vs Duration",
     pch = 19, cex = 0.6)

# Add regression line
fit <- lm(Credit.amount ~ Duration, data = germanfile)
abline(fit, lwd = 2)

# Show regression summary
summary(fit)

# 9. For each Purpose, indicate the number of rows
purpose_counts <- table(germanfile$Purpose)
purpose_counts

# 10. Create a pie chart then a barplot for Purpose counts
pie(purpose_counts, main = "Credit Purpose distribution (pie)")
barplot(purpose_counts, main = "Credit Purpose distribution (barplot)", las = 2)

# 11. Create a boxplot of the numerical variables of the dataset
numeric_cols <- sapply(germanfile, is.numeric)
boxplot(germanfile[, numeric_cols], main = "Boxplot of numeric variables", las = 2)

# 12. Same boxplot with a title and without outliers
boxplot(germanfile[, numeric_cols], main = "Boxplot (no outliers shown)", outline = FALSE, las = 2)

# 13. Display the pie chart and the boxplot side by side (par mfrow)
par(mfrow = c(1, 2))
pie(purpose_counts, main = "Purpose (pie)")
boxplot(germanfile[, numeric_cols], main = "Numeric vars (no outliers)", outline = FALSE, las = 2)
par(mfrow = c(1, 1))

# 14. Export the chart (pie chart and boxplot) to a PDF file
pdf("purpose_and_boxplot.pdf", width = 11, height = 6)
par(mfrow = c(1, 2))
pie(purpose_counts, main = "Purpose (pie)")
boxplot(germanfile[, numeric_cols], main = "Numeric vars (no outliers)", outline = FALSE, las = 2)
par(mfrow = c(1, 1))
dev.off()


#PART B

# 1. Display the names of the variables
names(germanfile)

# 2. Display basic statistical measures of the numerical variables
summary(germanfile[, numeric_cols])

# 3. Variance and standard deviation of Credit.amount
var(germanfile$Credit.amount)
sd(germanfile$Credit.amount)

# 4. Covariance between Credit.amount and Duration
cov_credit_duration <- cov(germanfile$Credit.amount, germanfile$Duration, use = "complete.obs")
cov_credit_duration

# 5. Insert variable Jeune that displays TRUE for Age >= 35
germanfile$Jeune <- germanfile$Age >= 35
table(germanfile$Jeune)

# 6. Create a function that calculates the amount per month of credit
amount_per_month <- function(credit_amount, duration_months) {
  if (any(duration_months == 0, na.rm = TRUE)) {
    stop("Duration must be non-zero")
  }
  amount_month <- credit_amount / duration_months
  return(amount_month)
}

germanfile$Amount_per_month <- amount_per_month(germanfile$Credit.amount, germanfile$Duration)
head(germanfile[, c("Credit.amount", "Duration", "Amount_per_month")])
