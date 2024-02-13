---
title: "statistic project"
output: pdf_document
date: "2023-07-03"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot. 
```{r}
#کتابخانه ها
library(ggplot2)
library(corrplot)
library(readxl)
library(readr)
library(fastDummies)
library(tidyverse)
library(tidyverse)
```


```{r}
#بارگزاری داده ها
data <- read.csv("C:\\Users\\digi kaj\\Desktop\\CarPrice_Assignment.csv")
```


```{r}
#boxplot
boxplot(data$carheight,data$carwidth,data$carlength)


```

```{r}
#missing value
library("tidyverse")
data %>%
  fill(names(.),.direction = "downup")


```



```{r}
# Remove rows with missing values
data <- na.omit(data)

# Convert categorical variables to numeric
data$fueltype <- as.numeric(data$fueltype)
data$aspiration <- as.numeric(data$aspiration)
data$doornumber <- as.numeric(data$doornumber)
data$carbody <- as.numeric(data$carbody)
data$drivewheel <- as.numeric(data$drivewheel)
data$enginelocation <- as.numeric(data$enginelocation)
data$enginetype <- as.numeric(data$enginetype)
data$cylindernumber <- as.numeric(data$cylindernumber)
data$fuelsystem <- as.numeric(data$fuelsystem)

```

```{r}
library(corrplot)
library(ggcorrplot)
library(Hmisc)

# Subset data to exclude "name" column
data_subset <- subset(data, select = -c(CarName))

# Compute correlation matrix
corr_matrix <- cor(data_subset)

# Create ggcorrplot
ggcorrplot(corr_matrix, type = "upper", colors = c("#6D5Ec1", "white", "#E46796"))

# Create correlation plot with imputed values
#corrplot(corr_matrix_imputed, method = "circle")

# Create ggcorrplot with imputed values
#ggcorrplot(corr_matrix_imputed, type = "upper", colors = c("#6D5Ec1", "white", "#E46796"))

```


```{r}
#4 hypothetican hypotheses
#1. یک همبستگی مثبت قابل توجه بین wheelbase و carlength وجود دارد.
#2. یک همبستگی مثیت قابل توجه بین carheight و carwidth آن وجود دارد.
#3. یک همبستگی مثبت قابل توجه بین قدرت اسب بخار و قیمت خودرو وجود دارد.
#4. یک همبستگی مثبت قابل توجه بین طول و عرض خودروها وجود دارد.

```




```{r}

new_data <- read.csv("C:\\Users\\digi kaj\\Desktop\\CarPrice_Assignment.csv")
data <- rbind(data, new_data)


  
```{r}
data$wheelbase_rank <- rank(data$wheelbase)
data$carlength_rank <- rank(data$carlength)
names(data)
# Calculate correlation coefficient and p-value
cor_test <- cor.test(data$Displacement_rank, data$HighwayMPG_rank, method = "spearman")

# Print results
print(cor_test)
```

```{r}
#2
data$carheight_rank <- rank(data$carheight)
data$carwidth_rank <- rank(data$carwidth
)

# Calculate correlation coefficient and p-value
cor_test <- cor.test(data$carheight_rank, data$carwidth_rank, method = "spearman")

# Print results
print(cor_test)
```


```{r}
#3
cor_test <- cor.test(data$highwaympg, data$citympg)

# Print results
print(cor_test)

```
```{r}
#4
cor_test <- cor.test(data$boreratio, data$stroke)

# Print results
print(cor_test)

```


```{r}
library(caret)


# Identify categorical columns
categorical_columns <- c("fueltype", "aspiration", "doornumber", "carbody", "drivewheel", "enginelocation", "enginetype", "cylindernumber", "fuelsystem")

# Convert data to data frame (if necessary)
data <- as.data.frame(data)

# Create dummy variables
formula <- as.formula(paste0("~", paste(categorical_columns, collapse = "+")))
#dummy_variables <- dummyVars(formula, data = data)

# Apply dummy variables
dummy_data <- predict(dummy_variables, newdata = data)

# Concatenate with original DataFrame
new_data <- cbind.data.frame(data, dummy_data)


print(new_data)
```


```{r}
#dummy
library(caret)
data <- data

# Step 2: Identify categorical columns
categorical_columns <- c("fueltype", "aspiration", "doornumber","carbody","drivewheel","enginelocation","enginetype","cylindernumber","fuelsystem")

# Step 3: Create dummy variables

formula <- as.formula(paste0("~", paste(categorical_columns, collapse = "+")))
dummy_variables <- dummyVars(formula, data = data)

# Step 4: Apply dummy variables
dummy_data <- predict(dummy_variables, newdata = data.frame(data))

# Step 5: Concatenate with original DataFrame
new_data <- cbind(data, dummy_data)

# Print the new DataFrame
print(new_data)
view(new_data)
```


```{r}
#test and train
library(future)

# Set the seed for reproducibility
set.seed(123)

# Randomly shuffle the data
shuffled_data <- data[sample(nrow(data)), ]

# Calculate the index to split the data
split_index <- round(0.7 * nrow(shuffled_data))

# Split the data into training and testing sets
train_data <- shuffled_data[1:split_index, ]
test_data <- shuffled_data[(split_index+1):nrow(shuffled_data), ]

 #به نسبت 70و30
```

```{r}
برای پیش بینی اهمیت ویژگی ها میتوان از روش های مختلفی مانند ارزیابی مدل (model evaluation)، ارزیابی ویژگیها (feature evaluation) و ارزیابی اهمیت ویژگی ها (feature importance evaluation) استفاده کرد
در ارزیابی مدل، میتوان با اجرای مدل بر روی داده های آموزش و تست و بررسی عملکرد آن، میزان اهمیت ویژگیها را بررسی کرد. اگر یک ویژگی در بهبود عملکرد مدل نقش مهمی داشته باشد، به عنوان یک ویژگی موثر شناخته میشود.
در ارزیابی ویژگی ها، میتوان با بررسی همبستگی بین ویژگی ها و متغیر پاسخ، ویژگی هایی که با متغیر پاسخ همبستگی کمتری دارند را به عنوان ویژگی های کم اثر شناسایی کرد.
با توجه به نقشه همبستگی و مقادیر بدست آمده، میتوان ویژگی هایی که با متغیر پاسخ همبستگی بیشتری دارند  همچنین ویژگی هایی که با متغیر پاسخ همبستگی کمتری دارند، به ترتیب به عنوان ویژگیهای موثر و کم اثر شناسایی کرد. با این حال، برای بررسی اهمیت ویژگی ها با دقت بیشتر، بهتر است از روش های ارزیابی اهمیت ویژگیها استفاده کنید.


```

```{r}
#chapter 2

```



```{r}

#رگرسیون بر روی train data

library(MASS)
library(stringr)

# خواندن داده ها از فایل CSV
data <- read.csv("C:\\Users\\digi kaj\\Desktop\\CarPrice_Assignment.csv", stringsAsFactors = TRUE)

# حذف ستون car_ID
data$car_ID <- NULL

# حذف ردیف هایی که دارای مقادیر نامعتبر هستند
data <- na.omit(data)

# تبدیل متغیرهای دسته ای به متغیرهای عددی
cat_cols <- c("fueltype", "aspiration", "doornumber", "carbody", "drivewheel", "enginelocation", "enginetype", "cylindernumber", "fuelsystem")
for (col in cat_cols) {
  data[[col]] <- as.numeric(data[[col]])
}

# missing value
data <- data %>%
  fill(names(.), .direction = "downup")

# تبدیل متغیرهای رشته های به متغیرهای فاکتور
for (col in names(data)) {
  if (is.character(data[[col]])) {
    data[[col]] <- factor(data[[col]])
  }
}

# test and train
set.seed(123)
train_ind <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_ind, ]
test_data <- data[-train_ind, ]

# بدست آوردن مدل رگرسیون خطی چند متغیره
model_1 <- lm(price ~ ., data = train_data)

# چاپ خلاصه ای از مدل
summary(model_1)


# پیش بینی قیمت خودروها با استفاده از train data
Predict_price_train <- predict(model_1, train_data)
train_rrs <- sum((Predict_price_train - train_data$price)^2)
train_ees <- sum((Predict_price_train - mean(Predict_price_train))^2)
train_MSE <- sum((Predict_price_train - train_data$price)^2) / length(Predict_price_train)

# بدست آوردن مدل رگرسیون خطی با استفاده از متغیرهای انتخاب شده
model_2 <- lm(price ~ wheelbase + carwidth + symboling, data = train_data)


# پیش بینی قیمت خودروها با استفاده از داده های آموزش
Predict_price_train <- predict(model_2, train_data)
train_rrs <- sum((Predict_price_train - train_data$price)^2)
train_ees <- sum((Predict_price_train - mean(Predict_price_train))^2)
train_MSE <- sum((Predict_price_train - train_data$price)^2) / length(Predict_price_train)
```



```{r}
#رگرسیون بر روی train and train data

# حذف ستون car_ID
data$car_ID <- NULL

# حذف ردیف هایی که دارای مقادیر نامعتبر هستند
data <- na.omit(data)

# تبدیل متغیرهای دسته ای به متغیرهای عددی
cat_cols <- c("fueltype", "aspiration", "doornumber", "carbody", "drivewheel", "enginelocation", "enginetype", "cylindernumber", "fuelsystem")
for (col in cat_cols) {
  data[[col]] <- as.numeric(data[[col]])
}

# missing value
data <- data %>%
  fill(names(.), .direction = "downup")

# تبدیل متغیرهای رشته های به متغیرهای فاکتور
for (col in names(data)) {
  if (is.character(data[[col]])) {
    data[[col]] <- factor(data[[col]])
  }
}

# test and train
set.seed(123)
train_ind <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_ind, ]
test_data <- data[-train_ind, ]

# بدست آوردن مدل رگرسیون خطی چند متغیره
model_1 <- lm(price ~ ., data = train_data)

# چاپ خلاصه ای از مدل
summary(model_1)

# پیش بینی قیمت خودروها با استفاده از test data
Predict_price <- predict(model_1, test_data)
test_rrs <- sum((Predict_price - test_data$price)^2)
test_ees <- sum((Predict_price - mean(Predict_price))^2)
test_MSE <- sum((Predict_price - test_data$price)^2) / length(Predict_price)


# پیش بینی قیمت خودروها با استفاده از train data
Predict_price_train <- predict(model_1, train_data)
train_rrs <- sum((Predict_price_train - train_data$price)^2)
train_ees <- sum((Predict_price_train - mean(Predict_price_train))^2)
train_MSE <- sum((Predict_price_train - train_data$price)^2) / length(Predict_price_train)

# بدست آوردن مدل رگرسیون خطی با استفاده از متغیرهای انتخاب شده
model_2 <- lm(price ~ wheelbase + carwidth + symboling, data = train_data)


# پیش بینی قیمت خودروها با استفاده از داده های آموزش
Predict_price_train <- predict(model_2, train_data)
train_rrs <- sum((Predict_price_train - train_data$price)^2)
train_ees <- sum((Predict_price_train - mean(Predict_price_train))^2)
train_MSE <- sum((Predict_price_train - train_data$price)^2) / length(Predict_price_train)



# پیش بینی قیمت خودروها با استفاده از داده های آزمون
Predict_price <- predict(model_2, test_data)
test_rrs <- sum((Predict_price - test_data$price)^2)
test_ees <- sum((Predict_price - mean(Predict_price))^2)
test_MSE <- sum((Predict_price - test_data$price)^2) / length(Predict_price)

```




```{r}
#گزارش معیارها

# گزارش معیارهای پیش بینی بر روی train data
train_Predict_price <- predict(model_1, train_data)
train_RSS <- sum((train_Predict_price - train_data$price)^2)
train_TSS <- sum((train_data$price - mean(train_data$price))^2)
train_MSE <- mean((train_Predict_price - train_data$price)^2)
train_R_Squared <- 1 - train_RSS/train_TSS
train_n <- length(train_data$price)
train_p <- length(model_1$coefficients) - 1
train_adjusted_R_Squared <- 1 - ((1 - train_R_Squared) * (train_n - 1))/(train_n - train_p - 1)

# گزارش معیارهای پیش بینی بر روی test data
test_Predict_price <- predict(model_1, test_data)
test_RSS <- sum((test_Predict_price - test_data$price)^2)
test_TSS <- sum((test_data$price - mean(test_data$price))^2)
test_MSE <- mean((test_Predict_price - test_data$price)^2)
test_R_Squared <- 1 - test_RSS/test_TSS
test_n <- length(test_data$price)
test_p <- length(model_1$coefficients) - 1
test_adjusted_R_Squared <- 1 - ((1 - test_R_Squared) * (test_n - 1))/(test_n - test_p - 1)

# چاپ معیارهای پیش بینی

cat("Test data:\n")
cat("RSS: ", test_RSS, "\n")
cat("TSS: ", test_TSS, "\n")
cat("MSE: ", test_MSE, "\n")
cat("R-squared: ", test_R_Squared, "\n")
cat("Adjusted R-squared: ", test_adjusted_R_Squared, "\n\n\n")


cat("Train data:\n")
cat("RSS: ", train_RSS, "\n")
cat("TSS: ", train_TSS, "\n")
cat("MSE: ", train_MSE, "\n")
cat("R-squared: ",train_R_Squared, "\n")
cat("Adjusted R-squared: ", train_adjusted_R_Squared, "\n")


```

```{r}

 معیارها به شرح زیر اند:

- RSS (Residual Sum of Squares): مجموع مربعات باقیمانده ها است و نشان میدهد میزان مغایرت بین مقادیر واقعی و پیش بینی شده توسط مدل.
- TSS (Total Sum of Squares): مجموع مربعات اختلاف واقعی و میانگین است و نشان میدهد کل متغیریت وابسته را در داده ها توصیف میکند.
- MSE (Mean Squared Error): میانگین مربعات باقیمانده ها است و نشان میدهد میزان خطا در پیش بینی مدل است.
- R-Squared (Coefficient of Determination): ضریب تعیین است که نشان میدهد چه میزان از واریابل وابسته توسط متغیرهای مستقل توضیح میشود. مقدار آن بین 0 و 1 است و هرچه نزدیکتر به 1 باشد، نشاندهنده تطابق بهتر مدل با داده ها است.
- Adjusted R-Squared (Adjusted Coefficient of Determination): ضریب تعیین تصحیح شده است که در نظر میگیرد که تعداد متغیرهای مستقل در مدل چقدر است. زمانی که تعداد متغیرها بیشتر می‌شود، R-Squared به شدت بالا میرود، اما ممکن است این افزایش بهبود عملکرد واقعی مدل را نشان ندهد. به همین دلیل از مقدار تعیین شده R-Squared برای تصحیح استفاده میشود.


```

```{r}
#نقشه مقایسه میزان ضرایب


library(ggplot2)

# استخراج ضرایب مدل
coefficients <- summary(model_1)$coefficients

# تبدیل داده‌های ضرایب به دیتافریم
coefficients_df <- data.frame(variable = row.names(coefficients), 
                              estimate = coefficients[, 1],
                              std_error = coefficients[, 2])

# رسم نمودار
ggplot(coefficients_df, aes(x = variable, y = estimate)) +
  geom_bar(stat = "identity", fill = "darkorange", alpha = 0.6) +
  geom_errorbar(aes(ymin = estimate - std_error, ymax = estimate + std_error),
                width = 0.4, color = "purple", alpha = 0.8) +
  coord_flip() +
  labs(title = "Comparison of Coefficients",
       x = "Variable",
       y = "Coefficient Estimate") +
  theme(plot.title = element_text(hjust = 0.5))

```

```{r}
#تحلیل نقشه مقایسه میزان ضرایب

ضریب هر متغیر مستقل با نوع خطی که به سمت بالا یا پایین از صفر است، نشان میدهد که آیا این متغیر مستقل باعث افزایش یا کاهش متغیر وابسته میشود. همچنین، طول خط خطا (error bar) نشان دهنده ی بازه ی اطمینان برای ضریب هر متغیر مستقل است. اگر خط خطا برای یک ضریب بسیار کوتاه باشد، این نشان دهنده ی اطمینان بالای ما از ضریب است.

باید توجه داشت که زیاد بودن یک ضریب به دلیل اهمیت بالای آن نیست، بلکه به معنای این است که متغیر مستقل مرتبط با متغیر وابسته است. همچنین، مقیاس داده ها نقشی در اهمیت ضرایب ندارد؛ زیرا مدل همواره به دنبال یافتن بهترین رابطه بین متغیرهای وابسته و مستقل است، بدون در نظر گرفتن مقیاس داده ها. به عنوان مثال، اگر مقیاس داده ها با یکدیگر تفاوت داشته باشد، ضرایب متغیرهای مستقل همچنان می توانند با یکدیگر مقایسه شوند؛ زیرا مدل بهترین رابطه بین متغیرهای وابسته و مستقل را پیدا می کند، بدون در نظر گرفتن مقیاس داده ها. با این حال، برای تفسیر ضرایب، باید دانش کافی در زمینه ی داده ها و مدل سازی داشته باشیم و همچنین باید به نکات مربوط به مقیاس داده ها و تعامل متغیرهای مستقل با یکدیگر و با متغیر وابسته توجه کنی

```

```{r}
#عملکرد روی دادگان تست و بررسی راهکار های بهبود

برای بهبود عملکرد مدل:
   تکنیک هایی مانند تبدیل متغیرهای دسته ای به متغیرهای دسته ای شامل اعداد و یا استفاده از روش های رگرسیون غیرخطی مانند رگرسیون لجستیکی. همچنین، با توجه به نتایج ارزیابی مدل، میتوان متغیرهای بی اهمیت را حذف کرده یا به داده های دیگری توجه کرد و متغیرهای جدیدی به مدل اضافه کرد. علاوه بر این، میتوان از روشهای مانیتورینگ و تجزیه و تحلیل دقیق خطا، برای شناسایی مشکلات و بهبود عملکرد مدل استفاده کرد.


```

```{r}
#امتیازی - رگرسیون لجستیک

library(MASS)
library(stringr)

# خواندن داده ها از فایل CSV
data <- read.csv("C:\\Users\\digi kaj\\Desktop\\CarPrice_Assignment.csv", stringsAsFactors = TRUE)

# حذف ستون car_ID
data$car_ID <- NULL

# حذف ردیف هایی که دارای مقادیر نامعتبر هستند
data <- na.omit(data)

# تبدیل متغیرهای دسته ای به متغیرهای عددی
cat_cols <- c("fueltype", "aspiration", "doornumber", "carbody", "drivewheel", "enginelocation", "enginetype", "cylindernumber", "fuelsystem")
for (col in cat_cols) {
  data[[col]] <- as.numeric(data[[col]])
}

# تبدیل متغیر قیمت به یک متغیر دودویی
data$price_cat <- ifelse(data$price > mean(data$price), 1, 0)

# پر کردن مقادیر خالی با مقدار قبلی یا بعدی
data <- data %>%
  fill(names(.), .direction = "downup")

# تبدیل متغیرهای رشته ای به متغیرهای فاکتور
for (col in names(data)) {
  if (is.character(data[[col]])) {
    data[[col]] <- factor(data[[col]])
  }
}

# تقسیم داده ها به دو مجموعه آموزش و آزمون
set.seed(123)
train_ind <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_ind, ]
test_data <- data[-train_ind, ]

# بدست آوردن مدل رگرسیون لجستیک
model_1 <- glm(price_cat ~ ., data = train_data, family = binomial)

# چاپ خلاصه ای از مدل
summary(model_1)

# پیش بینی قیمت خودروها با استفاده از داده های آزمون
Predict_price <- predict(model_1, test_data, type="response")
test_pred <- ifelse(Predict_price > 0.5, 1, 0)
confusion_matrix <- table(test_pred, test_data$price_cat)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
sensitivity <- confusion_matrix[2, 2] / (confusion_matrix[2, 2] + confusion_matrix[2, 1])
specificity <- confusion_matrix[1, 1] / (confusion_matrix[1, 1] + confusion_matrix[1, 2])

# پیش بینی قیمت خودروها با استفاده از داده های آموزش
Predict_price_train <- predict(model_1, train_data, type="response")
train_pred <- ifelse(Predict_price_train > 0.5, 1, 0)
confusion_matrix_train <- table(train_pred, train_data$price_cat)
accuracy_train <- sum(diag(confusion_matrix_train))/sum(confusion_matrix_train)
sensitivity_train <- confusion_matrix_train[2, 2] / (confusion_matrix_train[2, 2] + confusion_matrix_train[2, 1])
specificity_train <- confusion_matrix_train[1, 1] / (confusion_matrix_train[1, 1] + confusion_matrix_train[1, 2])

# چاپ معیارهای ارزیابی برای داده های آزمون
cat("Test dataset:\n")
cat("Accuracy: ", accuracy, "\n")
cat("Sensitivity: ", sensitivity, "\n")
cat("Specificity: ", specificity, "\n")

# چاپ معیارهای ارزیابی برای داده های آموزش
cat("Train dataset:\n")
cat("Accuracy: ", accuracy_train, "\n")
cat("Sensitivity: ", sensitivity_train, "\n")
cat("Specificity: ", specificity_train, "\n")
```





















