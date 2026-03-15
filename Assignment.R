library(ggcorrplot) # for heatmaps
library(tidyverse) # library that includes both ggplot & dplyr
library(readxl) # loading excel data

# --This section is for function for data visualizations--
# a function that plots a scatter plot
plt_scatter <- function(data, x, y, title) {
  ggplot(data, aes({{x}}, {{y}})) +
    geom_point(color="skyblue") + ggtitle(title) +
    theme_minimal()
}

# a function that plots a bar graph
plt_bar <- function(data, x, y, title) {
  ggplot(data, aes({{x}}, {{y}})) +
    geom_col(fill="skyblue", color="black", alpha=0.7) + ggtitle(title) +
    theme_minimal()
}

# a function that plots a histogram
plt_hist <- function(data, x, title) {
  ggplot(data, aes({{x}})) +
    geom_histogram(fill="skyblue", color="white") + scale_x_log10() + ggtitle(title) +
    theme_minimal()
}

# a function that plots a boxplot
plt_boxplot <- function(data, x, y, title) {
  ggplot(data, aes({{x}}, {{y}})) + 
    geom_boxplot(fill="skyblue") + scale_y_log10() + ggtitle(title) +
    theme_minimal()
}

# a function that plots a line plot
plt_line <- function(data, x, y, title) {
  ggplot(data, aes({{x}}, {{y}})) + 
    geom_line(color="skyblue") + expand_limits(y=0) + ggtitle(title) +
    theme_minimal()
}

# load the data from the directory
df <- read_excel("German Credit Risk.xlsx")

# load the first 5 rows of data to examine
head(df)

# --This section is for actual data analysis
# this is the summary of the whole dataset
summary(df)

# table that shows how sex categories spend their credit amount in total
table_1 <- df %>%
  group_by(Sex, Purpose) %>%
  summarize(totalCredamount = sum(Credit_amount)) %>%
  pivot_wider(names_from = Purpose, values_from = totalCredamount)

# grouping by sex per total credit amount
total_cred_amount <- df %>% 
  group_by(Sex) %>%
  summarize(Total_Credit_Amount = sum(Credit_amount)) %>%
  arrange(desc(Total_Credit_Amount))

avg_duration_job <- df %>% 
  group_by(Job) %>% 
  summarize(meanDuration = mean(Duration)) %>%
  arrange(desc(meanDuration))

avg_duration_age <- df %>%
  group_by(Age) %>%
  summarize(meanDuration = mean(Duration)) %>%
  arrange(Age)

avg_cred_amount_saving <- df %>%
  filter(Saving_accounts != "NA") %>%
  group_by(Saving_accounts) %>%
  summarize(meanCredAmount = mean(Credit_amount)) %>%
  arrange(desc(meanCredAmount))

avg_duration_job # in average, the highly skilled jobs have higher credit duration
# standardize the labels in sex category
sex_cat = c("Female", "Male")
total_cred_amount$Sex <- factor(total_cred_amount$Sex, labels=sex_cat)
total_cred_amount # in total, Males have the highest total credit amount

# frequency table across checking accounts (excluding null vals)
freq_count_acc <- table(df$Checking_account, exclude = "NA")

freq_count_acc

df_plot <- as.data.frame(freq_count_acc)

# column names for reference
colnames(df_plot) <- c("Account_Type", "Count")

plt_bar(df_plot, Account_Type, Count, "Frequency of Checking Accounts")

# Analysis: Total Credit Amount per Sex Category indicates that Male applicants 
# account for a significantly higher volume of total credit issued compared to Female 
# applicants, more than doubling their total credit footprint.
plt_bar(total_cred_amount, Sex, Total_Credit_Amount, "Total Credit Amount per Sex Category")

# plot the average credit duration per job category
plt_bar(avg_duration_job, Job, meanDuration, "Average Credit Duration per Job Category")

# Analysis: Average Credit Amount per Savings Account Category shows that 
# 'moderate' savers have the highest mean credit, while 'quite rich' and 
# 'rich' categories surprisingly tend to have lower average borrowing.
plt_bar(avg_cred_amount_saving, Saving_accounts, meanCredAmount, "Average Credit Amount per Savings Account Category")

# on the histplot, the frequency shows that highest values sit around in between 1000 and 3000
plt_hist(df, Credit_amount, "Credit Amount Distribution")

# frequency distribution on Duration of Credit Amounts
plt_hist(df, Duration, "Duration of Credit Amounts")

# correlation across values, shows that duration & credit amount are the only values that
# has moderarely strong positive correlation between them
ggcorrplot(cor(df[sapply(df, is.numeric)]), lab = TRUE)

# plot the scatter plot of duration vs credit amount
plt_scatter(df, Duration, Credit_amount, "Duration vs Credit Amount")

# Analysis: Average Credit Duration by Age shows significant fluctuations 
# across different age groups, with peak average durations occurring around age 58, 
# while younger and much older applicants tend to have lower average durations.
plt_line(avg_duration_age, Age, meanDuration, "Average Credit Duration by Age")
