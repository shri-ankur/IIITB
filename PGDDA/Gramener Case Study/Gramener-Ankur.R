# The following packages are loaded assuming they are already installed in the 
# R environment. If not, they can be installed using the command 
# "install.packages("package-name", dependencies = TRUE)"

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(scales)
library(ggcorrplot)

#Creating a base dataset from which all other datasets will be derived.
base.loan <- read.csv("loan.csv", header = TRUE, stringsAsFactors = FALSE)

str(base.loan)

summary(base.loan)

loan <- base.loan[, !(colMeans(is.na(base.loan)) > 0.5)]

#Columns to be converted into categorical column
categorical <- c("term","grade", "sub_grade", "emp_title", "emp_length", "home_ownership",
                 "verification_status","loan_status", "pymnt_plan", "purpose", "addr_state",
                 "initial_list_status", "policy_code",  "application_type")

loan[categorical] <- lapply(loan[categorical], as.factor)

str(loan)

summary(loan)

# As can be seen from the structure and summary dataset, application_type,
# initial_list_status, policy_code, pymnt_plan are all varibles with 1 level only and as such 
# they will not have any impact on the analysis. As such, these columns need to be
# removed. Also, url, desc, emp_title, title and zip_code will have too many factors from 
# analysis point of view

removed <- c("application_type", "initial_list_status", "pymnt_plan", "url",
             "desc", "emp_title", "title", "zip_code", "policy_code")

loan <- loan[, !colnames(loan) %in% removed]

str(loan)

summary(loan)

# There are many columns with 0s and nA,s only. Need to remove those columns
# Removing all columns where number of 0's is geater than 50% of the rows
loan <- loan[ , -c(which(as.numeric(colSums(loan == 0, na.rm = TRUE)) > nrow(loan)/2))]

str(loan)

summary(loan)

# There are some numeric columns which have been read as character.
# Converting them to numeric

numerical <- c("int_rate", "revol_util")

loan[numerical] <- lapply(loan[numerical], function(x) as.numeric(str_replace(x, "%", "" )))

str(loan)

summary(loan)

# All date columns are hving the same format. Convert all date columns to PosixCT objects
dates <- c("issue_d", "earliest_cr_line", "last_pymnt_d", "next_pymnt_d", "last_credit_pull_d")

loan[dates] <- lapply(loan[dates], function(x) { 
   x <- parse_date_time(paste("01-",x),orders = c("d-b-y")) 
   x <- as.POSIXct(ifelse(x > Sys.Date(), format(x, "19%y-%m-%d"), format(x)))
} )

str(loan)

summary(loan)

#Check if there are unique number of loans

length(unique(loan$id)) == nrow(loan)


#---------------Univariate, Segmented Univariate and Bivariate Analysis---------------------------------------------


table(loan$loan_status)

ggplot(loan, aes(x = loan_status,  fill = loan_status)) + 
  geom_bar() +
  geom_text(stat = "count", aes( label = ..count..), vjust = -0.2)

stloan <- loan %>% group_by(loan_status) %>%
                   tally() %>%
                   mutate(pct = n/sum(n) * 100)

ggplot(stloan, aes(x = loan_status, y= pct, fill = loan_status)) +
       geom_bar(stat = "identity", color = "black", position = "stack") +
       geom_text(stat = "identity", aes( label = round(pct)), vjust = -0.2) +
       labs(y = "Percent", x = "Loan status") 
       
       
# Thus approximately 14% of the loans are in charged Off or "Defaulted" state

# Creating 2 datasets for fully paid and defaulted and how the various independent
# variables differ in these 2

#Since we are interested in the differences between the independent variables
#in the Fully paid and charged off category, we will remove the current category
# as nothing can be said till the loan is paid or defaulted

final_loan <- loan[loan$loan_status != "Current",]

# 1. Loan amount
# find out how the loan amount variable is distributed

ggplot(final_loan, aes(x = loan_amnt)) + geom_histogram(fill = "sky blue", color = "black") +
       stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.2) 

#So most of the loans are in $8000 to $10000 range

# How loan amount differs in fully paid and defaulted category
ggplot(final_loan, aes(x = loan_amnt)) + geom_histogram(fill = "sky blue", color = "black") +
  stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.2) +
  facet_wrap(~ loan_status)

ggplot(final_loan, aes(x = loan_status, y = loan_amnt)) + geom_boxplot()

fivenum(final_loan$loan_amnt[final_loan$loan_status == "Fully Paid"])

fivenum(final_loan$loan_amnt[final_loan$loan_status == "Charged Off"])

# In general, the loan amounts in the default category are on the higher side,
# but the overall distribution is not much different.

# 2. Employment Length
final_loan$emp_length <- factor(final_loan$emp_length, levels = c("< 1 year", "1 year",
                                "2 years", "3 years", "4 years", "5 years", "6 years",
                                "7 years", "8 years", "9 years", "10+ years", "n/a"))

ggplot(final_loan, aes(x = emp_length, fill = emp_length)) +
  geom_bar() +
  geom_text(stat = "count", aes( label = ..count..), vjust = -0.2) +
  theme(axis.text.x = element_text(angle=45))

emp_len <- final_loan %>% group_by(loan_status, emp_length) %>%
                          tally() %>%
                          mutate(pct = n/sum(n) * 100)

ggplot(emp_len, aes(x = emp_length, y = pct, fill = emp_length)) +
  geom_bar(stat = "identity") +
  geom_text(stat = "identity", aes( label = round(pct)), vjust = -0.2) +
  theme(axis.text.x = element_text(angle=45)) +
  labs(y = "Percent", x = "Employment Length ") +
  facet_wrap(~ loan_status)

# Looking at the graph there isn't much difference between Defaulted and Fully
# paid categories except that the percentage of defaults in the "n/a" category
# is twice that of the Fully Paid loans.

# 3. Loan purpose

ggplot(final_loan, aes(x = purpose, fill = purpose)) +
  geom_bar() +
  geom_text(stat = "count", aes( label = ..count..), vjust = -0.2) +
  theme(axis.text.x = element_text(angle=45))

# From the graph it is clear that loans for debt consolidation and credit card are
# the highest. How does the pattern differ in the Defaulted and Fully Paid category.

loan_pur <- final_loan %>% group_by(loan_status, purpose) %>%
                          tally() %>%
                          mutate(pct = n/sum(n) * 100)

ggplot(loan_pur, aes(x = purpose, y = pct, fill = loan_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(stat = "identity", aes( label = round(pct)), vjust = -0.1) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(y = "Percent", x = "Loan Purpose ") +
  facet_wrap(~ loan_status) +
  coord_flip()

# As per the graph, the patterns in the Defaulter and Fully paid category are almost
# same except for loans taken for Small Business where the Defaulters percentage is
# twice that of the Fully paid category.

# 4. Address state

ggplot(final_loan, aes(x = addr_state, fill = addr_state)) +
  geom_bar() +
  geom_text(stat = "count", aes( label = ..count..), vjust = -0.2) +
  theme(axis.text.x = element_text(angle=45))

# As per the graph, maximum requests are from California followed by New York and 
# Florida. Are the patterns different in the Defaulters and Fully Paid categories?

loan_state <- final_loan %>% group_by(loan_status, addr_state) %>%
  tally() %>%
  mutate(pct = n/sum(n) * 100)

ggplot(loan_state, aes(x = addr_state, y = pct, fill = addr_state)) +
  geom_bar(stat = "identity") +
  geom_text(stat = "identity", aes( label = round(pct)), vjust = -0.2) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(y = "Percent", x = "State ") +
  facet_wrap(~ loan_status) +
  coord_flip()

# From the graph there is not much difference between the Defaulters and fully paid
# categories

# 5. Home ownership
ggplot(final_loan, aes(x = home_ownership, fill = home_ownership)) +
  geom_bar() +
  geom_text(stat = "count", aes( label = ..count..), vjust = -0.2)

# Maximum loan accounts have Rented homes followed by mortgaged homes.
# Check for any differnces in defaulted and Fully paid category.

owner <- final_loan %>% group_by(loan_status, home_ownership) %>%
  tally() %>%
  mutate(pct = n/sum(n) * 100)

ggplot(owner, aes(x = home_ownership, y = pct, fill = home_ownership)) +
  geom_bar(stat = "identity") +
  geom_text(stat = "identity", aes( label = round(pct)), vjust = -0.2) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(y = "Percent", x = "Home Ownership ") +
  facet_wrap(~ loan_status)

# From the graph, the patterns are almost same with a slight increase in Defaulters
# cases for rented homes and a slight decrease for Mortgaged homes.

# 6. Term

ggplot(final_loan, aes(x = term, fill = term)) +
  geom_bar() +
  geom_text(stat = "count", aes( label = ..count..), vjust = -0.2)

# maximum loans are for 36 months

loan_term <- final_loan %>% group_by(loan_status, term) %>%
  tally() %>%
  mutate(pct = n/sum(n) * 100)

ggplot(loan_term, aes(x = term, y = pct, fill = loan_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(stat = "identity", aes( label = round(pct)), vjust = 0.1) +
  labs(y = "Percent", x = "Loan Term ") 

# From the graph, it is clear that while the percentage of 60 month loans is 21 in
# Fully paid loans, it jumps to 43 percent in the Default category indicating that
# loan accounts with 60 months term are twice as likely to default.

# 7. Grade

ggplot(final_loan, aes(x = grade, fill = grade)) +
  geom_bar() +
  geom_text(stat = "count", aes( label = ..count..), vjust = -0.2)

# Most of the loan accounts are in grade B followed by grade A

loan_grade <- final_loan %>% group_by(loan_status, grade) %>%
  tally() %>%
  mutate(pct = n/sum(n) * 100)

ggplot(loan_grade, aes(x = grade, y = pct, fill = loan_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(stat = "identity", aes( label = round(pct)), vjust = -0.1) +
  labs(y = "Percent", x = "Loan Grade ") 

# percentage of Loans with Grade D to G is almost double in the Defaulters 
# category indicating that Grade D to G loans are twice as likely to default.

# 8. sub-grade

ggplot(final_loan, aes(x = sub_grade, fill = sub_grade)) +
  geom_bar() +
  geom_text(stat = "count", aes( label = ..count..), vjust = -0.2)

# Sub-grades A4 has the highest loan accounts followed by B3

loan_sub_grade <- final_loan %>% group_by(loan_status, sub_grade) %>%
  tally() %>%
  mutate(pct = n/sum(n) * 100)

ggplot(loan_sub_grade, aes(x = sub_grade, y = pct, fill = loan_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(stat = "identity", aes( label = round(pct,1)), vjust = -0.1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Percent", x = "Loan Grade ") +
  coord_flip()

# The percentage of loans in the D2 to G5 sub-grades rise significantly in the 
# Defaulters category indicating that sub-grades variable influences the 
# probability of default

# 9. Verification Status

ggplot(final_loan, aes(x = verification_status, fill = verification_status)) +
  geom_bar() +
  geom_text(stat = "count", aes( label = ..count..), vjust = -0.2)

# Majority of the accounts are Not verified followed by Verified and Source verified.

loan_verification <- final_loan %>% group_by(loan_status, verification_status) %>%
  tally() %>%
  mutate(pct = n/sum(n) * 100)

ggplot(loan_verification, aes(x = verification_status, y = pct, fill = verification_status)) +
  geom_bar(stat = "identity") +
  geom_text(stat = "identity", aes( label = round(pct,1)), vjust = -0.1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Percent", x = "Verification Status ") +
  facet_wrap(~ loan_status)
  
# The patterns look almost same, but there is a rise of about 6% in the Default category
# for verified incomes. This is contrary to expected outcome of having Unverified
# incomes being greater in the Defaulters category

# 10. Inquiries in the last 6 months
# Since inquiries in the last 6 months are limited in range from 0 to 8, this
# variable is better analyzed after converting to factor.
final_loan$inq_last_6mths <- as.factor(final_loan$inq_last_6mths)

ggplot(final_loan, aes(x = inq_last_6mths, fill = inq_last_6mths)) +
  geom_bar() +
  geom_text(stat = "count", aes( label = ..count..), vjust = -0.2)

# Maximum number is that of no enquiries followed by that of 1 enquiry
loan_inquiry <- final_loan %>% group_by(loan_status, inq_last_6mths) %>%
  tally() %>%
  mutate(pct = n/sum(n) * 100)

ggplot(loan_inquiry, aes(x = inq_last_6mths, y = pct, fill = loan_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(stat = "identity", aes( label = round(pct,1)), vjust = -0.1) +
  labs(y = "Percent", x = "Number of inquiries in last 6 months ") 

# Although the pattern looks largely the same, the percentage of loan accounts
# with 3 inquries rises significantly by more than 50% in the Defaulters category

# 11. Interest Rate

ggplot(final_loan, aes(x = int_rate)) + geom_histogram(fill = "red", color = "black", binwidth = 2)

# maximum number of loans are in the 11-13 % followed by 13-15% and 9-11%.
# How do the histograms differ?

breaks = seq(5,25,4)

final_loan$int_cut = cut(final_loan$int_rate, breaks = breaks)

loan_cut = final_loan %>%
  group_by(loan_status, int_cut) %>%
  tally() %>%
  mutate(pct = n / sum(n) * 100)

ggplot(loan_cut, aes(x = int_cut, y = pct, fill = loan_status)) + 
  geom_histogram(color = "black", position = "dodge", stat = "identity") +
  geom_text(stat = "identity", aes( label = round(pct,1)), vjust = -0.1) +
  labs(y = "Percentage", x = "Interest rate range ")
  
# As can be seen from the graph, the proportion of defaults increase once the 
# interest rate crosses 13%. Infact the proportion of defaults
# increase 2.5 times to 4.5 times as the interest rate crosses 17%. Thus interest 
# rate can be a strong predictor of default

# 12. Installment and Annual income 
# Neither Installment nor annual income by themselves can predict default.
# Rather installment as a percentage of monthly income will be a better predictor.

final_loan$percent_install <- final_loan$installment/(final_loan$annual_inc / 12) * 100

ggplot(final_loan, aes(x = percent_install)) + geom_histogram(fill = "green", color = "black", binwidth = 2)

breaks = seq(0,36,4)

final_loan$install_cut <- cut(final_loan$percent_install, breaks = breaks)

loan_install <- final_loan %>%
  group_by(loan_status, install_cut) %>%
  tally() %>%
  mutate(pct = n / sum(n) * 100)

ggplot(loan_install, aes(x = install_cut, y = pct, fill = loan_status)) + 
  geom_histogram(color = "black", position = "dodge", stat = "identity") +
  geom_text(stat = "identity", aes( label = round(pct,1)), vjust = -0.1) +
  labs(y = "Percentage", x = "Installment percent range ")

# From the graph we can see that the propotion of defaults is lower in the 0 - 8%
# range while it rises by about 25% in the 8-12% range and by about 50% in 12- 16%
# range to almost by 74% in the 16-20% range. Thus, Installment percent is a strong 
#predictor of default.

# 13. Debt to income ratio, dti

ggplot(final_loan, aes(x = dti)) + geom_histogram(fill = "yellow", color = "black", binwidth = 2)

# dti seems to be roughly normally distributed.

breaks = seq(0,32,4)

final_loan$dti_cut <- cut(final_loan$dti, breaks = breaks)

loan_dti <- final_loan %>%
  group_by(loan_status, dti_cut) %>%
  tally() %>%
  mutate(pct = n / sum(n) * 100)

ggplot(loan_dti, aes(x = dti_cut, y = pct, fill = loan_status)) + 
  geom_histogram(color = "black", position = "dodge", stat = "identity") +
  geom_text(stat = "identity", aes( label = round(pct,1)), vjust = -0.1) +
  labs(y = "Percentage", x = "dti range ")

# As can be seen from the graph, there is a rise of almost 13% in defaults once
# dti is in the 16-20% range and defaults rise by about 20% in the 20-24% range.
# Thus high dti can also be a predictor of default

# 14. Open credit line and total credit lines, open_acc and total_acc
# Number of open credit lines as a percentage of total credit lines might have some 
# bearing on defaults

final_loan$percent_acc <- final_loan$open_acc / final_loan$total_acc * 100

ggplot(final_loan, aes(x = percent_acc)) + geom_histogram(fill = "orange", color = "black", binwidth = 25)

breaks = seq(0,175,25)

final_loan$acc_cut <- cut(final_loan$percent_acc, breaks = breaks)

loan_acc <- final_loan %>%
  group_by(loan_status, acc_cut) %>%
  tally() %>%
  mutate(pct = n / sum(n) * 100)

ggplot(loan_acc, aes(x = acc_cut, y = pct, fill = loan_status)) + 
  geom_histogram(color = "black", position = "dodge", stat = "identity") +
  geom_text(stat = "identity", aes( label = round(pct,1)), vjust = -0.1) +
  labs(y = "Percentage", x = "Percentage of Open credit lines range ")

# The percentage of defaults rise when percentage of open credit lines rises 
# beyond 50% buttherise doesn't look significant.

#Correlation plots

cor_loan <- final_loan[, colnames(final_loan) %in% c("loan_amnt", "funded_amnt",
                                                     "funded_amnt_inv", "int_rate", "installment",
                                                     "annual_inc", "dti", "revol_bal", "revol_util",
                                                     "total_pymnt", "total_pymnt_inv", "total_rec_prncp",
                                                     "total_rec_int", "percent_install", "percent_acc")]
cor_values <- cor(cor_loan)

head(cor_values)

ggcorrplot(cor_values)
