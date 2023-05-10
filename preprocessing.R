# preprocessing

# load required packages
library(tidyverse)
library(readr)
library(readxl)
library(naniar)

# import data
mortgage <- read_csv("data/state_NY.csv")
unemployment <- read_excel("data/unemployment.xlsx")

# explore unemployment data
str(unemployment)

# select columns in 2020 from unemployment data and keep NY
county_data <- unemployment %>%
        select(1:3, Civilian_labor_force_2020, Employed_2020, Unemployed_2020, 
               Unemployment_rate_2020, Median_Household_Income_2020, 
               Med_HH_Income_Percent_of_State_Total_2020) %>%
        filter(State == "NY") %>%
        select(-State)


# count the number of missing values in each column
colSums(is.na(county_data))
str(county_data)

# explore mortgage data
str(mortgage)
head(mortgage)

# 1. modify the structure of the data
# replace all occurrences of 1111 with NA in the dataset
mortgage_clean <- as.data.frame(lapply(mortgage, function(x) {
        x[x == 1111] <- NA
        return(x)
}))

# replace all occurrences of 8888 or 9999 with NA in the dataset
mortgage_clean <- as.data.frame(lapply(mortgage_clean, function(x) {
        x[x %in% c("8888", "9999")] <- NA
        return(x)
}))

# list of columns to replace "Exempt" with NA and convert to numeric
columns_to_replace <- c("loan_to_value_ratio", "interest_rate", "rate_spread", "total_loan_costs",
                        "total_points_and_fees", "origination_charges", "discount_points", 
                        "lender_credits","loan_term", "prepayment_penalty_term", 
                        "intro_rate_period", "multifamily_affordable_units", "property_value")

# replace "Exempt" with NA and convert to numeric
for (column in columns_to_replace) {
        mortgage_clean[[column]] <- ifelse(mortgage_clean[[column]] == "Exempt", NA, 
                                           mortgage_clean[[column]])
        mortgage_clean[[column]] <- as.numeric(mortgage_clean[[column]])
}

# "Exempt" in debt_to_income_ratio remains a category

# columns to convert to categorical
columns_to_convert <- c("derived_msa.md", "county_code", "census_tract", "action_taken",
                        "purchaser_type", "preapproval", "loan_type", "loan_purpose", 
                        "lien_status","reverse_mortgage", "open.end_line_of_credit", 
                        "business_or_commercial_purpose", "hoepa_status", "construction_method", 
                        "occupancy_type", "manufactured_home_secured_property_type",
                        "manufactured_home_land_property_interest", "negative_amortization", 
                        "interest_only_payment", "balloon_payment", "other_nonamortizing_features",
                        "applicant_credit_score_type", "co.applicant_credit_score_type", 
                        "applicant_ethnicity.1", "applicant_ethnicity.2", 
                        "co.applicant_ethnicity.1", "co.applicant_ethnicity.2",
                        "co.applicant_ethnicity.3", "applicant_ethnicity_observed", 
                        "co.applicant_ethnicity_observed", "applicant_race.1", "applicant_race.2",
                        "applicant_race.3", "co.applicant_race.1", "co.applicant_race.2",
                        "co.applicant_race.3", "applicant_race_observed", 
                        "co.applicant_race_observed", "applicant_sex", "co.applicant_sex", 
                        "applicant_sex_observed", "co.applicant_sex_observed",
                        "submission_of_application", "initially_payable_to_institution", "aus.1", 
                        "aus.2", "aus.3", "aus.4", "aus.5", "denial_reason.1", "denial_reason.2",
                        "denial_reason.3", "denial_reason.4")

# convert the numeric columns to categorical
for (column_name in columns_to_convert) {
        mortgage_clean[[column_name]] <- as.factor(mortgage_clean[[column_name]])
}

# convert specific columns to boolean
mortgage_clean <- mortgage_clean %>%
        mutate(applicant_age_above_62 = ifelse(applicant_age_above_62 == "Yes", TRUE,
                                               ifelse(applicant_age_above_62 == "No", FALSE, NA)),
               co.applicant_age_above_62 = ifelse(co.applicant_age_above_62 == "Yes", TRUE,
                                                  ifelse(co.applicant_age_above_62 == "No", FALSE,
                                                         NA)),
                preapproval = ifelse(preapproval == 1, TRUE,
                                    ifelse(preapproval == 2, FALSE, NA)),
               reverse_mortgage = ifelse(reverse_mortgage == 1, TRUE,
                                         ifelse(reverse_mortgage == 2, FALSE, NA)),
               open.end_line_of_credit = ifelse(open.end_line_of_credit == 1, TRUE,
                                                ifelse(open.end_line_of_credit == 2, FALSE, NA)),
               business_or_commercial_purpose = ifelse(business_or_commercial_purpose == 1, TRUE,
                                                       ifelse(business_or_commercial_purpose == 2, 
                                                              FALSE, NA)),
               negative_amortization = ifelse(negative_amortization == 1, TRUE,
                                              ifelse(negative_amortization == 2, FALSE, NA)),
               interest_only_payment = ifelse(interest_only_payment == 1, TRUE,
                                              ifelse(interest_only_payment == 2, FALSE, NA)),
               balloon_payment = ifelse(balloon_payment == 1, TRUE,
                                        ifelse(balloon_payment == 2, FALSE, NA)),
               other_nonamortizing_features = ifelse(other_nonamortizing_features == 1, TRUE,
                                                     ifelse(other_nonamortizing_features == 2, FALSE, 
                                                            NA)))

# convert the rest of the character columns to categorical
for (column in colnames(mortgage_clean)) {
        if (is.character(mortgage_clean[[column]])) {
                mortgage_clean[[column]] <- as.factor(mortgage_clean[[column]])
        }
}

# modify outcome variable
mortgage_clean <- mortgage_clean %>%
        filter(action_taken %in% c(1,3)) %>%
        mutate(action_taken = ifelse(action_taken == 1, TRUE,
                                     ifelse(action_taken == 3, FALSE, NA)))

# drop year and state_code
mortgage_clean <- mortgage_clean %>%
        select(-activity_year, -state_code)

# verify the changes
str(mortgage_clean)

# 2. dealing with missing values
# count the number of missing values in each column
colSums(is.na(mortgage_clean))

# visualize missing values
mortgage_clean %>%
        gg_miss_var(show_pct = TRUE)

# drop features contains > 60% of missing values
mortgage_clean <- mortgage_clean %>%
        select(-c(total_points_and_fees, discount_points, lender_credits, prepayment_penalty_term,
                  intro_rate_period, multifamily_affordable_units, `applicant_ethnicity.2`,
                  `applicant_ethnicity.3`, `applicant_ethnicity.4`, `applicant_ethnicity.5`,
                  `co.applicant_ethnicity.2`, `co.applicant_ethnicity.3`, 
                  `co.applicant_ethnicity.4`, `co.applicant_ethnicity.5`, `applicant_race.2`,
                  `applicant_race.3`, `applicant_race.4`, `applicant_race.5`, 
                  `co.applicant_race.2`, `co.applicant_race.3`, `co.applicant_race.4`,
                  `co.applicant_race.5`, `co.applicant_age_above_62`, `aus.2`, `aus.3`, `aus.4`, 
                  `aus.5`, `denial_reason.2`, `denial_reason.3`, `denial_reason.4`, 
                  co.applicant_age))

mortgage_clean %>%
        gg_miss_var(show_pct = TRUE)

# impute missing values for features contains > 10%
# for numeric variables: impute with median values
numeric_columns <- c("income", "loan_to_value_ratio", "interest_rate", "rate_spread", 
                     "total_loan_costs", "origination_charges", "property_value")

mortgage_clean <- mortgage_clean %>%
        mutate(across(numeric_columns, ~ifelse(is.na(.), median(., na.rm=TRUE), .)))

# for categorical variable - impute with most frequent category
sort(table(mortgage_clean$debt_to_income_ratio))

mortgage_clean <- mortgage_clean %>%
        mutate(debt_to_income_ratio = ifelse(is.na(debt_to_income_ratio), "20%-<30%", 
                                             debt_to_income_ratio))
# removes rows containing any missing values
mortgage_clean <- mortgage_clean %>%
        drop_na()

# join the mortgage data with county_data
mortgage_join <- mortgage_clean %>%
        left_join(county_data, by = c("county_code" = "FIPS_code")) 

glimpse(mortgage_join)
summary(mortgage_join)

write.csv(mortgage_join, "data/mortgage_join.csv", row.names = FALSE)
