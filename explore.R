library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

INPUT <- "data/customer.csv"
OUTDIR <- "outputs"
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

df <- read_csv(INPUT, show_col_types = FALSE)

# Clean currency/commas -> numeric for relevant columns
to_numeric <- function(x) {
  as.numeric(gsub("[\\$,()\\-]", "", x))
}

num_candidates <- c("HHIncome","CreditDebt","OtherDebt","CarValue",
                    "CardSpendMonth","VoiceLastMonth","VoiceOverTenure",
                    "EquipmentLastMonth","EquipmentOverTenure",
                    "DataLastMonth","DataOverTenure")
for (c in intersect(num_candidates, names(df))) {
  df[[c]] <- to_numeric(df[[c]])
}

# Create total debt
if (all(c("CreditDebt","OtherDebt") %in% names(df))) {
  df <- df %>% mutate(TotalDebt = CreditDebt + OtherDebt)
}

# Single-variable plots
p_region <- ggplot(df, aes(x = Region)) + geom_bar() + theme_minimal() +
  ggtitle("Distribution of Regions")
ggsave(file.path(OUTDIR, "dist_regions.png"), p_region, width = 7, height = 4)

p_job <- ggplot(df, aes(x = JobCategory)) + geom_bar() + theme_minimal() +
  ggtitle("Distribution of Job Categories")
ggsave(file.path(OUTDIR, "dist_job_categories.png"), p_job, width = 7, height = 4)

p_union <- ggplot(df, aes(x = UnionMember)) + geom_bar() + theme_minimal() +
  ggtitle("Union Membership")
ggsave(file.path(OUTDIR, "union_member.png"), p_union, width = 7, height = 4)

p_carown <- ggplot(df, aes(x = CarOwnership)) + geom_bar() + theme_minimal() +
  ggtitle("Car Ownership")
ggsave(file.path(OUTDIR, "car_ownership.png"), p_carown, width = 7, height = 4)

p_income <- ggplot(df, aes(x = HHIncome)) + geom_histogram(bins = 40) + theme_minimal() +
  ggtitle("Household Income Distribution")
ggsave(file.path(OUTDIR, "income_hist.png"), p_income, width = 7, height = 4)

# Two-variable plots
if ("Age" %in% names(df) && "EmploymentLength" %in% names(df)) {
  p_age_emp <- ggplot(df, aes(x = as.numeric(Age), y = as.numeric(EmploymentLength))) +
    geom_point(alpha = 0.4) + theme_minimal() + ggtitle("Age vs Employment Length")
  ggsave(file.path(OUTDIR, "age_vs_employment.png"), p_age_emp, width = 7, height = 4)
}

if ("Gender" %in% names(df) && "HHIncome" %in% names(df)) {
  p_gender_income <- ggplot(df, aes(x = Gender, y = HHIncome)) +
    geom_boxplot() + theme_minimal() + ggtitle("Household Income by Gender")
  ggsave(file.path(OUTDIR, "income_by_gender.png"), p_gender_income, width = 7, height = 4)
}

if (all(c("CarOwnership","CarBrand") %in% names(df))) {
  p_carbrand <- ggplot(df, aes(x = CarOwnership, fill = CarBrand)) +
    geom_bar(position = "dodge") + theme_minimal() + ggtitle("Car Ownership vs Car Brand")
  ggsave(file.path(OUTDIR, "car_ownership_vs_brand.png"), p_carbrand, width = 7, height = 4)
}

if (all(c("JobCategory","UnionMember") %in% names(df))) {
  p_job_union <- ggplot(df, aes(x = JobCategory, fill = UnionMember)) +
    geom_bar(position = "dodge") + theme_minimal() + ggtitle("Job Category vs Union Member")
  ggsave(file.path(OUTDIR, "job_vs_union.png"), p_job_union, width = 7, height = 4)
}

if (all(c("HHIncome","TotalDebt") %in% names(df))) {
  p_log_corr <- ggplot(df, aes(x = log1p(HHIncome), y = log1p(TotalDebt))) +
    geom_point(alpha = 0.4) + theme_minimal() + ggtitle("log(HHIncome) vs log(TotalDebt)")
  ggsave(file.path(OUTDIR, "log_income_vs_log_debt.png"), p_log_corr, width = 7, height = 4)
}
