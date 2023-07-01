# 17.1 Preparation ----

## Load packages ----
pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable     # converting tables to pretty images
)

## Import data ----

linelist <- import(here("data", "linelist_cleaned.rds"))

# 17.2 Browse data ----

## skimr package ----
## get information about each variable in a dataset 
skim(linelist)

## Summary statistics ----
## get information about each column in a dataset 
summary(linelist)

### get_summary_stats ----
linelist %>% 
  get_summary_stats(
    age, wt_kg, ht_cm, ct_blood, temp,  # columns to calculate for
    type = "common")                    # summary stats to return

# 17.3 janitor package ----

## Simple tabyl ----

linelist %>% tabyl(age_cat, show_na = FALSE)

## Cross tabulation ----

linelist %>% tabyl(age_cat, gender)

## "Adorning" the tabyl ----

linelist %>%               # case linelist
  tabyl(age_cat) %>%       # tabulate counts and proportions by age category
  adorn_pct_formatting()   # convert proportions to percents


linelist %>%                                  
  tabyl(age_cat, gender) %>%                  # counts by age and gender
  adorn_totals(where = "row") %>%             # add total row
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1)            # convert proportions to percents


linelist %>%                                  # case linelist
  tabyl(age_cat, gender) %>%                  # cross-tabulate counts
  adorn_totals(where = "row") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions
  adorn_pct_formatting() %>%                  # convert to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Age Category",
    col_name = "Gender")

## Printing the tabyl ----

linelist %>%
  tabyl(age_cat, gender) %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Age Category",
    col_name = "Gender",
    placement = "combined") %>% # this is necessary to print as image
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit()          # format to one line per row 

## Use on other tables ----
linelist %>% 
  count(hospital) %>%   # dplyr function
  adorn_totals()        # janitor function

## Saving the tabyl ----

linelist %>%
  tabyl(age_cat, gender) %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Age Category",
    col_name = "Gender",
    placement = "combined") %>% 
  flextable::flextable() %>%                     # convert to image
  flextable::autofit() %>%                       # ensure only one line per row
  flextable::save_as_docx(path = here("outputs", "tabyl.docx"))   # save as Word document to filepath

## Statistics ----

age_by_outcome <- linelist %>% 
  tabyl(age_cat, outcome, show_na = FALSE) 

chisq.test(age_by_outcome)

# 17.4 dplyr package ----

## Get counts ----

linelist %>%                 # begin with linelist
  summarise(n_rows = n())    # return new summary dataframe with column n_rows

linelist %>% 
  group_by(age_cat) %>%     # group data by unique values in column age_cat
  summarise(n_rows = n())   # return number of rows *per group*

linelist %>% 
  count(age_cat)

linelist %>% 
  count(age_cat, outcome)

## Show all levels ----

linelist %>% 
  count(age_cat, .drop = FALSE)

## Proportions ----

age_summary <- linelist %>% 
  count(age_cat) %>%                     # group and count by gender (produces "n" column)
  mutate(                                # create percent of column - note the denominator
    percent = scales::percent(n / sum(n))) 

# print
age_summary

age_by_outcome <- linelist %>%                  # begin with linelist
  group_by(outcome) %>%                         # group by outcome 
  count(age_cat) %>%                            # group and count by age_cat, and then remove age_cat grouping
  mutate(percent = scales::percent(n / sum(n))) # calculate percent - note the denominator is by outcome group

# print
age_by_outcome

# 17.5 gtsummary package ----

# 17.6 base R ----