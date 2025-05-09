
#### Setup ####

library(tidyverse)
library(forcats)
library(haven)
library(ggstats)
library(patchwork)
library(santoku)

theme_set(theme_light())
theme_update(
  panel.grid = element_blank(),
  line = element_line(lineend = "round")
) 

# The EVS trend file, 1981-2017
evs_orig <- haven::read_dta("EVS/ZA7503_v3-0-0.dta") 


relabel_na <- function (x) {
  neg_or_already_na <- x < 0 | is.na(x)
  x <- as_factor(x)
  x[neg_or_already_na] <- NA
  x <- droplevels(x)
  x
}


neg_na <- function (x) {
  ifelse(as.numeric(x) < 0, NA, x)
}

#### Prepare data ####

# Find countries that took part in all waves
ctry_waves <- table(evs_orig$S009, evs_orig$S002EVS) 
ctry_waves <- as.matrix(ctry_waves)
in_all_waves <- apply(ctry_waves, 1, function (x) all(x > 0))
in_all_waves <- rownames(ctry_waves[in_all_waves, ])


evs <- evs_orig |> 
  mutate(
    Age = relabel_na(X003R),
    Age = fct_recode(Age, "65+" = "65 and more years"),
    across(c(E012, X002, X003, X023R, C038, C039, C040, C041, C011, C012, C017, C024, 
             C031, F028, A065, A097, F024, F034, F126, F127, F128, F140, G006,
             X007, X011, X028, D018, X001, A001:A006, A027:A042, A165, E015:E019,
             F050), 
           neg_na),
    Cohort = santoku::chop(X002, c(1930, 1940, 1950, 1960, 1970, 1980, 1990), santoku::lbl_discrete()),
    Cohort = fct_rev(Cohort),
    across(c(D054), relabel_na),
    Marriage_outdated = relabel_na(D022),
    across(c(C038, C039), ~ 6 - .), # originally, 1 = strongly agree, 5 = strongly disagree. Reverse.
    across(A001:A006, ~ 5 - .), # originally 1 = v important 4 = not at all
    across(E015:E019, ~ 4 - .), # originally 1 = good thing 3 = bad thing
    National_pride = 1 * (G006 == 1),
    Income = relabel_na(X047R_EVS),
    Male = X001 == 1,
    Wave   = relabel_na(S002EVS),
    Wave = fct_recode(Wave, "1981" = "1981-1984", "1990" = "1990-1993", 
                      "1999" = "1999-2001", "2008" = "2008-2010", "2017" = "2017-2021"),
    Education  = ifelse(X023R == 10, "To 21+", "Less than 21"),
    Education = fct_relevel(Education, "To 21+", "Less than 21"),
    Relig_weekly = 1 * (F028 %in% 1:2),
    Vol_work = 1 * (A097 == 0), # 0 = Voluntary work NOT "none" 
    Relig_person = 1 * (F034 == 1),
    Atheist = 1 * (F034 == 3), # "Convinced atheist
    Drugs = 1 * (F126 >= 8),
    Lying = 1 * (F127 >= 8),
    Adultery = 1 * (F128 >= 8),
    Keeping_money = 1 * (F140 >= 8),
    Married = 1 * (X007 == 1),
    Divorced = 1 * (X007 == 3),
    Has_children = 1 * (X011 > 0),
    Unmarried_children = 1 * (Has_children & ! Married),
    Nev_mar_children = 1 * (Has_children & X007 == 6),
    Nev_mar_children_2 = 1 * (Has_children & X007 %in% c(2, 6)),
    Employed = 1 * (X028 %in% 1:2),
    Emp_gt_30 = 1 * (X028 == 1),
    Emp_lt_30 = 1 * (X028 == 2),
    Trust = 1 * (A165 == 1),
  ) |> 
  # Only use countries that took part in all waves
  filter(S009 %in% in_all_waves)


#### Plot preparation ####

plot_income <- function (var, data = evs) {
  data |> 
    drop_na(Income, {{var}}) |> 
    ggplot(aes(Wave, {{var}}, color = Income, group = Income)) + 
    stat_weighted_mean(geom = "line", linewidth = 1.1) +
    stat_weighted_mean(geom = "point") +
    scale_color_manual(values = c("Low" = "orange2", "Medium" = "red", "High" = "darkred"))
}


plot_edu <- function (var, data = evs) {
  data |> 
    drop_na(Education, {{var}}) |> 
    ggplot(aes(Wave, {{var}}, color = Education, group = Education)) + 
    stat_weighted_mean(geom = "line", linewidth = 1.1) +
    stat_weighted_mean(geom = "point")
}


plot_age <- function (var, data = evs) {
  data |> 
    drop_na(Age, {{var}}) |> 
    ggplot(aes(Wave, {{var}}, color = Age, group = Age)) + 
    stat_weighted_mean(geom = "line", linewidth = 1.1) +
    stat_weighted_mean(geom = "point") +
    scale_color_viridis_d(option = "C", direction = -1)
}

plot_cohort <- function (var, data = evs) {
  data |> 
    drop_na(Cohort, {{var}}) |> 
    ggplot(aes(Wave, {{var}}, color = Cohort, group = Cohort)) + 
    stat_weighted_mean(geom = "line", linewidth = 1.1) +
    stat_weighted_mean(geom = "point") +
    stat_weighted_mean(aes(group = 1), color = "grey60", 
                       geom = "line", alpha = 0.5, linewidth = 1.7) +
    scale_color_viridis_d(option = "C", direction = -1)
}

sy <- scale_y_continuous(labels = scales::percent)
lyb <- labs(y = "")


#### Plots ####

evs_k <- evs |> filter(Has_children == 1)
# Has children, unmarried
l <- labs(title = "Unmarried with children", 
          subtitle = "% of EVS respondents with children who were not married at interview time",
          y = "")
pi <- plot_income(Unmarried_children, data = evs_k) + l + sy
pe <- plot_edu(Unmarried_children, data = evs_k) + sy + lyb
pi + pe


# Has children, never married
l <- labs(title = "Never married with children", 
          subtitle = "% of EVS respondents with children who have never married\nIncludes cohabiting/reg. partnership",
          y = "")
pi <- plot_income(Nev_mar_children_2, data = evs_k) + l + sy
pe <- plot_edu(Nev_mar_children_2, data = evs_k) + lyb + sy
pi + pe 


# Divorce
l <- labs(title = "Divorce", 
          subtitle = "% of EVS respondents who are divorced",
          y = "")
pi <- plot_income(Divorced) + l + sy
pe <- plot_edu(Divorced) + lyb + sy
pi + pe


# Agree: Marriage is an outdated institution
l <- labs(title = "Attitudes: marriage is outdated", 
          subtitle = "% of EVS respondents agreeing 'marriage is an outdated institution'",
          y = "")
pi <- plot_income(D022) + l + sy
pe <- plot_edu(D022) + lyb + sy
pi + pe
plot_cohort(D022) + l + sy

# Agree: A child needs a home with a mother and a father
l <- labs(title = "Attitudes: child needs both parents", 
          subtitle = "% of EVS respondents agreeing 'a child needs a home with a mother and a father'",
          y = "")
pi <- plot_income(D018) + l + sy
pe <- plot_edu(D018) + lyb + sy
pi + pe


l <- labs(title = "Full time employment",
          subtitle = "% working 30 hours weekly, among employed men 25-64",
          y = ""
)
evs_emp <- evs |> filter(Employed == 1, Male, Age != "15-24", Age != "65+")
pi <- plot_income(Emp_gt_30, data = evs_emp) + l + sy
pe <- plot_edu(Emp_gt_30, data = evs_emp) + lyb + sy
pi + pe


# People who don't work turn lazy (reversed: 1 = strong disagree, 5 = strong agree)
l <- labs(title = "Attitudes: people who don't work turn lazy", 
          subtitle = "Mean score (1 = strong disagree, 5 = strong agree)",
          y = "")
pi <- plot_income(C038) + l
pe <- plot_edu(C038) + lyb
pi + pe


# Work is a duty to society (same)
l <- labs(title = "Attitudes: work is a duty", 
          subtitle = "Mean score (1 = strong disagree, 5 = strong agree)",
          y = "")
pi <- plot_income(C039) + l
pe <- plot_edu(C039) + lyb
pi + pe
plot_cohort(C039) + l


# Attends services weekly
l <- labs(title = "Attends religious services weekly", 
          subtitle = "% of EVS respondents",
          y = "")
pi <- plot_income(Relig_weekly) + l + sy
pe <- plot_edu(Relig_weekly) + lyb + sy
pi + pe


# Member religious organization
l <- labs(title = "Member of church or religious organization", 
          subtitle = "% of EVS respondents",
          y = "")
pi <- plot_income(A065) + l + sy
pe <- plot_edu(A065) + lyb + sy
pi + pe


# Belong religious denomination
l <- labs(title = "Belong to religious denomination", 
          subtitle = "% member",
          y = "")
pi <- plot_income(F024) + l + sy
pe <- plot_edu(F024) + lyb + sy
pi + pe


# religious person
l <- labs(title = "Religious belief", 
          subtitle = "% saying they are a religious person",
          y = "")
plot_income(Relig_person) + l + sy
plot_edu(Relig_person) + l + sy
plot_age(Relig_person) + l + sy
plot_cohort(Relig_person) + l + sy


# Atheist
l <- labs(title = "Atheism", 
          subtitle = "% of EVS respondents saying they are a 'convinced atheist'",
          y = "")
pi <- plot_income(Atheist) + l + sy 
pe <- plot_edu(Atheist) + lyb + sy
pi + pe


# Justifiable: soft drogs
l <- labs(title = "Soft drugs: how justifiable", 
          subtitle = "% of EVS respondents giving 8 or more on 1-10 scale",
          y = "")
pi <- plot_income(Drugs) + l + sy
pe <- plot_edu(Drugs) + lyb + sy
pi + pe


# Justifiable: lying
l <- labs(title = "Lying: how justifiable", 
          subtitle = "% of EVS respondents giving 8 or more on 1-10 scale",
          y = "")
pi <- plot_income(Lying) + l + sy
pe <- plot_edu(Lying) + lyb + sy
pi + pe
pcl <- plot_cohort(Lying) + l + sy


# Justifiable: adultery
l <- labs(title = "Adultery: how justifiable", 
          subtitle = "% of EVS respondents giving 8 or more on 1-10 scale\n'married men/women having an affair'",
          y = "")
pi <- plot_income(Adultery) + l + sy
pe <- plot_edu(Adultery) + lyb + sy
pi + pe
pca <- plot_cohort(Adultery) + l + sy
pcl + pca


# National pride
l <- labs(title = "National pride", 
          subtitle = "% of EVS respondents 'very proud' of their nationality",
          y = "")
pi <- plot_income(National_pride) + l + sy
pe <- plot_edu(National_pride) + lyb + sy
pi + pe
plot_cohort(National_pride) + l + sy

# Other things you could do: what's important in a child (A027-A042)


