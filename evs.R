
library(tidyverse)
library(forcats)
library(haven)
library(ggstats)
# The EVS trend file, 1981-2017

theme_set(theme_light())
theme_update(
  panel.grid = element_blank(),
  line = element_line(lineend = "round")
) 


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

# Find countries that took part in all waves
ctry_waves <- table(evs_orig$S009, evs_orig$S002EVS) 
ctry_waves <- as.matrix(ctry_waves)
in_all_waves <- apply(ctry_waves, 1, function (x) all(x > 0))
in_all_waves <- rownames(ctry_waves[in_all_waves, ])


evs <- evs_orig |> 
  mutate(
    Age = relabel_na(X003R),
    Age = forcats::fct_recode(Age, "65+" = "65 and more years"),
    across(c(E012, X002, X003, X023R, C038, C039, C040, C011, C012, C017, C024, 
             C031, F028, A065, A097, F024, F034, F126, F127, F128, F140, G006,
             X007, X011, X028), 
           neg_na),
    across(c(D054, D018), relabel_na),
    Marriage_outdated = relabel_na(D022),
    across(c(C038, C039), ~ 6 - .), # originally, 1 = strongly agree, 5 = strongly disagree. Reverse.
    National_pride = 1 * (G006 == 1),
    Income = relabel_na(X047R_EVS),
    Wave   = relabel_na(S002EVS),
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
    Emp_lt_30 = 1 * (X028 == 2)
  ) |> 
  # Only use countries that took part in all waves
  filter(S009 %in% in_all_waves)


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

sy <- scale_y_continuous(labels = scales::percent)

# Agree: Marriage is an outdated institution
plot_income(D022)
plot_edu(D022)
plot_age(D022)

# Agree: A child needs a home with a mother and a father
plot_income(D018)
plot_edu(D018)
plot_age(D018)

# People who don't work turn lazy (reversed: 1 = strong disagree, 5 = strong agree)
l <- labs(title = "People who don't work turn lazy", 
          subtitle = "Mean score (5 = strong agree, 1 = strong disagree)",
          y = "")
plot_income(C038) + l
plot_edu(C038) + l
plot_age(C038) + l

# Work is a duty to society (same)
l <- labs(title = "Work is a duty", 
          subtitle = "Mean score (5 = strong agree, 1 = strong disagree)",
          y = "")
plot_income(C039) + l
plot_edu(C039) + l
plot_age(C039) + l


# Attends services weekly
l <- labs(title = "Attends religious services weekly", 
          subtitle = "% attending",
          y = "")
plot_income(Relig_weekly) + l + sy
plot_edu(Relig_weekly) + l + sy
plot_age(Relig_weekly) + l + sy

# Member religious organization
l <- labs(title = "Member of church or religious organization", 
          subtitle = "% member",
          y = "")
plot_income(A065) + l + sy
plot_edu(A065) + l + sy
plot_age(A065) + l + sy


# Belong religious denomination
l <- labs(title = "Belong to religious denomination", 
          subtitle = "% member",
          y = "")
plot_income(F024) + l + sy
plot_edu(F024) + l + sy
plot_age(F024) + l + sy

# religious person
l <- labs(title = "Religious belief", 
          subtitle = "% saying they are a religious person",
          y = "")
plot_income(Relig_person) + l + sy
plot_edu(Relig_person) + l + sy
plot_age(Relig_person) + l + sy


# Atheist
l <- labs(title = "Atheism", 
          subtitle = "% saying they are a convinced atheist",
          y = "")
plot_income(Atheist) + l + sy 
plot_edu(Atheist) + l + sy
plot_age(Atheist) + l + sy



# Justifiable: soft drogs
l <- labs(title = "Soft drugs: how justifiable", 
          subtitle = "% giving 8 or more on 1-10 scale",
          y = "")
plot_income(Drugs) + l + sy
plot_edu(Drugs) + l + sy
plot_age(Drugs) + l + sy


# Justifiable: lying
l <- labs(title = "Lying: how justifiable", 
          subtitle = "% giving 8 or more on 1-10 scale",
          y = "")
plot_income(Lying) + l + sy
plot_edu(Lying) + l + sy
plot_age(Lying) + l + sy


# Justifiable: adultery
l <- labs(title = "Adultery: how justifiable", 
          subtitle = "% giving 8 or more on 1-10 scale\n'married men/women having an affair'",
          y = "")
plot_income(Adultery) + l + sy
plot_edu(Adultery) + l + sy
plot_age(Adultery) + l + sy


# Justifiable: adultery
l <- labs(title = "National pride", 
          subtitle = "% 'very proud' of their nationality",
          y = "")
plot_income(National_pride) + l + sy
plot_edu(National_pride) + l + sy
plot_age(National_pride) + l + sy


# Has children, unmarried
l <- labs(title = "Unmarried with children", 
          subtitle = "% with children but not now married",
          y = "")
plot_income(Unmarried_children) + l + sy
plot_edu(Unmarried_children) + l + sy
plot_age(Unmarried_children) + l + sy


# Has children, never married

evs_k <- evs |> filter(Has_children == 1)
l <- labs(title = "Never married with children", 
          subtitle = "% with children who are never married",
          y = "")
plot_income(Nev_mar_children, data = evs_k) + l + sy
plot_edu(Nev_mar_children, data = evs_k) + l + sy
plot_age(Nev_mar_children, data = evs_k) + l + sy


# Has children, never married
l <- labs(title = "Never married with children (alt definition)", 
          subtitle = "% with children who are never married\nIncluding cohabiting/reg. partnership",
          y = "")
plot_income(Nev_mar_children_2, data = evs_k) + l + sy
plot_edu(Nev_mar_children_2, data = evs_k) + l + sy
plot_age(Nev_mar_children_2, data = evs_k) + l + sy


# Divorce
l <- labs(title = "Divorce", 
          subtitle = "% divorced",
          y = "")
plot_income(Divorced) + l + sy
plot_edu(Divorced) + l + sy
plot_age(Divorced) + l + sy


l <- labs(title = "Full time employment",
          subtitle = "% working 30 hours weekly, among employed",
          y = ""
          )
evs_emp <- evs |> filter(Employed == 1)
plot_income(Emp_gt_30, data = evs_emp) + l
plot_edu(Emp_gt_30, data = evs_emp) + l
plot_age(Emp_gt_30, data = evs_emp) + l
