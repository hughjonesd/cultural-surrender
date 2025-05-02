
library(tidyverse)
library(patchwork)
library(huxtable)

theme_set(theme_light())
theme_update(
  panel.grid = element_blank()
) 

# cntry country
# pspwght post-strat & design weight
# agea resp age
# evmar ever married
# bthcld ever given birth/fathered a child
# dvrcdeva ever divorced (dissolved civil union)
# hinctnta income decile 1-10 (others are NAs)
# rlgatnd religious attendance (1-3 is weekly or more)
# fcldbrn year first child born
# maryr year first married
# rshpsts relationship with person living with
# NB marsts is *not* useful, see codebook
# wkhtot total hours worked per week inc overtime
# mnactic main activity last 7 days
# 
# Attitudes:
# atchctr - how emot attached to country
# iagrtr - ideal age to retire
# alvgptn - approve if living with unmarried partner
# acldnmr - approve if child with person not married to
# advcyc - approve divorce children under 12
# plnftr - plan for future or take every day as it comes
# ip* - important to X

ess9 <- read_csv("ESS9e03_2/ESS9e03_2.csv") |> 
  mutate(
    across(c(evmar, bthcld, dvrcdeva, rlgblg), 
           ~ ifelse(. >= 7, NA, .)),
    across(c(rlgatnd, eisced, hinctnta, mnactic), 
           ~ ifelse(. > 10, NA, .)),
    across(c(agea), 
           ~ ifelse(. > 100, NA, .)),
    across(c(maryr, fcldbrn), 
           ~ ifelse(. > 2100, NA, .)),
    across(c(wkhtot),
           ~ ifelse(. > 168, NA, .)),
    across(c(starts_with("ip"), starts_with("imp")),
           ~ ifelse(. >= 7, NA, .)),
    across(c(starts_with("ip"), starts_with("imp")),
           ~ 7 - .)
  ) |> 
  mutate(
    Married = rshpsts %in% 1:2,
    Married = ifelse(rshpsts >= 77, NA, Married), # 66 = 'not applicable'. Usually not living with a partner
    "Partner status" = ifelse(Married, 2, 1 * rshpsts %in% 3:6),
    "Partner status" = factor(`Partner status`, labels = c("Alone", "Unmarried partner", "Spouse")),
    "Ever married" = evmar == 1,
    "Has child" = bthcld == 1,
    "Extramarital birth" = (! `Ever married`) | maryr > fcldbrn,
    "Extramarital birth" = ifelse(`Has child`, `Extramarital birth`, NA),
    University = ifelse(eisced >= 5, "University", "No university"),
    "Services weekly" = rlgatnd <=3,
    "Religious belonging" = rlgblg == 1,
    "Paid work last week" = mnactic == 1
  )


## Ever married ####

ess9 |> 
  drop_na(`Ever married`, University) |> 
  filter(
    between(agea, 40, 49)
  ) |> 
  mutate(`Ever married` = 1 * `Ever married`) |> 
  ggplot(aes(University, `Ever married`)) + 
  stat_summary(geom = "col", width = 0.33) +
  labs(title = "Marriage", 
       subtitle = "Per cent ever married, ESS respondents aged 40-49",
       x = "", y = "") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))


ess9 |> 
  drop_na(`Ever married`, hinctnta) |> 
  filter(
    between(agea, 40, 49)
  ) |> 
  mutate(`Ever married` = 1 * `Ever married`) |> 
  ggplot(aes(hinctnta, `Ever married`)) + 
  stat_summary(geom = "col") +
  labs(title = "Marriage", 
       subtitle = "Per cent ever married, ESS respondents aged 40-49",
       x = "Household income decile", y = "") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(breaks = 1:10)

# by country
ess9 |> 
  drop_na(`Ever married`, University) |> 
  filter(
    between(agea, 40, 49)
  ) |> 
  mutate(`Ever married` = 1 * `Ever married`) |> 
  mutate(
    Country = fct_reorder(cntry, University)
  ) |> 
  ggplot(aes(`Ever married`, University, fill = University)) + 
  stat_summary(geom = "col") +
  labs(title = "Marriage", 
       subtitle = "Per cent ever married, ESS respondents aged 40-49",
       x = "", y = "") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) + 
  facet_wrap(vars(Country)) + 
  theme(legend.position = "none")


## Married now ####

ess9 |> 
  drop_na(`Partner status`, University) |> 
  filter(
    between(agea, 40, 49)
  ) |> 
  count(University, `Partner status`) |> 
  mutate(.by = University, proportion = n / sum(n)) |> 
  ggplot(aes(University,  proportion, fill = `Partner status`)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Partnership status", 
       subtitle = "ESS respondents aged 40-49",
       x = "", y = "") +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_brewer(type = "qual", palette = 2) +
  theme(legend.title = element_blank())


ess9 |> 
  drop_na(`Married`, hinctnta) |> 
  filter(
    between(agea, 40, 49)
  ) |> 
  mutate(`Married` = 1 * `Married`) |> 
  ggplot(aes(hinctnta, `Married`)) + 
  stat_summary(geom = "col") +
  labs(title = "Marriage", 
       subtitle = "Per cent living with husband/wife, ESS respondents aged 40-49",
       x = "Household income decile", y = "") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(breaks = 1:10)


## Children in marriage ####

plot_u_emb <- ess9 |> 
  drop_na(`Extramarital birth`, University) |> 
  mutate(
    `Extramarital birth` = 1 * `Extramarital birth`
  ) |> 
  ggplot(aes(University, `Extramarital birth`)) + 
  geom_bar(stat = "summary", width = 0.33) + 
  stat_summary(geom = "linerange", fun.data = \(x) mean_se(x, mult = 1.96)) +
  labs(title = "Extramarital births", 
       subtitle = "Per cent not married before first child, ESS respondents with children",
       x = "", y = "") +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 0.3))


plot_dec_emb <- ess9 |> 
  drop_na(`Extramarital birth`, hinctnta) |> 
  mutate(
    `Extramarital birth` = 1 * `Extramarital birth`
  ) |> 
  ggplot(aes(hinctnta, `Extramarital birth`)) + 
  geom_bar(stat = "summary", width = 0.33) + 
  stat_summary(geom = "linerange", fun.data = \(x) mean_se(x, mult = 1.96)) +
  labs(title = "Extramarital births", 
       subtitle = "Per cent not married before first child, ESS respondents with children",
       x = "Household income decile", y = "") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 1:10) + 
  coord_cartesian(ylim = c(0, 0.3))

plot_u_emb + plot_dec_emb
  
# Controlling for age


lm(`Extramarital birth` ~ poly(agea, 3) + University, drop_na(ess9, agea)) |> 
  summary()

lm(`Extramarital birth` ~ poly(agea, 3) + hinctnta, drop_na(ess9, agea)) |> 
  summary()

# 31-40 year olds
lm(`Extramarital birth` ~ 0 + factor(hinctnta), filter(ess9, agea %in% 31:40)) |> 
  summary()



## Children, never married ####

plot_u_nevmb <- ess9 |> 
  drop_na(`Ever married`, University, `Has child`) |> 
  mutate(
    `Never married` = 1 - `Ever married`
  ) |> 
  filter(`Has child`) |> 
  ggplot(aes(University, `Never married`)) + 
  geom_bar(stat = "summary", width = 0.33) + 
  stat_summary(geom = "linerange", fun.data = \(x) mean_se(x, mult = 1.96)) +
  labs(title = "Never-married births", 
       subtitle = "Per cent never married, ESS respondents with children",
       x = "", y = "") +
  scale_y_continuous(labels = scales::percent) 


plot_dec_nevmb <- ess9 |> 
  drop_na(`Ever married`, hinctnta, `Has child`) |> 
  mutate(
    `Never married` = 1 - `Ever married`
  ) |> 
  filter(`Has child`) |> 
  ggplot(aes(hinctnta, `Never married`)) + 
  geom_bar(stat = "summary", width = 0.33) + 
  stat_summary(geom = "linerange", fun.data = \(x) mean_se(x, mult = 1.96)) +
  labs(title = "Never-married births", 
       subtitle = "Per cent never married, ESS respondents with children",
       x = "Household income decile", y = "") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 1:10)

plot_u_nevmb + plot_dec_nevmb



plot_u_nevmb_u40 <- ess9 |> 
  drop_na(`Ever married`, University, `Has child`) |> 
  mutate(
    `Never married` = 1 - `Ever married`
  ) |> 
  filter(`Has child`, agea <= 40) |> 
  ggplot(aes(University, `Never married`)) + 
  geom_bar(stat = "summary", width = 0.33) + 
  stat_summary(geom = "linerange", fun.data = \(x) mean_se(x, mult = 1.96)) +
  labs(title = "Never-married births", 
       subtitle = "Per cent never married\nESS respondents 40 or below with children",
       x = "", y = "") +
  scale_y_continuous(labels = scales::percent) 


plot_dec_nevmb_u40 <- ess9 |> 
  drop_na(`Ever married`, hinctnta, `Has child`) |> 
  mutate(
    `Never married` = 1 - `Ever married`
  ) |> 
  filter(`Has child`, , agea <= 40) |> 
  ggplot(aes(hinctnta, `Never married`)) + 
  geom_bar(stat = "summary", width = 0.33) + 
  stat_summary(geom = "linerange", fun.data = \(x) mean_se(x, mult = 1.96)) +
  labs(title = "Never-married births", 
       subtitle = "Per cent never married\nESS respondents 40 or below with children",
       x = "Household income decile", y = "") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 1:10)

plot_u_nevmb_u40 + plot_dec_nevmb_u40


#### Religion ####

plot_u_attend <- ess9 |> 
  drop_na(`Services weekly`, University) |> 
  mutate(`Services weekly` = 1 * `Services weekly`) |> 
  ggplot(aes(University, `Services weekly`)) + 
    stat_summary(geom = "col", width = 0.3) +
    labs(
      title = "Religious attendance by education", 
      subtitle = "Per cent attending services at least weekly, ESS9 respondents",
      x = "", y = "") +
    scale_y_continuous(labels = scales::percent) +
    coord_cartesian(ylim = c(0, 0.2))


plot_dec_attend <- ess9 |> 
  drop_na(`Services weekly`, hinctnta) |> 
  mutate(`Services weekly` = 1 * `Services weekly`) |> 
  ggplot(aes(hinctnta, `Services weekly`)) + 
  stat_summary(geom = "col", width = 0.3) +
  labs(
    title = "Religious attendance by household income", 
    subtitle = "Per cent attending services at least weekly, ESS9 respondents",
    x = "Household income decile", y = "") +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 0.2)) +
  scale_x_continuous(breaks = 1:10)
  

plot_u_belong <- ess9 |> 
  drop_na(`Religious belonging`, University) |> 
  mutate(`Religious belonging` = 1 * `Religious belonging`) |> 
  ggplot(aes(University, `Religious belonging`)) + 
  stat_summary(geom = "col", width = 0.3) +
  labs(
    title = "Religious belonging by education", 
    subtitle = "Per cent saying they belong to a religion, ESS9 respondents",
    x = "", y = "") +
  scale_y_continuous(labels = scales::percent)


plot_dec_belong <- ess9 |> 
  drop_na(`Religious belonging`, hinctnta) |> 
  mutate(`Religious belonging` = 1 * `Religious belonging`) |> 
  ggplot(aes(hinctnta, `Religious belonging`)) + 
  stat_summary(geom = "col", width = 0.3) +
  labs(
    title = "Religious belonging by household income", 
    subtitle = "Per cent saying they belong to a religion, ESS9 respondents",
    x = "Household income decile", y = "") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 1:10)


plot_u_belong + plot_u_attend
plot_dec_belong + plot_dec_attend

ess9 |> 
  drop_na(`Religious belonging`, University) |> 
  mutate(`Belongs to a religion` = 1 * `Religious belonging`) |> 
  ggplot(aes(University, `Belongs to a religion`)) + 
  stat_summary(geom = "col") +
  labs(title = "% belonging to a religion", subtitle = "ESS9 respondents") +
  scale_y_continuous(labels = scales::percent)


ess9 |> 
  mutate(
    Married = marsts %in% 1:2,
    "Ever married" = evmar == 1,
    "Has child" = bthcld == 1,
    University = ifelse(ess9$eisced >= 5, "Yes", "No"),
  ) |> 
  filter(
    between(agea, 40, 49)
  ) |> 
  summarize(.by = c(University),
            "% ever married" = mean(`Ever married`)
  ) |> 
  ggplot(aes(University, `% ever married`, fill = University)) + geom_col()


ess9 |> 
  mutate(
    Married = marsts %in% 1:2,
    "Ever married" = evmar == 1,
    "Has child" = bthcld == 1,
    University = ifelse(ess9$eisced >= 5, "Yes", "No"),
  ) |> 
  filter(
    between(agea, 40, 49)
  ) |> 
  summarize(.by = c(cntry, University),
    "% ever married" = mean(`Ever married`)
  ) |> 
  mutate(
    Country = fct_reorder(cntry, `% ever married`)
  ) |> 
  ggplot(aes(`% ever married`, Country, fill = University)) + geom_col(position = "dodge")


## Paid work ####


plot_u_paid <- ess9 |> 
  drop_na(`Paid work last week`, University) |> 
  mutate(
    `Paid work last week` = 1 * `Paid work last week`
  ) |> 
  ggplot(aes(University, `Paid work last week`)) + 
  geom_bar(stat = "summary", width = 0.33) + 
  stat_summary(geom = "linerange", fun.data = \(x) mean_se(x, mult = 1.96)) +
  labs(title = "Paid work", 
       subtitle = "Per cent whose main activity was paid work last week, ESS respondents",
       x = "", y = "") +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 0.75))

plot_dec_paid <- ess9 |> 
  drop_na(`Paid work last week`, hinctnta) |> 
  mutate(
    `Paid work last week` = 1 * `Paid work last week`
  ) |> 
  ggplot(aes(hinctnta, `Paid work last week`)) + 
  geom_bar(stat = "summary", width = 0.33) + 
  stat_summary(geom = "linerange", fun.data = \(x) mean_se(x, mult = 1.96)) +
  labs(title = "Paid work", 
       subtitle = "Per cent whose main activity was paid work last week, ESS respondents",
       x = "Household income decile", y = "") +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 1))

plot_u_paid + plot_dec_paid

men3049 <- ess9 |> filter(gndr == 1, between(agea, 30, 49))

plot_u_lt40 <- men3049 |> 
  drop_na(wkhtot, University) |> 
  ggplot(aes(University, 1*(wkhtot < 40))) + 
  geom_bar(stat = "summary", width = 0.33) +
  labs(title = "Short hours", 
       subtitle = "Per cent working less than 40 hours per week\nMales 30-49, in work",
       x = "", y = "") +
  scale_y_continuous(labels = scales::percent)

plot_u_gt48 <- men3049 |> 
  drop_na(wkhtot, University) |> 
  ggplot(aes(University, 1*(wkhtot > 48))) + 
  geom_bar(stat = "summary", width = 0.33) +
  labs(title = "Long hours", 
       subtitle = "Per cent working more than 48 hours per week\nMales 30-49, in work",
       x = "", y = "") +
  scale_y_continuous(labels = scales::percent)

plot_u_lt40 + plot_u_gt48


plot_dec_lt40 <- men3049 |> 
  drop_na(wkhtot, hinctnta) |> 
  ggplot(aes(hinctnta, 1*(wkhtot < 40))) + 
  geom_bar(stat = "summary", width = 0.33) +
  labs(title = "Short hours", 
       subtitle = "Per cent working less than 40 hours per week\nMales 30-49, in work",
       x = "Household income decile", y = "") + 
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::percent)

plot_dec_gt48 <- men3049 |> 
  drop_na(wkhtot, hinctnta) |> 
  ggplot(aes(hinctnta, 1*(wkhtot > 48))) + 
  geom_bar(stat = "summary", width = 0.33) +
  labs(title = "Long hours", 
       subtitle = "Per cent working more than 48 hours per week\nMales 30-49, in work",
       x = "Household income decile", y = "") + 
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::percent)

plot_dec_lt40 + plot_dec_gt48


## Attitudes ####
## 

attitude_plot <- function (x, title = substitute(x)) {
  ess9$Age <- ifelse(ess9$agea >= 50, "50 and over", "Under 50")
  plot_u <- ess9 |> 
    drop_na({{x}}, University, Age) |> 
    ggplot(aes(University, {{x}}, color = Age)) + 
    stat_summary(fun.data = \(y) mean_se(y, mult = 1.96)) +    
    labs(title = title,
         subtitle = paste0("Mean of ", tolower(title), ": ESS respondents"), 
         x = "", 
         y = "") + 
    theme(legend.position = "none") +
    scale_color_manual(values = c("50 and over" = "navy", "Under 50" = "darkred"))
  
  plot_dec <- ess9 |> 
    drop_na({{x}}, hinctnta, Age) |> 
    ggplot(aes(hinctnta, {{x}}, color = Age)) + 
    stat_summary(fun.data = \(y) mean_se(y, mult = 1.96)) +    
    labs(
         x = "Household income decile", 
         y = "") +
    theme(legend.title = element_blank()) +
    scale_color_manual(values = c("50 and over" = "navy", "Under 50" = "darkred")) +
    scale_x_continuous(breaks = 1:10)

  plot_u + plot_dec
}



attitude_plot(iphlppl)  # help people
attitude_plot(ipgdtim)  # good time
attitude_plot(impfun)   # have fun
attitude_plot(impdiff)  # try new and different things
attitude_plot(ipcrtiv) # be creative
attitude_plot(imprich)  # be rich

attitude_plot(ipmodst)  # modest
attitude_plot(ipfrule)  # follow rule
attitude_plot(ipbhprp)  # behave properly
attitude_plot(imptrad) # follow traditions

# correlations between the two groups of attitudes
ess9 |> 
  select(ipgdtim, impfun, impdiff, ipcrtiv, impfree, 
         ipmodst, ipfrule, ipbhprp, imptrad) |> 
  cor(use = "pair") |> 
  round(3) |> 
  as_hux() |> 
  map_text_color(by_colorspace("grey", "yellow"))

ess9$attitude_fun <- ess9 |> 
  select(ipgdtim, impfun, impdiff, ipcrtiv, impfree) |> 
  rowMeans()

ess9$attitude_trad <- ess9 |> 
  select(ipmodst, ipfrule, ipbhprp, imptrad) |> 
  rowMeans()

attitude_plot(attitude_fun, "Individualist attitudes")
attitude_plot(attitude_trad, "Traditional attitudes")
