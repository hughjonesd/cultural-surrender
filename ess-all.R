
essa <- read_csv("ESS-all-rounds/ESS-all-rounds.csv") |> mutate(
  across(c(marital, chldhhe, dvrcdeva, rlgblg), 
         ~ ifelse(. >= 6, NA, .)),
  across(c(rlgatnd, eisced, hinctnta, marsts), 
         ~ ifelse(. > 10, NA, .)),
  across(c(agea), 
         ~ ifelse(. > 100, NA, .))
) |> 
  mutate(
    Married = marsts %in% 1:2,
    "Ever married" = marital != 5,
    "Has child" = chldhhe == 1,
    University = ifelse(eisced >= 5, "University", "No university"),
    "Services weekly" = rlgatnd <= 3,
    "Religious belonging" = rlgblg == 1
  )
