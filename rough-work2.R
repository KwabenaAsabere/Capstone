library(dplyr)
library(tidyr)
library(ggplot2)

df_artstr <- df7 %>%
  pivot_longer(
    cols = c(artstrbc, artstrac),
    names_to = "variable",
    values_to = "response"
  ) %>%
  mutate(
    time = if_else(grepl("bc$", variable), "Before", "After")
  )

df_artstr_summary <- df_artstr %>%
  group_by(sex, time) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

df_artstr_totals <- df_artstr %>%
  group_by(sex) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(time = "Total")

df_artstr_summary <- bind_rows(df_artstr_summary, df_artstr_totals) %>%
  mutate(
    proportion = n_yes / n,
    se = sqrt(proportion * (1 - proportion) / n),
    lower = proportion - 1.96 * se,
    upper = proportion + 1.96 * se
  )

ggplot(df_artstr_summary, aes(x = sex, y = proportion, fill = time)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.7), width = 0.2, size = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proportion with 'Yes' for artstr (Before, After, Total)",
    x = "Sex",
    y = "Proportion with 'Yes' (%)",
    fill = "Time"
  ) +
  theme_minimal()
