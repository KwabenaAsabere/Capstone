df6 %>% group_by(sex,artrunbc) %>% 
  count()

df6 %>% group_by(sex,artrunac) %>% 
  count()

df6

df_artrun <- df6 %>%
  mutate(
    artrunbc = as.character(artrunbc),
    artrunac = as.character(artrunac)
  ) %>%
  pivot_longer(
    cols = c(artrunbc, artrunac),
    names_to = "variable",
    values_to = "response"
  ) %>%
  mutate(
    time = if_else(grepl("bc$", variable), "Before Covid-19", "After Covid-19")
  )

df_artrun

df_artrun_summary <- df_artrun %>%
  group_by(time,sex) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

df_artrun_totals <- df_artrun %>%
  group_by(time) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
 
# mutate(time = "Total")

df_artrun_totals



run_out_of_art_plot <- ggplot(df_trial, aes(x = sex, y = proportion,fill = time)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.5) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2,
    size = 1
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 0.12)) +
  labs(
    title = "Run Out of ART",
    x = "Time",
    y = "Proportion of individuals",
    fill = "Time"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.position = "top"
  )
run_out_of_art_plot


df_artrun_summary<- bind_rows(df_artrun_summary, df_artrun_totals) %>%
  mutate(
    proportion = n_yes / n,
    se = sqrt(proportion * (1 - proportion) / n),
    lower = proportion - 1.96 * se,
    upper = proportion + 1.96 * se
  )%>% 
  mutate(time = as_factor(time) %>% 
           fct_relevel("Before Covid-19"),
        sex= if_else(is.na(sex),"Total",sex))

df_trial <- bind_rows(df_artrun_summary, df_artrun_totals) %>%
  mutate(
    proportion = n_yes / n,
    se = sqrt(proportion * (1 - proportion) / n),
    lower = proportion - 1.96 * se,
    upper = proportion + 1.96 * se
  )%>% 
  mutate(time = as_factor(time) %>% 
           fct_relevel("Before Covid-19"),
         sex= if_else(is.na(sex),"Total",sex))



# missed visits -----------------------------------------------------------

df6 %>% group_by(sex,hivbc) %>% 
  count()

df6 %>% group_by(sex,hivac) %>% 
  count()

df_hiv_summary <- df_hiv %>%
  group_by(time,sex) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

df_hiv_totals <- df_hiv %>%
  group_by(time) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

df_hiv_summary <- bind_rows(df_hiv_summary, df_hiv_totals) %>%
  mutate(
    proportion = n_yes / n,
    se = sqrt(proportion * (1 - proportion) / n),
    lower = proportion - 1.96 * se,
    upper = proportion + 1.96 * se
  )

df_hiv_summary <-  df_hiv_summary %>% 
  mutate(time = as_factor(time) %>% 
           fct_relevel("Before Covid-19"),
         sex= if_else(is.na(sex),"Total",sex)) %>% 
  mutate(time = as_factor(time) %>% 
           fct_relevel("Before Covid-19"),
         sex= if_else(is.na(sex),"Total",sex))

df_hiv_summary



missed_scheduled_visit_plot <-  ggplot(df_hiv_summary, aes(x = sex, y = proportion, fill = time)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.5) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2,
    size = 1
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 0.12)) +
  labs(
    title = "Missed Scheduled Visits",
    x = "Sex",
    y = "Proportion of individuals",
    fill = "Time"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.position = "top"
  )

missed_scheduled_visit_plot


# Reduced Pill Intake -----------------------------------------------------

df_artstr <- df6 %>%
  pivot_longer(
    cols = c(artstrbc, artstrac),
    names_to = "variable",
    values_to = "response"
  ) %>%
  mutate(
    time = if_else(grepl("bc$", variable), "Before Covid-19", "After Covid-19")
  )

df_artstr_summary <- df_artstr %>%
  group_by(time,sex) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

df_artstr_totals <- df_artstr %>%
  group_by(time) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) 

df_artstr_summary <- bind_rows(df_artstr_summary, df_artstr_totals) %>%
  mutate(
    proportion = n_yes / n,
    se = sqrt(proportion * (1 - proportion) / n),
    lower = proportion - 1.96 * se,
    upper = proportion + 1.96 * se
  ) %>% 
  mutate(time = as_factor(time) %>% 
           fct_relevel("Before Covid-19"),
         sex= if_else(is.na(sex),"Total",sex))



df_artstr_summary

reduced_art_intake_plot <- ggplot(df_artstr_summary, aes(x = sex, y = proportion, fill = time)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.5) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2,
    size = 1
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 0.12))+
  labs(
    title = " Reduced ART Intake",
    x = "Sex",
    y = "Proportion of Individuals",
    fill = "Time"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.position = "top"
  )

reduced_art_intake_plot


