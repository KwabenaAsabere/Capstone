# reduced Pill Intake -----------------------------------------------------

df6 %>% group_by(sex,artstrbc) %>% 
  count()

df6 %>% group_by(sex,artstrac) %>% 
  count()

df_artstr_summary <-  df_artstr_summary %>% 
  mutate(time = as_factor(time) %>% 
           fct_relevel("Before Covid-19"))


ggplot(df_artstr_summary, aes(x = time, y = proportion, fill = sex)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.5) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2,
    size = 1
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(
    title = " Reduced ART Intake",
    x = "Time",
    y = "Proportion",
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



# Run out of Pills --------------------------------------------------------

df6 %>% group_by(sex,artrunbc) %>% 
  count()

df6 %>% group_by(sex,artrunac) %>% 
  count()

df_artrun_summary <- bind_rows(df_artrun_summary, df_artrun_totals) %>%
  mutate(
    proportion = n_yes / n,
    se = sqrt(proportion * (1 - proportion) / n),
    lower = proportion - 1.96 * se,
    upper = proportion + 1.96 * se
  )

df_artrun_summary <-  df_artrun_summary %>% 
  mutate(time = as_factor(time) %>% 
           fct_relevel("Before Covid-19"))

df_artrun_summary

ggplot(df_artrun_summary, aes(x = time, y = proportion, fill = sex)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.5) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2,
    size = 1
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Run Out of ART",
    x = "Time",
    y = "Proportion of individuals",
    fill = "Sex"
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


# Missed Scheduled Visit --------------------------------------------------

df6 %>% group_by(sex,hivbc) %>% 
  count()

df6 %>% group_by(sex,hivac) %>% 
  count()

glimpse(df6)





# combination plots -------------------------------------------------------

p1 <- ggplot(df_hiv_summary, aes(x = time, y = proportion, fill = sex)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.5) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2,
    size = 1
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 0.12)) +
  labs(
    caption = "Missed Scheduled Visits",
    x = "Time",
    y = "Proportion of individuals",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "bold", size = 10),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    legend.position = "top"
  )
p1 ## Missed scheduled visits

# -------------------------------------------------------------------------


### Reduced pill intake
p2 <- ggplot(df_artstr_summary, aes(x = time, y = proportion, fill = sex)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.5) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2,
    size = 1
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 0.12))+
  labs(
    caption = " Reduced ART Intake",
    x = "Time",
    y = "Proportion of Individuals",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "bold", size = 10),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    legend.position = "top"
  )
p2


# Run out of pills --------------------------------------------------------

p3 <- ggplot(df_artrun_summary, aes(x = time, y = proportion, fill = sex)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.5) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2,
    size = 1
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 0.12)) +
  labs(
    caption =  "Run Out of ART",
    x = "Time",
    y = "Proportion of individuals",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    plot.caption =  element_text(hjust = 0, face = "bold", size = 10),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    legend.position = "top"
  )

p3

p1/p2/p3




# Community Type ----------------------------------------------------------


df_com <- rakai %>% 
  select(comm_num,artrunbc,
         artstrbc,hivac,copies,new_copies,hivac,artstrac,artstrbc,artrunac,hivbc) %>% 
  mutate(
    community_type = case_when(
      comm_num %in% c(38,770,771,774) ~ "Fishing community",
      .default = "Inland Community") %>% 
      fct_relevel("Inland Community") %>% 
      ff_label("Community type"),
    
    hivac = if_else(hivac ==1, "Yes","No") %>% 
      ff_label("Missed scheduled visit for HIV care") %>% 
      as_factor(),
    
    artrunac = if_else(artrunac ==1,"Yes","No") %>% 
      as_factor() %>% 
      ff_label("Run out of ART before next refill"),
    
    hivbc = if_else(hivbc ==1,"Yes","No") %>% 
      ff_label("Missed scheduled visit for HIV care") %>% 
      as_factor(),
    
    artstrac = if_else(artstrac ==1,"Yes","No") %>% 
      as_factor() %>% 
      ff_label("Taken ART pills less frequently / in smaller
amounts to conserve supply"),
    
    artstrbc = if_else(artrunbc ==1,"Yes","No") %>% 
      as_factor() %>% 
      ff_label("Run out of ART before next refill"),
    
    artstrbc = if_else(artstrbc ==1,"Yes","No") %>% 
      as_factor() %>% 
      ff_label("Taken ART pills less frequently / in smaller
amounts to conserve supply")
  )



# Reduced ART intake by Community -----------------------------------------

df_com %>% group_by(community_type,artstrbc) %>% 
  count()

df_com %>% group_by(community_type,artstrac) %>% 
  count()

df_com_artstr <- df_com %>%
  pivot_longer(
    cols = c(artstrbc, artstrac),
    names_to = "variable",
    values_to = "response"
  ) %>%
  mutate(
    time = if_else(grepl("bc$", variable), "Before Covid-19", "After Covid-19")
  )

df_com_artstr_summary <- df_com_artstr %>%
  group_by(community_type, time) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

df_com_artstr_totals <- df_com_artstr %>%
  group_by(community_type) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(time = "Total")

df_com_artstr_summary <- bind_rows(df_com_artstr_summary, df_com_artstr_totals) %>%
  mutate(
    proportion = n_yes / n,
    se = sqrt(proportion * (1 - proportion) / n),
    lower = proportion - 1.96 * se,
    upper = proportion + 1.96 * se
  )%>% 
  mutate(time = as_factor(time) %>% 
           fct_relevel("Before Covid-19"))

df_com_artstr_summary

reduced_art_intake_by_community_plot <- ggplot(df_com_artstr_summary, aes(x = time, y = proportion, fill = community_type)) +
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
    x = "Time",
    y = "Proportion of Individuals",
    fill = "Community Type"
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

reduced_art_intake_by_community_plot



# community missed scheduled visits ---------------------------------------

df_com %>% group_by(community_type,hivbc) %>% 
  count()

df_com %>% group_by(community_type,hivac) %>% 
  count()

df_com_hiv <- df_com %>%
  mutate(
    hivbc = as.character(hivbc),
    hivac = as.character(hivac)
  ) %>%
  pivot_longer(
    cols = c(hivbc, hivac),
    names_to = "variable",
    values_to = "response"
  ) %>%
  mutate(
    time = if_else(grepl("bc$", variable), "Before Covid-19", "After Covid-19")
  )

df_com_hiv_summary <- df_com_hiv %>%
  group_by(community_type, time) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

df_com_hiv_totals <- df_com_hiv %>%
  group_by(community_type) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(time = "Total")

df_com_hiv_summary <- bind_rows(df_com_hiv_summary, df_com_hiv_totals) %>%
  mutate(
    proportion = n_yes / n,
    se = sqrt(proportion * (1 - proportion) / n),
    lower = proportion - 1.96 * se,
    upper = proportion + 1.96 * se
  )%>% 
  mutate(time = as_factor(time) %>% 
           fct_relevel("Before Covid-19"))



df_com_hiv_summary

missed_scheduled_visit_by_community_type_plot <-  ggplot(df_com_hiv_summary, aes(x = time, y = proportion, fill = community_type)) +
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
    x = "Time",
    y = "Proportion of individuals",
    fill = "Community Type"
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

missed_scheduled_visit_by_community_type_plot



# run out of art by community  ------------------------------------------------------

df_com %>% group_by(community_type,artrunbc) %>% 
  count()

df_com %>% group_by(community_type,artrunac) %>% 
  count()

df_com_artrun <- df_com %>%
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

df_com_artrun_summary <- df_com_artrun %>%
  group_by(community_type, time) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

df_com_artrun_totals <- df_com_artrun %>%
  group_by(community_type) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(time = "Total")

df_com_artrun_summary <- bind_rows(df_com_artrun_summary, df_com_artrun_totals) %>%
  mutate(
    proportion = n_yes / n,
    se = sqrt(proportion * (1 - proportion) / n),
    lower = proportion - 1.96 * se,
    upper = proportion + 1.96 * se
  ) %>% 
  mutate(time = as_factor(time) %>% 
           fct_relevel("Before Covid-19"))



  
df_com_artrun_summary


run_out_of_art_by_community_plot <- ggplot(df_com_artrun_summary, aes(x = time, y = proportion, fill = community_type)) +
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
    fill = "Community Type"
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
run_out_of_art_by_community_plot





# community subplots ------------------------------------------------------

pc1 <- ggplot(df_com_artstr_summary, aes(x = time, y = proportion, fill = community_type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.5) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2,
    size = 1
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 0.12)) +
  labs(
    caption = "Missed Scheduled Visits",
    x = "Time",
    y = "Proportion of individuals",
    fill = "Community Type"
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "bold", size = 10),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    legend.position = "top"
  )
pc1 ## 

pc2 <- ggplot(df_com_hiv_summary, aes(x = time, y = proportion, fill = community_type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.5) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2,
    size = 1
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 0.12)) +
  labs(
    caption = "Missed Scheduled Visits",
    x = "Time",
    y = "Proportion of individuals",
    fill = "Community Type"
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "bold", size = 10),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    legend.position = "top"
  )
pc2 ## Missed scheduled visits


## Run out of pills
pc3 <- ggplot(df_com_artrun_summary, aes(x = time, y = proportion, fill = community_type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.5) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2,
    size = 1
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 0.12)) +
  labs(
    caption =  "Run Out of ART",
    x = "Time",
    y = "Proportion of individuals",
    fill = "Community Type"
  ) +
  theme_minimal() +
  theme(
    plot.caption =  element_text(hjust = 0, face = "bold", size = 10),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    legend.position = "top"
  )

pc3


























