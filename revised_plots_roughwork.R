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

# -------------------------------------------------------------------------

rakai %>% 
 mutate(
  art_duration = case_when(
     artyrs > 2 &  artyrs <= 5 ~ "2-5 years",
    artyrs > 5 ~ ">5 years",
    .default =  "<2 years"
  ) %>% 
    fct_relevel("<2 years","2-5 years") %>% 
    ff_label("Time on ART")) %>% 
  pull(art_duration) %>% unique()




# Viral Load Suppression --------------------------------------------------

df7 
df_vl_summary <- df7 %>%
  mutate(
    b4_supp = if_else(viral_load_b4 == "Viral Load Sppression", 1, 0),
    after_supp = if_else(viral_load_after == "Viral Load Sppression", 1, 0)
  ) %>%
  group_by(sex) %>%
  summarise(
    n_b4_supp = sum(b4_supp, na.rm = TRUE),
    n_b4 = sum(!is.na(b4_supp)),
    n_after_supp = sum(after_supp, na.rm = TRUE),
    n_after = sum(!is.na(after_supp))
  ) %>%
  ungroup() %>%

  mutate(
    prop_b4 = n_b4_supp / n_b4,
    prop_after = n_after_supp / n_after,
    prop_both = (n_b4_supp + n_after_supp) / (n_b4 + n_after),
    
    se_b4 = sqrt(prop_b4 * (1 - prop_b4) / n_b4),
    lower_b4 = prop_b4 - 1.96 * se_b4,
    upper_b4 = prop_b4 + 1.96 * se_b4,
    
    se_after = sqrt(prop_after * (1 - prop_after) / n_after),
    lower_after = prop_after - 1.96 * se_after,
    upper_after = prop_after + 1.96 * se_after,
    
    se_both = sqrt(prop_both * (1 - prop_both) / (n_b4 + n_after)),
    lower_both = prop_both - 1.96 * se_both,
    upper_both = prop_both + 1.96 * se_both
  ) %>%
  select(
    sex, prop_b4, prop_after, prop_both,
    lower_b4, upper_b4, lower_after, upper_after, lower_both, upper_both
  ) %>%
  pivot_longer(
    cols = c(prop_b4, prop_after, prop_both),
    names_to = "time_point",
    values_to = "proportion"
  ) %>%
  mutate(
    lower = case_when(
      time_point == "prop_b4"    ~ lower_b4,
      time_point == "prop_after" ~ lower_after,
      TRUE                       ~ lower_both
    ),
    upper = case_when(
      time_point == "prop_b4"    ~ upper_b4,
      time_point == "prop_after" ~ upper_after,
      TRUE                       ~ upper_both
    ),
    time_point = case_when(
      time_point == "prop_b4"    ~ "Before Covid-19",
      time_point == "prop_after" ~ "After Covid-19",
      TRUE                       ~ "Total"
    ) %>% 
      fct_relevel("Before Covid-19")
  )

df_vl_summary




df_artstr_totals <- df_artstr %>%
  group_by(time) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) 



df7 %>%
  mutate(
    b4_supp = if_else(viral_load_b4 == "Viral Load Sppression", 1, 0),
    after_supp = if_else(viral_load_after == "Viral Load Sppression", 1, 0)
  ) %>%
  group_by(sex) %>%
  summarise(
    n_b4_supp = sum(b4_supp, na.rm = TRUE),
    n_b4 = sum(!is.na(b4_supp)),
    n_after_supp = sum(after_supp, na.rm = TRUE),
    n_after = sum(!is.na(after_supp)),
    .groups = "drop"
  )


# -------------------------------------------------------------------------


df6 %>%
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



# -------------------------------------------------------------------------

df7 <- df6 %>% filter(copies != "INV.IC ",!is.na(copies),!is.na(new_copies)) %>% 
  mutate(
    copies = str_remove_all(copies, "<\\s*"),
    copies = if_else(copies == "BD", "0", copies),
    copies = as.numeric(copies),
    
    new_copies = str_remove_all(new_copies, "<\\s*"),
    new_copies = if_else(new_copies == "BD", "0", new_copies),
    new_copies = as.numeric(new_copies)
  ) %>% 
  mutate(viral_load_b4 = if_else(copies < 200, "Viral Load Suppression","Viraemia") %>% 
           ff_label("HIV RNA viral load, in copies/ml"),
         viral_load_after = if_else(new_copies < 200,"Viral Load Suppression","Viraemia") %>% 
           ff_label("HIV RNA viral load, in copies/ml"))

df7

df_vl_supp  <- df7 %>%
  mutate(
    suppbc = if_else(viral_load_b4 == "Viral Load Suppression", 1, 0),
    suppac = if_else(viral_load_after == "Viral Load Suppression", 1, 0)
  ) %>% 
  select(-c(copies,new_copies))%>% 
  pivot_longer(
    cols = c(viral_load_b4,viral_load_after),
    names_to = "variable",
    values_to = "viral_load"
  ) %>%
  mutate(
    time = if_else(grepl("b4$", variable), "Before Covid-19", "After Covid-19"),
  )
  
  
df_vl_summary <- df_vl_supp %>% filter(!is.na(viral_load)) %>% 
  group_by(time,viral_load) %>%
  summarise(
    n_suppressed = n(),
    .groups = "drop"
  ) %>% 
  mutate(time = as_factor(time) %>%  fct_relevel("Before Covid-19"))

df_vl_summary <-  df_vl_summary %>% 
  group_by(time) %>% 
  summarise(n = sum(n_suppressed)) %>% 
  left_join(df_vl_summary,join_by(time))

df_vl_summary <- df_vl_summary  %>%
  mutate(
    proportion = n_suppressed / n,
    se = sqrt(proportion * (1 - proportion) / n),
    lower = proportion - 1.96 * se,
    upper = proportion + 1.96 * se
  ) 


 


table(df7$viral_load_b4)
table(df7$viral_load_after)
table(df_vl_supp$viral_load)



ggplot(df_vl_summary, aes(x = time_point, y = proportion, fill = sex)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.7),
    width = 0.5
  ) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2,
    size = 0.5
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Time", y = "Proportion with Suppressed VL", fill = "Sex") +
  
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    legend.position = "top"
  )
df6

df_disrupt_1 <- df6 %>% 
  select(-c(copies,new_copies)) %>% 
 # filter(hivac !=8, hivbc!=8,artrunbc!=8,artstrac!=8,artstrbc!=8,artrunac!=8) %>% 
  mutate(across(artrunbc:artstrbc,
       ~ map_dbl(.x, ~ if_else(.x == "No", 0, if_else(.x == "Yes", 1, NA_real_))))) %>% 
  mutate(any_disruption = if_else(rowSums(across(artrunbc:artstrbc),na.rm = TRUE) > 0,1,0))

df_disrupt_1

df_disrupt_2 <- df4 %>% 
  select(sex,ageyrs,mobility,art_duration,community_type) %>% 
  mutate(
    age_cat = case_when(
      ageyrs < 30 ~ "<30",
      ageyrs >= 30 & ageyrs <= 39 ~  "30-39",
      ageyrs >=40 & ageyrs <= 49 ~ "40-49") %>% 
      fct_relevel("<30") %>% 
      ff_label("Age group")) %>% 
  select(-ageyrs)
  





  rakai %>% 
  select(ageyrs,artrunbc,artrunac,
         hivac,hivbc,copies,new_copies,artstrac,artstrbc) %>% 
  filter(hivac !=8, hivbc!=8,artrunbc!=8,artstrac!=8,artstrbc!=8,artrunac!=8) %>% 
  mutate(
    age_cat = case_when(
      ageyrs < 30 ~ "<30",
      ageyrs >= 30 & ageyrs <= 39 ~  "30-39",
      ageyrs >=40 & ageyrs <= 49 ~ "40-49") %>% 
      fct_relevel("<30") %>% 
      ff_label("Age group"))





df3 %>% 
  mutate(
    hivac = if_else(hivac ==1, "Yes","No") %>% 
      ff_label("Missed scheduled visit for HIV care") %>% 
      as_factor(),
    
    hivbc = if_else(hivbc ==1,"Yes","No") %>% 
      ff_label("Missed scheduled visit for HIV care") %>% 
      as_factor(),
    
    artrunac = if_else(artrunac ==1,"Yes","No") %>% 
      as_factor() %>% 
      ff_label("Run out of ART before next refill"),
    
    artrunbc = if_else(artrunbc ==1,"Yes","No") %>% 
      as_factor() %>% 
      ff_label("Run out of ART before next refill"),
    
    artstrac = if_else(artstrac ==1,"Yes","No") %>% 
      as_factor() %>% 
      ff_label("Taken ART pills less frequently / in smaller
amounts to conserve supply"),
    
    artstrbc = if_else(artstrbc ==1,"Yes","No") %>% 
      as_factor() %>% 
      ff_label("Taken ART pills less frequently / in smaller
amounts to conserve supply")
  )



 df3 %>% 
  mutate(
    hivac = if_else(hivac == "Yes", 1, 0) %>% 
      ff_label("Missed scheduled visit for HIV care"),
    
    hivbc = if_else(hivbc == "Yes", 1, 0) %>% 
      ff_label("Missed scheduled visit for HIV care"),
    
    artrunac = if_else(artrunac == "Yes", 1, 0) %>% 
      ff_label("Run out of ART before next refill"),
    
    artrunbc = if_else(artrunbc == "Yes", 1, 0) %>% 
      ff_label("Run out of ART before next refill"),
    
    artstrac = if_else(artstrac == "Yes", 1, 0) %>% 
      ff_label("Taken ART pills less frequently / in smaller amounts to conserve supply"),
    
    artstrbc = if_else(artstrbc == "Yes", 1, 0) %>% 
      ff_label("Taken ART pills less frequently / in smaller amounts to conserve supply")
  )




 rakai %>% 
   select(ageyrs,sex,mobility,arthoac,artrunac,artstrac,
    artyrs,comm_num,artrunbc,artstrbc,hivac,hivbc,copies,new_copies) %>% 
   mutate(
     age_cat = case_when(
       ageyrs < 30 ~ "<30",
       ageyrs >= 30 & ageyrs <= 39 ~  "30-39",
       ageyrs >=40 & ageyrs <= 49 ~ "40-49") %>% 
       fct_relevel("<30") %>% 
       ff_label("Age group"),
     
     sex = if_else(sex == "F","Female","Male") %>% 
       as_factor() %>%
       fct_relevel("Female") %>% 
       ff_label("Sex"),
     
     mobility = case_when(
       mobility %in% c(3,8,10) ~ "In-migrant",
       .default = "Long-term resident") %>% 
       fct_relevel("In-migrant") %>% 
       ff_label("Migration"),
     
     community_type = case_when(
       comm_num %in% c(38,770,771,774) ~ "Fishing community",
       .default = "Inland Community") %>% 
       fct_relevel("Inland Community") %>% 
       ff_label("Community type"),
     fishing_comm = if_else(community_type == "Fishing Community",1,0) %>% 
       ff_label("Lake Victoria Fishing Community"),
     
     art_duration = case_when(
       artyrs >= 2 &  artyrs <= 5 ~ "2-5 years",
       artyrs > 5 ~ ">5 years",
       .default =  "<2 years"
     ) %>% 
       fct_relevel("<2 years","2-5 years") %>% 
       ff_label("Time on ART"),
     
     hivac = if_else(hivac == "Yes", 1, 0) %>% 
       ff_label("Missed scheduled visit for HIV care"),
     
     hivbc = if_else(hivbc == "Yes", 1, 0) %>% 
       ff_label("Missed scheduled visit for HIV care"),
     
     artrunac = if_else(artrunac == "Yes", 1, 0) %>% 
       ff_label("Run out of ART before next refill"),
     
     artrunbc = if_else(artrunbc == "Yes", 1, 0) %>% 
       ff_label("Run out of ART before next refill"),
     
     artstrac = if_else(artstrac == "Yes", 1, 0) %>% 
       ff_label("Taken ART pills less frequently / in smaller amounts to conserve supply"),
     
     artstrbc = if_else(artstrbc == "Yes", 1, 0) %>% 
       ff_label("Taken ART pills less frequently / in smaller amounts to conserve supply"),
     
     
     
    )



 library(dplyr)
 library(ggplot2)
 
 # Helper function to calculate proportions and confidence intervals
 prepare_data <- function(data, group_var) {
   data %>%
     group_by({{ group_var }}) %>%
     summarise(
       n_b4 = sum(any_disruption_b4, na.rm = TRUE),
       total_b4 = n(),
       n_after = sum(any_disruption_after, na.rm = TRUE),
       total_after = n(),
       .groups = "drop"
     ) %>%
     mutate(
       proportion_b4 = n_b4 / total_b4,
       proportion_after = n_after / total_after,
       se_b4 = sqrt(proportion_b4 * (1 - proportion_b4) / total_b4),
       se_after = sqrt(proportion_after * (1 - proportion_after) / total_after),
       lower_b4 = proportion_b4 - 1.96 * se_b4,
       upper_b4 = proportion_b4 + 1.96 * se_b4,
       lower_after = proportion_after - 1.96 * se_after,
       upper_after = proportion_after + 1.96 * se_after
     ) %>%
     pivot_longer(
       cols = starts_with("proportion"),
       names_to = "time",
       values_to = "proportion"
     ) %>%
     mutate(
       time = if_else(time == "proportion_b4", "Before Covid-19", "After Covid-19"),
       lower = if_else(time == "Before Covid-19", lower_b4, lower_after),
       upper = if_else(time == "Before Covid-19", upper_b4, upper_after)
     ) %>%
     select({{ group_var }}, time, proportion, lower, upper)
 }
 
 # Prepare data for each grouping variable
 summary_by_sex <- prepare_data(df_disruption, sex)
 summary_by_age <- prepare_data(df_disruption, age_cat)
 summary_by_mobility <- prepare_data(df_disruption, mobility)
 summary_by_community <- prepare_data(df_disruption, community_type)
 
 
 plot_disruption <- function(data, x_var, title) {
   ggplot(data, aes(x = {{ x_var }}, y = proportion, fill = time)) +
     geom_col(position = position_dodge(width = 0.7), width = 0.5) +
     geom_errorbar(
       aes(ymin = lower, ymax = upper),
       position = position_dodge(width = 0.7),
       width = 0.2,
       size = 1
     ) +
     scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.30)) +
     labs(
       title = title,
       x = as.character(substitute(x_var)),
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
       legend.position = c(0, 1), 
       legend.justification = c(0, 1),
       legend.direction = "horizontal"
     )
 }
 
 # Plot for disruption by sex
 plot_disruption(summary_by_sex, sex, "Any Disruption by Sex")
 
 # Plot for disruption by age category
 plot_disruption(summary_by_age, age_cat, "Any Disruption by Age Category")
 
 # Plot for disruption by mobility
 plot_disruption(summary_by_mobility, mobility, "Any Disruption by Mobility")
 
 # Plot for disruption by community type
 plot_disruption(summary_by_community, community_type, "Any Disruption by Community Type")
 




