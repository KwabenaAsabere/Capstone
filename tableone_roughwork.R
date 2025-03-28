rakai %>% pull(artmos) %>% unique()

table(rakai$artdays)
table(rakai$artwks)
table(rakai$artmos)
table(rakai$educyrs)
table(rakai$educate)
table(rakai$religion)
missing_glimpse(rakai)

df1
df3 <- df1 %>% 
  mutate(ageyrs = ageyrs %>%  ff_label("Age (years)"),
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
         
         primary_occupation = case_when(
           occup1 %in% c(1,2,5) ~ "Agriculture/Homebrewing",
           occup1 %in% c(10,11) ~ "Trading or shopkeeping",
           occup1 %in% c(12,18) ~ "Bar work or waitressing",
           occup1 %in% c(2,3,4) ~ "House work",
           occup1  == 7 ~ "Fishing-related occupation",
           .default = "Other") %>% 
           fct_relevel("Agriculture/Homebrewing") %>% 
           ff_label("Primary Occupation"),
         
         age_cat = case_when(
                             ageyrs < 30 ~ "<30",
                             ageyrs >= 30 & ageyrs <= 39 ~  "30-39",
                             ageyrs >=40 & ageyrs <= 49 ~ "40-49") %>% 
           fct_relevel("<30") %>% 
           ff_label("Age group"),
         
         current_marital_status = case_when(
           currmarr == 1 ~ "Currently married",
           currmarr == 2 ~ "Previously married",
           currmarr == 8 ~ "Never Married"
           ) %>% 
           fct_relevel("Currently married") %>% 
           ff_label("Current marital status"),
         
         art_duration = case_when(
           artyrs >= 1 & artyrs < 2 ~ "2-5 years",
           artyrs > 2 &  artyrs <= 5 ~ "2-5 years",
           artyrs > 5 ~ ">5 years",
           .default =  "<1 year"
         ) %>% 
           fct_relevel("<1 year") %>% 
           ff_label("Time on ART"),
         
         education_level = case_when(
           educyrs == 8 ~ "No formal education",
           educyrs %in% c(1,2) ~ "Primary",
           educyrs %in% c(3,4) ~ "Secondary",
           educyrs %in% c(6,7,10,11) ~ "Apprenticeship/Professional",
           educyrs == 5 ~ "Technical/University"
          ) %>% 
           fct_relevel("No formal education") %>% 
           ff_label("Educational attainment"),
         
         religion = case_when(
           religion %in% c(1,6) ~ "Other or none",
           religion %in% c(2,3,4) ~ "Catholic/Christian",
           religion == 5 ~ "Muslim"
         ) %>% 
           ff_label("Religion")
                                    
         
  )


glimpse(df3)

df4 <- df3 %>% 
  select(
    ageyrs,age_cat,sex,current_marital_status,education_level,primary_occupation,
    mobility,community_type,art_duration
  )



df4 %>% 
  tbl_summary(
    by = sex,
    statistic = 
      list(all_categorical() ~ "{n} ({p}%)",
           all_continuous() ~ "{median} ({IQR})"),
    digits = list(all_categorical() ~ 0,
                  all_continuous() ~ 0)
  ) %>% 
  add_overall() %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  modify_spanning_header(
    update = all_stat_cols() ~ "**Sex**"
  ) %>% 
  modify_footnote(update = all_stat_cols() ~ 
                    "*median(IQR) for continuous;n(%) for categorical variables")
  
  
  glimpse(df3)
  
  
  table(rakai$religion)
missing_glimpse(df4)  
  
  table(df4$art_duration)
table(rakai$artyrs)  
  
  

df4 %>% 
  tbl_summary(
    by = sex,
    statistic = 
      list(all_categorical() ~ "{n} ({p}%)",
           all_continuous() ~ "{median} ({IQR})"),
    digits = list(all_categorical() ~ 0,
                  all_continuous() ~ 0)
  ) %>% 
  add_overall() %>% 
  add_n() %>%
  add_p() %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  modify_spanning_header(
    update = all_stat_cols() ~ "**Sex**"
  ) %>% 
  modify_footnote(update = all_stat_cols() ~ 
 "*median(IQR) for continuous; n(%) for categorical variables*")



library(gtsummary)
library(dplyr)

df4 %>%
  tbl_summary(
    by = sex,
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous()  ~ "{median} ({IQR})"
    ),
    digits = list(
      all_categorical() ~ 0,
      all_continuous()  ~ 0
    )
  ) %>%
  add_overall() %>%
 # add_n() %>%
  add_p(
    test = list(
      all_categorical() ~ "chisq.test",
      all_continuous()  ~ "wilcox.test"
    )
  ) %>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_spanning_header(
    update = all_stat_cols() ~ "**Sex**"
  ) %>%
  modify_footnote(
    update = c("p.value") ~ 
      "a p-values calculated using Pearson’s chi-square tests of association,
      unless otherwise specified.<br>
       b p-values calculated using Wilcoxon rank-sum tests 
    comparing median values and interquartile ranges (IQR)."
  )



df5 <- rakai %>% 
  select(sex,artrunbc,
         artstrbc,hivac,copies,new_copies,hivac,artstrac,artstrbc,artrunac)

df6 <- df5 %>% 
  mutate(
    sex = if_else(sex == "F","Female","Male") %>% 
      as_factor() %>%
      fct_relevel("Female") %>% 
      as_factor() %>% 
      ff_label("Sex"),
  
    hivac = if_else(hivac ==1, "Yes","No") %>% 
    ff_label("Missed scheduled visit for HIV care"),
  artrunac = if_else(artrunac ==1,"Yes","No") %>% 
    as_factor() %>% 
    ff_label("Run out of ART before next refill"),
  
  artstrac = if_else(artstrac ==1,"Yes","No") %>% 
    as_factor() %>% 
    ff_label("Taken ART pills less frequently / in smaller
amounts to conserve supply"),
  
  artrunbc = if_else(artrunbc ==1,"Yes","No") %>% 
    as_factor() %>% 
    ff_label("Run out of ART before next refill"),
  
  artstrbc = if_else(artstrbc ==1,"Yes","No") %>% 
    as_factor() %>% 
    ff_label("Taken ART pills less frequently / in smaller
amounts to conserve supply")
  )
  


df6 %>% pull(copies) 

  df6 %>% 
  mutate(
    copies = str_remove_all(copies, "<\\s*"),
    copies = if_else(copies %in% c("BD", "NA", ""), "0", copies),
    copies = as.numeric(copies),
    
    new_copies = str_remove_all(new_copies, "<\\s*"), 
    new_copies = if_else(new_copies %in% c("BD", "NA", ""), "0", new_copies),
    new_copies = as.numeric(new_copies)
  )
   

# Inspect rows that become NA
df6 %>% 
  filter(!str_remove_all(copies, "<\\s*") %in% c("BD") & 
           is.na(as.numeric(str_remove_all(copies, "<\\s*"))))



  table(df6$copies)

df6 %>%select(copies) %>%  is.na() %>% sum()
df6 %>%select(new_copies) %>%  is.na() %>% sum()



df7 <- df6 %>% filter(copies != "INV.IC ",!is.na(copies),!is.na(new_copies)) %>% 
  mutate(
    copies = str_remove_all(copies, "<\\s*"),
    copies = if_else(copies == "BD", "0", copies),
    copies = as.numeric(copies),
    
    new_copies = str_remove_all(new_copies, "<\\s*"),
    new_copies = if_else(new_copies == "BD", "0", new_copies),
    new_copies = as.numeric(new_copies)
  ) %>% 
  mutate(viral_load_b4 = if_else(copies < 200, "Viral Load Sppression","Viraemia") %>% 
           ff_label("HIV RNA viral load, in copies/ml"),
         viral_load_after = if_else(new_copies < 200,"Viral Load Sppression","Viraemia") %>% 
           ff_label("HIV RNA viral load, in copies/ml"))




df7 

library(dplyr)
library(ggplot2)
library(tidyr)

df_summary <- df7 %>%
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
    se_b4 = sqrt(prop_b4 * (1 - prop_b4) / n_b4),
    lower_b4 = prop_b4 - 1.96 * se_b4,
    upper_b4 = prop_b4 + 1.96 * se_b4,
    se_after = sqrt(prop_after * (1 - prop_after) / n_after),
    lower_after = prop_after - 1.96 * se_after,
    upper_after = prop_after + 1.96 * se_after
  ) %>%
  select(sex, prop_b4, prop_after, lower_b4, upper_b4, lower_after, upper_after) %>%
  pivot_longer(
    cols = c(prop_b4, prop_after),
    names_to = "time_point",
    values_to = "proportion"
  ) %>%
  mutate(
    lower = if_else(time_point == "prop_b4", lower_b4, lower_after),
    upper = if_else(time_point == "prop_b4", upper_b4, upper_after),
    time_point = if_else(time_point == "prop_b4", "Before Covid-19", "After Covid-19")
  )

ggplot(df_summary, aes(x = sex, y = proportion, fill = time_point)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.9), width = 0.2) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Sex", y = "Proportion with Suppressed VL", fill = "Time") +
  theme_minimal()


ggplot(df_summary, aes(x = sex, y = proportion, fill = time_point)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.7),
    width = 0.5
  ) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2,
    size = 1
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Sex", y = "Proportion with Suppressed VL", fill = "Time") +
  theme_minimal()





df7 %>% select(copies) %>% is.na() %>% sum()
df7 %>%select(new_copies) %>%  is.na() %>% sum()



df_summary <- df7 %>%
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
      time_point == "prop_b4"    ~ "Before",
      time_point == "prop_after" ~ "After",
      TRUE                       ~ "Total"
    )
  )


ggplot(df_summary, aes(x = sex, y = proportion, fill = time_point)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.7),
    width = 0.5
  ) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2,
    size = 1
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Sex", y = "Proportion with Suppressed VL", fill = "Time") +
  theme_minimal()


df7

df7 %>% 
  select(sex,artstrac,artstrbc,artrunbc,artrunac)


# -------------------------------------------------------------------------


df_artstr <- df7 %>%
  pivot_longer(
    cols = c(artstrbc, artstrac),
    names_to = "variable",
    values_to = "response"
  ) %>%
  mutate(
    time = if_else(grepl("bc$", variable), "Before Covid-19", "After Covid-19"),
    variable = "Taken ART pills less frequently / in smaller
    amounts to conserve supply"  
  )

df_artstr_summary <- df_artstr %>%
  group_by(sex, time) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(proportion = n_yes / n)


ggplot(df_artstr_summary, aes(x = sex, y = proportion, fill = time)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of 'Yes' for artstr",
    x = "Sex", y = "Proportion taking ART pills less frequently / in smaller
    amounts",
    fill = "Time"
  ) +
  theme_minimal()
# -------------------------------------------------------------------------





df_artrun <- df7 %>%
  pivot_longer(
    cols = c(artrunbc, artrunac),
    names_to = "variable",
    values_to = "response"
  ) %>%
  mutate(
    time = if_else(grepl("bc$", variable), "Before", "After"),
    variable = "Run out of ART before next refill"
  )

df_artrun_summary <- df_artrun %>%
  group_by(sex, time) %>%
  summarise(
    n_yes = sum(response == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(proportion = n_yes / n)

# -------------------------------------------------------------------------



ggplot(df_artstr_summary, aes(x = sex, y = proportion, fill = time)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.5) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2,
    size = 1
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proportion with reduced ART pill intake",
    x = "Sex",
    y = "Proportion of individuals (%)",
    fill = "Time"
  ) +
  scale_fill_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.position = "top"
  )

  


# -------------------------------------------------------------------------

head(df6)
df_artstr
df_artstr_summary

glimpse(df_artstr_summary)

ggplot(df_artstr_summary, aes(x = time, y = proportion, fill = sex)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.5) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2,
    size = 1
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


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
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))











































