library(tidyverse)
library(broom)
library(finalfit)
library(MASS)
library(conflicted)
library(gtsummary)
library(gt)
library(flextable)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


rakai <- read_rds("df_viraemia.rds")
glimpse(rakai)

df_viraemia <- rakai %>% drop_na()

model_nb1 <-df_viraemia %>%
  glm.nb(viraemia_after_dum ~ any_disruption_after + any_disruption_b4 + sex +
           age_cat+ mobility+community_type + art_duration, data = .)
model_nb1 %>% tidy(exponentiate = TRUE,conf.int = TRUE)

nb_model <- df_viraemia %>% 
  glm.nb(any_disruption_after ~ any_disruption_b4 + sex +
           age_cat+ mobility+community_type + art_duration + viraemia_b4_dum, data = .)


nb_model %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  names()

nb_model_tidy <- nb_model %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE)

nb_model_tidy %>% 
  select(term, estimate, conf.low, conf.high) %>% 
  mutate(across(where(is.numeric), \(x) round(x, 2)))


fp <- read_rds("df_forest_plot.rds")
fp

df_forest <- nb_model_tidy %>%
 dplyr:: filter(term != "(Intercept)") %>%
  mutate(term = case_when(
    #term == "any_disruption_after" ~ "ART Disruption After (Yes)",
    term == "any_disruption_b4" ~ "ART Disruption Before (Yes)",
    term == "sexMale" ~ "Sex (Male vs Female)",
    term == "age_cat<30" ~ "Age: <30 vs 40-49",
    term == "age_cat30-39" ~ "Age: 30-39 vs 40-49",
    term == "mobilityLong-term resident" ~ "Mobility (Long-term Resident)",
    term == "community_typeFishing community" ~ "Community Type (Fishing)",
    term == "art_duration2-5 years" ~ "ART Duration: 2-5 years",
    term == "art_duration>5 years" ~ "ART Duration: >5 years",
    term == "viraemia_b4_dum" ~ "Viraemia before Covid",
    TRUE ~ term
  )) %>%
  arrange(desc(estimate))

df_forest
# saveRDS(df_forest, file = "disruption_after_regression")

ggplot(df_forest, aes(y = reorder(term, estimate), x = estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "black") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Forest Plot of Regression Results",
    x = "Prevalence Ratio (95% CI)",
    y = "Variables"
  ) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.y = element_blank()
  )




nb_model %>% glance()

any_disruption_after ~ any_disruption_b4 + sex +
  age_cat+ mobility+community_type + art_duration + viraemia_b4_dum

explanatory <- c("any_disruption_b4","sex","age_cat","mobility","community_type",
               "art_duration","viraemia_b4_dum")

explanatory_vars <-  c("any_disruption_b4","sex","age_cat","mobility","community_type",
                       "art_duration","viraemia_b4_dum")

dependent_var <- "art_disruption_after"

df_viraemia

df_viraemia %>% 
  finalfit(dependent = dependent_var, 
           explanatory = explanatory_vars, 
           metrics = TRUE,  # Includes model fit metrics
           family = "quasipoisson")  # Use Quasi-Poisson regression


df_viraemia %>% 
  or_plot(dependent = dependent_var, 
          explanatory = explanatory_vars, 
          family = "quasipoisson")



df_viraemia %>% 
  or_plot(dependent = dependent_var, 
          explanatory = explanatory_vars, 
          glm_function = glm.nb)

nb_model_tidy %>% 
  select(term, estimate, conf.low, conf.high) %>% 
  mutate(across(.cols = where(is.numeric), .fns = \(x) round(x, 2)))

models <- explanatory_vars %>% 
  str_c("any_disruption_after ~ ", .) %>% 
  map(
    .f = ~glm.nb(
      formula = as.formula(.x),
      data = df_viraemia) 
  ) %>% 
  map(
    .f = ~tidy(.x, exponentiate = TRUE, conf.int = TRUE)
  ) %>% 
  bind_rows() %>% 
  mutate(across(where(is.numeric), \(x) round(x,2)))

models %>% 
 dplyr:: filter(term != "(Intercept)") %>% 
  select(term,estimate,conf.low,conf.high) %>% 
  gt()


models %>% 
  dplyr:: filter(term != "(Intercept)") %>% 
  select(term,estimate,conf.low,conf.high) %>%
  flextable()

nb_univariate_tidy <- models %>% 
  dplyr:: filter(term != "(Intercept)") %>% 
  select(term,estimate,conf.low,conf.high)

nb_multivariate_tidy <- nb_model_tidy %>% 
  select(term, estimate, conf.low, conf.high) %>% 
  mutate(across(.cols = where(is.numeric), .fns = \(x) round(x, 2))) %>% 
  dplyr:: filter(term != "(Intercept)") 
  
  
  nb_univariate_tidy
  nb_multivariate_tidy

  nb_univariate_tidy <- nb_univariate_tidy %>%
    mutate(model = "Univariate")

  nb_multivariate_tidy <- nb_multivariate_tidy %>%
    mutate(model = "Multivariate")
 
   forest_data <- bind_rows(nb_univariate_tidy, nb_multivariate_tidy)
   
   forest_data <-  forest_data %>% 
     mutate(term = case_when(
       #term == "any_disruption_after" ~ "ART Disruption After (Yes)",
       term == "any_disruption_b4" ~ "ART Disruption Before (Yes)",
       term == "sexMale" ~ "Sex (Male vs Female)",
       term == "age_cat<30" ~ "Age: <30 vs 40-49",
       term == "age_cat30-39" ~ "Age: 30-39 vs 40-49",
       term == "mobilityLong-term resident" ~ "Mobility (Long-term Resident)",
       term == "community_typeFishing community" ~ "Community Type (Fishing)",
       term == "art_duration2-5 years" ~ "ART Duration: 2-5 years",
       term == "art_duration>5 years" ~ "ART Duration: >5 years",
       term == "viraemia_b4_dum" ~ "Viraemia before Covid",
       TRUE ~ term
     )) %>%
     arrange(desc(estimate))
   

   forest_data

   ggplot(forest_data, aes(x = term, y = estimate, color = model)) +
     geom_point(position = position_dodge(width = 0.5), size = 2) + 
     geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                   position = position_dodge(width = 0.5), width = 0.2) + 
     geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  
     coord_flip() +  # Flip coordinates for better readability
     scale_color_manual(values = c("Univariate" = "blue", "Multivariate" = "red")) +
     labs(title = "Forest Plot of Prevalence Ratios",
          x = "Variables",
          y = "Prevalence Ratio (95% CI)",
          color = "Model Type") + 
     theme_minimal(base_size = 14)  
   



   models2 <- explanatory_vars %>% 
     str_c("any_disruption_after ~ ", .) %>%  
     map(~ glm.nb(as.formula(.x), data = df_viraemia)) %>%  
     map(~ tidy(.x, exponentiate = TRUE, conf.int = TRUE)) %>%  
     map2(explanatory_vars, ~ mutate(.x, variable = .y, model = "Univariate")) %>%  
     bind_rows() %>%  
     mutate(across(where(is.numeric), \(x) round(x, 2)))  %>% 
     dplyr:: filter(term != "(Intercept)")  %>% 
     select(-c(std.error,statistic,p.value))

   models2   
   ggplot(models2, aes(x = term, y = estimate)) +
     geom_point(position = position_dodge(width = 0.5), size = 3) +
     geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                   position = position_dodge(width = 0.5), width = 0.2) +
     geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
     coord_flip() +
     theme_minimal()
   

# Stratifying by sex ------------------------------------------------------

   
   
  model_nb_sex2 <- df_viraemia %>%
     group_by(sex) %>%
     nest() %>%
     mutate(
       model = map(data, ~ glm.nb(viraemia_after_dum ~ any_disruption_after + 
                                    any_disruption_b4 + age_cat + mobility + 
                                    community_type + art_duration, 
                                  data = .x)), 
       results = map(model, ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE)) 
     ) %>%
     unnest(cols = results) %>%  
     select(sex, term, estimate, conf.low, conf.high) %>%  
     mutate(across(where(is.numeric), \(x) round(x, 2)))  %>% 
     dplyr:: filter(term != "(Intercept)") 

  model_nb_sex2_fp <-  model_nb_sex2 %>% 
    mutate(term = case_when(
      #term == "any_disruption_after" ~ "ART Disruption After (Yes)",
      term == "any_disruption_b4" ~ "ART Disruption Before (Yes)",
      term == "sexMale" ~ "Sex (Male vs Female)",
      term == "age_cat<30" ~ "Age: <30 vs 40-49",
      term == "age_cat30-39" ~ "Age: 30-39 vs 40-49",
      term == "mobilityLong-term resident" ~ "Mobility (Long-term Resident)",
      term == "community_typeFishing community" ~ "Community Type (Fishing)",
      term == "art_duration2-5 years" ~ "ART Duration: 2-5 years",
      term == "art_duration>5 years" ~ "ART Duration: >5 years",
      term == "viraemia_b4_dum" ~ "Viraemia before Covid",
      TRUE ~ term
    )) %>%
    arrange(desc(estimate))
  
  
  
    ggplot(model_nb_sex2_fp, aes(x = term, y = estimate, color = sex)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  position = position_dodge(width = 0.5), width = 0.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    coord_flip() +
    labs(title = "Prevalence Ratios by Sex",
         x = "Variables",
         y = "Prevalence Ratio (95% CI)") +
    theme_minimal()
    

# Enhanced Forest Plot ----------------------------------------------------

    library(ggplot2)
    library(dplyr)
    
    # Format estimate and confidence intervals as a label
    viraemia_forest_df <- viraemia_forest_df %>%
      mutate(label = paste0(round(estimate, 2), " (", round(conf.low, 2), " - ", round(conf.high, 2), ")"))
    
    # Create the forest plot
    ggplot(viraemia_forest_df, aes(x = term, y = estimate, color = model)) +
      geom_point(position = position_dodge(width = 0.5), size = 3) +  # Adjust point size
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                    position = position_dodge(width = 0.5), width = 0.2) +  
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  # Reference line at 1.0
      geom_text(aes(label = label), 
                position = position_dodge(width = 0.5), 
                vjust = -0.5, size = 4) +  # Add prevalence ratio labels above points
      coord_flip() +  # Flip coordinates for better readability
      scale_color_manual(values = c("Univariate" = "blue", "Multivariate" = "red")) +
      labs(title = "Forest Plot of Prevalence Ratios",
           x = "Variables",
           y = "Prevalence Ratio (95% CI)",
           color = "Model Type") + 
      theme_minimal(base_size = 14) +  
      theme(
        axis.text.y = element_text(size = 12),  # Increase y-axis text size
        axis.title = element_text(size = 14),   # Increase axis label size
        legend.position = "right"               # Move legend to the right
      )

    
    
    ggplot(viraemia_forest_df, aes(x = term, y = estimate, color = model)) +
      geom_point(position = position_dodge(width = 0.5), size = 2) + 
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                    position = position_dodge(width = 0.5), width = 0.2) + 
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  
      coord_flip() +  # Flip coordinates for better readability
      scale_color_manual(values = c("Unadjusted" = "blue", "Adjusted" = "red")) +
      labs(#title = "Forest Plot of Prevalence Ratios",
           x = " ",
           y = "Prevalence Risk Ratio of Viraemia after Covid-19 ",
          color = " ") + 
      theme_minimal(base_size = 10)+
      theme(legend.position = "bottom")
    
    
    
    
    ggplot(viraemia_forest_df, aes(x = term, y = estimate, color = model)) +
      geom_point(position = position_dodge(width = 0.5), size = 2) + 
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                    position = position_dodge(width = 0.5), width = 0.2) + 
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  
      coord_flip() +  # Flip coordinates for better readability
      scale_color_manual(values = c("Unadjusted" = "blue", "Adjusted" = "red")) +
      labs(
        # title = "Forest Plot of Prevalence Ratios",
        x = " ",
        y = "Prevalence Risk Ratio of Viraemia after Covid-19",
        color = " "
      ) + 
      theme_minimal(base_size = 10) +
      theme(
        legend.position = "bottom",
        axis.text = element_text(face = "bold", size = 10),   
        axis.title = element_text(face = "bold", size = 10),   
        legend.text = element_text(face = "bold", size = 10), 
        legend.title = element_text(face = "bold", size = 10)  
      )
    
    
    
    ggplot(sex_forest_df, aes(x = term, y = estimate, color = sex)) +
      geom_point(position = position_dodge(width = 0.5), size = 3) +  
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                    position = position_dodge(width = 0.5), width = 0.2) +  
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") + 
      coord_flip() +  
      scale_y_continuous(breaks = c(1:max(sex_forest_df$conf.high)),  
                         limits = c(0, max(sex_forest_df$conf.high) + 1)) +  
      labs(
       #title = "Prevalence Ratios by Sex",
        x = "",
        y = "Prevalence Risk Ratio of Viraemia after Covid-19 By Sex"
      ) +
      theme_minimal(base_size = 12) +  
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),  
        axis.text = element_text(face = "bold", size = 10),    
        axis.title = element_text(face = "bold", size = 10),   
        legend.text = element_text(face = "bold", size = 10),  
        legend.title = element_text(face = "bold", size = 10)  
      )
    
    

# Regression Tables -------------------------------------------------------

    model_disruption_multivariate <- df_viraemia %>% 
      glm.nb(any_disruption_after ~ any_disruption_b4 + sex +
    age_cat+ mobility+community_type + art_duration + viraemia_b4_dum, data = .)     
    
model_disruption_multivariate %>% 
  tbl_regression(exponentiate = TRUE,conf.int = TRUE)

model


univariate_table <- df_viraemia %>% 
  select(all_of(dependent_var), all_of(explanatory_vars)) %>% 
  tbl_uvregression(
    method = glm.nb,  # Fit univariate Negative Binomial models
    y = art_disruption_after,  # Dependent variable
    method.args = list(),  # Additional arguments (optional)
    exponentiate = TRUE  # Convert log coefficients to prevalence risk ratios
  ) %>%
  bold_p(t = 0.05) %>%  # Bold significant p-values
  modify_header(label = "**Variable**") %>%   # Rename first column
  modify_spanning_header(everything() ~ "**Univariate Negative Binomial Regression**")  # Table title

  
c("any_disruption_b4","sex","age_cat","mobility","community_type",
  "art_duration","viraemia_b4_dum")

dependent_var <- "art_disruption_after"

df_viraemia %>% 
  select(any_disruption_b4,any_disruption_after,sex,age_cat,
         mobility,community_type,art_duration,viraemia_after_dum) %>% 
  tbl_uvregression(
    method = glm.nb,  
    y = any_disruption_after,  
    method.args = list(),  
    exponentiate = TRUE  
  ) %>%
  bold_p(t = 0.05) %>%  
  modify_header(label = "**Variable**") %>%   
  modify_spanning_header(everything() ~ "**Univariate Negative Binomial Regression**")  
  
  
df_viraemia %>% 
  select(art_disruption_b4,any_disruption_after,sex,age_cat,
         mobility,community_type,art_duration,viraemia_after_dum) %>% 
  tbl_summary( by = any_disruption_after)
  
  
model_viraemia_sex


# sex regression table ----------------------------------------------------

library(dplyr)
library(gt)   # For table formatting
library(flextable)  # For Word document formatting


publication_table <- sex_forest_df %>%
  arrange(term, sex) %>%  # Ensure Male and Female results for each term are together
  mutate(
    PRR_CI = paste0(estimate, " (", conf.low, " - ", conf.high, ")")  # Combine PRR & CI
  ) %>%
  select(term, sex, PRR_CI) %>%  # Keep only necessary columns
  pivot_wider(names_from = sex, values_from = PRR_CI)  # Spread Male and Female columns

# Print the cleaned table
publication_table


publication_table %>%
  gt() %>%
  tab_header(
    title = "Prevalence Risk Ratios by Sex",
    #subtitle = "Comparison of Univariate Negative Binomial Models"
  ) %>%
  cols_label(
    term = "Variable",
    Female = "Female PRR (95% CI)",
    Male = "Male PRR (95% CI)"
  ) %>%
  tab_options(table.font.names = "Times New Roman") %>%
  fmt_number(columns = 2:3, decimals = 2) %>%
  opt_table_lines()



df_viraemia %>% 
  select(art_disruption_b4,any_disruption_after,sex,age_cat,
         mobility,community_type,art_duration,viraemia_after_dum) %>% 
  tbl_summary( by = sex)



summary_table <- df_viraemia %>% 
  select(art_disruption_b4, art_disruption_after, sex, age_cat, 
         mobility, community_type, art_duration) %>% 
  tbl_summary(by = sex) %>%
  bold_labels() 
summary_table


dim(df_viraemia)
rakai <- read_csv("rakai_updated.csv")
dim(rakai)


dff8 <- rakai %>% 
  select(sex,artrunbc,artrunac,
         hivac,hivbc,copies,new_copies,artstrac,artstrbc) %>% 
  filter(hivac ==8|hivbc==8|artrunbc==8|artstrac==8|artstrbc==8|artrunac==8)

dff8 %>% 
  dim()



vll <- rakai %>% filter(copies != "INV.IC ",!is.na(copies),!is.na(new_copies))









