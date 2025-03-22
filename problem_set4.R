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


biostat <- read_csv("biostat3.csv")
biostat %>% head()

model1 <- biostat %>% 
  glm(D ~ gender + cursmoke + diabetes + bpmeds + bmicat + agecat + tbin,
      offset = log(Y), 
      family = poisson(link = "log"),
      data = .)

adjusted_model <- model1 %>% 
  tidy(exponentiate = TRUE,conf.int = TRUE)
adjusted_model

summary(model1)
exp(coef(model1))
confint(model1)


dependent_variable <- "D"
explanatory_variable <- c("gender","cursmoke","diabetes","bpmeds","bmicat",
                          "agecat","tbin")

model2 <- explanatory_variable %>% 
  str_c(" D ~ ",.) %>% 
  map(
    .f = ~glm(
      formula = as.formula(.x),
      offset = log(Y),
      family = poisson(link = "log"),
      data = biostat)
    ) %>% 
  map(
    .f = ~tidy(.x, exponentiate = TRUE, conf.int = TRUE)
      )%>% 
  bind_rows()%>% 
  mutate(across(where(is.numeric), \(x) round(x,2)))
  

model2
table(biostat$agecat)
glimpse(biostat)

biostat <- biostat %>% 
  mutate(across(bmicat:agecat, as_factor))


adjusted_model %>% 
  select(term,estimate,conf.low,conf.high)

model2  %>% 
  select(term,estimate,conf.low,conf.high)



adjusted_model_poisson <- adjusted_model


nb_model <- biostat %>% 
  glm.nb(D ~ gender + cursmoke + diabetes + bpmeds + bmicat + agecat +
           tbin + offset(log(Y)),
      data = .)


adjusted_model_nb = nb_model%>% 
  tidy(exponentiate = TRUE,conf.int = TRUE)
  


adjusted_model_nb <-  adjusted_model_nb%>% 
  select(term,estimate,conf.low,conf.high) %>% 
  mutate(model = "Negative Binomial")

adjusted_model_poisson <-  adjusted_model_poisson%>% 
  select(term,estimate,conf.low,conf.high) %>% 
  mutate(model = "Poisson")

adjusted_model_nb
adjusted_model_poisson

combined_models <-  bind_rows(adjusted_model_nb,adjusted_model_poisson)
combined_models




ggplot(combined_models, aes(x = term, y = estimate, color = model)) +
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
    y = "Incidence Rate Ratio",
    color = ""
  ) +
  theme_minimal(base_size = 12) +  
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),  
    axis.text = element_text(face = "bold", size = 10),    
    axis.title = element_text(face = "bold", size = 10),   
    legend.text = element_text(face = "bold", size = 10),  
    legend.title = element_text(face = "bold", size = 10)  
  )


saveRDS(biostat,"framingham.rds")
biostat <- read_rds("framingham.rds")
glimpse(fram)
glimpse(biostat)




