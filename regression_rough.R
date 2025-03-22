glimpse(df_viraemia)

library(sandwich)
library(lmtest)

dependent = "any_disruption_after"
explanatory = "viraemia_after_dum"
model_vl <-  glm(viraemia_after_dum ~ any_disruption_after,
                                family = poisson(link = "log"), data = df_viraemia)

model_robust <- coeftest(model_vl, vcov = vcovHC(model_vl, type = "HC0"))

robust_se <- sandwich::vcovHC(pr_model, type = "HC0")
pr_model_robust <- lmtest::coeftest(pr_model, vcov = robust_se)

# Extract prevalence ratio and confidence intervals
pr_table <- broom::tidy(pr_model, exponentiate = TRUE, conf.int = TRUE)

library(broom)

robust_se
pr_model_robust %>% tidy()
pr_table
tidy(pr_model)


tbl_regression(pr_model,exponentiate = TRUE,conf.int = TRUE)

diag(robust_se)



library(MASS)

model_nb <- glm.nb(viraemia_after_dum ~ any_disruption_after,data = df_viraemia)

summary(model_nb)
tidy(model_nb,exponentiate = TRUE,conf.int = TRUE)

tbl_regression(model_nb,exponentiate = TRUE,conf.int = TRUE)
table(df_viraemia$art_disruption_after)

cross_table <- df_viraemia %>%
  tbl_cross(
    row = viraemia_after_dum,  # Explanatory variable
    col = any_disruption_after,  # Outcome variable
    percent = "row",
    margin = NULL  # Suppress row and column totals
  )

cross_table


df_viraemia %>%
  tbl_cross(
    row = viraemia_after_covid,  # Explanatory variable
    col = art_disruption_after,  # Outcome variable
    percent = "row",
    margin = NULL  # Suppress row and column totals
  )





#tbl_regression(model_nb,exponentiate = TRUE,conf.int = TRUE)
tidy(model_nb,exponentiate = TRUE,conf.int = TRUE)
reg_results <- tidy(model_nb,exponentiate = TRUE,conf.int = TRUE)
 
reg_results
colnames(reg_results)

regression_summary <-  reg_results %>% 
  select(term,estimate,conf.low,conf.high) %>%   
  rename(PRR = estimate,
         CI_lower = conf.low,
         CI_upper = conf.high)

regression_summary %>% gt()

cross_table <-  tbl_cross(
  df_viraemia,
  row = viraemia_after_covid,
  col = art_disruption_after,
  percent = "row",
  margin = NULL
) %>%
  as_tibble()

# Rename columns in cross_table
cross_table <- cross_table %>%
  rename(
    RowType = 1,  # First column for row labels
    Yes = 2,      # Second column for "Yes" data
    No = 3        # Third column for "No" data
  )


cross_table


tbl_cross(
  df_viraemia,
  col = viraemia_after_covid,
  row = art_disruption_after,
  percent = "row",
  margin = NULL
)


# Extract the counts from your table
a <- 28   # ART disruption = Yes, Viraemia = Yes
b <- 107  # ART disruption = No, Viraemia = Yes
c <- 347  # ART disruption = Yes, Viraemia = No
d <- 2303 # ART disruption = No, Viraemia = No

# Calculate PRR
prr <- (a / (a + b)) / (c / (c + d))
prr_ci <- exp(c(log(prr) - 1.96 * sqrt((1/a - 1/(a + b)) + (1/c - 1/(c + d))),
                log(prr) + 1.96 * sqrt((1/a - 1/(a + b)) + (1/c - 1/(c + d)))))



library(tidyverse)
library(gt)

# Create the cross table
cross_table <- tbl_cross(
  df_viraemia,
  col = viraemia_after_covid,
  row = art_disruption_after,
  percent = "row",
  margin = NULL
) %>%
  as_tibble()
colnames(cross_table) <- c("Viraemia", "Yes", "No")


cross_table

# Add PRR and confidence intervals
# Add PRR and confidence intervals
cross_table <- cross_table %>%
  mutate(
    PRR = case_when(
      Viraemia == "Yes" ~ sprintf("%.2f (%.2f, %.2f)", prr, prr_ci[1], prr_ci[2]),
      TRUE ~ NA_character_
    )
  )


# Format the table using gt
custom_table <- cross_table %>%
  gt() %>%
  tab_header(title = "Cross Table with Prevalence Risk Ratio (PRR)")

custom_table



tbl_cross(
  df_viraemia,
  col = viraemia_after_covid,
  row = art_disruption_after,
  percent = "row",
  margin = NULL
)


prr_table_full <- df_viraemia %>% 
  select(art_disruption_b4,sex,age_cat,mobility,community_type,
         art_duration,viraemia_after_covid,art_disruption_after) %>% 
  
  tbl_summary(
  by = viraemia_after_covid,
  percent = "row",
  include = c(art_disruption_after,art_disruption_b4,sex,age_cat,mobility,community_type,
              art_duration,viraemia_after_covid)
) %>% 
modify_spanning_header(
  all_stat_cols() ~ "**Viraemia After COVID**" # Add a header for the `by` variable
)

prr_table_full






tbl_ureg <- df_viraemia %>% 
  select(art_disruption_after,sex,age_cat,mobility,community_type,
         art_duration,viraemia_after_dum,art_disruption_b4) %>% 
tbl_uvregression(
  method = glm.nb,
  y = viraemia_after_dum,
 # x = everything(),
  method.args = list(), 
  exponentiate = TRUE
)

tbl_ureg <- tbl_ureg %>%
  modify_column_hide(columns = c("N", "p.value","N_obs"))

tbl_ureg



tbl_combined <- tbl_merge(
  tbls = list(prr_table_full, tbl_ureg), # Tables to combine
  tab_spanner = c("**Viraemia After COVID**", "**Prevalence Risk Ratios**") # Headers for each table
) 
# Display the combined table
tbl_combined



table(df_viraemia$community_type,df_viraemia$viraemia_after_covid)
prop.table(table(df_viraemia$community_type,df_viraemia$viraemia_after_covid),margin = 1)


# Create flextable
ft <- flextable(table_data) %>%
  set_header_labels(
    Characteristic = "Characteristic",
    No = "No (n = 2,650)",
    Yes = "Yes (n = 135)",
    PRR = "PRR",
    CI = "95% CI"
  ) %>%
  theme_vanilla() %>% # Apply a simple theme
  autofit()

# Export to Word
library(officer)
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "Viraemia_After_COVID_Table.docx")


library(flextable)
ft_combined <- as_flex_table(tbl_combined)
ft_combined

ft_combined <- ft_combined %>%
  theme_vanilla() %>%                     # Apply a vanilla theme
  set_table_properties(width = 1.0) %>%   # Adjust table width
  autofit() 
ft_combined

library(officer)

# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(value = ft_combined) %>%
  body_add_par(value = "Table 1: Combined Summary and Regression Results", style = "heading 1")

# Save the Word document
print(doc, target = "Combined_Table_Output.docx")


table(df_viraemia$any_disruption_b4,df_viraemia$viraemia_after_covid)
prop.table(table(df_viraemia$any_disruption_b4,df_viraemia$viraemia_after_covid)
           ,margin =1 )






































































