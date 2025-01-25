#regression_rough2
library(epiR)

# Create 2x2 table
table_2x2 <- table(
  df_viraemia$art_disruption_after,
  df_viraemia$viraemia_after_covid
)

# Calculate prevalence ratio and confidence intervals
epi.2by2(table_2x2, method = "cohort.count", conf.level = 0.95)

# Fit Poisson regression model with robust standard errors
pr_model <- glm(
  any_disruption_after ~ viraemia_after_dum,
  data = df_viraemia,
  family = poisson(link = "log")
)

# Calculate robust standard errors
robust_se <- sandwich::vcovHC(pr_model, type = "HC0")
pr_model_robust <- lmtest::coeftest(pr_model, vcov = robust_se)

# Extract prevalence ratio and confidence intervals
pr_table <- broom::tidy(pr_model, exponentiate = TRUE, conf.int = TRUE)



cross_table <- df_viraemia %>%
  tbl_cross(
    row = viraemia_after_dum,  # Explanatory variable
    col = any_disruption_after,  # Outcome variable
    percent = "row",
    margin = NULL  # Suppress row and column totals
  ) %>%
  add_stat_label() %>%
  modify_table_body(
    ~ .x %>%
      # Add prevalence ratios (PR) and CIs as columns
      mutate(
        label = case_when(
          label == "0" ~ "No",  # Recode 0 as No
          label == "1" ~ "Yes",  # Recode 1 as Yes
          TRUE ~ label
        ),
        PR = case_when(
          label == "Yes" ~ sprintf("%.2f", pr_table$estimate[2]),  # PR for viraemia_after_dum = 1 (Yes)
          TRUE ~ NA_character_
        ),
        CI = case_when(
          label == "Yes" ~ sprintf("(%.2f–%.2f)", pr_table$conf.low[2], pr_table$conf.high[2]),
          TRUE ~ NA_character_
        )
      )
  ) %>%
  modify_header(
    stat_1 ~ "**No**",  # Rename the first column to No
    stat_2 ~ "**Yes**",  # Rename the second column to Yes
    PR ~ "**Prevalence Ratio**",
    CI ~ "**95% Confidence Interval**"
  ) %>%
  modify_caption("**Cross-Tabulation with Prevalence Ratios and Confidence Intervals**")


# View final table
cross_table


install.packages("conflicted")

model_nb <- glm.nb(viraemia_after_dum ~ any_disruption_after,data = df_viraemia)

summary(model_nb)
tidy(model_nb,exponentiate = TRUE,conf.int = TRUE)

tbl_regression(model_nb,exponentiate = TRUE,conf.int = TRUE)
table(df_viraemia$art_disruption_after)











