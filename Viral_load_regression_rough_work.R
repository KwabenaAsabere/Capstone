df_disruption
df_vl_supp

df_rakai_reg <- rakai %>% 
  select(ageyrs,sex,mobility,arthoac,artrunac,artstrac,
         artyrs,comm_num,artrunbc,artstrbc,hivac,hivbc,copies,new_copies) %>% 
  filter(copies != "INV.IC ",!is.na(copies),!is.na(new_copies)) %>% 
  
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
           ff_label("HIV RNA viral load, in copies/ml")) %>% 
  mutate(
    suppbc = if_else(viral_load_b4 == "Viral Load Suppression", 1, 0),
    suppac = if_else(viral_load_after == "Viral Load Suppression", 1, 0)
  ) %>% 
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
    
    hivac = if_else(hivac == 1, 1, 0) %>% 
      ff_label("Missed scheduled visit for HIV care"),
    
    hivbc = if_else(hivbc == 1, 1, 0) %>% 
      ff_label("Missed scheduled visit for HIV care"),
    
    artrunac = if_else(artrunac == 1, 1, 0) %>% 
      ff_label("Run out of ART before next refill"),
    
    artrunbc = if_else(artrunbc == 1, 1, 0) %>% 
      ff_label("Run out of ART before next refill"),
    
    artstrac = if_else(artstrac == 1, 1, 0) %>% 
      ff_label("Taken ART pills less frequently / in smaller amounts to conserve supply"),
    
    artstrbc = if_else(artstrbc == 1, 1, 0) %>% 
      ff_label("Taken ART pills less frequently / in smaller amounts to conserve supply"),
    
  ) 


  select(sex,age_cat,community_type,art_duration,mobility,
         hivac,artrunac,artstrac,hivbc,artstrbc,artrunbc) 
  
  
  
df_viraemia <-   df_rakai_reg %>% 
    select(sex,age_cat,mobility,community_type,art_duration,
           hivbc,artrunbc,artstrbc,hivac,artrunac,artstrac,viral_load_b4,
           viral_load_after,suppbc,suppac) %>% 
   mutate(any_disruption_b4 = if_else(rowSums(across(hivbc:artrunbc),na.rm = TRUE) > 0,1,0),
  any_disruption_after = if_else(rowSums(across(hivac:artstrac),na.rm = TRUE) > 0,1,0))
  
  
 library(janitor) 
  
df_viraemia %>% 
  tabyl(any_disruption_after,viral_load_after)

 df_viraemia %>% 
  mutate(
    viraemia_after_covid = if_else(viral_load_after == "Viraemia","Yes","No") %>% 
      fct_relevel("Yes") %>% 
      ff_label("Viraemia"),
    viraemia_after_dum = if_else(viral_load_after == "Viraemia",1,0) %>% 
      ff_label("Viraemia"),
    
    viraemia_b4_covid = if_else(viral_load_b4 == "Viraemia","Yes","No") %>% 
      fct_relevel("Yes") %>% 
      ff_label("Viraemia"),
    viraemia_b4_dum = if_else(viral_load_b4 == "Viraemia",1,0) %>% 
      ff_label("Viraemia"),
    
    art_disruption_b4 = if_else(any_disruption_b4 == 1,"Yes","No") %>% 
      fct_relevel("Yes") %>% 
      ff_label("Any ART Disruption"),
    
    art_disruption_after = if_else(any_disruption_after == 1,"Yes","No") %>% 
      fct_relevel("Yes") %>% 
      ff_label("Any ART Disruption")
  )
  
  
  
  

# -------------------------------------------------------------------------

df_viraemia %>% 
  tabyl(any_disruption_after,viral_load_after)  
  
  df_viraemia %>% 
    tabyl(art_disruption_after,viraemia_after_covid)

  df_viraemia %>% 
  summary_factorlist(explanatory =  art_disruption_after,dependent = viraemia_after_covid)  

  glimpse(df_viraemia)  
  
  
  "viraemia_after_covid" %in% colnames(df_viraemia)
  
  library(finalfit)
  
  df_viraemia %>% 
    summary_factorlist(
      explanatory = "art_disruption_after", 
      dependent = "viraemia_after_covid"
    )
  
  
  
  library(finalfit)
  
  # Define explanatory and dependent variables
  explanatory <- "art_disruption_after"
  dependent <- "viraemia_after_covid"
  
  # Create table with prevalence ratio and CIs
  df_viraemia %>%
    summary_factorlist(dependent, explanatory, add_dependent_label = TRUE)
   
  
  df_viraemia %>%
    tbl_cross(
      row = art_disruption_after,
      col = viraemia_after_covid,
      percent = "row"
    ) %>%
    add_stat_label()
  
  library(epiR)
  
  # Create 2x2 table
  table_2x2 <- table(
    df_viraemia$art_disruption_after,
    df_viraemia$viraemia_after_covid
  )
  
  # Calculate prevalence ratio and confidence intervals
  epi.2by2(table_2x2, method = "cohort.count", conf.level = 0.95)
  
  
  

# -------------------------------------------------------------------------

  df_viraemia %>%
    tbl_cross(
      row = art_disruption_after,
      col = viraemia_after_covid,
      percent = "row"
    ) %>%
    add_stat_label()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
