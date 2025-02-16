#' Created: 8 FEB 2025
#' Last modified: 15 FEB 2025
#' Purpose: Provides template code for bounding causal risks

# Data management ----
## Run source code for data and functions ----
# These source statements assume the related scripts are within your existing working directory
# adjust the file path accordinly if they live elsewhere
source("functions.r") 
source("data.r") # if code fails, you will need to specify data type, i.e. dt = "joint" or "nonjoint"

#' Recall our notation:
#'  z = Instrumental variables, e.g., time pre-/post-policy implementation
#'  a = Treatment/exposure variable, e.g., being an ERPO respondent (1) or not (0)
#'  y = Outcome, e.g., suicide death (1) or not (0)
#'  l = Level, or stratification veriable, e.g. demographic variables, county, etc. 


# Generating estimands -----
## Joint code ----
if (dt == "joint") {
  # Suicide risk among handgun owners who were not ERPO respondents
  # P[Y = 1 | Z = 1, A = 0]
  risk_z1a0 <- py1z1a0(data)
  
  # Suicide risk among handgun owners who were ERPO respondents
  # P[Y = 1|Z = 1, A = 1]
  risk_z1a1 <- py1z1a1(data)
  
  # Suicide risk among handgun owners who were ERPO respondents had they, 
  # counter to the fact, not recieved an ERPO
  # P[Y^a=0 = 1|Z = 1, A = 1]
  lb_e <- lb(data)
  ub_e <- ub(data)
  
  # effectiveness
  eff_RD <- eff(data)
  
  # risk targeting 
  t_lb <- t.lb(data)
  t_ub <- t.ub(data)
  
  l <- "all-levels"
  a <- sum(data$a, na.rm = T)
  
  ### Re-run by "L" ----
  # Adjust the following variable list to include/exclude others for export
  df <- data.frame(l, # level
                   a, # count of exposures, e.g. ERPOs
                   risk_z1a0, # point-identified suicide risk among unexposed
                   risk_z1a1, # point-identified suicide risk among exposed
                   lb_e, ub_e, # bounds of counterfactual (e.g. ERPO respondents, had they not been)
                   eff_RD, # point-identified effectiveness 
                   t_lb, t_ub) # risk-targeting bounds
  
  #' if you only want the global bounds, or do not have power to do a sub analysis, you can ignore the following code
  list <- data %>% filter(a >= 1) %>% # only loop over counties with at least one ERPO in current view
    pull(l) %>% unique() 
  
  for (i in list) {
    data_i <- data %>% filter(l == i) 
    if (sum(data_i$a, na.rm = T) == 0) {next} # if no ERPOs, skip 
    dataz0 <- data_i %>% filter(z == 0, l == i) 
    dataz1 <- data_i %>% filter(z == 1, l == i) 
    if (sum(subset(dataz0)$y) == 0) {next} # if no suicides, skip 
    # Suicide risk among handgun owners who were not ERPO respondents
    # P[Y = 1 | Z = 1, A = 0]
    risk_z1a0 <- py1z1a0(data_i)
    # Suicide risk among handgun owners who were ERPO respondents
    # P[Y = 1|Z = 1, A = 1]
    risk_z1a1 <- py1z1a1(data_i)
    # Suicide risk among handgun owners who were ERPO respondents had they, 
    # counter to the fact, not recieved an ERPO
    # P[Y^a=0 = 1|Z = 1, A = 1]
    lb_e <- lb(data_i)
    # Defining upper bounds
    ub_e <- ub(data_i)
    # effectiveness
    eff_RD <- eff(data_i)
    # risk targeting 
    t_lb <- t.lb(data_i)
    t_ub <- t.ub(data_i)
    l <- max(data_i$l, na.rm = T)
    a <- sum(data_i$a, na.rm = T)
    if (is.na(t_lb)) {print(paste0("Bounds cannot be calculated for: ",max(data_i$l, na.rm = T)));
      next}
    output <- data.frame(l, a, risk_z1a0, risk_z1a1, lb_e, ub_e, 
                         eff_RD, t_lb, t_ub)
    df <- distinct(rbind(df, output))
  }
  
  write.csv(df, "output.csv")
  print("Bounds for joint data setting generated")
} else if (dt == "nonjoint") {
## Non-join code ----
  # Suicide risk among handgun owners who were not ERPO respondents
  # P[Y = 1 | Z = 1, A = 0]
  lb_nt <- lb.nt(data)
  ub_nt <- ub.nt(data)
  
  # Suicide risk among handgun owners who were ERPO respondents
  # P[Y = 1|Z = 1, A = 1]
  lb_co <- lb.co(data)
  ub_co <- ub.co(data)
  
  # Suicide risk among handgun owners who were ERPO respondents had they, 
  # counter to the fact, not recieved an ERPO
  # P[Y^a=0 = 1|Z = 1, A = 1]
  lb_e <- lb(data)
  ub_e <- ub(data)
  
  # effectiveness
  eff_RD <- eff(data)
  
  # risk targeting 
  t_lb <- t.lb(data)
  t_ub <- t.ub(data)
  
  l <- "all-levels"
  a <- sum(data$a, na.rm = T)
  
  ### Re-run by "L" ----
  # Adjust the following variable list to include/exclude others for export
  df <- data.frame(l, # level
                   a, # count of exposures, e.g. ERPOs
                   lb_nt, ub_nt, # bounds of causal risk among those never treated, e.g. no ERPO
                   lb_co, ub_co, # bounds of causal risk among "compliers", e.g. ERPO recipient
                   lb_e, ub_e, # bounds of counterfactual (e.g. ERPO respondents, had they not been)
                   eff_RD, # point-identified effectiveness 
                   t_lb, t_ub) # risk-targeting bounds
  
  #' if you only want the global bounds, or do not have power to do a sub analysis, you can ignore the following code
  list <- data %>% filter(a >= 1) %>% # only loop over counties with at least one ERPO in current view
    pull(l) %>% unique() 
  
  for (i in list) {
    data_i <- data %>% filter(l == i) 
    if (sum(data_i$a, na.rm = T) == 0) {next} # if no ERPOs, skip 
    dataz0 <- data_i %>% filter(z == 0, l == i) 
    dataz1 <- data_i %>% filter(z == 1, l == i) 
    if (sum(subset(dataz0)$y) == 0) {next} # if no suicides, skip 
    # Suicide risk among handgun owners who were not ERPO respondents
    # P[Y = 1 | Z = 1, A = 0]
    lb_nt <- lb.nt(data_i)
    ub_nt <- ub.nt(data_i) 
    # Suicide risk among handgun owners who were ERPO respondents
    # P[Y = 1|Z = 1, A = 1]
    lb_co <- lb.co(data_i)
    ub_co <- ub.co(data_i)
    # Suicide risk among handgun owners who were ERPO respondents had they, 
    # counter to the fact, not recieved an ERPO
    # P[Y^a=0 = 1|Z = 1, A = 1]
    lb_e <- lb(data_i)
    # Defining upper bounds
    ub_e <- ub(data_i)
    # effectiveness
    eff_RD <- eff(data_i)
    # risk targeting 
    t_lb <- t.lb(data_i)
    t_ub <- t.ub(data_i)
    l <- max(data_i$l, na.rm = T)
    a <- sum(data_i$a, na.rm = T)
    if (is.na(t_lb)) {print(paste0("Bounds cannot be calculated for: ",max(data_i$l, na.rm = T)));
      next}
    output <- data.frame(l, a, lb_nt, ub_nt, lb_co, ub_co, lb_e, ub_e, 
                         eff_RD, t_lb, t_ub)
    df <- distinct(rbind(df, output))
  }
  print("Bounds for non-joint data setting generated")
  write.csv(df, "output.csv")
} else {
  print("Error: specify data type")
}


