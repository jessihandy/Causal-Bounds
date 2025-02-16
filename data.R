#' Date created: 7 FEB 2025
#' Date last modified: 15 FEB 2025
#' Purpose: simulates a fake dataset to provide a practice framework
#' for the related Bounding script for practitioners. 

#' SPECIFY DATA-TYPE: 
#' Un-comment appropriate data set type by removing the appropriate "#" comment symbol
# dt = "joint"
# dt = "nonjoint"

#' Notation:
#'  z = Instrumental variables, e.g., time pre-/post-policy implementation
#'  a = Treatment/exposure variable, e.g., being an ERPO respondent (1) or not (0)
#'  y = Outcome, e.g., suicide death (1) or not (0)
#'  l = Level, or stratification veriable, e.g. demographic variables, county, etc. 

# Loading necessary packages ----
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(
  tidyverse     # data management/syntax
)

set.seed(1245) # setting the seed keeps the "randomization" consistent so results are reproducible

if (dt == "joint") {
  #' Generating fake data in a joint datasetting
  #' i.e., targeted policy linked to death records 
  data <- data.frame(
    z = rbinom(3000000, 1, 0.5), # A random sequence of 3M records with a 50% chance of being 0 or 1
    y = rbinom(3000000, 1, 0.000142), # Similar to above but matching on 2022 US suicide prevalence as probability of a 1
    a = rbinom(3000000, 1, 0.000069), # Using ERPO rate per 100k population in CA 2023 reported by Everytown
    l = rbinom(3000000, 5, 0.15) # arbitrary level variable
  )
  data$a <- if_else(data$z == 0, 0, data$a) # if z = 0, overwrite a to 1 (an ERPO would be illogical in a pre-policy era)
} else if (dt == "nonjoint") {
  # Generating fake data in a non-joint datasetting 
    # as such, base data and ERPO data come from separate, non linkable sources
  data_base <- data.frame(
    z = rbinom(3000000, 1, 0.5), # A random sequence of 1M records with a 50% chance of being 0 or 1
    y = rbinom(3000000, 1, 0.000142), # Similar to above but matching on 2022 US suicide prevalence as probability of a 1
    l = rbinom(3000000, 5, 0.15) # arbitrary level variable 
  )
  data_erpo <- data.frame(
    a = rbinom(3000000, 1, 0.000069), # Using ERPO rate per 100k population in CA 2023 reported by Everytown
    l = rbinom(3000000, 5, 0.15) # arbitrary level variable 
  )
  
  #' In a non-joint data setting, treatment/exposure data may be provided as line-items with no identifiers (like above) or 
  #' possibly pre-grouped by some level (e.g., year, demographics, etc.). To join the data to our base table, we will need to
  #' create that grouped dataset. In your own work, you may need to group by multiple l's, i.e. if you're comparing multiple 
  #' geographies across multiple years. 
  
  data_erpo <- data_erpo %>% 
    group_by(l, z = 1) %>%
    summarise(a = sum(a))
  # We will have to do this grouping for the base table as well.
  data <- data_base %>%
    group_by(z, l) %>%
    summarize(y = sum(y), 
              n = n())
  data <- data %>%
    left_join(data_erpo) %>%
    replace(is.na(.), 0)
  rm(data_base, data_erpo)
} else {
  print("Specify data-type")
}


