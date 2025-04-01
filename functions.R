#' Author: Jessica Handy
#' Date created: 8 FEB 2025
#' Date last modified: 1 APR 2025
#' Purpose: Defines functions needed for point estimation and bounds

# ---- Base probabilities ----
py1z1 <- function(data){
  #' Pr[Y = 1| Z = 1], i.e. probability of 
  #' suicide given post-policy implementation
  data_z1 = filter(data, z == 1)
  sum(data_z1$y)/if_else("n" %in% colnames(data_z1), 
                         sum(data_z1$n), 
                         nrow(data_z1))
}
py1z0 <- function(data){
  #' Pr[Y = 1| Z = 0], i.e. probability of 
  #' suicide given pre-policy 
  data_z0 = filter(data, z == 0)
  sum(data_z0$y)/if_else("n" %in% colnames(data_z0), 
                         sum(data_z0$n), 
                         nrow(data_z0))
}
pa1z1 <- function(data){
  #' Pr[A = 1| Z = 1], i.e. probability of being an 
  #' ERPO respondent given post-policy implementation
  data_z1 = filter(data, z == 1)
  sum(data_z1$a, na.rm = T)/if_else("n" %in% colnames(data_z1), 
                                    sum(data_z1$n), 
                                    nrow(data_z1))
}
pa0z1 <- function(data){
  #' Pr[A = 1| Z = 1], i.e. probability of not being an 
  #' ERPO respondent given post-policy implementation
  1-pa1z1(data)
}

# ---- Point identifying risk (for joint data setting) ----
py1z1a1 <- function(data){
  #' Pr[Y = 1| Z = 1, A = 1]
  data_z1a1 = filter(data, z == 1, a >= 1) 
  sum(data_z1a1$y, na.rm = T)/nrow(data_z1a1)
}
py1z1a0 <- function(data){
  #' Pr[Y = 1| Z = 1, A = 0]
  data_z1a0 = filter(data, z == 1, a == 0) 
  sum(data_z1a0$y, na.rm = T)/nrow(data_z1a0)
}

# ---- Bounds ----
lb.nt <- function(data){
  if_else(
    (min(1, 
         py1z1(data)/pa0z1(data),
         py1z0(data)/pa0z1(data))) < 
      (max(0,
           (py1z1(data)-pa1z1(data)) / pa0z1(data), 
           (py1z0(data)-pa1z1(data)) / pa0z1(data)))
    # error where min(a) = 1 < max(b) = 1 resolved by checking that values are not equal 
    & !isTRUE(all.equal((min(1, 
                      py1z1(data)/pa0z1(data),
                      py1z0(data)/pa0z1(data))),
                 (max(0,
                      (py1z1(data)-pa1z1(data)) / pa0z1(data), 
                      (py1z0(data)-pa1z1(data)) / pa0z1(data))))), 
    NA, 
    max(0,
      (py1z1(data)-pa1z1(data)) / pa0z1(data), 
      (py1z0(data)-pa1z1(data)) / pa0z1(data)))
  }
ub.nt <- function(data){
  if_else(
    (min(1, 
      py1z1(data)/pa0z1(data),
      py1z0(data)/pa0z1(data))) < 
    (max(0,
         (py1z1(data)-pa1z1(data)) / pa0z1(data), 
         (py1z0(data)-pa1z1(data)) / pa0z1(data)))
    # error where min(a) = 1 < max(b) = 1 resolved by checking that values are not equal 
    & !isTRUE(all.equal((min(1, 
                      py1z1(data)/pa0z1(data),
                      py1z0(data)/pa0z1(data))),
                 (max(0,
                      (py1z1(data)-pa1z1(data)) / pa0z1(data), 
                      (py1z0(data)-pa1z1(data)) / pa0z1(data))))), 
    NA, 
    min(1, 
        py1z1(data)/pa0z1(data),
        py1z0(data)/pa0z1(data)))
} 

lb.co <- function(data){
  if_else(
    (min(1, 
         py1z1(data)/pa1z1(data),
         ((py1z1(data)-py1z0(data))/pa1z1(data))+1)) < 
      (max(0,
           (py1z1(data)-pa0z1(data)) / pa1z1(data), 
           (py1z1(data)-py1z0(data)) / pa1z1(data)))
    # error where min(a) = 1 < max(b) = 1 resolved by checking that values are not equal 
    & !isTRUE(all.equal((min(1, 
                      py1z1(data)/pa1z1(data),
                      ((py1z1(data)-py1z0(data))/pa1z1(data))+1)),
                 (max(0,
                      (py1z1(data)-pa0z1(data)) / pa1z1(data), 
                      (py1z1(data)-py1z0(data)) / pa1z1(data))))),
    NA, 
    (max(0,
         (py1z1(data)-pa0z1(data)) / pa1z1(data), 
         (py1z1(data)-py1z0(data)) / pa1z1(data))))
}
ub.co <- function(data){
  if_else(
    (min(1, 
         py1z1(data)/pa1z1(data),
         ((py1z1(data)-py1z0(data))/pa1z1(data))+1)) < 
      (max(0,
           (py1z1(data)-pa0z1(data)) / pa1z1(data), 
           (py1z1(data)-py1z0(data)) / pa1z1(data)))
    # error where min(a) = 1 < max(b) = 1 resolved by checking that values are not equal 
    & !isTRUE(all.equal((min(1, 
                      py1z1(data)/pa1z1(data),
                      ((py1z1(data)-py1z0(data))/pa1z1(data))+1)),
                 (max(0,
                      (py1z1(data)-pa0z1(data)) / pa1z1(data), 
                      (py1z1(data)-py1z0(data)) / pa1z1(data))))),
    NA, 
    (min(1, 
         py1z1(data)/pa1z1(data),
         ((py1z1(data)-py1z0(data))/pa1z1(data))+1)))
} 

lb <- function(data){
  if_else(
    (min(1, 
         py1z0(data)/pa1z1(data),
         ((py1z0(data)-py1z1(data))/pa1z1(data))+1)) < 
      (max(0,
           (py1z0(data)-pa0z1(data)) / pa1z1(data), 
           (py1z0(data)-py1z1(data)) / pa1z1(data)))
    # error where min(a) = 1 < max(b) = 1 resolved by checking that values are not equal 
    & !isTRUE(all.equal((min(1, 
                      py1z0(data)/pa1z1(data),
                      ((py1z0(data)-py1z1(data))/pa1z1(data))+1)),
                 (max(0,
                      (py1z0(data)-pa0z1(data)) / pa1z1(data), 
                      (py1z0(data)-py1z1(data)) / pa1z1(data))))),
    NA, 
    max(0,
        (py1z0(data)-pa0z1(data)) / pa1z1(data), 
        (py1z0(data)-py1z1(data)) / pa1z1(data)))
}
ub <- function(data){
  if_else(
    (min(1, 
         py1z0(data)/pa1z1(data),
         ((py1z0(data)-py1z1(data))/pa1z1(data))+1)) < 
      (max(0,
           (py1z0(data)-pa0z1(data)) / pa1z1(data), 
           (py1z0(data)-py1z1(data)) / pa1z1(data)))
    # error where min(a) = 1 < max(b) = 1 resolved by checking that values are not equal
    & !isTRUE(all.equal((min(1, 
                      py1z0(data)/pa1z1(data),
                      ((py1z0(data)-py1z1(data))/pa1z1(data))+1)),
                 (max(0,
                      (py1z0(data)-pa0z1(data)) / pa1z1(data), 
                      (py1z0(data)-py1z1(data)) / pa1z1(data))))), 
    NA, 
    min(1, 
        py1z0(data)/pa1z1(data),
        ((py1z0(data)-py1z1(data))/pa1z1(data))+1))
} 

eff <- function(data){
  if_else(pa1z1(data)>0,
          (py1z1(data)-py1z0(data))/pa1z1(data),
          NA)
}

t.lb <- function(data){
  lb(data)/ub.nt(data)
}

t.ub <- function(data){
  ub(data)/lb.nt(data)
}
