
library (plyr)
library(tidyverse)
library(ggplot2)
library(gtools)
library(latex2exp)
library(ggpubr)
library(parallel)
library(matrixStats)


#####################
pr_linear <- function(N, A, B, C, D){
  
  i_a <- 3: N
  i_b <- 2: (N-1)
  i_c <- 1: (N-2)
  i_d <- 0: (N-3)
  
  a = A
  b = B
  c = C
  d = D
  
  # step one: simulate every item in the unfolded formula 
  allis <-  expand.grid(C_a = i_a, C_b = i_b,  C_c = i_c, C_d = i_d) %>% 
    filter(C_a > C_b & C_b > C_c & C_c > C_d)
  
  # step two: lnf(i,d)f(s,c)f(t,b)f(z, a) = lnf(i,d) + lnf(s,c) + lnf(t,b) + lnf(z,a), N >= z > t > s > i >= 0
  
  allis <- allis %>% 
    mutate(
      ln_pr_C_a = dbinom(C_a, N, a, log = TRUE),
      ln_pr_C_b = dbinom(C_b, N, b, log = TRUE),
      ln_pr_C_c = dbinom(C_c, N, c, log = TRUE),
      ln_pr_C_d = dbinom(C_d, N, d, log = TRUE),
      LSE_entry = ln_pr_C_a + ln_pr_C_b + ln_pr_C_c + ln_pr_C_d)
  
  
  # step two: construct lx
  lx <- allis$LSE_entry
  
  # step one: calculate ln(P(R1))
  output <- logSumExp(lx)
  output <- exp(output)
  return(output)
}

#########################
pr_one_pair_case_1 <- function(N, A, B, C, D){
  
  i_ab <- 2:N
  i_c <- 1: (N-1)
  i_d <- 0: (N-2)
  a = A
  b = B
  c = C
  d = D
  
  
  # step one: simulate all possibilities/ every item in the unfolded formula
  allis <- expand.grid(C_ab = i_ab, C_c = i_c, C_d = i_d) %>% 
    filter(C_ab > C_c & C_c > C_d)
  
  # step two: lnf(i,d)f(I,c)f(I,b)f(H, a) = lnf(i,d) + lnf(I,c) + lnf(I,b) + lnf(H,a), N >= H > I > i >= 0
  
  allis <- allis %>% 
    mutate(
      ln_pr_C_a = dbinom(C_ab, N, a, log = TRUE),
      ln_pr_C_b = dbinom(C_ab, N, b, log = TRUE),
      ln_pr_C_c = dbinom(C_c, N, c, log = TRUE),
      ln_pr_C_d = dbinom(C_d, N, d, log = TRUE),
      LSE_entry = ln_pr_C_a + ln_pr_C_b + ln_pr_C_c + ln_pr_C_d)
  
  # step two: construct lx
  lx <- allis$LSE_entry
  
  # step one: calculate ln(P(R1))
  output <- logSumExp(lx)
  output <- exp(output)
  
  return(output)
}

#######################
pr_one_pair_case_2 <- function(N, A, B, C, D){
  
  i_a <- 2:N
  i_bc <- 1: (N-1)
  i_d <- 0: (N-2)
  a = A
  b = B
  c = C
  d = D
  
  # step one: simulate all possibilities/ every item in the unfolded formula
  allis <- expand.grid(C_a = i_a, C_bc = i_bc, C_d = i_d) %>% 
    filter(C_a > C_bc & C_bc > C_d)
  
  # step two: lnf(i,d)f(I,c)f(I,b)f(H, a) = lnf(i,d) + lnf(I,c) + lnf(I,b) + lnf(H,a), N >= H > I > i >= 0
  
  allis <- allis %>% 
    mutate(
      ln_pr_C_a = dbinom(C_a, N, a, log = TRUE),
      ln_pr_C_b = dbinom(C_bc, N, b, log = TRUE),
      ln_pr_C_c = dbinom(C_bc, N, c,  log = TRUE),
      ln_pr_C_d = dbinom(C_d, N, d, log = TRUE),
      LSE_entry = ln_pr_C_a + ln_pr_C_b + ln_pr_C_c + ln_pr_C_d)
  
  # step two: construct lx
  lx <- allis$LSE_entry
  
  # step one: calculate ln(P(R1))
  output <- logSumExp(lx)
  output <- exp(output)
  
  return(output)
}

###########################
pr_one_pair_case_3 <- function(N, A, B, C, D){
  
  i_a <- 2:N
  i_b <- 1: (N-1)
  i_cd <- 0: (N-2)
  a = A
  b = B
  c = C
  d = D
  
  # step one: simulate all possibilities/ every item in the unfolded formula
  allis <- expand.grid(C_a = i_a, C_b = i_b, C_cd = i_cd) %>% 
    filter(C_a > C_b & C_b > C_cd)
  
  # step two: lnf(i,d)f(I,c)f(I,b)f(H, a) = lnf(i,d) + lnf(I,c) + lnf(I,b) + lnf(H,a), N >= H > I > i >= 0
  
  allis <- allis %>% 
    mutate(
      ln_pr_C_a = dbinom(C_a, N, a, log = TRUE),
      ln_pr_C_b = dbinom(C_b, N, b, log = TRUE),
      ln_pr_C_c = dbinom(C_cd, N, c,  log = TRUE),
      ln_pr_C_d = dbinom(C_cd, N, d, log = TRUE),
      LSE_entry = ln_pr_C_a + ln_pr_C_b + ln_pr_C_c + ln_pr_C_d)
  
  # step two: construct lx
  lx <- allis$LSE_entry
  
  # step one: calculate ln(P(R1))
  output <- logSumExp(lx)
  output <- exp(output)
  
  return(output)
}

###########################
pr_three_tied_case1 <- function(N, A, B, C, D){
  
  i_abc <- 1:N
  i_d <- 0: (N-1)
  a = A
  b = B
  c = C
  d = D
  
  # step one: simulate all possibilities/ every item in the unfolded formula
  allis <- expand.grid(C_abc = i_abc, C_d = i_d) %>% 
    filter(C_abc > C_d)
  
  # step two: lnf(i,d)f(I,c)f(I,b)f(H, a) = lnf(i,d) + lnf(I,c) + lnf(I,b) + lnf(H,a), N >= H > I > i >= 0
  
  allis <- allis %>% 
    mutate(
      ln_pr_C_a = dbinom(C_abc, N, a, log = TRUE),
      ln_pr_C_b = dbinom(C_abc, N, b, log = TRUE),
      ln_pr_C_c = dbinom(C_abc, N, c,  log = TRUE),
      ln_pr_C_d = dbinom(C_d, N, d, log = TRUE),
      LSE_entry = ln_pr_C_a + ln_pr_C_b + ln_pr_C_c + ln_pr_C_d)
  
  # step two: construct lx
  lx <- allis$LSE_entry
  
  # step one: calculate ln(P(R1))
  output <- logSumExp(lx)
  output <- exp(output)
  
  return(output)
}

##########################
pr_two_pairs<- function(N, A, B, C, D){
  
  i_ab <- 1:N
  i_cd <- 0: (N-1)
  a = A
  b = B
  c = C
  d = D
  
  allis <- expand.grid(C_ab = i_ab, C_cd = i_cd) %>% 
    filter(C_ab > C_cd)
  
  
  # step one: calculate lnf(i,d)f(i,c)f(I,a)f(I,b) = lnf(i,d) + lnf(i,c) + lnf(I,a) + lnf(I,b)
  allis <- allis %>% 
    mutate(
      ln_pr_C_a = dbinom(C_ab, N, a, log = TRUE),
      ln_pr_C_b = dbinom(C_ab, N, b, log = TRUE),
      ln_pr_C_c = dbinom(C_cd, N, c,  log = TRUE),
      ln_pr_C_d = dbinom(C_cd, N, d, log = TRUE),
      LSE_entry = ln_pr_C_a + ln_pr_C_b + ln_pr_C_c + ln_pr_C_d)
  
  # step two: construct lx
  lx <- allis$LSE_entry
  
  # step one: calculate ln(P(R1))
  output <- logSumExp(lx)
  output <- exp(output)
  
  return(output)
}

##########################
pr_three_tied_case2 <- function(N, A, B, C, D){
  
  i_a <- 1:N
  i_bcd <- 0: (N-1)
  a = A
  b = B
  c = C
  d = D
  
  # step one: simulate all possibilities/ every item in the unfolded formula
  allis <- expand.grid(C_a = i_a, C_bcd = i_bcd) %>% 
    filter(C_a > C_bcd)
  
  # step two: lnf(i,d)f(I,c)f(I,b)f(H, a) = lnf(i,d) + lnf(I,c) + lnf(I,b) + lnf(H,a), N >= H > I > i >= 0
  
  allis <- allis %>% 
    mutate(
      ln_pr_C_a = dbinom(C_a, N, a, log = TRUE),
      ln_pr_C_b = dbinom(C_bcd, N, b, log = TRUE),
      ln_pr_C_c = dbinom(C_bcd, N, c,  log = TRUE),
      ln_pr_C_d = dbinom(C_bcd, N, d, log = TRUE),
      LSE_entry = ln_pr_C_a + ln_pr_C_b + ln_pr_C_c + ln_pr_C_d)
  
  # step two: construct lx
  lx <- allis$LSE_entry
  
  # step one: calculate ln(P(R1))
  output <- logSumExp(lx)
  output <- exp(output)
  
  return(output)
}

#########################
pr_all_equal <- function(N, A, B, C, D){
  i <- 0:N
  a = A
  b = B
  c = C
  d = D
  
  # step one, enumerate all possibiliteis and prepare lx
  allis <- data.frame(C = i)
  
  allis <- allis %>%
    mutate(
      ln_pr_C_a = dbinom(C, N, a, log = TRUE),
      ln_pr_C_b = dbinom(C, N, b, log = TRUE),
      ln_pr_C_c = dbinom(C, N, c, log = TRUE),
      ln_pr_C_d = dbinom(C, N, d, log = TRUE),
      LSE_entry = ln_pr_C_a + ln_pr_C_b + ln_pr_C_c + ln_pr_C_d)
  
  
  # step two: construct lx
  lx <- allis$LSE_entry
  
  # step three: calculate LSE(lx) = ln(P(R1))
  output <- logSumExp(lx) 
  output <- exp(output)
  
  return(output)
}
