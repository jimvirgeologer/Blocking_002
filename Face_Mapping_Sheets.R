library(purrr)
library(tidyverse)
library(readxl)
library(writexl)
library(adagio)
library(dplyr)
library(MASS)
library(visdat)
library(Rmpfr)


###########DATA BASE###############
setwd("~/Current Work/Blocking_R_Project_002/Blocking_002")
file.list <- list.files(path = './FACE_MAPPING_MG/MALIGAYA/Ramp 2/SANDY NORTH 4', pattern = '.xls', recursive = TRUE, full.names = TRUE)
file.list <- file.list[!grepl("000", file.list)]


########### Simple Read Excel ############ 
face_sheet_read <- function(i) {
  x = read_xls(i,sheet = 1)
  x[, 22] <- x[3, 15]
  x[, 23] <- x[2, 15]
  x[, 24] <- x[52, 4]
  x[, 25] <- as.numeric(x[29, 5]) * as.numeric(x[29, 7])
  x[, 26] <- ""
  x <- x[-6, ]
  x <- x[, -c(1:2, 16:21)]
  colnames(x) <-
    c(
      "c1",
      "c2",
      "c3",
      "c4",
      "c5",
      "c6",
      "c7",
      "c8",
      "c9",
      "c10",
      "c11",
      "c12",
      "c13",
      "c14",
      "c15",
      "c16",
      "c17",
      "c18"
    )
  
  
  
########## MV_TXG ############
  MV_TXG <- as.numeric(x[28, 3]) * as.numeric(x[28, 5])
  t <- (MV_TXG * 30000) %>% zapsmall(10)
  
  
  x <- x %>% transmute(
    SHEET = as.character(c14),
    DATE = as.numeric(c16),
    DATE = as.Date(DATE, origin = "1899-12-30"),
    SAMPLE_ID = as.numeric(c1),
    LEVEL = as.numeric(c15),
    FROM = round(as.numeric(c2), 2),
    TO = round(as.numeric(c3), 2),
    LENGTH = round(as.numeric(c4), 16),
    AU_gpt = round(as.numeric(c6), 16),
    AG_gpt = round(as.numeric(c7), 2),
    CU_perc = round(as.numeric (c8), 2),
    PB_perc = round(as.numeric(c9), 2),
    ZN_perc = round(as.numeric (c10), 2),
    W_AU = round(AU_gpt * LENGTH, 16),
    MV = as.character(NA)
  ) %>%
    filter(!is.na(W_AU),!is.na(FROM),
           W_AU != 0)
  
x$SHEET <- gsub("FC_","",as.character(x$SHEET)) ############ Removing FC to the sheet name
  

sol3 <- x$W_AU * 30000 %>% round(digits = 15) ######### multiplying 3000 to remove the 1/3 decimal problem
S <- sol3 %>% zapsmall(1)

  
  
  t <- ifelse(t <= 0 , 2, t) %>% as.integer()
  S <- ifelse(S > t , t - 1, S) %>% as.integer()

  
  subsum <- function(b, c) {
    sol <- subsetsum(b, c)
    sol$inds
  }

  result <- subsum(S,t)

x[result, "MV"] <- "MV"

  final <- cbind(x,t,S)
  return(final)
}


############ Making the database#########
df <- lapply(file.list, face_sheet_read ) %>% 
  bind_rows %>%
  as.data.frame()

################ put mine geo in the df ##########
df$SOURCE<- "Mine Geo"


########### Checking the MV############
df %>% count(MV)

