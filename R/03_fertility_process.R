setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 03_fertility_process.R
# process MSDS data 
# caculate ethnic age specific fertility rate
# obtain transition matrix of mixed babies 
# ============================================================

eth_newethpop = case_when(
  eth_code %in% c("A", "B")             ~ "WBI",
  eth_code == "C"                        ~ "WHO",
  eth_code %in% c("D", "E", "F", "G")  ~ "MIX",
  eth_code == "H"                        ~ "IND",
  eth_code == "J"                        ~ "PAK",
  eth_code == "K"                        ~ "BAN",
  eth_code == "R"                        ~ "CHI",
  eth_code == "L"                        ~ "OAS",
  eth_code == "N"                        ~ "BLA",
  eth_code == "M"                        ~ "BLC",
  eth_code == "P"                        ~ "OBL",
  eth_code == "S"                        ~ "OTH",
  eth_code %in% c("Z", "99")            ~ NA_character_
)