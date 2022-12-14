# Meta --------------------------------------------------------------------

## Date Created:  7/22/2020
## Date Edited:   8/25/2022
## Description:   Code file to import and tidy CMS POS data from 1984-2021


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)


# Import raw data ---------------------------------------------------------



for (y in 1995:2010) {
  pos.data <- read_csv(paste0("./Data/pos",y,".csv/pos",y,".csv"))
  pos.data <- pos.data %>%
    rename(category=prvdr_ctgry_cd,
           category_sub=prvdr_ctgry_sbtyp_cd,           
           provider=prvdr_num,
           own_change=chow_dt,
           beds_cert=crtfd_bed_cnt,
           beds_tot=bed_cnt,
           name=fac_name,
           street=st_adr,
           zip=zip_cd,
           own_type=gnrl_cntl_type_cd,
           term_date=trmntn_exprtn_dt,
           state=state_cd) %>%
    mutate(term_date=as.Date(as.character(term_date), format='%Y%m%d'),
           own_change=as.Date(as.character(term_date), format='%Y%m%d'),
           year=y) %>%
    select(provider, category, category_sub, own_change, beds_cert, beds_tot, name,
           street, city, state, zip, term_date, own_type, year)
  
  assign(paste0("pos.",y),pos.data)  
}


for (y in 2011:2021) {
  pos.data <- read_csv(paste0("./Data/pos",y,".csv/pos",y,".csv"))
  colnames(pos.data) <- tolower(colnames(pos.data))
  
  pos.data <- pos.data %>%
    rename(category=prvdr_ctgry_cd,
           category_sub=prvdr_ctgry_sbtyp_cd,           
           provider=prvdr_num,
           own_change=chow_dt,
           beds_cert=crtfd_bed_cnt,
           beds_tot=bed_cnt,
           name=fac_name,
           street=st_adr,
           zip=zip_cd,
           city=city_name,
           own_type=gnrl_cntl_type_cd,
           term_date=trmntn_exprtn_dt,
           state=state_cd) %>%
    mutate(term_date=as.Date(as.character(term_date), format='%Y%m%d'),
           own_change=as.Date(as.character(term_date), format='%Y%m%d'),
           year=y) %>%
    select(provider, category, category_sub, own_change, beds_cert, beds_tot, name,
           street, city, state, zip, term_date, own_type, year)
  
  assign(paste0("pos.",y),pos.data)  
}


# Final data --------------------------------------------------------------

## append yearly data
final.pos.data <- pos.1999
for (y in 1999:2021) {
  final.pos.data <- bind_rows(final.pos.data, get(paste0("pos.",y)))
}


## recode relevant variables
final.pos.data <- final.pos.data %>%
  mutate(category_sub=str_pad(category_sub, width=2, side="left", pad="0")) %>%
  mutate(category = 
           case_when(
             category=="01" ~ "Hospital",
             category %in% c("02","03","04") ~ "SNF",
             category=="05" ~ "Home Health",
             category=="06" ~ "Psychiatric",
             category=="07" ~ "Portable X-Ray",
             category=="08" ~ "Physical Therapy",
             category=="09" ~ "ESRD",
             category=="10" ~ "Nursing",
             category=="11" ~ "Intermediate Care",
             category=="12" ~ "Rural Health Clinic",
             category=="13" ~ "Outpatient Rehab",
             category=="14" ~ "ASC",
             category=="15" ~ "Hospice",
             category=="16" ~ "Organ Procurement",
             category %in% c("17","21") ~ "CLIA Lab",
             category=="18" ~ "Community Health Center",
             category=="19" ~ "Screening Mammography",
             category=="20" ~ "Federally Qualified Health Center"),
         category_sub =
           case_when(
             category_sub=="01" ~ "Short Term",
             category_sub=="02" ~ "Long Term",
             category_sub=="03" ~ "Religious Nonmedical",
             category_sub=="04" ~ "Psychiatric",
             category_sub=="05" ~ "Rehabilitation",
             category_sub=="06" ~ "Children",
             category_sub=="07" ~ "Partial Psychiatric",
             category_sub=="11" ~ "Critical Access",
             category_sub=="20" ~ "Transplant",
             category_sub=="22" ~ "Medicaid-only Non-psychiatric",
             category_sub=="23" ~ "Medicaid-only Psyciatric"),
         own_type =
           case_when(
             own_type=="01" ~ "Non-profit Church",
             own_type=="02" ~ "Non-profit Private",
             own_type=="03" ~ "Non-profit Other",
             own_type=="04" ~ "Profit",
             own_type=="05" ~ "Govt Federal",
             own_type=="06" ~ "Govt State",
             own_type=="07" ~ "Govt Local",
             own_type=="08" ~ "Govt Hospital District",
             own_type=="09" ~ "Physician Owned",
             own_type=="10" ~ "Tribal"),
         profit_status = 
           case_when(
             own_type %in% c("Non-profit Church", "Non-profit Private", "Non-profit Other") ~ "Non Profit",
             own_type %in% c("Physician Owned","Profit") ~ "For Profit",
             own_type %in% c("Govt Federal", "Govt State", "Govt Local", "Govt Hospital District") ~ "Government",
             own_type=="Tribal" ~ "Tribal")
  )

write_tsv(final.pos.data,'./output/pos-data-combined.txt',append=FALSE,col_names=TRUE)





