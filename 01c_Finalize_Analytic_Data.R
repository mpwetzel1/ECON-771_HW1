# Meta --------------------------------------------------------------------

## Author:        Martha Wetzel
## Date Created:  09/14/22
## Notes:         
## Updates:      


# Preliminaries -----------------------------------------------------------


# Import  -----------------------------------------------------------
final.hcris.data <- read_tsv("./Output/HCRIS_Data.txt")
final.pos.data <- read_tsv("./output/pos-data-combined.txt")
medex <- read_csv("./Data/KFF_medicaid_expansion_2019_MW.csv")

# Clean Medicaid Expansion data  -----------------------------------
# State abbreviations
medex$stabb <- state.abb[match(medex$State,state.name)]
# Manual fix for DC
medex$stabb <- ifelse(medex$State=="District of Columbia", "DC", medex$stabb)

# Extract expansion dates
# Real talk, I gave up on figuring out how to do this in R and just used Excel

# Combine  -----------------------------------------------------------

# Merge HCRIS and POS data on provider number and year
hosps <- final.pos.data %>% filter(category=="Hospital") 
hosps <- subset(hosps, select=-c(name,street,city,zip))

# There's duplicates in the POS file, but none of the duplicate rows differ on ownership status, so can just delete the dups
hosps2 <- hosps[!duplicated(hosps[,c("provider","year")]),]

hcrispos <- merge(x=hosps2, y=final.hcris.data, by.x=c("provider","year"), by.y=c("provider_number","year"), all.x=F, all.y=F )

# Check on the state vars
stateprobs <- subset(hcrispos, select=c(state.x, state.y)) %>% filter(state.x != state.y)
xtabs(~state.x+state.y, stateprobs)
hcrispos %>%count(state.x)

# Conclude: use state.x (from HCRIS data). It includes territories that will get dropped in final data set
analytic <- merge(x=hcrispos, y=medex, by.x="state.x", by.y="stabb", all.x=F, all.y=F )

# Check to make sure the number of dropped rows is expected based on territory hospital counts
nrow(hcrispos)-nrow(analytic)

# Additional Prep   -------------------------------------------------


# Prior to 2003, both uncomp care variables are always missing. 
# Post 2003, hospitals report either uncomp_care or uncomp_care_charges, but not both
analytic <- analytic %>% mutate(uncomp_care2= ifelse(is.na(uncomp_care), 
                                                         abs(tot_uncomp_care_charges) - abs(tot_uncomp_care_partial_pmts) + abs(bad_debt), 
                                                         abs(uncomp_care)))

# Set impossible values to N/A
analytic$uncomp_care2 <- ifelse(analytic$uncomp_care2 > analytic$tot_charges, NA, analytic$uncomp_care2)
analytic$uncomp_care2 <- ifelse(analytic$uncomp_care2 <0, NA, analytic$uncomp_care2)

analytic <- analytic %>% mutate(uncomp_care_mil =uncomp_care2/1000000,
                                tot_charge_mil = tot_charges/1000000,
                                tot_rev_mil = tot_pat_rev/1000000,
                                uncomp_cost = uncomp_care2*cost_to_charge,
                                uncomp_cost_mil = uncomp_cost/1000000,  )


analytic <- analytic %>% filter(year>=2003 & year < 2020)

analytic$Expansion_Date_dt <- as.Date(analytic$Expansion_Date, format="%m/%d/%Y")
analytic$exp_yr <- year(analytic$Expansion_Date_dt)

# Flag the ever treated group
analytic$evertx <-ifelse(is.na(analytic$Expansion_Date),0,1)

analytic <- analytic %>% 
  mutate(post = (year > exp_yr),
         tx_active=(evertx==1 & post==1))

analytic <- analytic %>% 
  mutate(tx_yrs=ifelse(evertx==1, year - exp_yr,-1),
         tx_yrs=ifelse(tx_yrs< -5, -5, tx_yrs))

# Quick checks

check <- distinct(subset(analytic, select=c(state.x, year, Expansion_Date, evertx, post, tx_active, tx_yrs)))


#Check how the uncompensated care vs total charges are looking
ggplot(data=analytic, aes(x=uncomp_cost_mil, y=tot_charge_mil, group=profit_status)) +
  geom_point(aes(color=profit_status)) +
  facet_wrap(vars(year))

# Looks like one facility is a wild outlier
subset(analytic, select=c("tot_uncomp_care_charges","uncomp_care","uncomp_care2","tot_uncomp_care_partial_pmts",
                          "bad_debt","cost_to_charge","uncomp_cost_mil", "year","provider")) %>% 
  filter(uncomp_cost_mil>3000)


subset(analytic, select=c("tot_uncomp_care_charges","uncomp_care","uncomp_care2","tot_uncomp_care_partial_pmts",
                          "bad_debt","cost_to_charge","tot_charges", "year","provider")) %>% 
  filter(provider=="050373")

# Dump that facility
analytic <- analytic %>% filter(provider != "050373")


write_tsv(analytic,'./output/analytic_final.txt',append=FALSE,col_names=TRUE)
save.image()

# SAS is having problems with that file for some reason, try a CSV for the SAS checks

limited <- analytic %>% subset(select=c(uncomp_care2, evertx, tx_active, exp_yr, year, state.x))
write_csv(limited,'./output/limited.csv',append=FALSE,col_names=TRUE)

