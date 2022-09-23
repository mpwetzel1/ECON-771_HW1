# Meta --------------------------------------------------------------------

## Author:        Martha Wetzel
## Date Created:  09/14/22
## Notes:         
## Updates:      


# Preliminaries -----------------------------------------------------------
install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, Hmisc, kableExtra, modelsummary, 
               fixest, gt, flextable, stringr, did, DRDID, rmarkdown)

install.packages("devtools")
library(devtools)
install_github("asheshrambachan/HonestDiD", dependencies = TRUE)
library(HonestDiD)



# Load auxilary functions required for question 9
source('Aux Functions for Honest CS.R')

# Set up some output formatting for modelsummary
f <- function(x) formatC(x, digits = 2, big.mark = ",", format = "f")

# Import analytic data set if not already active

#----   Question 1: Summary Stats ----

analytic <- analytic %>% mutate(year_c = as.character(year))

analyticg <- analytic %>% group_by(year_c)

uncompcare <- analyticg %>% summarise(N=sum(!is.na(uncomp_cost)),
                                Mean=round(mean(uncomp_cost,na.rm=TRUE),0), 
                                SD=round(sd(uncomp_cost,na.rm=TRUE),0),
                                Min=round(min(uncomp_cost, na.rm=TRUE),0),
                                Max=round(max(uncomp_cost, na.rm=TRUE),0))




kable(uncompcare, format="html", format.args=list(big.mark=",")) %>%
  kable_styling() %>%
  save_kable("./Output/Tables/Q1a.html")

ggplot(data = analyticg, aes(uncomp_care2)) + geom_boxplot() +facet_wrap(vars(year))

revenue <- analyticg %>% summarise(N=sum(!is.na(tot_rev_mil)),
                                    Mean=round(Mean(tot_rev_mil,na.rm=TRUE),0), 
                                    SD=round(sd(tot_rev_mil,na.rm=TRUE),2),
                                    Min = round(min(tot_rev_mil, na.rm=TRUE),0),
                                    Max= round(max(tot_rev_mil, na.rm=TRUE),0))

kable(revenue, format="html", format.args=list(big.mark=",")) %>%
  kable_styling() %>%
  save_kable("./Output/Tables/Q1b.html")



#----   Question 2: Plot  ----

uncompcare.own <- analytic %>% group_by(year, profit_status ) %>% summarise(N=sum(!is.na(uncomp_cost_mil)),
                                      Mean=mean(uncomp_cost_mil,na.rm=TRUE), 
                                      Median=median(uncomp_cost_mil,na.rm=TRUE)) %>% 
    filter(profit_status %in% c("Non Profit","For Profit" ) ) 

  
  
q2_plot <- ggplot(data=uncompcare.own, aes(x=year, y=Mean, group=profit_status)) +
  geom_line(aes(linetype=profit_status, color=profit_status))+
  ylab("Mean Uncompensated Care in Millions ($)")+
  xlab("Year") +
  labs(linetype="Profit Status", color="Profit Status") +
  theme_classic()

q2_plot

# Exploratory

ggplot(data=uncompcare.own, aes(x=year, y=Median, group=profit_status)) +
  geom_line(aes(linetype=profit_status, color=profit_status))




# Separate by ever treated status
uncompcare.tx <- analytic %>% group_by(year, evertx ) %>% summarise(N=sum(!is.na(uncomp_cost_mil)),
                                                                            Mean=mean(uncomp_cost_mil,na.rm=TRUE), 
                                                                            Median=median(uncomp_cost_mil,na.rm=TRUE)) %>% 
  mutate(evertx = as.logical(evertx))

ggplot(data=uncompcare.tx, aes(x=year, y=Mean, group=evertx)) +
  geom_line(aes(color=evertx))+
  geom_point()+
  ylab("Mean Uncompensated Care in Millions ($)")+
  xlab("Year") 

ggplot(data=uncompcare.tx, aes(x=year, y=Median, group=evertx)) +
  geom_line(aes(color=evertx))+
  geom_point()+
  ylab("Median Uncompensated Care in Millions ($)")+
  xlab("Year") 

#----   Question 3: Simple DiD with TWFE  ----
# 4 ways:
#  - Full sample
#  - 2014 expanders
#  - 2015 expanders
#  - 2016 expanders


# Run some simple linear models to get a sense of model fit
lm1 <- lm(uncomp_care_mil ~ tx_active +state.x + year , data = analytic)
plot(lm1, which = 3)


# Run actual TWFE models
reg_dd_3a <- feols(uncomp_cost_mil ~ tx_active | state.x + year, data=analytic)

reg_dd_3b <- analytic %>% filter(exp_yr==2014 | evertx==0) %>%
  feols(uncomp_cost_mil ~ tx_active | state.x + year)

reg_dd_3c <- analytic %>% filter(exp_yr==2015 | evertx==0) %>%
  feols(uncomp_cost_mil ~ tx_active | state.x + year)

reg_dd_3d <- analytic %>% filter(exp_yr==2016 | evertx==0) %>%
  feols(uncomp_cost_mil ~ tx_active | state.x + year)

models <- list("All Expansions" = reg_dd_3a, 
               "2014 Expanders" = reg_dd_3b, 
               "2015 Expanders" = reg_dd_3c, 
               "2016 Expanders" = reg_dd_3d )

cm <- c("tx_activeTRUE"="Treatment")
q3_twfe <- modelsummary(models, gof_omit = 'AIC|BIC|RMSE', coef_map = cm , 
                        output = "flextable", fmt=f)
q3_twfe



#----   Question 4: Event Study  ----
q4a_event <- analytic %>% feols(uncomp_cost_mil~i(tx_yrs, evertx, ref=-1) | state.x + year,
                  cluster=~state.x)

modelsummary(q4a_event)

q4aplot <- iplot(q4a_event, 
      xlab = 'Time to treatment',
      main = 'Event study')

q4b_event <- analytic %>% filter(exp_yr==2014 | evertx==0) %>% feols(uncomp_cost_mil~i(tx_yrs, evertx, ref=-1) | state.x + year,
                                cluster=~state.x)

q4aplot <- iplot(q4a_event, 
                 xlab = 'Time to treatment',
                 main = 'Event study')

models_q4_list <- list(q4a_event, q4b_event)


# fix names 
rename_events <- function(old_names) {
  new_names <- gsub(":","", gsub("=","",str_sub(old_names,9,10)))
  setNames(new_names, old_names)
}

q4_es <- modelsummary(models_q4_list, gof_omit = 'AIC|BIC|RMSE', coef_rename =rename_events,
                      output = "flextable", fmt=f)


q4_es


#----   Question 5: Sun & Abraham  ----

analytic <- analytic %>% mutate(exp_yr_sa = ifelse(evertx==0, 10000, exp_yr))

q5_sa <- feols(uncomp_cost_mil~sunab(exp_yr_sa, tx_yrs) | state.x + year,
                cluster=~state.x,
                data=analytic)


# Get the disaggregated results
q5_sa_noag <- feols(uncomp_cost_mil~sunab(exp_yr_sa, tx_yrs, no_agg=TRUE) | state.x + year,
               cluster=~state.x,
               data=analytic)

modelsummary(q5_sa_noag)
q5_est <- as.data.frame(get_estimates(q5_sa_noag))


q5_est <- q5_est %>% mutate(cohort=str_sub(term,-4,-1),
                            Est=paste(round(estimate,2)," (", round(std.error, 2),")"),
                            Rel_yr=gsub(":","",str_sub(term,9,10))) %>%
        filter(cohort != 2019)


q5_est_t <- pivot_wider(q5_est, id_cols= Rel_yr, values_from =Est, names_from=cohort )


#----   Question 6: Sun & Abraham Graph  ----
modelsummary(q5_sa)
q6plot <- iplot(q5_sa,
      xlab = 'Time to treatment',
      main = 'Event study (Sun & Abraham)')

q6plot

#----   Question 7: Callaway & Sant'Anna   ----


# This is the code Ian used in class slides but I don't understand it - how does the matching work if no covariates specified?

analytic <- analytic %>% group_by(State) %>%
  mutate(stategroup=cur_group_id()) %>% ungroup()

analytic <- analytic %>% mutate(exp_yr_ca= ifelse(is.na(exp_yr),0,exp_yr))

q7_ca <- att_gt(yname="uncomp_cost_mil", tname="year", idname="stategroup",
                 gname="exp_yr_ca",
                 data=analytic, panel=TRUE, est_method="dr",
                 allow_unbalanced_panel=TRUE,
                 print_details=TRUE )

summary(q7_ca)

q7_ca_event <- aggte(q7_ca, type="dynamic")

q7_graph <- ggdid(q7_ca_event,
      legend=FALSE)

# How do you get stats on the weights used for PSW?

# Have to use a universal base period for Q 8

q7_ca_unv <- att_gt(yname="uncomp_cost_mil", tname="year", idname="stategroup",
                gname="exp_yr_ca",
                data=analytic, panel=TRUE, est_method="dr",
                allow_unbalanced_panel=TRUE,
                base_period = "universal")

q7_ca_ev_unv <- aggte(q7_ca_unv, type="dynamic")
ggdid(q7_ca_ev_unv,
      legend=FALSE)


#----   Question 8: Rambachan and Roth - Parallel Trends  ----


q7_ca_ev_unv <- honest_did(q7_ca_ev_unv,
                                 type="relative_magnitude")


#---- Save the output ----
save(uncompcare, revenue, q2_plot, q3_twfe, q4_es, q5_est_t, q6plot, q7_ca_event, q7_graph,
     file="./Output/R Objects/final.Rdata.")

