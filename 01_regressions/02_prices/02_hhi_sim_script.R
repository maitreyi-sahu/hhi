## ------------------------
## Run array task for policy regressions including draw-level uncertainty from value scores
## Author: MSahu & Sawyer Crosby
## Date: 11/09/2021
## ------------------------

## ------------------------
## Setup
## ------------------------

rm(list=ls())
set.seed(729104)

library(tidyverse)
library(data.table)
library(broom)
library(plm)
library(ggplot2)

usval_path <- "/ihme/resource_tracking/us_value/data/"
data_path <- "/snfs1/Project/IRH/US_value/Data/"
sims_path <- paste0(usval_path, "policy/simulation/")

## ------------------------
## Formatting
## ------------------------

# IMPORT prices, default when var is empty 



# IMPORT policy var
hosp_hhi <- fread(paste0(sim_path, "/hosp_hhi_vars.csv"))
hosp_hhi <- hosp_hhi[,c("location_name", "year_id", cov), with = F]

## standardize name to "cov"
setnames(hosp_hhi, cov, "cov")

## remove NAs
hosp_hhi <- hosp_hhi[!is.na(cov) & cov != "" & cov != "NR"]

## recode "No", "Yes" to 0,1
if("No" %in% hosp_hhi$cov){
  hosp_hhi[,ncov := as.numeric(plyr::revalue(cov, c("No" = "0", "Yes" = "1")))]
  hosp_hhi[,cov := NULL]
  setnames(hosp_hhi, "ncov", "cov")
}

## check that policy variable is numeric
if(!is.numeric(hosp_hhi$cov)) stop("Variable not numeric")

## check no negatives
if(min(hosp_hhi$cov) < 0) stop("Negative values")

## If not binary, log
if(length(unique(hosp_hhi$cov)) > 2){
  ## add median multiple if it has any zeroes
  ## if there are are negatives, STOP
  if(min(hosp_hhi$cov) == 0){
    hosp_hhi[,cov := cov + .01 * median(cov)]
  }
  hosp_hhi[,cov := log(cov)]
}

## merge in value data 
reg_data <- merge(hosp_hhi, value, by = IDcols)

## stop if zero rows
if(reg_data[,.N == 0]) stop("Zero rows of data")

## convert to pdframe
reg_data <- pdata.frame(reg_data, index = IDcols) 

## ------------------------
## Model
## ------------------------

# Choose a vc matrix
robust <- "Arellano"

# Simulations per run 
sims <- 1 # only one per draw
draw_names <- paste0("d",1:1000)

if(var!=""){
  draw_names <- var
}


# Setup dataframe
simbetas <- data.table()
for (d in draw_names) {
  
  ## Fixed effect regression for draw of value score on policy variable
  if(reg_type %in% c("timeFE", "locFE")){
    res <- plm(get(d) ~ cov, reg_data, model = "within") # loc FE only  OR year FE only -- based on order of ID cols
  }else if(reg_type == "bothFE"){
    res <- plm(get(d) ~ cov, reg_data, model = "within", effect = "twoways") # both FEs
  }else if(reg_type == "noFE"){
    res <- lm(get(d) ~ cov, reg_data)
  }
  
  
  ## Extract coefficients from this run
  pe <- coef(res)

  ## Choose a variance-covariance matrix
  vc <- vcovHC(res,  method = "white1")   # Arellano (1987) heteroskedastic and serial correlation robust VC

  ## Draw simulated betas from this run
  sb <- data.table(beta = MASS::mvrnorm(n=1, mu=pe, Sigma=vc), summary(res)$coefficients, r.squared = summary(res)$r.squared[1])
  
  sb[,draw := d]
  
  ## bind on 
  simbetas <- rbind(simbetas, sb)
  
  print(d)
}

## compute estimates, se, and ci taking into account estimation uncertainty in y and beta
betas <- simbetas[,.(
  variable = cov,
  beta1hat = mean(beta), 
  beta1se = sd(beta), 
  beta1_lb = quantile(beta, .025), 
  beta1_ub = quantile(beta, .975),
  r_squared = mean(r.squared),
  r_squared_lower = quantile(r.squared, .025),
  r_squared_upper = quantile(r.squared, 0.975)
)]

if(nrow(simbetas) == 1){
  betas <- simbetas[,.(
    dep_var = var,
    variable = cov,
    beta1hat = beta, 
    beta1se = `Std. Error`, 
    beta1_lb = beta - 1.96*`Std. Error`, 
    beta1_ub = beta + 1.96*`Std. Error`,
    r_squared = r.squared,
    r_squared_lower = r.squared,
    r_squared_upper = r.squared,
    beta = Estimate,
    p_val = `Pr(>|t|)`
  )]
}

## ------------------------
## Write out
## ------------------------
fwrite(betas, paste0(sim_path, "/output/", cov, ".csv"))



## ------------------------
## Make scatter of covariate with mean value score
## ------------------------

value <- fread(paste0(usval_path,"sfa_output/sensitivity/", value_stamp, "/value_scores.csv"))
value <- value[, .(mean_value = mean(value_score)), by = c("year_id", "location_name")]

value_policy <- merge(value, hosp_hhi, by = IDcols)

plot <- ggplot(value_policy, aes(x = cov, y = mean_value))+facet_wrap(~location_name, scales = "free")+geom_point()+theme_bw()+
  labs(x = paste0("log(", cov, ")"),
       title = paste0("Relationship between value score and ", cov),
        subtitle = paste0("beta: ", round(betas$beta1hat, digits = 2),
                          " (", round(betas$beta1_lb, digits = 2),
                          "-", round(betas$beta1_ub, digits = 2), ")"),
       y = "mean value score")

plot <- ggplot(value_policy, aes(x = cov, y = mean_value))+facet_wrap(~year_id, scales = "free")+geom_point()+theme_bw()+
  labs(x = paste0("log(", cov, ")"),
       title = paste0("Relationship between value score and ", cov),
       subtitle = paste0("beta: ", round(betas$beta1hat, digits = 2),
                         " (", round(betas$beta1_lb, digits = 2),
                         "-", round(betas$beta1_ub, digits = 2), ")"),
       y = "mean value score")+
  geom_smooth(method = "lm")

pdf(paste0(sim_path, "/plots/", cov, ".pdf"), width = 11, height = 8)
print(plot)
dev.off()

