## ----------------------------------------
## Forest plot
## 10/8/21
## Sawyer Crosby
## ----------------------------------------
rm(list = ls())

stamp <- "17sep2023_hospital_hhi_timeFE"

pacman::p_load(data.table, tidyverse, ggrepel, openxlsx, patchwork)

## ----------------------------------------
## setup
## ----------------------------------------

usval_path <- "/ihme/resource_tracking/us_value/data/"
in_dir <- paste0(usval_path, "policy/simulation/simbetas_", stamp)
out_dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/plots/regressions"

## get data
DT <- fread(paste0(in_dir, "/compiled_output.csv"))
#DT[, sig := ifelse(p_val < 0.05, T, F)]

## simplify data
#DT <- DT[,.(variable, beta = beta1hat, beta_lower = beta1_lb, beta_upper = beta1_ub, sig)]
DT <- DT[,.(variable, beta = beta1hat, beta_lower = beta1_lb, beta_upper = beta1_ub, r_squared, r_squared_lower, r_squared_upper)]

## get N years
#N <- fread("/ihme/dex/us_county/state_envelope/state_covariate_data_N_years.csv")
N <- fread(paste0(in_dir, "/hhi_vars_num_years.csv"))
#N <- N[year_id == 2014]

N <- N[,.(n_years = sum(has_data)), by = variable]

## ----------------------------------------
## Deal with names and ordering
## ----------------------------------------

## set variable labels AND ORDER
variable_names_0 <- DT$variable

## add in number of years
var_names <- data.table(variable = variable_names_0, name = variable_names_0)
var_names <- merge(var_names, N, by = "variable")
var_names[,name := paste0(str_squish(name), " (Y=", n_years, ")")]

## string wrap them
var_names[,name := str_wrap(name, width = 57)]
variable_names <- var_names$name
names(variable_names) <- var_names$variable

## revert to original order
variable_names <- variable_names[names(variable_names_0)]

## ----------------------------------------
## Prep data for plotting
## ----------------------------------------

## correct beta values
# DT[!variable %in% binary_vars,`:=`(beta = beta*10, beta_lower = beta_lower*10, beta_upper = beta_upper*10)]

## assign significance

# TODO - remark this
DT[(sign(beta_lower) == sign(beta) & sign(beta) == sign(beta_upper)), sig := T]
DT[is.na(sig), sig := F]

## assign direction
DT[sig == T & beta > 0, sign := "Significantly positive"]
DT[sig == T & beta < 0, sign := "Significantly negative"]
DT[sig == F, sign := "Not significant"]

## assign colors
colors <- c(
  "Significantly positive" = "mediumseagreen",
  "Significantly negative" = "#CD1D2C",
  "Not significant" =  "black"
)

## assign colors in data (for Y axis labels)
DT[,color := plyr::revalue(sign, colors)]

## recode variables
DT[,var_label := var_names$name]
DT[,variable := plyr::revalue(
  variable, 
  rev(variable_names)
)]

# Group

DT$group <- "Hospital concentration"

# Labels

var_labels <- DT$var_label
year_labels <-  sub(".*\\(([^)]+)\\)$", "\\1", var_labels)


## ----------------------------------------
## Plot setup
## ----------------------------------------

## plot fcn 
plot_fcn <- function(
  dt,           ## data table
  grp,          ## what group? (string) 
  legend = T,   ## include legend? 
  legend_position, 
  xaxis = NULL, ## name of X axis? 
  title = NULL ## title?
){

  tmp <- dt[group == grp]
  
  suppressWarnings({
    P <- ggplot(tmp) +
      
      ## line
      geom_vline(xintercept = 0) +
      
      ## titles
      labs(
        x = if(is.null(xaxis)) "" else xaxis,
        y = "", 
        title = if(is.null(title)) "" else title
      ) +
      
      ## points and lines
      geom_point(aes(x = beta, y = variable, color = sign), size=2) +
      geom_segment(aes(x = beta_lower, xend = beta_upper, y = variable, yend = variable,  color = sign), size=.7) + 
      
      # Background lines   
      # scale_x_continuous(minor_breaks = c(-4:1)) +
      
      ## set colors
      scale_color_manual(values = colors) + 
      
      ## facet
      facet_wrap(~group, ncol = 1) +
      
      ## limits
      # xlim(c(dt[,min(beta_lower)], dt[,max(beta_upper)])) +
      
      # variable labels
     geom_text(aes(x = -40, y = variable, label = paste0("(",year_labels,")")), color = "navy") +
      
      ## theme options
      theme_bw() + 
      theme(
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank(),
        axis.text.y= element_text(face='plain',size=12, color = tmp[order(variable), color]),
        strip.background = element_rect(fill="#bcc4ca"), 
        plot.margin=grid::unit(c(0,0,0,0), "mm"),
        text = element_text(size = 15))
    
    if(legend){
      P <- P + theme(legend.position = legend_position)
    }else{
      P <- P + theme(legend.position = "none")
    }
    return(P)
  })
}


P <- plot_fcn(
  dt = DT, 
  grp = "Hospital concentration", 
  legend = F,
  xaxis = paste0(
    "Percent change in value due to a 10% change in each HHI variable", 
    paste0(rep(" ", 30), collapse = " "))## adding whitespace
)

## ----------------------------------------
## Make plots
## ----------------------------------------

## make plots
filepath <- out_dir
name <- paste0("Figure_5_", stamp)
wdt <- 10
hgt = 10
filetypes <- c("pdf")
    
for(f in filetypes){
  ggsave(
    filename = paste0(name, ".", f),
    plot = P,
    device = f,
    path = filepath,
    width = wdt,
    height = hgt,
    units = "in"
  )
}



# now save clean compiled results
DT_save <- DT[, .(group, name, variable, sign, beta, beta_lower, beta_upper, r_squared, r_squared_lower, r_squared_upper)]

write.xlsx(DT_save, paste0(in_dir, "/clean_compiled_output.xlsx"))


