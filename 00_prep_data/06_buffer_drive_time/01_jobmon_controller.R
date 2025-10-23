# Maitreyi Sahu
# April 2024

# Controlller script to launch the drive time HHI's
# Output file: "drive_time_hhi_compiled.csv" 

# ------------------------------------------------------------------------------

# Make sure these packages load correctly! Sometimes need to clean the environment first, as follows:

# 1. close all your rstudio sessions 
# 2. Run /ihme/singularity-images/rstudio/shells/r_env_cleaning.sh
# 3. Get a new session.

# DO NOT use rm(list=ls()), it breaks everything!!!!

pacman::p_load(dplyr, arrow)
library(jobmonr) # make sure this is loaded without pacman

code_path <- "~/repos/us_value/1_data_compilation/4_hospital_hhi/00_prep_data/"

dir <- "/mnt/share/resource_tracking/us_value/data/"
data_dir <- paste0(dir, "hospital_hhi/raw/aha/AHA raw data/")
out_dir <- paste0(dir, "hospital_hhi/processed/") 

# Parallelization variables
radii <- data.frame(radii = c(15, 30, 60))
years <- data.frame(years = 2000:2019)
hosp_type <- data.frame(hosp_type = c("all", "gen_surg"))

#-- JOB INFO FOR CURRENT RUN ---------------------------------------------------

user <- Sys.info()[["user"]]
log_dir <- paste0("/share/temp/slurmoutput/", user)
dir.create(file.path(log_dir), showWarnings = FALSE)
dir.create(file.path(log_dir, "output"), showWarnings = FALSE)
dir.create(file.path(log_dir, "errors"), showWarnings = FALSE)

cluster <- "slurm"
queue <- "all.q"
cluster_proj <- "proj_integrated_analytics"
r_shell <- "/ihme/singularity-images/rstudio/shells/execRscript.sh -s"

#-- SET UP JOBMON WORKFLOW AND TEMPLATES ---------------------------------------

tool <- jobmonr::tool(name = paste0('drive_time_hhi')) # if getting an error "jobmon_client not found", need to restart R session
workflow <- jobmonr::workflow(tool = tool,
                              workflow_args = paste0('drive_time_hhi_', Sys.time()),
                              name = paste0("Drive Time Buffer"))

# Liming Xu fix for "ihme_general" overwriting project issue.
tool <- set_default_tool_resources(
  tool = tool,
  default_cluster_name = "slurm",
  resources = list("project" = "proj_integrated_analytics"))

jobmonr::set_default_workflow_resources(
  workflow = workflow,
  default_cluster_name = cluster,
  resources = list(
    "project" = cluster_proj,
    "working_dir" = getwd(),
    "stdout" = paste0(log_dir, "/output"),
    "stderr" = paste0(log_dir, "/errors")))

# load task templates and resource parameters
source(paste0(code_path, "jobmon_template.R"))

#-- CREATE TASKS ---------------------------------------------------------------

# Unique values to parallelize
combinations <- radii %>% 
  cross_join(years) %>% 
  cross_join(hosp_type) %>%
  setNames(c("radius", "year_id", "hosp_type"))
  
# Create/clear list of tasks.
hhi_tasks <- list()

# Now create the tasks
for (i in 1:nrow(combinations)) {
  
  # Create a task for each row
  task <- jobmonr::task(
    task_template = template_hhi, 
    cluster_name = cluster,
    compute_resources = resources_hhi,
    name = paste0("HHI_row_", i),
    r_shell = r_shell,
    scriptname = paste0(code_path, "06_buffer_drive_time/02_driving_distance_hhi.R"),
    radius = combinations[i, "radius"],
    year_id = combinations[i, "year_id"],
    hosp_type = combinations[i, "hosp_type"])
  
  # Add the task to the list of tasks
  hhi_tasks[[i]] <- task
}

#-- ADD TASKS AND RUN WORKFLOW -------------------------------------------------

# Add tasks to the workflow
workflow <- jobmonr::add_tasks(workflow, c(hhi_tasks))

status <- jobmonr::run(
  workflow=workflow,
  resume=FALSE,
  seconds_until_timeout=36000
)

if (status != "D") {
  stop("The workflow failed")
} else {
  message("The workflow completed successfully!")
}

#-- COMPILE DATA AND GENERATE OUTPUT FILE --------------------------------------

# Compile feather files
compile_hhi <- function(radius) {
  x <- open_dataset(paste0(out_dir, "buffer/drive_time/radius=", radius), format = "feather") %>% collect()
  return(x)
}
compiled_hhi <- lapply(radii$radii, compile_hhi)
compiled_hhi <- do.call(rbind,compiled_hhi)

# Save
stamp <- Sys.Date()
write_feather(compiled_hhi, paste0(out_dir, "buffer/drive_time/drive_time_hhi_compiled_", stamp, ".feather"))
