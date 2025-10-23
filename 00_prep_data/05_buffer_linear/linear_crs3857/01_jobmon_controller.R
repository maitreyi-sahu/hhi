# Maitreyi Sahu
# Aug 24, 2023

# Make sure these packages load correctly! Sometimes need to clean the environment first, as follows:

# 1. close all your rstudio sessions 
# 2. Run /ihme/singularity-images/rstudio/shells/r_env_cleaning.sh
# 3. Get a new session.

# DO NOT use rm(list=ls()), it breaks everything!!!!

# ------------------------------------------------------------------------------

# Setup

pacman::p_load(dplyr, arrow)
library(jobmonr)

code_path <- "~/repos/us_value/1_data_compilation/4_hospital_hhi/buffer/"

dir <- "/mnt/share/resource_tracking/us_value/data/"
data_dir <- paste0(dir, "hospital_hhi/raw/aha/AHA raw data/")
out_dir <- paste0(dir, "hospital_hhi/processed/")


# Parallelization variables
radii <- data.frame(radii = c(15, 30, 50))
hospital_years <- fread(paste0(out_dir, "aha_clean_2000_2019.csv")) %>% select(ID, YEAR)

#-- JOB INFO FOR CURRENT RUN ---------------------------------------------------

user <- Sys.info()[["user"]]
log_dir <- paste0("/share/temp/slurmoutput/", user)
dir.create(file.path(log_dir), showWarnings = FALSE)
dir.create(file.path(log_dir, "output"), showWarnings = FALSE)
dir.create(file.path(log_dir, "errors"), showWarnings = FALSE)

cluster <- "slurm"
queue <- "all.q"
cluster_proj <- "proj_dex"
r_shell <- "/ihme/singularity-images/rstudio/shells/execRscript.sh -s"

#-- SET UP JOBMON WORKFLOW AND TEMPLATES -------------------------------------

tool <- jobmonr::tool(name = paste0('linear_hhi')) # if getting an error "jobmon_client not found", need to restart R session
workflow <- jobmonr::workflow(tool = tool,
                              workflow_args = paste0('linear_hhi_', Sys.time()),
                              name = paste0("Linear Buffer"))

# Liming Xu fix for "ihme_general" overwriting project issue.
tool <- set_default_tool_resources(
  tool = tool,
  default_cluster_name = "slurm",
  resources = list("project" = "proj_dex"))

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

#-- CREATE TASKS -------------------------------------------------------------

# Unique values to parallelize
combinations <- hospital_years %>% left_join(radii, by = character()) %>%  # cross-join
  select(radii, YEAR, ID) %>% 
  setNames(c("radius", "year", "hospital_id"))

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
    scriptname = paste0(code_path, "02_linear_distance_hhi.R"),
    radius = combinations[i, "radius"],
    year = combinations[i, "year"],
    hospital_id = combinations[i, "hospital_id"] )
  
  # Add the task to the list of tasks
  hhi_tasks[[i]] <- task
}

#-- ADD TASKS AND RUN WORKFLOW --------------------------------------------------------

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

#-- COMPILE DATA AND GENERATE OUTPUT FILE ----------------------------------------------

individual_hhi <- open_dataset(paste0(out_dir, "buffer/linear/"), format = "feather") %>% collect()
write.csv(individual_hhi, paste0(out_dir, "buffer/linear_distance_hhi_compiled.csv"), row.names = F)