# Task templates and task parameters for the Jobmon workflow

#-- TASK PARAMETERS ------------------------------------------------------------

resources_hhi <- list(
  "cores" = 4, 
  "queue" = queue,
  "runtime" = (60 * 30),
  "memory" = "10G",
  "constraints" = "archive"
)

# resources_hhi_intersecting <- list(
#   "cores" = 4, 
#   "queue" = queue,
#   "runtime" = (60 * 30),
#   "memory" = "35G",
#   "constraints" = "archive"
# )


#-- TASK TEMPLATES -------------------------------------------------------------

template_hhi <- jobmonr::task_template(
  tool = tool,
  template_name = "hhi",
  command_template = (paste0("PYTHONPATH= OMP_NUM_THREADS=", resources_hhi$cores,
                           " {r_shell} {scriptname} {radius} {year_id} {hosp_type}")),
  node_args = list("radius", "year_id", "hosp_type"),
  op_args = list("r_shell", "scriptname")
)

set_default_template_resources(
  task_template = template_hhi,
  default_cluster_name = cluster,
  resources = resources_hhi
)

## 

# template_hhi_intersecting <- jobmonr::task_template(
#   tool = tool,
#   template_name = "hhi",
#   command_template = (paste0("PYTHONPATH= OMP_NUM_THREADS=", resources_hhi_intersecting$cores,
#                              " {r_shell} {scriptname} {year_id} {hosp_type}")),
#   node_args = list("year_id", "hosp_type"),
#   op_args = list("r_shell", "scriptname")
# )


## Not used anymore (by hospital)

# template_hhi_linear <- jobmonr::task_template(
#   tool = tool,
#   template_name = "linear_hhi",
#   command_template = (paste0("PYTHONPATH= OMP_NUM_THREADS=", resources_hhi$cores,
#                            " {r_shell} {scriptname} {radius} {hospital_id} {year_id}")),
#   node_args = list("radius", "hospital_id", "year_id"),
#   op_args = list("r_shell", "scriptname")
# )


