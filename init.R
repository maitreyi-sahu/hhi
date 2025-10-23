user = 'MS3390'
local = T

dir_head <- if (Sys.info()[["sysname"]] == "Darwin") "/Users/" else "C:/Users/"
if (local) {
  dir <- paste0(dir_head, user, '/projects_local/hhi_local/')
} else {
  dir <- paste0(
    dir_head,
    user,
    '/Partners HealthCare Dropbox/Maitreyi Sahu/projects/hhi/'
  )
}

data_dir <- paste0(dir, 'data/')
plot_dir <- paste0(dir, 'plots/')
results_dir <- paste0(dir, 'results/')

# ------------------------------------------------------------------------------------

options(scipen = 999, digits = 5)

pacman::p_load(
  data.table,
  readxl,
  arrow,
  dplyr,
  lubridate,
  patchwork,
  ggplot2,
  gridExtra,
  scales,
  sf
)

# ---- Helper: safe month parsing ----
.parse_month <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  # Try ymd() first; if all NA, assume "YYYY-MM" and add day
  out <- suppressWarnings(lubridate::ymd(x))
  if (all(is.na(out))) {
    out <- suppressWarnings(as.Date(paste0(x, "-01")))
  }
  out
}

# ---------------------------------------------------------------------------------------