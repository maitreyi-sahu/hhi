
## =================================================
## Array Job 
## =================================================

ARRAY <- function(
  script, # string filepath
  n_jobs, # string "#"
  memory, # string "#G"
  threads, # string "#"
  time, # string "##:##:##"
  error_dir, # string filepath
  output_dir, # string filepath
  name, # string 
  hold = NULL, ## string - jid to wait on
  arg = NULL ## extra argument to add, if you want
){
  
  ## locate r shell
  r_shell <- "/share/singularity-images/rstudio/shells/execRscript.sh -s" # liming's shell
  
  ## make qsub
  qsub <-  paste("qsub -q all.q",
                 paste0("-l m_mem_free=", memory,
                        # ",archive=TRUE",
                        ",fthread=", threads,
                        ",h_rt=", time),
                 "-P proj_dex",
                 "-now no",
                 "-e", error_dir,
                 "-o", output_dir,
                 "-cwd",
                 "-N", name,  # name job
                 "-t", paste0("1:", n_jobs), # how many jobs there are 
                 "-tc 500", ## limit to 500 at once (friendly cluster respect)
                 "-terse",
                 sep = " ") 
  
  ## add hold if exists
  if(length(hold) > 0){
    qsub <- paste0(qsub, " -hold_jid ", hold)
  }
  
  ## launch job
  jid <- system(paste(qsub, r_shell, script, arg), intern = T)
  
  ## clean JID
  jid <- str_extract(jid, "[:digit:]+")
  
  ## and return jid
  return(jid)
}

## =================================================
## One Job  
## =================================================

QSUB <- function(
  script, # string filepath
  memory, # string "#G"
  threads, # string "#"
  time, # string "##:##:##"
  error_dir, # string filepath
  output_dir, # string filepath
  name, # string
  hold = NULL, ## string - jid to wait on
  arg = NULL ## extra argument to add, if you want
){
  
  ## locate r shell
  r_shell <- "/share/singularity-images/rstudio/shells/execRscript.sh -s" # liming's shell
  
  ## make qsub
  qsub <-  paste("qsub -q all.q",
                 paste0("-l m_mem_free=", memory,
                        # ",archive=TRUE",
                        ",fthread=", threads,
                        ",h_rt=", time),
                 "-P proj_dex",
                 "-now no",
                 "-e", error_dir,
                 "-o", output_dir,
                 "-cwd",
                 "-N", name,  # name job
                 "-terse",
                 sep = " ") 
  
  ## add hold if exists
  if(length(hold) > 0){
    qsub <- paste0(qsub, " -hold_jid ", hold)
  }
  
  ## launch job
  jid <- system(paste(qsub, r_shell, script, arg), intern = T)
  
  ## and return jid
  return(jid)
}