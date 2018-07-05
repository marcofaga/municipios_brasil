showapag <- function() {
  
  workspace <- ls(envir = globalenv())
  ap <- workspace[substr(workspace, 1, 2) == "ap"]
  rm(list = workspace[workspace %in% ap], pos = ".GlobalEnv")

}

