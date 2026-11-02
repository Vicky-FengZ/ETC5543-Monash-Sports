# Quarto starts R in this folder, so the project's own .Rprofile is not read.
# Activate the project's renv library here, so the report (and the package
# versions written to references/packages.bib) use the versions in renv.lock.
local({
  root <- normalizePath("..", winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(root, "renv", "activate.R"))) {
    Sys.setenv(RENV_PROJECT = root)
    owd <- setwd(root)
    on.exit(setwd(owd), add = TRUE)
    source(file.path("renv", "activate.R"))
  }
})
