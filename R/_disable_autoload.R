# Stops Shiny from sourcing the files in R/ automatically when the app
# starts. app.R sources every file it needs (including R/data/, which
# autoloading would skip), so autoloading would only read them twice.
