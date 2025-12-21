# Initialize AoC directory structure for a given year
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Please provide a year argument")
}

year_dir <- paste0("AoC", args[1])

if (dir.exists(year_dir)) {
  stop(paste("Directory", year_dir, "already exists"))
}

dir.create(year_dir)

for (i in 1:25) {
  day_dir <- file.path(year_dir, paste0("Day", i))
  dir.create(day_dir)
  
  file.create(file.path(day_dir, paste0("day", i, ".R")))
}