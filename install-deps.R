deps <- read.dcf("DESCRIPTION", fields = "Imports")[[1]]
deps <- strsplit(deps, ",\\s*")[[1]]
deps <- trimws(deps)
install.packages(deps)
