# Source-order contract for import helpers: shared cell parsing first,
# followed by each family's reader, cleaning and validation scripts.
# Do not change this load order without checking all parser dependencies.

# -------------------------------------------------------------------------
# Dashboard data layer loader
# -------------------------------------------------------------------------
# Keep dependencies explicit: common helpers first, then each report module's
# reader, cleaning/layout helpers and validation functions.

source("R/data/common.R")
source("R/data/extensibility.R")

source("R/data/monthly_site/import.R")
source("R/data/monthly_site/cleaning.R")
source("R/data/monthly_site/validation.R")

source("R/data/venues/import.R")
source("R/data/venues/cleaning.R")
source("R/data/venues/validation.R")

source("R/data/business_unit_templates/import.R")
source("R/data/business_unit_templates/cleaning.R")
source("R/data/business_unit_templates/validation.R")
