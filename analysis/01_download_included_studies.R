# Download the screened studies and filter the included studies

# Identify an R pacakge to help download from google drive
install.packages("googledrive")

# Download the file

# Checks on the file

# Filter the file
saveRDS(df, "analysis/data-derived/included_studies.rds")
