food_dat <- read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/refs/heads/main/piedmont-triad-data/analysis_data.csv")

usethis::use_data(food_dat, overwrite = TRUE)
