library(readr)
library(janitor)

data_students <- read_csv("https://pos.it/r4ds-students-csv", na = c("", "NA", "N/A")) |> clean_names()



head(data_students)
