lm_patho <- read.csv("df.csv")
dir.create("../data")
save(lm_patho, file = "../data/lm_patho.rda")
