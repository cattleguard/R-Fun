library(randomForest)
library(base64enc)

one.k.processed <- read.csv("corpus_subset_1k.csv", header = T)

# factorize arch and remove hash from features.
one.k.processed$X.arch. <- as.factor(one.k.processed$X.arch.)

# Show rf before adding in the features for mips[el]
fitbits <- randomForest(X.arch. ~ . -X.1 -X -x00 -endian.1 -endian.2 -endian.3, one.k.processed, ntree=500, mtry=7, importance = T)
