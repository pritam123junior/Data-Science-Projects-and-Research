
Dataset<-read.csv("C:/Data Science/Red_wine_Quality.csv",header = TRUE,sep = ",")
print(Dataset)
Pearson_Test <- c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", 
              "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol" )
Dataset$quality <- as.numeric(as.character(Dataset$quality))

for (variable in Pearson_Test) {
  cat("Pearson's Test for", variable, "and Quality:\n")
  if (is.numeric(Dataset[[variable]])) {
    print(cor.test(Dataset[[variable]], Dataset$quality, method = "pearson"))
  } else {
    cat("Variable", variable, "is not numeric.\n\n")
  }
  cat("\n")
}
Selected_Features<-Dataset[, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", 
                              "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol","quality")]
head(Selected_Features)
summary(Selected_Features)
