library(readr)
library(tidyverse)
library(caTools)
library(ROCR)
# Charger les données depuis le fichier CSV
dataset <- read_csv("C:/dataset.csv")
View(dataset)

# Afficher les premières lignes du dataframe pour vérification
head(dataset)

# Vérifier la structure des données
str(dataset)

# Vérifier les valeurs manquantes
missing_values <- colSums(is.na(dataset))

# Afficher les colonnes avec des valeurs manquantes
cols_with_missing <- names(missing_values[missing_values > 0])
print(paste("Colonnes avec des valeurs manquantes : ", cols_with_missing))

# Utiliser la médiane pour l'imputation
for (col in cols_with_missing) {
  if (sum(is.na(dataset[[col]])) > 0) {
    dataset[[col]][is.na(dataset[[col]])] <- median(dataset[[col]], na.rm = TRUE)
  }
}

# Vérifier à nouveau les valeurs manquantes après l'imputation
missing_values_after_imputation <- colSums(is.na(dataset))
if (sum(missing_values_after_imputation) == 0) {
  print("Toutes les valeurs manquantes ont été traitées avec succès.")
} else {
  print("Il reste des valeurs manquantes dans le jeu de données.")
}


# Diviser les données en ensembles d'entraînement et de test
set.seed(123)  
split <- sample.split(dataset$TenYearCHD, SplitRatio = 0.7)
train_data <- subset(dataset, split == TRUE)
test_data <- subset(dataset, split == FALSE)

# Modèle de régression logistique
model <- glm(TenYearCHD ~ ., data = train_data, family = binomial)

# Afficher un résumé du modèle
summary(model)

# Prédire sur l'ensemble de test
predictions <- predict(model, newdata = test_data, type = "response")

# Utiliser les prédictions pour évaluer la performance du modèle
prediction_obj <- prediction(predictions, test_data$TenYearCHD)
performance_obj <- performance(prediction_obj, "tpr", "fpr")

# Afficher la courbe ROC
plot(performance_obj, main = "Courbe ROC", col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "red")



