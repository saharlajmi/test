# Chargement des packages
library(MASS)
library(ggplot2)
library(broom)
library(readr)
library(tidyverse)
library(caTools)
library(ROCR)
# Chargement du jeu de données
# Assurez-vous que le chemin du fichier CSV est correct
data <- read.csv("C:/dataset.csv")
View(dataset)

# Visualisation des variables
par(mfrow = c(2, 2))
hist(data$age, main = "Histogramme de l'âge", xlab = "Âge", col = "lightblue")
hist(data$totChol, main = "Histogramme du cholestérol total", xlab = "Cholestérol total", col = "lightgreen")
hist(data$sysBP, main = "Histogramme de la pression systolique", xlab = "Pression systolique", col = "lightpink")



# Préparation des données
# Ajoutez ici toutes les étapes de prétraitement nécessaires, comme la gestion des valeurs manquantes, l'encodage des variables catégorielles, etc.
# Vérification des valeurs manquantes
sum(is.na(data))

# Vérification des types de données
str(data)

# Statistiques descriptives
summary(data)

# Afficher les colonnes avec des valeurs manquantes
missing_values <- colSums(is.na(dataset))
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

# Matrice de corrélation
cor_matrix <- cor(data[c("age", "totChol", "sysBP")])
print(cor_matrix)

# Définition des variables indépendantes (X) et de la variable dépendante (y)
X <- data.frame(age = data$age, totChol = data$totChol, sysBP = data$sysBP)
y <- data$TenYearCHD

# Modèle de régression de Poisson
poisson_model <- glm(y ~ age + totChol + sysBP, family = poisson, data = data)

# Récupération des statistiques du modèle avec broom
model_stats <- tidy(poisson_model)

# Affichage des résultats
print(model_stats)

# Diagnostics du modèle
# vérification de la surdispersion
residual_deviance <- poisson_model$deviance
df_residuals <- poisson_model$df.residual
p_value_overdispersion <- 1 - pchisq(residual_deviance, df_residuals)

# Affichage des résultats de surdispersion
cat("Residual Deviance:", residual_deviance, "\n")
cat("Degrees of Freedom for Residuals:", df_residuals, "\n")
cat("P-value for Overdispersion Test:", p_value_overdispersion, "\n")

# Visualisation des résidus
residuals <- residuals(poisson_model, type = "pearson")
print(residuals)

# Prédictions et évaluation du modèle
predictions <- predict(poisson_model, newdata = data, type = "response")

# Comparaison des prédictions avec les valeurs observées
comparison <- data.frame(Observed = data$TenYearCHD, Predicted = predictions)

# Graphique de comparaison des valeurs observées et prédites avec ggplot2
ggplot(comparison, aes(x = Observed, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6, size = 3) +  
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  
  labs(title = "Comparaison des valeurs observées et prédites",
       x = "Valeurs observées (Réelles)", y = "Valeurs prédites (Modèle)") +  
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title = element_text(size = 12),  
        axis.text = element_text(size = 10),  
        legend.position = "none")  


# Évaluation du modèle avec l'AIC
aic_value <- AIC(poisson_model)
cat("AIC:", aic_value, "\n")

# Déviations résiduelles
dev_residuals <- residuals(poisson_model, type = "pearson")
summary(dev_residuals)


# Matrice de confusion
confusion_matrix <- table(data$TenYearCHD, as.numeric(predictions > 0.5))
print(confusion_matrix)

# Courbe ROC et AUC-ROC
install.packages("pROC")
library(pROC)
roc_curve <- roc(data$TenYearCHD, predictions)
auc_score <- auc(roc_curve)
cat("Aire sous la courbe ROC (AUC-ROC):", auc_score, "\n")
plot(roc_curve, main = "Courbe ROC", col = "blue", lwd = 2)

