

# Libraries ----------------------------------------------------------------
library(readxl)             
library(dplyr)
library(car)
library(MASS)
library(leaps)
library(Metrics)
library(glmnet)

# Importera data ----------------------------------------------------------
df <- read_excel("datainsamling_blocket_volvo.xlsx")

# Förbehandling -----------------------------------------------------------
df <- df %>%
  mutate(
    Försäljningspris = as.numeric(gsub("[^0-9]", "", Försäljningspris)),
    Miltal = as.numeric(gsub("[^0-9]", "", Miltal)),
    Hästkrafter = as.numeric(gsub("[^0-9]", "", Hästkrafter)),
    Motorstorlek = as.numeric(gsub("[^0-9\\.]", "", Motorstorlek)),
    Modellår = as.numeric(Modellår),
    Elbil = ifelse(is.na(Motorstorlek), 1, 0),
    Motorstorlek = ifelse(is.na(Motorstorlek), 0, Motorstorlek)
  )

# Standardisera och gruppera Bränsle --------------------------------------
df$Bränsle <- df$Bränsle %>%
  tolower() %>%
  trimws() %>%
  gsub("miljöbränse/hybrid", "miljöbränsle/hybrid", .) %>%
  gsub("miljöbränsle/ hybrid", "miljöbränsle/hybrid", .) %>%
  gsub("/ ", "/", .)
df$Bränsle[df$Bränsle %in% c("etanol", "gas", "miljöbränsle/hybrid")] <- "miljö"
df$Bränsle <- as.factor(df$Bränsle)

# Rensa övriga kategorivariabler ------------------------------------------
df <- df %>%
  mutate(across(c(Växellåda, Biltyp, Drivning, Färg, Region, Modell), ~ as.factor(tolower(trimws(as.character(.))))))

# Ta bort irrelevanta variabler -------------------------------------------
df <- df %>%
  dplyr::select(-c(URL, Datum_i_trafik, Märke))

# Ta bort orimliga värden -------------------------------------------------
df <- df %>% filter(Miltal != 0, Försäljningspris > 100)

dim(df)
head(df)
str(df)
summary(df)

# Dela upp i train, val, test ---------------------------------------------
spec <- c(train = 0.6, validate = 0.2, test = 0.2)
set.seed(123)
g <- sample(cut(seq(nrow(df)), nrow(df) * cumsum(c(0, spec)), labels = names(spec)))
res <- split(df, g)
df_train <- res$train
df_val <- res$validate
df_test <- res$test

# Se till att alla faktorer i val/test bara har nivåer som finns i träningen
factor_vars <- c("Växellåda", "Biltyp", "Drivning", "Färg", "Region", "Modell", "Bränsle")

# Gör dem till faktorer först, om det inte redan är det
for (var in factor_vars) {
  df_train[[var]] <- factor(df_train[[var]])
  df_val[[var]] <- factor(df_val[[var]])
  df_test[[var]] <- factor(df_test[[var]])
}

# Filtrera bort nivåer som inte finns i träningen
for (var in factor_vars) {
  df_val <- df_val[df_val[[var]] %in% levels(df_train[[var]]), ]
  df_test <- df_test[df_test[[var]] %in% levels(df_train[[var]]), ]
}


# Modell 1: Teoristyrd modell ---------------------------------------------
lm_1 <- lm(Försäljningspris ~ Miltal + Hästkrafter + Modellår + Motorstorlek + Bränsle + Elbil, data = df_train)
summary(lm_1)

par(mfrow = c(2, 2))
plot(lm_1)
vif(lm_1)

# Modell 2: Fullmodell, manuell rensning ----------------------------------
df_train_mod2 <- df_train
lm_2_full <- lm(Försäljningspris ~ ., data = df_train_mod2)
summary(lm_2_full)

# Ta bort svaga/aliased variabler
lm_2 <- lm(Försäljningspris ~ . - Modell - Region - Färg, data = df_train_mod2)
summary(lm_2)

par(mfrow = c(2, 2))
plot(lm_2)

# Hantera aliased variabler
na_coef <- names(coef(lm_2))[is.na(coef(lm_2))]
if (length(na_coef) > 0) {
  keep_vars <- setdiff(names(coef(lm_2))[-1], na_coef)
  keep_vars <- gsub("", "", keep_vars)
  formula_clean <- as.formula(paste("Försäljningspris ~", paste(keep_vars, collapse = " + ")))
  lm_2 <- lm(formula_clean, data = df_train_mod2)
}

vif(lm_2)

# Modell 3: Lasso-regression ----------------------------------------------
df_train <- df_train %>% na.omit()
x_train <- model.matrix(Försäljningspris ~ ., data = df_train)[, -1]
y_train <- df_train$Försäljningspris

set.seed(123)
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
plot(cv_lasso)
best_lambda <- cv_lasso$lambda.min
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)
coef(lasso_model)

# Prediktion på valideringsdata ------------------------------------------
df_val_mod2 <- df_val[, colnames(df_train_mod2)]
val_pred_1 <- predict(lm_1, newdata = df_val)
val_pred_2 <- predict(lm_2, newdata = df_val_mod2)

# Lasso: matcha kolumner med exakt samma namn och ordning
x_val_full <- model.matrix(Försäljningspris ~ ., data = df_val)
x_val <- matrix(0, nrow = nrow(x_val_full), ncol = ncol(x_train))
colnames(x_val) <- colnames(x_train)
common_cols <- intersect(colnames(x_train), colnames(x_val_full))
x_val[, common_cols] <- x_val_full[, common_cols]
val_pred_3 <- predict(lasso_model, s = best_lambda, newx = x_val)

# Faktiska värden och antal
y_val   <- df_val$Försäljningspris
n_val   <- length(y_val)

# Sum of Squared Errors & Total Sum of Squares
SSE_val <- sum((y_val - as.vector(val_pred_3))^2)
TSS_val <- sum((y_val - mean(y_val))^2)

# R² och justerat R²
R2_val    <- 1 - SSE_val / TSS_val
k_nonzero <- sum(coef(lasso_model) != 0) - 1    # minus intercept
adjR2_val <- 1 - (1 - R2_val) * (n_val - 1) / (n_val - k_nonzero - 1)

# Approximativt BIC
BIC_val   <- n_val * log(SSE_val / n_val) + k_nonzero * log(n_val)

cat("Lasso (validering): R² =", round(R2_val,4),
    " adj.R² =", round(adjR2_val,4),
    " BIC =", round(BIC_val,1), "\n")

# Utvärdering: RMSE --------------------------------------------------------
results <- data.frame(
  Modell   = c("Modell 1", "Modell 2", "Modell 3"),
  RMSE_val = c(
    rmse(df_val$Försäljningspris, val_pred_1),
    rmse(df_val$Försäljningspris, val_pred_2),
    rmse(df_val$Försäljningspris, val_pred_3)
  ),
  Adj_R2   = c(
    summary(lm_1)$adj.r.squared,
    summary(lm_2)$adj.r.squared,
    adjR2_val
  ),
  BIC      = c(
    BIC(lm_1),
    BIC(lm_2),
    BIC_val
  )
)
print(results)

# === Modell 3 (Lasso): Testdata med intervall och post-Lasso OLS =============

# 1. Skapa designmatris för testdata
x_test_full <- model.matrix(Försäljningspris ~ ., data = df_test)[, -1]
x_test <- matrix(0, nrow = nrow(x_test_full), ncol = ncol(x_train))
colnames(x_test) <- colnames(x_train)
common_cols <- intersect(colnames(x_train), colnames(x_test_full))
x_test[, common_cols] <- x_test_full[, common_cols]

# 2. Prediktion med Lasso
test_pred_3 <- predict(lasso_model, s = best_lambda, newx = x_test)

# 3. Utvärdering
test_rmse_3 <- rmse(df_test$Försäljningspris, as.vector(test_pred_3))
cat("Test-RMSE för Modell 3 (Lasso):", round(test_rmse_3, 2), "\n")

# R² och justerat R²
y_test    <- df_test$Försäljningspris
n_test    <- length(y_test)
SSE_test  <- sum((y_test - as.vector(test_pred_3))^2)
TSS_test  <- sum((y_test - mean(y_test))^2)
R2_test   <- 1 - SSE_test / TSS_test
k_nonzero <- sum(coef(lasso_model) != 0) - 1
adjR2_test <- 1 - (1 - R2_test) * (n_test - 1) / (n_test - k_nonzero - 1)
cat("Lasso (test): R² =", round(R2_test, 4),
    " adj.R² =", round(adjR2_test, 4), "\n")

# Approximativt BIC
BIC_test <- n_test * log(SSE_test / n_test) + k_nonzero * log(n_test)
cat("Lasso (test): BIC ≈", round(BIC_test, 1), "\n")

# Figur 3 – Faktiskt vs. Predikterat pris (Lasso)
plot(df_test$Försäljningspris, test_pred_3,
     xlab = "Faktiskt pris", ylab = "Predikterat pris",
     main = "Modell 3: Faktiskt vs. Predikterat pris (Lasso)",
     pch = 19, col = "steelblue")
abline(0, 1, col = "red", lty = 2)

# === Post-Lasso OLS: För att kunna skapa intervall ==========================
# 1. Skapa designmatris för träningsdata
df_train_mm <- model.matrix(Försäljningspris ~ ., data = df_train)
df_train_lasso <- as.data.frame(df_train_mm[, -1])  # ta bort intercept
df_train_lasso$Försäljningspris <- df_train$Försäljningspris

# 2. Hämta icke-noll koefficientnamn och skapa formel
nonzero <- rownames(coef(lasso_model))[as.vector(coef(lasso_model)) != 0][-1]  # utan intercept
safe_vars <- paste0("`", nonzero, "`")
formula_lm3 <- as.formula(paste("Försäljningspris ~", paste(safe_vars, collapse = " + ")))

# 3. Kör OLS på de valda variablerna
lm_3_ols <- lm(formula_lm3, data = df_train_lasso)

# 4. Skapa designmatris för testdata med samma struktur som träningsdata
df_test_mm <- model.matrix(Försäljningspris ~ ., data = df_test)
df_test_lasso <- as.data.frame(df_test_mm[, -1])
df_test_lasso$Försäljningspris <- df_test$Försäljningspris

# 5. Säkerställ att testdatan har samma kolumner som träningsdatan
missing_cols <- setdiff(colnames(df_train_lasso), colnames(df_test_lasso))
for (col in missing_cols) {
  df_test_lasso[[col]] <- 0
}
df_test_lasso <- df_test_lasso[, colnames(df_train_lasso)]

# 6. Extrahera de första två raderna för prediktion
new_data <- df_test_lasso[1:2, ]

# 7. Utför prediktion och intervallberäkning
confidence_intervals <- predict(lm_3_ols, newdata = new_data, interval = "confidence", level = 0.95)
prediction_intervals <- predict(lm_3_ols, newdata = new_data, interval = "prediction", level = 0.95)

# 8. Visa intervallen
cat("\nKonfidensintervall:\n")
print(confidence_intervals)

cat("\nPrediktionsintervall:\n")
print(prediction_intervals)

