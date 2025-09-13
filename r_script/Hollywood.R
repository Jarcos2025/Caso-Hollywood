library(readxl)
Hollywood <- read_excel("Desktop/Hollywood/Hollywood.xls",  sheet = "Exhibit 1")
View(Hollywood)                                                                          

names(Hollywood) <- make.names(names(Hollywood))

filtered_hollywood <- subset(Hollywood, Budget >= 20000000 & Budget <= 100000000)

filtered_hollywood$ROI_US <- (filtered_hollywood$Total.U.S..Gross - filtered_hollywood$Budget) / filtered_hollywood$Budget

filtered_hollywood$Is_Comedy <- ifelse(filtered_hollywood$Genre == "Comedy", 1, 0)

print(paste("Número de películas en el análisis:", nrow(filtered_hollywood)))

#PUNTO 1
summary_stats <- summary(filtered_hollywood[, c("Opening.Gross", "Total.U.S..Gross", "Total.Non.U.S..Gross", "Opening.Theatres")])
print(summary_stats)

comedy_count <- sum(filtered_hollywood$Is_Comedy)
print(paste("Número de comedias:", comedy_count))

r_rated_count <- sum(filtered_hollywood$MPAA_D)
print(paste("Número de películas con clasificación R:", r_rated_count))

#PUNTO 2
# a.

# b.
roi_test <- t.test(filtered_hollywood$ROI_US)
print(roi_test$conf.int)

# c.
roi_hypothesis_test <- t.test(filtered_hollywood$ROI_US, mu = 0.12, alternative = "greater")
print(roi_hypothesis_test)

# PUNTO 3
# a. 
gross_genre_test <- t.test(Total.U.S..Gross ~ Is_Comedy, data = filtered_hollywood)
print(gross_genre_test)

# b. 
roi_genre_test <- t.test(ROI_US ~ Is_Comedy, data = filtered_hollywood)
print(roi_genre_test)

# PUNTO 4
rating_test <- t.test(Total.U.S..Gross ~ MPAA_D, data = filtered_hollywood)
print(rating_test)

# PUNTO 5
# a.
model_pre_prod_full <- lm(Total.U.S..Gross ~ Budget + Is_Comedy + MPAA_D + Sequel + Known.Story, data = filtered_hollywood)
summary(model_pre_prod_full)

# b.
model_pre_prod_final <- lm(Total.U.S..Gross ~ Budget + Sequel, data = filtered_hollywood)
summary(model_pre_prod_final)

# PUNTO 6
# a.
model_opening_full <- lm(Opening.Gross ~ Budget + Is_Comedy + MPAA_D + Sequel + Known.Story + Summer + Holiday + Christmas + Opening.Theatres, data = filtered_hollywood)
summary(model_opening_full)

# b.
model_opening_final <- lm(Opening.Gross ~ Budget + Sequel + Summer + Opening.Theatres, data = filtered_hollywood)
summary(model_opening_final)

# d. 
point_estimate_change <- 100 * coef(model_opening_final)["Opening.Theatres"]
ci_change <- 100 * confint(model_opening_final, "Opening.Theatres", level = 0.95)

print(paste("Estimación puntual del cambio por 100 cines:", round(point_estimate_change)))
print("Intervalo de confianza del 95% para el cambio:")
print(ci_change)

# PUNTO 7
# a.
model_wisdom <- lm(Total.U.S..Gross ~ Opening.Gross, data = filtered_hollywood)
summary(model_wisdom)

# c & f
b1 <- coef(model_wisdom)[2]
se_b1 <- summary(model_wisdom)$coefficients[2, 2]
t_stat <- (b1 - 4) / se_b1
p_value <- 2 * pt(abs(t_stat), df = df.residual(model_wisdom), lower.tail = FALSE)
print(paste("P-value para la hipótesis de que la pendiente es 4:", p_value))

# g.
r_squared <- summary(model_wisdom)$r.squared
print(paste("R-cuadrado:", r_squared))

# PUNTO 8
# a & b
model_after_opening_final <- lm(Total.U.S..Gross ~ Opening.Gross + Critics..Opinion, data = filtered_hollywood)
summary(model_after_opening_final)

# c.
flags_data <- subset(filtered_hollywood, Movie == "Flags of Our Fathers")
prediction <- predict(model_after_opening_final, newdata = flags_data, interval = "prediction", level = 0.95)
print("Predicción para 'Flags of Our Fathers':")
print(prediction)

# d. 
value_of_critics <- 10 * coef(model_after_opening_final)["Critics.Opinion"]
print(paste("Valor de 10 puntos de críticos en taquilla:", round(value_of_critics)))

# PUNTO 9
model_interaction <- lm(Total.U.S..Gross ~ Opening.Gross + Critics..Opinion * Is_Comedy, data = filtered_hollywood)
summary(model_interaction)
