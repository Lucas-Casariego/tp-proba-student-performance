# ==============================================================================
# ANÁLISIS DE RENDIMIENTO ACADÉMICO: HÁBITOS DE ESTUDIO Y SUEÑO
# ==============================================================================
# Variables:
#   Cuantitativas : study_hours_per_day, sleep_hours
#   Cualitativas  : parent_education (High School / Bachelor / Master / PhD)
#                   grade (F / D / C / B / A)
# ==============================================================================

if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("El paquete 'dplyr' no está instalado. Instálalo antes de ejecutar este script.")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("El paquete 'ggplot2' no está instalado. Instálalo antes de ejecutar este script.")
}

library(dplyr)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1. CARGA Y PREPARACIÓN DE DATOS
# ------------------------------------------------------------------------------

csv_students <- read.csv("student_performance_data.csv")

df <- csv_students[, c("study_hours_per_day", "sleep_hours", "parent_education", "grade")]

# Convertir a factores ordenados
df$grade <- factor(df$grade,
                   levels = c("F", "D", "C", "B", "A"), ordered = TRUE)

df$parent_education <- factor(df$parent_education,
                              levels = c("High School", "Bachelor", "Master", "PhD"), ordered = TRUE)

# Variable derivada: aprobación (A / B / C = Aprobado; D / F = Desaprobado)
df$aprobado <- ifelse(df$grade %in% c("A", "B", "C", "D"),
                      "Aprobado",
                      "Desaprobado")

cat("N =", nrow(df), "\n")

# ------------------------------------------------------------------------------
# 2. ANÁLISIS DESCRIPTIVO INDIVIDUAL DE LAS 4 VARIABLES
# ------------------------------------------------------------------------------

cat("\n========== ESTADÍSTICOS DESCRIPTIVOS ==========\n")

cat("\n--- 1. Horas de Estudio por Día (study_hours_per_day) ---\n")

print(summary(df$study_hours_per_day))

cat("Desviación estándar:", round(sd(df$study_hours_per_day), 4), "\n")
cat("Varianza:           ", round(var(df$study_hours_per_day), 4), "\n")


cat("\n--- 2. Horas de Sueño por Día (sleep_hours) ---\n")

print(summary(df$sleep_hours))

cat("Desviación estándar:", round(sd(df$sleep_hours), 4), "\n")
cat("Varianza:           ", round(var(df$sleep_hours), 4), "\n")


cat("\n--- 3. Nivel Educativo de los Padres (parent_education) ---\n")

freq_pe <- table(df$parent_education)
print(freq_pe)

cat("Porcentajes:\n")
print(round(prop.table(freq_pe) * 100, 2))


cat("\n--- 4. Calificación (grade) ---\n")

freq_gr <- table(df$grade)
print(freq_gr)

cat("Porcentajes:\n")
print(round(prop.table(freq_gr) * 100, 2))

# ------------------------------------------------------------------------------
# 3. UMBRALES Y CUADRANTES (base del análisis principal)
# Se utilizan las medianas para separar "poco" de "mucho" en cada variable
# continua, permitiendo comparar grupos con criterio reproducible.
# ------------------------------------------------------------------------------

mediana_estudio <- median(df$study_hours_per_day)   # ~5.42 h

mediana_sueno   <- median(df$sleep_hours)            # ~6.52 h

cat("\n\nMediana estudio:", mediana_estudio, "h/día  |  Mediana sueño:", mediana_sueno, "h/día\n")

df <- df %>%
  mutate(
    grupo_estudio = ifelse(study_hours_per_day < mediana_estudio,
                           "Poco estudio", "Mucho estudio"),
    grupo_sueno   = ifelse(sleep_hours < mediana_sueno,
                           "Poco sueño", "Mucho sueño"),
    cuadrante     = paste0(grupo_estudio, " + ", grupo_sueno)
  )

# ------------------------------------------------------------------------------
# 4. ANÁLISIS DE LOS 6 PARES DE VARIABLES
# ------------------------------------------------------------------------------

cat("\n========== ANÁLISIS POR PARES ==========\n")

# Par 1: study_hours_per_day × grade  (cuantitativa × cualitativa)
cat("\n--- study_hours_per_day × grade ---\n")

resumen_estudio_x_grade <- df %>%
  group_by(grade) %>%
  summarise(n       = n(),
            media   = round(mean(study_hours_per_day), 2),
            mediana = round(median(study_hours_per_day), 2),
            sd      = round(sd(study_hours_per_day), 2),
            .groups = "drop")

print(resumen_estudio_x_grade)


# Par 2: sleep_hours × grade  (cuantitativa × cualitativa)
cat("\n--- sleep_hours × grade ---\n")

resumen_sueno_x_grade <- df %>%
  group_by(grade) %>%
  summarise(n       = n(),
            media   = round(mean(sleep_hours), 2),
            mediana = round(median(sleep_hours), 2),
            sd      = round(sd(sleep_hours), 2),
            .groups = "drop")

print(resumen_sueno_x_grade)


# Par 3: study_hours_per_day × parent_education  (cuantitativa × cualitativa)
cat("\n--- study_hours_per_day × parent_education ---\n")

resumen_estudio_x_pe <- df %>%
  group_by(parent_education) %>%
  summarise(n       = n(),
            media   = round(mean(study_hours_per_day), 2),
            mediana = round(median(study_hours_per_day), 2),
            sd      = round(sd(study_hours_per_day), 2),
            .groups = "drop")

print(resumen_estudio_x_pe)


# Par 4: sleep_hours × parent_education  (cuantitativa × cualitativa)
cat("\n--- sleep_hours × parent_education ---\n")

resumen_sueno_x_pe <- df %>%
  group_by(parent_education) %>%
  summarise(n       = n(),
            media   = round(mean(sleep_hours), 2),
            mediana = round(median(sleep_hours), 2),
            sd      = round(sd(sleep_hours), 2),
            .groups = "drop")

print(resumen_sueno_x_pe)


# Par 5: grade × parent_education  (cualitativa × cualitativa)
cat("\n--- grade × parent_education ---\n")
tabla_gxpe <- table(df$parent_education, df$grade)

cat("Frecuencias absolutas:\n")
print(tabla_gxpe)

cat("\nPorcentajes por nivel educativo (fila):\n")
print(round(prop.table(tabla_gxpe, margin = 1) * 100, 1))


# Par 6: study_hours_per_day × sleep_hours  (cuantitativa × cuantitativa)
cat("\n--- study_hours_per_day × sleep_hours ---\n")

cat("Correlación de Pearson:",
    round(cor(df$study_hours_per_day, df$sleep_hours), 4), "\n")

# Nota: valor cercano a 0 implica que estudiar más no necesariamente
# se asocia con dormir menos (o viceversa) a nivel global.

# ------------------------------------------------------------------------------
# 5. ANÁLISIS PRINCIPAL: CUADRANTES  ESTUDIO × SUEÑO → RENDIMIENTO
#
# Pregunta central: entre quienes estudian POCO, ¿trasnochar (dormir poco)
# compensa el déficit de estudio? ¿Y en quienes estudian MUCHO, afecta?
# ------------------------------------------------------------------------------

cat("\n========== ANÁLISIS DE CUADRANTES ==========\n")

cat("Umbral estudio:", mediana_estudio, "h  |  Umbral sueño:", mediana_sueno, "h\n\n")

resumen_cuadrantes <- df %>%
  group_by(cuadrante) %>%
  summarise(
    n              = n(),
    pct_total      = round(n() / nrow(df) * 100, 1),
    pct_aprobado   = round(mean(aprobado == "Aprobado") * 100, 1),
    pct_A_o_B      = round(mean(grade %in% c("A", "B")) * 100, 1),
    media_estudio  = round(mean(study_hours_per_day), 2),
    media_sueno    = round(mean(sleep_hours), 2),
    .groups        = "drop"
  ) %>%
  arrange(desc(pct_aprobado))

print(resumen_cuadrantes)


# --- Foco 1: entre los que estudian POCO ---
cat("\n--- Entre quienes estudian POCO (< mediana): ¿dormir poco ayuda? ---\n")

poco_estudio <- df %>% filter(grupo_estudio == "Poco estudio")

print(poco_estudio %>%
        group_by(grupo_sueno) %>%
        summarise(
          n            = n(),
          pct_aprobado = round(mean(aprobado == "Aprobado") * 100, 1),
          pct_A_o_B    = round(mean(grade %in% c("A", "B")) * 100, 1),
          media_grade_num = round(mean(as.numeric(grade)), 2),  # 1=F … 5=A
          .groups      = "drop"
        ))


# --- Foco 2: entre los que estudian MUCHO ---
cat("\n--- Entre quienes estudian MUCHO (>= mediana): ¿dormir poco afecta? ---\n")

mucho_estudio <- df %>% filter(grupo_estudio == "Mucho estudio")

print(mucho_estudio %>%
        group_by(grupo_sueno) %>%
        summarise(
          n            = n(),
          pct_aprobado = round(mean(aprobado == "Aprobado") * 100, 1),
          pct_A_o_B    = round(mean(grade %in% c("A", "B")) * 100, 1),
          media_grade_num = round(mean(as.numeric(grade)), 2),
          .groups      = "drop"
        ))


# --- Distribución completa de grades por cuadrante ---
cat("\n--- Distribución de grades por cuadrante ---\n")

tab_cuad_grade <- df %>%
  group_by(cuadrante, grade) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cuadrante) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(cuadrante, grade)

print(tab_cuad_grade, n = Inf)

# ------------------------------------------------------------------------------
# 6. VISUALIZACIONES
# ------------------------------------------------------------------------------

paleta_grade <- c("F" = "#d62728", "D" = "#ff7f0e",
                  "C" = "#bcbd22", "B" = "#2ca02c", "A" = "#1f77b4")

# ── Histogramas individuales ──────────────────────────────────────────────────

p_hist_study <- ggplot(df, aes(x = study_hours_per_day)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  geom_vline(xintercept = mediana_estudio, linetype = "dashed",
             color = "red", linewidth = 0.9) +
  annotate("text", x = mediana_estudio + 0.4, y = Inf, vjust = 2,
           label = paste("Mediana:", mediana_estudio, "h"), color = "red", size = 3.5) +
  labs(title = "Distribución de Horas de Estudio Diarias",
       x = "Horas de estudio / día", y = "Frecuencia") +
  theme_minimal()

print(p_hist_study)


p_hist_sleep <- ggplot(df, aes(x = sleep_hours)) +
  geom_histogram(binwidth = 0.5, fill = "mediumpurple", color = "white") +
  geom_vline(xintercept = mediana_sueno, linetype = "dashed",
             color = "red", linewidth = 0.9) +
  annotate("text", x = mediana_sueno + 0.4, y = Inf, vjust = 2,
           label = paste("Mediana:", mediana_sueno, "h"), color = "red", size = 3.5) +
  labs(title = "Distribución de Horas de Sueño Diarias",
       x = "Horas de sueño / día", y = "Frecuencia") +
  theme_minimal()

print(p_hist_sleep)

# ── Barras de variables cualitativas ─────────────────────────────────────────

p_bar_grade <- ggplot(df, aes(x = grade, fill = grade)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = paleta_grade) +
  labs(title = "Distribución de Calificaciones",
       x = "Calificación", y = "Frecuencia") +
  theme_minimal() + theme(legend.position = "none")

print(p_bar_grade)


p_bar_pe <- ggplot(df, aes(x = parent_education, fill = parent_education)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.4, size = 3.5) +
  labs(title = "Distribución del Nivel Educativo de los Padres",
       x = "Nivel educativo", y = "Frecuencia") +
  theme_minimal() + theme(legend.position = "none")

print(p_bar_pe)

# ── Pares de variables ────────────────────────────────────────────────────────

# Par 1: study_hours × grade
p1 <- ggplot(df, aes(x = grade, y = study_hours_per_day, fill = grade)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta_grade) +
  labs(title = "Horas de Estudio según Calificación",
       x = "Calificación", y = "Horas de estudio / día") +
  theme_minimal() + theme(legend.position = "none")

print(p1)


# Par 2: sleep_hours × grade
p2 <- ggplot(df, aes(x = grade, y = sleep_hours, fill = grade)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta_grade) +
  labs(title = "Horas de Sueño según Calificación",
       x = "Calificación", y = "Horas de sueño / día") +
  theme_minimal() + theme(legend.position = "none")

print(p2)


# Par 3: study_hours × parent_education
p3 <- ggplot(df, aes(x = parent_education, y = study_hours_per_day,
                     fill = parent_education)) +
  geom_boxplot() +
  labs(title = "Horas de Estudio según Nivel Educativo de los Padres",
       x = "Nivel educativo de los padres", y = "Horas de estudio / día") +
  theme_minimal() + theme(legend.position = "none")

print(p3)


# Par 4: sleep_hours × parent_education
p4 <- ggplot(df, aes(x = parent_education, y = sleep_hours,
                     fill = parent_education)) +
  geom_boxplot() +
  labs(title = "Horas de Sueño según Nivel Educativo de los Padres",
       x = "Nivel educativo de los padres", y = "Horas de sueño / día") +
  theme_minimal() + theme(legend.position = "none")

print(p4)


# Par 5: grade × parent_education
p5 <- ggplot(df, aes(x = parent_education, fill = grade)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = paleta_grade) +
  labs(title = "Distribución de Calificaciones por Nivel Educativo de los Padres",
       x = "Nivel educativo de los padres", y = "Proporción", fill = "Nota") +
  theme_minimal()

print(p5)


# Par 6: study_hours × sleep_hours (scatter, color = grade)
p6 <- ggplot(df, aes(x = study_hours_per_day, y = sleep_hours, color = grade)) +
  geom_point(alpha = 0.25, size = 1.2) +
  geom_vline(xintercept = mediana_estudio, linetype = "dashed",
             color = "gray40", linewidth = 0.7) +
  geom_hline(yintercept = mediana_sueno, linetype = "dashed",
             color = "gray40", linewidth = 0.7) +
  scale_color_manual(values = paleta_grade) +
  labs(title = "Horas de Estudio vs. Horas de Sueño",
       subtitle = "Líneas punteadas = medianas | Color = calificación obtenida",
       x = "Horas de estudio / día", y = "Horas de sueño / día", color = "Nota") +
  theme_minimal()

print(p6)

# ── Análisis de cuadrantes ────────────────────────────────────────────────────

# Tasa de aprobación por cuadrante (barras horizontales)
resumen_cuad_apro <- df %>%
  group_by(cuadrante, aprobado) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cuadrante) %>%
  mutate(pct = n / sum(n) * 100)

p_cuad_apro <- ggplot(resumen_cuad_apro,
                      aes(x = reorder(cuadrante, pct * (aprobado == "Aprobado")),
                          y = pct, fill = aprobado)) +
  geom_col() +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Aprobado" = "#2ca02c", "Desaprobado" = "#d62728")) +
  coord_flip() +
  labs(title = "Tasa de Aprobación por Cuadrante (Estudio × Sueño)",
       x = NULL, y = "Porcentaje", fill = "Estado") +
  theme_minimal()

print(p_cuad_apro)


# Distribución completa de grades por cuadrante
p_cuad_grade <- ggplot(df, aes(x = cuadrante, fill = grade)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = paleta_grade) +
  coord_flip() +
  labs(title = "Distribución de Calificaciones por Cuadrante",
       x = NULL, y = "Proporción", fill = "Nota") +
  theme_minimal()

print(p_cuad_grade)


# Comparativa dentro de "Poco estudio": ¿trasnochar ayuda?
poco_df <- df %>% filter(grupo_estudio == "Poco estudio")

p_poco_sueno <- ggplot(poco_df, aes(x = grupo_sueno, fill = grade)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = paleta_grade) +
  labs(title = "Calificaciones en el grupo de POCO ESTUDIO",
       subtitle = paste0("¿Trasnochar (< ", mediana_sueno,
                         " h de sueño) compensa haber estudiado poco?"),
       x = NULL, y = "Proporción", fill = "Nota") +
  theme_minimal()

print(p_poco_sueno)


# Comparativa dentro de "Mucho estudio": ¿trasnochar afecta?
mucho_df <- df %>% filter(grupo_estudio == "Mucho estudio")

p_mucho_sueno <- ggplot(mucho_df, aes(x = grupo_sueno, fill = grade)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = paleta_grade) +
  labs(title = "Calificaciones en el grupo de MUCHO ESTUDIO",
       subtitle = paste0("¿Trasnochar (< ", mediana_sueno,
                         " h de sueño) perjudica a quien estudia mucho?"),
       x = NULL, y = "Proporción", fill = "Nota") +
  theme_minimal()

print(p_mucho_sueno)
