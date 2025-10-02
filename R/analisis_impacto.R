
# ============================================================
# PASO 2 — VALIDACIÓN Y EXPLORACIÓN (tablas + gráficas)
# ============================================================

# --- Paquetes necesarios (auto-instala si faltan) ---
pkgs <- c("dplyr","tidyr","ggplot2","knitr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, repos = "https://cran.r-project.org")
library(dplyr); library(tidyr); library(ggplot2); library(knitr)

# --- Cargar data si no está en memoria ---
if (!exists("panel")) {
  if (!file.exists("data/impact_dataset.csv"))
    stop("No encuentro data/impact_dataset.csv. Genera la base primero.")
  panel <- read.csv("data/impact_dataset.csv")
}


## -----------------------
# 1) Chequeos estructurales
# -----------------------
cat("\n[Chequeos] Cada id debe tener 2 filas (pre & post):\n")
print(panel %>% count(id) %>% summarise(min=min(n), max=max(n)))

cat("\n[Chequeos] Tratamiento sólo en post (post=0 → D debe ser 0):\n")
print(table(panel$post, panel$D))
if (all(panel$D[panel$post==0] == 0)) cat("OK: D==0 en pre.\n") else cat("Atención: hay D=1 en pre.\n")

cat("\n[Chequeos] RCT: assign_rct solo definido si arm_rct==1:\n")
print(with(panel, table(arm_rct, is.na(assign_rct))))

cat("\n[Chequeos] DiD: group_did constante por cluster:\n")
print(
  panel %>%
    group_by(cluster_id) %>%
    summarise(n_groups = n_distinct(group_did), .groups="drop") %>%
    summarise(max_n_groups = max(n_groups))
)

cat("\n[Chequeos] RDD: elegible_rdd == 1(rdd_score>=0):\n")
print(all(panel$eligible_rdd == as.integer(panel$rdd_score >= 0)))
#-----------------------
# 2) Tablas de resumen
# -----------------------

# 2.1 Resumen por periodo
tab_periodo <- panel %>%
  group_by(post) %>%
  summarise(n = n(),
            y_mean = mean(y, na.rm=TRUE),
            y_sd   = sd(y, na.rm=TRUE),
            D_mean = mean(D, na.rm=TRUE),
            .groups = "drop")
cat("\nTabla — Resumen por periodo (pre=0, post=1):\n")
print(kable(tab_periodo, digits = 3))

# 2.2 Balance en RCT (pre) por asignación — FIX robusto a nombres con "_"
rct_pre  <- subset(panel, arm_rct==1 & post==0)
num_vars <- c("age","female","ses","urban","baseline_ability","dist_km")
bal_tab <- rct_pre %>%
  summarise(across(
    all_of(num_vars),
    list(
      mean_t = ~mean(.x[assign_rct==1], na.rm=TRUE),
      mean_c = ~mean(.x[assign_rct==0], na.rm=TRUE),
      sd_t   = ~sd(.x[assign_rct==1],   na.rm=TRUE),
      sd_c   = ~sd(.x[assign_rct==0],   na.rm=TRUE)
    )
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("var","stat"),
    names_pattern = "^(.*)_(mean_t|mean_c|sd_t|sd_c)$",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    sd_pooled = sqrt((sd_t^2 + sd_c^2)/2),
    std_diff  = (mean_t - mean_c) / sd_pooled
  ) %>%
  select(var, mean_t, mean_c, std_diff)

cat("\nTabla — Balance pre (brazo RCT): diferencias estandarizadas:\n")
print(kable(bal_tab, digits=3))

# 2.3 Medias pre/post por grupo DiD
tab_did <- panel %>%
  group_by(group_did, post) %>%
  summarise(y_mean = mean(y, na.rm=TRUE),
            D_mean = mean(D, na.rm=TRUE), .groups="drop")
cat("\nTabla — Medias por grupo (DiD):\n")
print(kable(tab_did, digits=3))

# -----------------------
# 3) Gráficas de validación
# -----------------------

# 3.1 Distribución de y por periodo
print(
  ggplot(panel, aes(x=y, fill=factor(post))) +
    geom_density(alpha=.4) +
    labs(title="Distribución del puntaje (y) por periodo",
         x="y (puntaje)", fill="post (0=pre,1=post)")
)

# 3.2 Medias y CI por grupo DiD (pre/post)
sum_did <- panel %>%
  group_by(group_did, post) %>%
  summarise(n=n(), y_mean=mean(y, na.rm=TRUE), y_sd=sd(y, na.rm=TRUE), .groups="drop") %>%
  mutate(se = y_sd/sqrt(n),
         lwr = y_mean - 1.96*se,
         upr = y_mean + 1.96*se)
print(
  ggplot(sum_did, aes(x=factor(post), y=y_mean, group=factor(group_did), color=factor(group_did))) +
    geom_point(size=2) +
    geom_line() +
    geom_errorbar(aes(ymin=lwr, ymax=upr), width=.12) +
    labs(title="Medias de y (IC 95%) por grupo (DiD)",
         x="Periodo (0=pre, 1=post)", y="Media de y", color="group_did")
)

# 3.3 Relevancia del instrumento (tasa de D por Z_encourage, post)
iv_post <- subset(panel, post==1)
sum_iv <- iv_post %>%
  group_by(Z_encourage) %>%
  summarise(n=n(),
            D_mean=mean(D, na.rm=TRUE),
            se=sqrt(pmax(D_mean*(1-D_mean)/n, 0)),
            lwr = pmax(D_mean - 1.96*se, 0),
            upr = pmin(D_mean + 1.96*se, 1),
            .groups="drop")
print(
  ggplot(sum_iv, aes(x=factor(Z_encourage), y=D_mean)) +
    geom_col() +
    geom_errorbar(aes(ymin=lwr, ymax=upr), width=.15) +
    labs(title="Tasa de tratamiento por instrumento (post)",
         x="Z_encourage (0/1)", y="Pr(D=1)")
)

# 3.4 Vista RDD (y vs rdd_score) cerca del corte
rdd_post <- subset(panel, post==1 & abs(rdd_score) <= 10)
print(
  ggplot(rdd_post, aes(x=rdd_score, y=y)) +
    geom_point(alpha=.3) +
    geom_smooth(method="loess", span=.5, se=FALSE) +
    geom_vline(xintercept=0, linetype="dashed") +
    labs(title="RDD — y vs rdd_score (±10 alrededor del corte)",
         x="rdd_score (corte=0)", y="y")
)

cat("\n✅ Validación y exploración completadas.\n")

# ============================================================
# PASO 3 — RCT/ITT y LATE (2SLS)
# - ITT: efecto de SER ASIGNADO a tutorías (post × assign_rct)
# - LATE/TOT: efecto local entre cumplidores usando 2SLS con assign_rct
# ============================================================

# Paquetes
pkgs <- c("dplyr","broom","AER","lmtest","sandwich","ggplot2","knitr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)>0) install.packages(to_install, repos="https://cran.r-project.org")
library(dplyr); library(broom); library(AER); library(lmtest); library(sandwich); library(ggplot2); library(knitr)

# --- Submuestra del brazo experimental (donde hubo aleatorización) ---
rct <- subset(panel, arm_rct == 1)

# --- (A) Chequeos rápidos RCT ---
# Cumplimiento: Pr(D=1 | assign_rct) en post
comp_tab <- rct %>%
  filter(post==1) %>%
  group_by(assign_rct) %>%
  summarise(n=n(),
            takeup = mean(D),
            .groups="drop")
kable(comp_tab, digits=3, caption="Cumplimiento en post (brazo RCT)")

# --- (B) ITT: efecto de ASIGNACIÓN (post × assign_rct) ---
m_itt <- lm(
  y ~ post*assign_rct + age + female + ses + urban + baseline_ability,
  data = rct
)

# SE robustos por cluster
V_itt <- sandwich::vcovCL(m_itt, cluster = rct$cluster_id)
itt_tab <- broom::tidy(m_itt, conf.int = TRUE, vcov = V_itt)

kable(itt_tab, digits=3, caption="ITT (SE robustos y IC 95%)")
cat("\nInterpretación: el coeficiente de 'post:assign_rct' es el ITT.\n")

# Gráfico de medias pre/post por asignación (visual de ITT)
sum_itt <- rct %>%
  group_by(post, assign_rct) %>%
  summarise(n=n(), y_mean=mean(y), se=sd(y)/sqrt(n()), .groups="drop") %>%
  mutate(lwr=y_mean-1.96*se, upr=y_mean+1.96*se)

print(
  ggplot(sum_itt, aes(x=factor(post), y=y_mean, group=factor(assign_rct), color=factor(assign_rct))) +
    geom_point(size=2) + geom_line() +
    geom_errorbar(aes(ymin=lwr, ymax=upr), width=.12) +
    labs(title="RCT/ITT — Medias de y por asignación",
         x="Periodo (0=pre, 1=post)", y="Media de y", color="assign_rct")
)

# --- (C) LATE / TOT con 2SLS (solo periodo post) ---
rct_post <- subset(rct, post==1)

# Primera etapa: D ~ assign_rct + X
first_stage <- lm(D ~ assign_rct + age + female + ses + urban + baseline_ability, data = rct_post)
fs_sum <- summary(first_stage)
t_assign <- fs_sum$coefficients["assign_rct","t value"]
F_weak   <- as.numeric(t_assign^2)  # F de relevancia para un solo instrumento
cat(sprintf("\nPrimera etapa — F(assign_rct): %.2f (regla práctica: >10)\n", F_weak))

# Segunda etapa (2SLS) / IV: y ~ D + X | assign_rct + X
iv_fit <- AER::ivreg(
  y ~ D + age + female + ses + urban + baseline_ability |
    assign_rct + age + female + ses + urban + baseline_ability,
  data = rct_post
)

# SE robustos por cluster
V_iv <- sandwich::vcovCL(iv_fit, cluster = rct_post$cluster_id)
iv_tab <- broom::tidy(iv_fit, conf.int = TRUE, vcov = V_iv)

kable(iv_tab, digits=3, caption="LATE / TOT con 2SLS (SE robustos por cluster)")
cat("\nInterpretación: el coeficiente de 'D' es el LATE/TOT entre cumplidores.\n")

# Relación ITT ≈ LATE × (1a etapa)
itt_coef <- itt_tab$estimate[itt_tab$term=="post:assign_rct"]
pi_hat   <- coef(first_stage)["assign_rct"]
cat(sprintf("\nComprobación (aprox): ITT ≈ LATE × Pr(D=1|Z=1)−Pr(D=1|Z=0)\nITT estimado: %.3f | 1a etapa (π̂): %.3f | LATE ≈ ITT/π̂: %.3f\n",
            itt_coef, pi_hat, itt_coef/pi_hat))

# ============================================================
# PASO 4 — Diferencias en Diferencias (DiD)
# Idea: comparar el cambio pre→post entre clusters con rollout (group_did=1)
#       y sin rollout (group_did=0). Reportamos SE robustos por cluster.
# ============================================================

# Paquetes
pkgs <- c("dplyr","broom","lmtest","sandwich","ggplot2","knitr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)>0) install.packages(to_install, repos="https://cran.r-project.org")
library(dplyr); library(broom); library(lmtest); library(sandwich); library(ggplot2); library(knitr)

# --- (A) Resumen y gráfico de medias (diagnóstico visual) ---
sum_did <- panel %>%
  group_by(group_did, post) %>%
  summarise(n=n(),
            y_mean=mean(y, na.rm=TRUE),
            se=sd(y, na.rm=TRUE)/sqrt(n),
            .groups="drop") %>%
  mutate(lwr=y_mean-1.96*se, upr=y_mean+1.96*se)

kable(sum_did, digits=3, caption="Medias de y por grupo (DiD)")

print(
  ggplot(sum_did, aes(x=factor(post), y=y_mean, group=factor(group_did), color=factor(group_did))) +
    geom_point(size=2) + geom_line() +
    geom_errorbar(aes(ymin=lwr, ymax=upr), width=.12) +
    labs(title="DiD — Medias de y (IC95%) por grupo",
         x="Periodo (0=pre, 1=post)", y="Media de y", color="group_did")
)

# --- (B) DiD básico (sin covariables) ---
did0 <- lm(y ~ post*group_did, data = panel)
did0_rob <- coeftest(did0, vcov = vcovCL(did0, cluster = ~ cluster_id))
print(did0_rob)
cat("\nEl coeficiente de 'post:group_did' es el efecto DiD básico.\n")

# --- (C) DiD con covariables (mejor precisión) ---
did1 <- lm(y ~ post*group_did + age + female + ses + urban + baseline_ability + dist_km + factor(region),
           data = panel)
V1 <- vcovCL(did1, cluster = ~ cluster_id)
did1_tab <- tidy(did1, vcov = V1, conf.int = TRUE)
kable(did1_tab, digits=3, caption="DiD con covariables (SE robustos por cluster)")
did_effect <- did1_tab$estimate[did1_tab$term=="post:group_did"]
did_ci_l   <- did1_tab$conf.low[did1_tab$term=="post:group_did"]
did_ci_u   <- did1_tab$conf.high[did1_tab$term=="post:group_did"]
cat(sprintf("\nEfecto DiD (con covariables): %.3f  [IC95%%: %.3f, %.3f]\n", did_effect, did_ci_l, did_ci_u))

# --- (D) TWFE (Two-Way Fixed Effects) equivalente con 2 periodos ---
# Incluye efectos fijos por individuo y por tiempo.
did_fe <- lm(y ~ group_did:post + factor(id) + factor(post), data = panel)
did_fe_rob <- coeftest(did_fe, vcov = vcovCL(did_fe, cluster = ~ cluster_id))
print(did_fe_rob)
cat("\nEn 2 periodos, el coeficiente de 'group_did:post' coincide conceptualmente con DiD.\n")

# --- (E) Chequeo de niveles en pre (no es prueba de 'paralelas', pero da contexto) ---
pre_levels <- subset(panel, post==0) %>%
  group_by(group_did) %>%
  summarise(y_mean=mean(y, na.rm=TRUE), sd=sd(y, na.rm=TRUE), n=n(), .groups="drop")
kable(pre_levels, digits=3, caption="Niveles de y en pre por grupo (contexto)")

# ============================================================
# PASO 5 — Propensity Score Matching (PSM) — ATT en post (FIX NA)
# Imputa NA en ses/dist_km y añade dummies de missing para no sesgar.
# ============================================================

# Paquetes
pkgs <- c("dplyr","MatchIt","broom","knitr","ggplot2","sandwich","lmtest")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)>0) install.packages(to_install, repos="https://cran.r-project.org")
library(dplyr); library(MatchIt); library(broom); library(knitr); library(ggplot2); library(sandwich); library(lmtest)
suppressWarnings({ has_cobalt <- requireNamespace("cobalt", quietly = TRUE) })

# --- (A) Preparar datos (solo post) con imputación simple ---
post_data <- subset(panel, post==1) %>%
  mutate(
    region = factor(region),
    # imputaciones
    ses_imp     = ifelse(is.na(ses),      round(median(ses, na.rm=TRUE)), ses),
    dist_km_imp = ifelse(is.na(dist_km),        median(dist_km, na.rm=TRUE), dist_km),
    # indicadores de missing
    ses_miss    = as.integer(is.na(ses)),
    dist_miss   = as.integer(is.na(dist_km))
  )

# Fórmula del puntaje de propensión (incluye dummies de missing)
form_ps <- D ~ age + female + ses_imp + ses_miss + urban +
  baseline_ability + dist_km_imp + dist_miss + region

# --- (B) Matching: vecino más cercano, sin reemplazo, caliper 0.2 ---
m.out <- matchit(form_ps,
                 data     = post_data,
                 method   = "nearest",
                 ratio    = 1,
                 replace  = FALSE,
                 caliper  = 0.2,        # ≈ 0.2 SD del logit
                 estimand = "ATT")

# Resumen de balance antes/después
sum_m <- summary(m.out)
print(sum_m)

# Gráfico de balance (si 'cobalt' está disponible)
if (has_cobalt) {
  cobalt::love.plot(m.out,
                    thresholds = c(m = .1),
                    var.order  = "unadjusted",
                    abs        = TRUE,
                    title      = "PSM — Diferencias estandarizadas (antes vs después)")
} else {
  message("Tip: instala 'cobalt' para el gráfico de balance: install.packages('cobalt')")
}

# --- (C) Diagnóstico de solapamiento ---
ps_df <- data.frame(D = post_data$D, ps = m.out$distance)
print(
  ggplot(ps_df, aes(x = ps, fill = factor(D))) +
    geom_density(alpha = .4) +
    labs(title = "Distribución del propensity score (post)",
         x = "Propensity score", fill = "D (0/1)")
)

# --- (D) Estimar ATT en la muestra emparejada ---
md <- match.data(m.out)  # añade 'weights' y 'subclass' (parejas)
att_fit <- lm(y ~ D, data = md, weights = weights)

# Errores estándar robustos agrupados por pareja (subclass)
V_att  <- vcovCL(att_fit, cluster = md$subclass)
att_tab <- tidy(att_fit, conf.int = TRUE, vcov = V_att)
kable(att_tab, digits = 3, caption = "ATT con PSM (SE robustos agrupados por pareja)")

# Reporte limpio
att_est <- att_tab$estimate[att_tab$term == "D"]
att_lwr <- att_tab$conf.low[att_tab$term == "D"]
att_upr <- att_tab$conf.high[att_tab$term == "D"]
cat(sprintf("\nATT (PSM) = %.3f  [IC95%%: %.3f, %.3f]\n", att_est, att_lwr, att_upr))

# --- (E) Medias ponderadas de y por D en emparejados ---
post_means <- md %>%
  group_by(D) %>%
  summarise(y_mean = weighted.mean(y, w = weights),
            n      = n(),
            .groups="drop")
kable(post_means, digits=3, caption="Medias ponderadas de y por D (emparejados)")

# ============================================================
# PASO 6 — IV/2SLS con instrumento de incentivo (Z_encourage)
# Objetivo: estimar LATE/TOT usando Z como instrumento de D (periodo post)
# Salidas: relevancia, 1ª etapa, forma reducida y 2SLS (SE robustos por cluster)
# ============================================================

# Paquetes
pkgs <- c("dplyr","AER","broom","lmtest","sandwich","ggplot2","knitr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)>0) install.packages(to_install, repos="https://cran.r-project.org")
library(dplyr); library(AER); library(broom); library(lmtest); library(sandwich)
library(ggplot2); library(knitr)

# --- Datos: solo periodo post ---
iv_data <- subset(panel, post == 1) %>% mutate(region = factor(region))

# --- (A) Diagnóstico de relevancia (Pr(D=1) por Z) ---
iv_sum <- iv_data %>%
  group_by(Z_encourage) %>%
  summarise(n = n(), D_mean = mean(D, na.rm = TRUE), .groups = "drop")
kable(iv_sum, digits = 3, caption = "Pr(D=1) por instrumento (post)")
print(
  ggplot(iv_sum, aes(x = factor(Z_encourage), y = D_mean)) +
    geom_col() +
    labs(title="Relevancia del instrumento: tasa de tratamiento por Z",
         x="Z_encourage (0/1)", y="Pr(D=1)")
)

# --- (B) Primera etapa: D ~ Z + X (SE robustos por cluster) ---
fs <- lm(D ~ Z_encourage + age + female + ses + urban + baseline_ability + dist_km + region,
         data = iv_data)
V_fs  <- vcovCL(fs, cluster = ~ cluster_id)
ct_fs <- coeftest(fs, vcov = V_fs)  # matriz base

# Convertimos a data.frame estable (sin tibble) para imprimir bonito
fs_df <- data.frame(
  term        = rownames(ct_fs),
  Estimate    = ct_fs[, 1],
  `Std. Error`= ct_fs[, 2],
  `t value`   = ct_fs[, 3],
  `Pr(>|t|)`  = ct_fs[, 4],
  check.names = FALSE,
  row.names   = NULL
)
kable(fs_df, digits = 3, caption = "Primera etapa (SE robustos por cluster)")

# F de relevancia aproximada (t^2 del coeficiente de Z)
t_z <- fs_df$`t value`[fs_df$term == "Z_encourage"]
F_z <- as.numeric(t_z^2)
cat(sprintf("\nF de relevancia (aprox) para Z_encourage: %.2f  (regla práctica > 10)\n", F_z))

# --- (C) Forma reducida: y ~ Z + X (SE robustos por cluster) ---
rf <- lm(y ~ Z_encourage + age + female + ses + urban + baseline_ability + dist_km + region,
         data = iv_data)
V_rf  <- vcovCL(rf, cluster = ~ cluster_id)
ct_rf <- coeftest(rf, vcov = V_rf)
rf_df <- data.frame(
  term        = rownames(ct_rf),
  Estimate    = ct_rf[, 1],
  `Std. Error`= ct_rf[, 2],
  `t value`   = ct_rf[, 3],
  `Pr(>|t|)`  = ct_rf[, 4],
  check.names = FALSE,
  row.names   = NULL
)
kable(rf_df, digits = 3, caption = "Forma reducida (SE robustos por cluster)")

# --- (D) 2SLS / IV: y ~ D + X  |  Z + X (SE robustos por cluster) ---
iv_fit <- ivreg(
  y ~ D + age + female + ses + urban + baseline_ability + dist_km + region |
    Z_encourage + age + female + ses + urban + baseline_ability + dist_km + region,
  data = iv_data
)
V_iv   <- vcovCL(iv_fit, cluster = ~ cluster_id)
iv_tab <- tidy(iv_fit, conf.int = TRUE, vcov = V_iv)
kable(iv_tab, digits = 3, caption = "2SLS con Z_encourage (SE robustos por cluster)")
cat("\nInterpretación: el coeficiente de 'D' es el efecto causal local (LATE/TOT) entre compliers inducidos por Z.\n")

# --- (E) Identidad tipo Wald: LATE ≈ (forma reducida) / (primera etapa) ---
pi_hat  <- coef(fs)["Z_encourage"]
beta_rf <- coef(rf)["Z_encourage"]
wald_ivratio <- as.numeric(beta_rf / pi_hat)

if (is.finite(wald_ivratio)) {
  cat(sprintf("\nChequeo Wald: RF / 1ª etapa ≈ LATE  →  %.3f\n", wald_ivratio))
} else {
  cat("\nChequeo Wald: π̂ ≈ 0 (instrumento débil); no se muestra el cociente.\n")
}


# ============================================================
# PASO 7 — RDD (corte en rdd_score = 0)
# Estima un efecto local con rdrobust y hace chequeos de soporte.
# ============================================================

# Paquetes
pkgs <- c("dplyr","rdrobust","ggplot2","knitr","rddensity")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)>0) install.packages(to_install, repos="https://cran.r-project.org")
library(dplyr); library(rdrobust); library(ggplot2); library(knitr); library(rddensity)

# --- (A) Datos: periodo post y limpieza mínima ---
rdd_data <- subset(panel, post == 1) %>%
  dplyr::select(y, rdd_score, cluster_id, baseline_ability) %>%
  dplyr::filter(!is.na(y), !is.na(rdd_score))

# --- (B) Diagnóstico visual básico ---
print(
  ggplot(rdd_data, aes(x = rdd_score, y = y)) +
    geom_point(alpha = .15) +
    geom_smooth(method = "loess", span = .4, se = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(title = "RDD — y vs rdd_score (corte = 0)",
         x = "rdd_score", y = "y")
)

# --- (C) Estimación local (rdrobust) ---
rd_fit <- rdrobust(y = rdd_data$y, x = rdd_data$rdd_score, c = 0, vce = "HC1")
print(summary(rd_fit))

# Extraer estimación principal e IC95%
rd_est <- rd_fit$Estimate[1]
rd_se  <- rd_fit$se[1]
rd_lwr <- rd_est - 1.96*rd_se
rd_upr <- rd_est + 1.96*rd_se
cat(sprintf("\nRDD (rdrobust, local linear): %.3f  [IC95%%: %.3f, %.3f]\n", rd_est, rd_lwr, rd_upr))

# --- (D) Gráfico binned canónico (rdplot) ---
rdplot(y = rdd_data$y, x = rdd_data$rdd_score, c = 0,
       title = "RDD — rdplot de y alrededor del corte",
       y.label = "y", x.label = "rdd_score")

# --- (E) Chequeos rápidos (recomendado) ---

## (E1) Test de densidad (manipulación del running variable)
dens <- rddensity::rddensity(X = rdd_data$rdd_score, c = 0)
cat("\nTest de densidad (McCrary):\n")
print(summary(dens))

# IMPORTANTE: pasar los argumentos SIN nombres
rddensity::rdplotdensity(dens, rdd_data$rdd_score)

## (E2) Placebo en covariable: continuidad de 'baseline_ability' en el corte
rd_cov <- rdrobust(y = rdd_data$baseline_ability, x = rdd_data$rdd_score, c = 0, vce = "HC1")
cov_est <- rd_cov$Estimate[1]; cov_se <- rd_cov$se[1]
cat(sprintf("\nPlacebo covariable (baseline_ability) — salto en corte: %.3f  (se=%.3f)\n",
            cov_est, cov_se))


# ============================================================
# PASO 8 — Tabla comparativa de estimaciones
# Reúne: ITT (RCT), LATE (RCT-IV), DiD, ATT (PSM), IV (Z), RDD
# ============================================================

# Paquetes
pkgs <- c("dplyr","tibble","MatchIt","AER","rdrobust",
          "sandwich","lmtest","broom","ggplot2","knitr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)>0) install.packages(to_install, repos="https://cran.r-project.org")
library(dplyr); library(tibble); library(MatchIt); library(AER); library(rdrobust)
library(sandwich); library(lmtest); library(broom); library(ggplot2); library(knitr)

# ---------- ITT (RCT) ----------
rct <- subset(panel, arm_rct==1)
m_itt <- lm(y ~ post*assign_rct + age + female + ses + urban + baseline_ability, data=rct)
V_itt <- vcovCL(m_itt, cluster = ~ cluster_id)
itt_td <- tidy(m_itt, vcov = V_itt, conf.int = TRUE)
itt_row <- itt_td[itt_td$term=="post:assign_rct", ]
itt_est <- itt_row$estimate; itt_se <- itt_row$std.error
itt_ciL <- itt_row$conf.low;  itt_ciU <- itt_row$conf.high

# ---------- LATE (RCT-IV con asignación) ----------
rct_post <- subset(rct, post==1)
late_fit <- ivreg(y ~ D | assign_rct, data = rct_post)  # 2SLS simple
V_late   <- vcovCL(late_fit, cluster = ~ cluster_id)
late_td  <- tidy(late_fit, vcov = V_late, conf.int = TRUE)
late_row <- late_td[late_td$term=="D", ]
late_est <- late_row$estimate; late_se <- late_row$std.error
late_ciL <- late_row$conf.low; late_ciU <- late_row$conf.high

# ---------- DiD (con covariables) ----------
did1 <- lm(y ~ post*group_did + age + female + ses + urban + baseline_ability +
             dist_km + factor(region), data = panel)
V_did  <- vcovCL(did1, cluster = ~ cluster_id)
did_td <- tidy(did1, vcov = V_did, conf.int = TRUE)
did_row <- did_td[did_td$term=="post:group_did", ]
did_est <- did_row$estimate; did_se <- did_row$std.error
did_ciL <- did_row$conf.low; did_ciU <- did_row$conf.high

# ---------- ATT (PSM en post, con imputación + dummies de missing) ----------
post_data <- subset(panel, post==1) %>%
  mutate(
    region      = factor(region),
    ses_imp     = ifelse(is.na(ses),      round(median(ses, na.rm=TRUE)), ses),
    dist_km_imp = ifelse(is.na(dist_km),        median(dist_km, na.rm=TRUE), dist_km),
    ses_miss    = as.integer(is.na(ses)),
    dist_miss   = as.integer(is.na(dist_km))
  )

m.out <- matchit(D ~ age + female + ses_imp + ses_miss + urban +
                   baseline_ability + dist_km_imp + dist_miss + region,
                 data = post_data,
                 method = "nearest", ratio = 1, replace = FALSE, caliper = 0.2,
                 estimand = "ATT")

md <- match.data(m.out)  # trae 'weights' y 'subclass'
att_fit <- lm(y ~ D, data = md, weights = weights)
V_att   <- vcovCL(att_fit, cluster = md$subclass)
att_td  <- tidy(att_fit, vcov = V_att, conf.int = TRUE)
att_row <- att_td[att_td$term=="D", ]
att_est <- att_row$estimate; att_se <- att_row$std.error
att_ciL <- att_row$conf.low; att_ciU <- att_row$conf.high

# ---------- IV (encouragement Z_encourage, en post) ----------
iv_data <- subset(panel, post==1) %>% mutate(region = factor(region))
iv_enc <- ivreg(
  y ~ D + age + female + ses + urban + baseline_ability + dist_km + region |
    Z_encourage + age + female + ses + urban + baseline_ability + dist_km + region,
  data = iv_data
)
V_iv2  <- vcovCL(iv_enc, cluster = ~ cluster_id)
iv2_td <- tidy(iv_enc, vcov = V_iv2, conf.int = TRUE)
iv2_row <- iv2_td[iv2_td$term=="D", ]
iv2_est <- iv2_row$estimate; iv2_se <- iv2_row$std.error
iv2_ciL <- iv2_row$conf.low; iv2_ciU <- iv2_row$conf.high

# ---------- RDD (local en el corte) ----------
rdd_data <- subset(panel, post==1)
rd <- rdrobust(y = rdd_data$y, x = rdd_data$rdd_score, c = 0, vce = "HC1")
rd_est  <- rd$Estimate[1]
rd_se   <- rd$se[1]
rd_ciL  <- rd_est - 1.96*rd_se
rd_ciU  <- rd_est + 1.96*rd_se

# ---------- Tabla comparativa ----------
comparativa <- tibble(
  metodo   = c("ITT (RCT)", "LATE (RCT-IV)", "DiD (covariables)", "ATT (PSM)", "IV (Z_encourage)", "RDD (local)"),
  estimador= c(itt_est, late_est, did_est, att_est, iv2_est, rd_est),
  se       = c(itt_se, late_se, did_se, att_se, iv2_se, rd_se),
  ci_low   = c(itt_ciL, late_ciL, did_ciL, att_ciL, iv2_ciL, rd_ciL),
  ci_high  = c(itt_ciU, late_ciU, did_ciU, att_ciU, iv2_ciU, rd_ciU)
)

kable(comparativa, digits = 3, caption = "Comparativa de estimadores (IC 95%)")

# ---------- Forest plot (opcional) ----------
comparativa %>%
  mutate(metodo = factor(metodo, levels = rev(metodo))) %>%
  ggplot(aes(y = metodo, x = estimador)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.15) +
  labs(title = "Comparativa de efectos (IC 95%)",
       x = "Efecto estimado", y = NULL)
