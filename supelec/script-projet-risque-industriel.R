#==============================================================================#
#                                                                              #
#        Quantification des incertitudes dans les simulations numériques       #
#                          Projet « balistique » (drone)                       #
#                                                                              #
#  Auteur  : Ilyas Hanine                                                      #
#  Cadre   : prédiction de la portée y_max d'un drone catapulté sans moteur    #
#  Sortie d'intérêt : y_max(x), distance horizontale parcourue                 #
#  Entrées : x = (v0, theta0, m, L, beta1, beta2, beta3), 7 paramètres         #
#            indépendants, uniformes.                                          #
#                                                                              #
#  Dépendances externes (à placer dans le répertoire de travail) :            #
#    - cas_balistique.R      : TirageX(n), TirageZ(n), code.vect(X)            #
#    - Wilks.quantile.R      : Wilks.quantile(...)                             #
#    - passage_U_X_projet.R  : passage_U_X_projet(U) (espace standard -> phys.)#
#                                                                              #
#  Organisation (cf. rapport) :                                                #
#    2. Propagation d'incertitudes                                             #
#       2.1 Étude en tendance centrale                                         #
#       2.2 Quantiles                                                          #
#       2.3 Événements rares                                                   #
#    3. Analyse de sensibilité                                                 #
#       3.1 Métamodélisation                                                   #
#       3.2 Indices de Sobol                                                   #
#==============================================================================#


#==============================================================================#
# 0. PRÉAMBULE                                                                 #
#==============================================================================#

rm(list = ls())     # nettoyage de l'environnement
graphics.off()      # fermeture des fenêtres graphiques

## Reproductibilité ------------------------------------------------------------
# Les valeurs numériques exactes dépendent du tirage aléatoire ; on fixe la
# graine pour que les exécutions soient reproductibles d'une fois sur l'autre.
set.seed(42)

## Packages --------------------------------------------------------------------
library(ADGofTest)    # test d'adéquation d'Anderson-Darling
library(moments)      # skewness / kurtosis
library(boot)         # rééchantillonnage bootstrap
library(evd)          # théorie des valeurs extrêmes
library(mistral)      # estimation d'événements rares (FORM, Subset, MP)
library(DiceDesign)   # plans d'expériences (LHS, maximin, factoriel)
library(DiceKriging)  # régression par processus gaussien (krigeage)
library(sensitivity)  # analyse de sensibilité (indices de Sobol)

## Fonctions du modèle ---------------------------------------------------------
source("cas_balistique.R")     # TirageX, TirageZ, code.vect
source("Wilks.quantile.R")     # Wilks.quantile
source("passage_U_X_projet.R") # passage_U_X_projet (transformation iso-probabiliste)

## Constantes du problème ------------------------------------------------------
d           <- 7                                  # dimension de l'espace d'entrée
param_names <- c("v0", "t0", "m", "L", "b1", "b2", "b3")

# Bornes des lois uniformes des 7 paramètres d'entrée (cf. énoncé)
binf <- c(v0 = 100, t0 = pi/10, m = 1, L = 1, b1 = 0.05, b2 = 0.01, b3 = 9.805)
bsup <- c(v0 = 125, t0 = pi/5,  m = 2, L = 2, b1 = 0.20, b2 = 0.11, b3 = 9.815)

## Exemple d'utilisation des fonctions du modèle -------------------------------
X <- TirageX(10)        # 10 réalisations des paramètres d'entrée
Y <- code.vect(X)       # portées y_max associées
summary(Y)


#==============================================================================#
# 2. PROPAGATION D'INCERTITUDES                                                #
#==============================================================================#

#------------------------------------------------------------------------------#
# 2.1  Étude en tendance centrale                                              #
#------------------------------------------------------------------------------#
# Méthode de Monte Carlo : on propage l'incertitude des entrées et on étudie
# la distribution de la sortie y_max.

N_ETUDE <- 200                 # taille de l'échantillon (réutilisée en 2.2)
X <- TirageX(N_ETUDE)
Y <- code.vect(X)

## Q1 — Résumé statistique -----------------------------------------------------
summary(Y)

## Q2 — L'échantillon peut-il être gaussien ? ----------------------------------
# y_max est une distance, donc strictement positive : a priori non gaussienne.
hist(Y, probability = TRUE, col = "red", main = "Histogramme de y_max", xlab = "y_max")
lines(density(Y), col = "blue", lwd = 2)

qqnorm(Y); qqline(Y)           # QQ-plot : écarts visibles dans les queues

# Test de normalité retenu dans le rapport : Shapiro-Wilk
shapiro.test(Y)                # p-value << 0.05 => non normalité au seuil 5 %
# Tests complémentaires (mêmes conclusions)
ad.test(Y, pnorm, mean(Y), sd(Y))       # Anderson-Darling
ks.test(Y, "pnorm", mean(Y), sd(Y))     # Kolmogorov-Smirnov

## Q3 — Moyenne, écart-type et intervalles de confiance à 95 % -----------------
mean_Y <- mean(Y)
sd_Y   <- sd(Y)
alpha  <- 0.05

# IC de la moyenne (théorème central limite, n > 30 -> loi de Student)
q_student <- qt(1 - alpha/2, N_ETUDE - 1)
IC_mean_Y <- mean_Y + c(-1, 1) * q_student * sd_Y / sqrt(N_ETUDE)

# IC de l'écart-type (approximation asymptotique faisant intervenir le kurtosis)
q_norm <- qnorm(1 - alpha/2)
k      <- kurtosis(Y)
var_inf <- sd_Y^2 * (1 - q_norm * sqrt((k - 1) / N_ETUDE))
var_sup <- sd_Y^2 * (1 + q_norm * sqrt((k - 1) / N_ETUDE))
IC_sd_Y <- sqrt(c(var_inf, var_sup))

mean_Y; IC_mean_Y
sd_Y;   IC_sd_Y

## Q4 — IC bootstrap à 95 % pour le kurtosis -----------------------------------
mykurt <- function(x, idx) kurtosis(x[idx])           # statistique d'intérêt
kurt_boot <- boot(data = Y, statistic = mykurt, R = 1000)

# IC par percentiles de la distribution bootstrap (méthode du TP)
ic_kurt <- boot.ci(kurt_boot, conf = 0.95, type = "perc")
IC_kurt_Y <- ic_kurt$percent[4:5]
IC_kurt_Y                                             # kurtosis positif et élevé


#------------------------------------------------------------------------------#
# 2.2  Quantiles                                                               #
#------------------------------------------------------------------------------#
# On réutilise l'échantillon de taille N_ETUDE de la partie 2.1.

## Q1 — Quantile empirique à 95 % ----------------------------------------------
qY95 <- quantile(Y, probs = 0.95)
qY95

## Q2 — Quantile de Wilks (majorant) aux niveaux de confiance 90, 95, 99 % -----
qY95_wilks_90 <- Wilks.quantile(alpha = 0.95, beta = 0.90, data = Y, bilateral = FALSE)
qY95_wilks_95 <- Wilks.quantile(alpha = 0.95, beta = 0.95, data = Y, bilateral = FALSE)
qY95_wilks_99 <- Wilks.quantile(alpha = 0.95, beta = 0.99, data = Y, bilateral = FALSE)
qY95_wilks_90; qY95_wilks_95; qY95_wilks_99

## Q3 — Nombre minimal d'échantillons fourni par Wilks -------------------------
# La méthode de Wilks donne le n minimal pour majorer le quantile à un niveau
# de confiance donné. Exemple au niveau 90 % :
qY95_wilks_90$nmin

## Q4 — Confiance maximale atteignable pour le quantile à 95 % avec n = N_ETUDE -
# Problème inverse : on cherche beta tel que nmin(beta) = N_ETUDE.
f_beta <- function(b) {
  Wilks.quantile(alpha = 0.95, beta = b, data = NULL, bilateral = FALSE)$nmin - N_ETUDE
}
beta_max <- uniroot(f_beta, c(0.95, 1 - 1e-6), tol = 1e-7)$root
beta_max                                              # ~ 0.99996


#------------------------------------------------------------------------------#
# 2.3  Événements rares : estimation de P(y_max >= 1350)                        #
#------------------------------------------------------------------------------#
# Méthode retenue dans le rapport : FORM (approximation de la surface de
# défaillance par un hyperplan dans l'espace standard). On présente d'abord la
# transformation iso-probabiliste et la fonction d'état limite, puis FORM ;
# d'autres méthodes explorées sont regroupées en fin de section.

SEUIL <- 1350                  # longueur du terrain (m) : défaillance si y_max >= SEUIL

## Transformation iso-probabiliste et fonction d'état limite -------------------
# passage_U_X_projet : espace standard gaussien U -> espace physique X.
# Vérification rapide : les marges de X reconstruit doivent coïncider avec TirageX.
U_test <- matrix(rnorm(100 * d), ncol = d)            # 100 points standard
X_test <- passage_U_X_projet(U_test)
summary(X_test)
summary(TirageX(100))

# Fonction d'état limite g(U) : défaillance <=> g < 0
etat_limite <- function(U) {
  U <- t(as.matrix(U))
  X <- passage_U_X_projet(U)
  - code.vect(X) + SEUIL
}

## Méthode FORM (retenue) ------------------------------------------------------
# Lois d'entrée dans l'espace standard : d lois normales centrées réduites.
distributions <- replicate(d, list(type = "norm", mean = 0, sd = 1), simplify = FALSE)

res_FORM <- FORM(f = etat_limite, u.dep = rep(10, d),
                 inputDist = distributions, N.calls = 1e3)

res_FORM$pf            # probabilité de défaillance estimée (~ 0.004)
res_FORM$compt.f       # nombre d'appels au modèle
res_FORM$design.point  # point de conception

## ---------------------------------------------------------------------------- #
## Autres méthodes explorées (non retenues dans le rapport)                     #
## ---------------------------------------------------------------------------- #

# a) Monte Carlo « brut » (référence / vérification)
n_MC <- 1e4
Y_MC <- code.vect(TirageX(n_MC))
P_MC <- mean(Y_MC >= SEUIL)
var_P_MC <- P_MC * (1 - P_MC) / n_MC
cv_MC <- sqrt((1 - P_MC) / (n_MC * P_MC)) * 100        # coefficient de variation (%)
t_alpha <- qt(1 - alpha/2, n_MC - 1)
IC95_P_MC <- P_MC + c(-1, 1) * t_alpha * sqrt(P_MC * (1 - P_MC)) / sqrt(n_MC)
P_MC; cv_MC; IC95_P_MC

# b) Simulation par sous-ensembles (Subset Simulation)
res_subset <- SubsetSimulation(dimension = d, lsf = etat_limite, p_0 = 0.001, N = 1000)
res_subset$p

# c) Moving Particles
res_MP <- MP(dimension = d, lsf = etat_limite, N = 100, q = 0, N.batch = 1)

# d) Théorie des valeurs extrêmes : méthode des maxima par blocs (GEV)
max_Y <- replicate(50, max(code.vect(TirageX(20))))
fit_gev <- fgev(max_Y)
fit_gev
par(mfrow = c(2, 2)); plot(fit_gev, col = "blue"); par(mfrow = c(1, 1))

# Extrapolation de P(y_max >= SEUIL) à partir de la loi GEV ajustée
loc_g   <- fit_gev$param[1]
scale_g <- fit_gev$param[2]
shape_g <- fit_gev$param[3]
P_extrap <- -1 / length(max_Y) * log(pgev(SEUIL, loc_g, scale_g, shape_g))
as.numeric(P_extrap)


#==============================================================================#
# 3. ANALYSE DE SENSIBILITÉ                                                    #
#==============================================================================#
# Le modèle est coûteux : on passe par un métamodèle avant d'estimer les
# indices de Sobol.

#------------------------------------------------------------------------------#
# 3.1  Métamodélisation                                                        #
#------------------------------------------------------------------------------#

## Choix du plan d'expériences -------------------------------------------------
# Le plan factoriel complet (2^d points) est exact mais trop coûteux dès que d
# est grand : 2^7 = 128 points rien que pour deux niveaux. On lui préfère un
# plan LHS, et parmi les LHS, celui qui maximise la distance minimale entre
# points (critère « maximin »).
plan_fact <- factDesign(dimension = d, levels = 2)$design
nrow(plan_fact)                # coût du plan factoriel complet (2^d)

# Comparaison de trois plans à 100 points en dimension d (critère maximin) :
plan_rand    <- matrix(runif(100 * d), nrow = 100, ncol = d)   # aléatoire pur
plan_LHS     <- lhsDesign(n = 100, dimension = d)$design        # LHS « simple »
xinit        <- lhsDesign(n = 100, dimension = d)$design
plan_maximin <- maximinSA_LHS(xinit)$design                     # LHS optimisé (recuit)

cat("Critère maximin (mindist) :\n")
cat("  plan aléatoire :", mindist(plan_rand), "\n")
cat("  LHS simple     :", mindist(plan_LHS), "\n")
cat("  LHS maximin    :", mindist(plan_maximin), "\n")   # plan retenu (le plus élevé)

# Visualisation du plan retenu (projection sur les deux premières dimensions)
plot(plan_maximin[, 1:2], pch = 16, col = "magenta", cex = 1.5,
     xlab = "x1", ylab = "x2", main = "Plan LHS maximin (projection 2D)")

## Passage du plan [0,1]^d vers l'espace physique des paramètres ---------------
X_app <- sapply(seq_len(d), function(k)
  qunif(plan_maximin[, k], min = binf[k], max = bsup[k]))
colnames(X_app) <- param_names
X_app <- as.data.frame(X_app)
Y_app <- code.vect(X_app)

# Archivage éventuel du plan (réutilisable ensuite)
# write.table(X_app, file = "LHS_projet.dat")

## Construction des métamodèles ------------------------------------------------
formule  <- z ~ v0 + t0 + m + L + b1 + b2 + b3
modeleRL <- lm(formule, data = data.frame(X_app, z = Y_app))    # régression linéaire
modelePG <- km(formula = formule, design = X_app, response = Y_app)  # processus gaussien

## Apprentissage : prédictions sur le plan d'apprentissage ---------------------
predRL <- predict(modeleRL, X_app)
predPG <- predict(modelePG, X_app, type = "UK")

plot(Y_app, Y_app, type = "l", col = "black",
     xlab = "y_max (modèle)", ylab = "y_max (métamodèle)",
     main = "Apprentissage : linéaire (bleu) vs gaussien (rouge)")
points(Y_app, predRL,        col = "blue")
points(Y_app, predPG$mean,   col = "red")

## Validation sur un jeu indépendant -------------------------------------------
NV <- 100
X_val <- TirageX(NV)
X_val <- as.data.frame(matrix(c(X_val), ncol = d, dimnames = list(seq_len(NV), param_names)))
Y_val <- code.vect(X_val)

predRL_val <- predict(modeleRL, X_val)
predPG_val <- predict(modelePG, X_val, type = "UK")

plot(Y_val, Y_val, type = "l", col = "black",
     xlab = "y_max (modèle)", ylab = "y_max (métamodèle)",
     main = "Validation : linéaire (bleu) vs gaussien (rouge)")
points(Y_val, predRL_val,      col = "blue")
points(Y_val, predPG_val$mean, col = "red")

## Critère prédictif Q2 --------------------------------------------------------
Q2_RL <- 1 - mean((Y_val - predRL_val)^2)      / var(Y_val)
Q2_PG <- 1 - mean((Y_val - predPG_val$mean)^2) / var(Y_val)
Q2_RL; Q2_PG                                   # le processus gaussien est nettement meilleur

## Confiance de prédiction : validation croisée Leave-One-Out (GP) -------------
LOO_GP <- leaveOneOut.km(modelePG, type = "UK", trend.reestim = FALSE)
err_VC_GP <- mean((LOO_GP$mean - Y_app)^2 / LOO_GP$sd^2)
err_VC_GP

# Les prédictions LOO et leur IC à 95 % : un bon métamodèle laisse les points
# observés à l'intérieur de l'intervalle de confiance.
ord <- order(LOO_GP$mean)
plot(LOO_GP$mean[ord], type = "l", col = "blue", ylim = range(Y_app),
     xlab = "Index (trié)", ylab = "y_max",
     main = "Confiance LOO du métamodèle gaussien")
lines(LOO_GP$mean[ord] + 1.96 * LOO_GP$sd[ord], col = "red")
lines(LOO_GP$mean[ord] - 1.96 * LOO_GP$sd[ord], col = "red")
points(Y_app[ord], col = "black", cex = 0.6)


#------------------------------------------------------------------------------#
# 3.2  Indices de Sobol                                                        #
#------------------------------------------------------------------------------#
# Estimation par Monte Carlo (estimateur de Jansen) à partir du métamodèle
# gaussien. Coût total N = (d + 2) * n.

## Q1 — Estimation des indices de premier ordre et totaux ----------------------
n_sob <- 1000                  # cf. cours : n ~ 1000  =>  N = (d+2)*n = 9000 appels
xs1 <- TirageX(n_sob)
xs2 <- TirageX(n_sob)
colnames(xs1) <- colnames(xs2) <- param_names

sob   <- soboljansen(model = NULL, xs1, xs2, nboot = 100)
Y_sob <- predict(modelePG, newdata = data.frame(sob$X), type = "UK")$mean
res_sob <- tell(sob, Y_sob)    # calcul des indices à partir des réponses du métamodèle

print(res_sob)
plot(res_sob)                  # indices de Sobol (effets principaux et totaux)

## Q2 — Estimation tenant compte de l'incertitude du métamodèle ----------------
# sobolGP propage l'incertitude de prédiction du processus gaussien.
res_sob_GP <- sobolGP(modelePG, type = "UK", MCmethod = "soboljansen",
                      xs1, xs2, nsim = 100, conf = 0.95, nboot = 100)
plot(res_sob_GP)

# Interprétation (cf. rapport) : la portance (b1) et la vitesse initiale (v0)
# dominent la variance de y_max ; la gravité (b3) influe peu (faible plage de
# variation) ; la longueur L, absente des équations, a un indice nul.

#==============================================================================#
#                                 FIN DU SCRIPT                                #
#==============================================================================#
