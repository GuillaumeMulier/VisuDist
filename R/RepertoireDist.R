# Script de création de l'objet permettant de stocker les différentes distributions #
# Auteur : G. Mulier                                                                #
# Créé le 07/05/2025, modifié le 01/07/2025                                         #

ListeDistribution <- list(
  "Normal" = list(
    "dens" = dnorm,
    "name_dens" = "dnorm",
    "cdf" = pnorm,
    "name_cdf" = "pnorm",
    "params" = list("mean" = list("value" = 0, "label" = "Mean of normal distribution", "min" = NA, "max" = NA),
                    "sd" = list("value" = 1, "label" = "Standard deviation of normal distribution", "min" = 1e-6, "max" = NA))
  ),
  "Beta" = list(
    "dens" = dbeta,
    "name_dens" = "dbeta",
    "cdf" = pbeta,
    "name_cdf" = "pbeta",
    "params" = list("shape1" = list("value" = 1, "label" = "&alpha; of beta distribution", "min" = 1e-6, "max" = NA),
                    "shape2" = list("value" = 1, "label" = "&beta; of beta distribution", "min" = 1e-6, "max" = NA))
  ),
  "Exponential" = list(
    "dens" = dexp,
    "name_dens" = "dexp",
    "cdf" = pexp,
    "name_cdf" = "pexp",
    "params" = list("rate" = list("value" = 1, "label" = "Rate of exponential distribution", "min" = 1e-6, "max" = NA))
  )
)

ListeDifferences <- list(
  "Normal" = list(
    "dens" = function(x, m1, m2, s1, s2) dnorm(x, m1 - m2, sqrt(s1 ** 2 + s2 ** 2)),
    "name_dens" = "ddiffnorm <- function(x, m1, m2, s1, s2) dnorm(x, m1 - m2, sqrt(s1 ** 2 + s2 ** 2))//
ddiffnorm",
    "cdf" = function(q, m1, m2, s1, s2) pnorm(q, m1 - m2, sqrt(s1 ** 2 + s2 ** 2)),
    "name_cdf" = "pdiffnorm <- function(q, m1, m2, s1, s2) pnorm(q, m1 - m2, sqrt(s1 ** 2 + s2 ** 2))//
pdiffnorm",
    "params" = list("m1" = list("value" = 0, "label" = "Mean of first normal distribution", "min" = NA, "max" = NA),
                    "s1" = list("value" = 1, "label" = "Standard deviation of first normal distribution", "min" = 1e-6, "max" = NA),
                    "m2" = list("value" = 0, "label" = "Mean of second normal distribution", "min" = NA, "max" = NA),
                    "s2" = list("value" = 1, "label" = "Standard deviation of second normal distribution", "min" = 1e-6, "max" = NA))
  ),
  "Beta" = list(
    "dens" = function(x, shape1_1, shape2_1, shape1_2, shape2_2) dbetadiff(x, c(shape1_1, shape2_1), c(shape1_2, shape2_2)),
    "name_dens" = "# https://genentech.github.io/phase1b/main/index.html
# devtools::install_github(\"https://github.com/Genentech/phase1b/\", force = TRUE)
library(phase1b)//
ddiffbeta <- function(x, shape1_1, shape2_1, shape1_2, shape2_2) dbetadiff(x, c(shape1_1, shape2_1), c(shape1_2, shape2_2))//
ddiffbeta",
    "cdf" = function(q, shape1_1, shape2_1, shape1_2, shape2_2) pbetadiff(q, c(shape1_1, shape2_1), c(shape1_2, shape2_2)),
    "name_cdf" = "# https://genentech.github.io/phase1b/main/index.html
# devtools::install_github(\"https://github.com/Genentech/phase1b/\", force = TRUE)
library(phase1b)//
pdiffbeta <- function(q, shape1_1, shape2_1, shape1_2, shape2_2) pbetadiff(q, c(shape1_1, shape2_1), c(shape1_2, shape2_2))//
pdiffbeta",
    "params" = list("shape1_1" = list("value" = 1, "label" = "&alpha; of first beta distribution", "min" = 1e-6, "max" = NA),
                    "shape2_1" = list("value" = 1, "label" = "&beta; of first beta distribution", "min" = 1e-6, "max" = NA),
                    "shape1_2" = list("value" = 1, "label" = "&alpha; of second beta distribution", "min" = 1e-6, "max" = NA),
                    "shape2_2" = list("value" = 1, "label" = "&beta; of second beta distribution", "min" = 1e-6, "max" = NA))
  )
)
