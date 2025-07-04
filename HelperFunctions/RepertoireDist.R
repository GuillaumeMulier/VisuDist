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
