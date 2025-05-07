# Script de création de l'objet permettant de stocker les différentes distributions #
# Auteur : G. Mulier                                                                #
# Créé le 07/05/2025, modifié le 07/05/2025                                         #

ListeDistribution <- list(
  "Normale" = list(
    "dens" = dnorm,
    "cdf" = pnorm
  ),
  "Beta" = list(
    "dens" = dbeta,
    "cdf" = pbeta
  ),
  "Exponentielle" = list(
    "dens" = dexp,
    "cdf" = pexp
  )
)