#---
#title: "ROUDMANE-DELAFORGE-Project"
#output: html_notebook
#---

library(tidyverse, quietly = TRUE)
library(mda802)
library(MASS)
library(FactoMineR)
library(factoextra)

# 1. Préparation des données et enrichissement
don_train=read.csv("train.csv",sep=",")
don_test=read.csv("test.csv",sep=",")
don_train
# 2. Analyse descriptive par ACP

# Nous n'utilisons les colonnes 1 et 2, la premiÃ¨re car elle n'est pas prÃ©sente dans la doc,
# la deuxiÃ¨me car elle n'est pas numÃ©rique et surement inutile pour la prÃ©diction.

res_pca <- PCA(don_train[,12:ncol(don_train)], scale.unit = TRUE, graph = T, ncp=5)

# Nous dÃ©cidons de conserver de conserver 2 dimensions, en observant la cassure dans le graphe et en 
# ayant calculÃ© l'indicateyur de karlis_saporta_spinaki . Ces dimensiosn expliquent : 71.02% de variance.

karlis_saporta_spinaki=1+2*sqrt((ncol(don_train)-1)/(nrow(don_train)-1))
karlis_saporta_spinaki

fviz_screeplot(res_pca, ncp=5)

res_pca$eig

# Observons Ã©galement les contributions de nos individus aux dimensions 1 et 2

fviz_contrib(res_pca, choice = "var", axes = 1)
res_pca$var$contrib

# Sur cette dimension, les contributions des variables semblent acceptable.
# Notre variable Ã  expliquer par la suite est l'une des variables qui contribue
# le plus Ã  l'axe un.

fviz_contrib(res_pca, choice = "var", axes = 2)
res_pca$var$contrib

# Sur cette dimension, les contributions des variables semblent acceptable.

# Observons Ã©galement les contributions de nos individus aux dimensions 1 et 2

fviz_contrib(res_pca, choice = "ind", axes = 1)

# Sur cette dimension, les contributions semblent acceptable, 
# et les plus forte valeurs ne font pas partie des individus dont nous avons
# completÃ© les valeurs, pas d'inquiÃ¨tude.

fviz_contrib(res_pca, choice = "ind", axes = 2)

# Sur cette dimension, malgrÃ© un contribution de plus de 35%
# ce n'est pas un individu dont nous avons
# completÃ© les valeurs, pas d'inquiÃ¨tude.

# Suite Ã  Ã§a. Nous observons le graph des dimensions 1 et 2.

fviz_pca_var(res_pca, axes=c(1,2), repel = TRUE)

# Nos variables sont pour la plupart bien expliquÃ©es, nous sommes satisfait de notre choix de
# ne conserver que deux dimensions. Certaines variables sont opposÃ©ment expliquÃ©es par nos
# axes : (sigmam & sigmap) comportment opposÃ©s (thetam & thetap)

fviz_pca_ind(res_pca, axes=c(1,2), repel = TRUE)
fviz_pca_biplot(res_pca, repel = TRUE)

# Le biplot nous montre que de nombreuses variables s'expliquent bien sur l'axe un vu qu'il
# explique 58,7% de la variance. En soit trÃ¨s peu de point sont mal expliquÃ©s au final. 

# 3. ModÃ©lisation de la prÃ©diction de h13d par modÃ¨les linÃ©aires (classique, pcr et pls)

# On retire les deux premiers car ils sont inutiles. 
# Et la valeur Ã  expliquer de notre jeu de test.

set_test_h13d = set_test[,3]

# Standardisation du jeu de test

set_test_standard <- set_test
# standardisation
set_test_standard[, 3:ncol(set_test)] <- t(apply(set_test[, 3:ncol(set_test)], MARGIN = 1, FUN = function(x) { (x - res_pca$call$centre)/res_pca$call$ecart.type}))
# projection
test_proj <- as.matrix((set_test_standard[, 3:ncol(set_test)])) %*% as.matrix(res_pca$svd$V)


