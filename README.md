# Explore2 pêche aux outils [<img src="https://github.com/super-lou/Explore2_toolbox/blob/c3b69377345919a2048826beac0841c37086db4e/resources/logo/LogoExplore2.png" align="right" width=160 alt=""/>](https://professionnels.ofb.fr/fr/node/1244)

<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-green)](https://lifecycle.r-lib.org/articles/stages.html)
![](https://img.shields.io/github/last-commit/super-lou/Explore2_peche_aux_outils)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md) 
<!-- badges: end -->

**Explore2 pêche aux outils** est un ensemble de codes qui permettent de prendre en main les données issues du projet [Explore2](https://professionnels.ofb.fr/fr/node/1244) disponible sur le portail [DRIAS-Eau](https://drias-eau.fr/) dans le cadre d'un aterlier [ZABR](https://www.zabr.assograie.org/peche-aux-outils-n9-le-changement-climatique-quelles-donnees-disponibles-quelles-modalites-de-traitement/).

Les rapports et messages du projet Explore2 sont disponibles sur l'entrepôt [Recherche Data Gouv](https://entrepot.recherche.data.gouv.fr/dataverse/explore2).

Ce projet a été rendu possible dans le cadre d'un développement réalisé par l'Institut National de Recherche pour l’Agriculture, l’Alimentation et l’Environnement, [INRAE](https://agriculture.gouv.fr/inrae-linstitut-national-de-recherche-pour-lagriculture-lalimentation-et-lenvironnement).


## Téléchargement du code
Soit en clonant le répertoire git :
``` 
git clone https://github.com/super-lou/Explore2_peche_aux_outils.git
```
Soit en téléchargeant l'[archive zip](https://github.com/super-lou/Explore2_peche_aux_outils/archive/refs/heads/main.zip) puis en la décompressant.


## Description

#### `main.R`
C'est le fichier principal qui regroupe les différentes actions possibles dans le cadre de cet atelier. Le code est développé de manière linéaire et est commenté afin de pouvoir identifier les étapes intéressantes selon les besoins.

#### `R`
Le répertoire de fonction en R utile pour l'exécution de `main.R`.

#### `figures`
Ce répertoire contient les exemples de figures qu'il est possible de tracer avec ce code. 

#### `robots`
Ce répertoire contient deux tableaux `csv` qui regroupent les informations concernant Explore2 contenues sur le server HTTP de DRIAS-Eau.
Le script `robot.R` permet de reproduire ces tableaux mais est à exécuter avec précaution et uniquement si le serveur HTTP semble avoir changé de structure (ex : les URLs ne semblent plus valides).

#### `shapefiles`
Ce répertoire regroupe quelques shapefiles utiles pour le tracé de carte.

#### `manual_URL_DRIAS_*.txt`
Ces deux fichiers permettent de regrouper manuellement des URLs de téléchargement des données disponibles sur DRIAS-Eau et de ensuite pouvoir les télécharger avec le code disponible dans `main.R`.


## FAQ
*I have a question.*

-   **Solution**: Search existing issue list and if no one has a similar question create a new issue.

*I found a bug.*

-   **Good Solution**: Search existing issue list and if no one has reported it create a new issue.
-   **Better Solution**: Along with issue submission provide a minimal reproducible example of the bug.
-   **Best Solution**: Fix the issue and submit a pull request. This is the fastest way to get a bug fixed.


## Code of Conduct
Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
