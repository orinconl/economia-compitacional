# install.R — Binder ejecuta este script durante el build
options(repos = c(CRAN = "https://cran.r-project.org"))

pkgs <- c(
  "dplyr","tidyr","ggplot2","knitr","broom",
  "AER","lmtest","sandwich","MatchIt",
  "rdrobust","rddensity"
)

to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, Ncpus = max(1, parallel::detectCores()-1))
