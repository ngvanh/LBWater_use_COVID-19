pkglist <- c('readr',
             'dplyr',
             'naniar',
             'reshape2',
             'tidyr',
             'ggplot2',
             'gridExtra',
             'cowplot',
             'scales',
             'rmarkdown',
             'knitr',
             'xts',
             'zoo')
miapkgs <- pkglist[!(pkglist %in% installed.packages()[,"Package"])]
if (length(miapkgs) > 0){
  install.packages(miapkgs)
}
lapply(pkglist, library, character.only=TRUE)
