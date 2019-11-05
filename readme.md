# matingRhinos

This is the GitHub repository hosting the R package matingRhinos.

The package aims at documenting how we obtained the results for the paper
entitled 'Mate choice, reproductive success and inbreeding in white rhinos
(_Ceratotherium simum_ Burchell, 1817): new insights for conservation management'
by Kretzschmar P., Auld H., Boag P., Ganslosser U., Scott C., Van Coeverden de
Groot P.J. & Courtiol A. (accepted in Evolutionary Applications).

For this reason, the package will not evolve much in the future. The only
planned updates are those that will be necessary to maintain compatibility with
other packages, so that our code won't break. This package has not been
conceived for general use. It only serves to document the steps of our analysis,
and must be used with our data.


## How to explore the sources of the package?

To see our source code, you can either browse the files above within GitHub or
download the *.tar.gz file containing all sources bundled with some data to
try out our functions in R.
This file is available [here](https://github.com/courtiol/drat/blob/gh-pages/src/contrib/matingRhinos_0.9.1.tar.gz).


## Installation

Whichever option you choose, to install successfully the R package you will need:

* an up-to-date installation of R (https://cran.r-project.org);

* the following R packages ```drat``` installed. You can install them by typing:

```{r, eval = FALSE}
install.packages(c("drat", "ggplot2", "ggforce", "cowplot"))
```

Then, simply type the following:

```{r, eval = FALSE}
drat::addRepo("courtiol")
install.packages("matingRhinos", type = "source")
```


## Usage

1. load the package:

```{r, eval = FALSE}
library("matingRhinos")
```

2. access the main documentation by typing:

```{r, eval = FALSE}
help("matingRhinos")
```
