# matingRhinos

This is the GitHub repository hosting the R package matingRhinos. The package will not
evolve much in the future because it aims at documenting how we obtained the
results for the paper entitled "Mate choice, reproductive success and inbreeding 
in white rhinos" by Kretzschmar Petra et al. (in prep).
The only planned updates are those that will be necessary to maintain
compatibility with other packages, so that our code won't break.

This package has not been conceived for general use. It serves to document the
steps of our analysis.


## How to explore the sources of the package?

To see our source code, you can either browse the files above within GitHub or
download the *.tar.gz file containing all sources bundled with some data to
try out our functions in R.
This file is available here: XXX (link to come)

## Installation (not yet possible)

Whichever option you choose, to install successfully the R package you will need:

* an up-to-date installation of R (https://cran.r-project.org);

* the R package ```drat``` installed. You can install it by typing:

```{r, eval = FALSE}
install.packages("drat")
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
