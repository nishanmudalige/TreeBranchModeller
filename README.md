# TreeBranchModeller
R package for the paper "Learning how a tree branches out: A statistical approach"

## Installation

TreeBranchModeller relies on the following dependences:

```
  > required.packages = c("gtools", "crayon", "stringr", "flexsurv", "betareg", "utils", "stats")
  > sapply(required.packages, install.packages)
```

In order to install BOLD.R directly through R, please type the following into the R console.

```
  > ## If devtools are not installed
  > ## install.packages("devtools")
  > 
  > library(devtools)
  > devtools::install_github("nishanmudalige/TreeBranchModeller", force = TRUE)
```

## Usage

We recommend using a light coloured theme in R or RStudio (such as the default theme).

```
> library(TreeBranchModeller)
> library(tidyverse)
```

### Model for Direction.



## Help

For questions, please email nishan [dot] mudalige [at] gmail [dot] com .
