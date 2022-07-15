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

## Example: Christmas Tree data

### Read the data


```
> ## Read The data set
> ct_data = read.table("https://raw.githubusercontent.com/nishanmudalige/TreeBranchModeller/main/data/CTobsn.txt", header = T)
> 
> ct_data_level2 = 
  ct_data %>% select(id1, x1, C1.1, C1.2, C1.3, L1, N1,
                     id2, x2, C2.1, C2.2, C2.3, L2, N2) %>% distinct()

> ct_data_level3 =
+  ct_data %>% select(id1, x1, C1.1, C1.2, C1.3, L1, N1,
+                     id2, x2, C2.1, C2.2, C2.3, L2, N2,
                     id3, x3, C3.1, C3.2, C3.3, L3, N3) %>% filter(id3 != 0) %>% distinct()

ct_data_level4 =
  ct_data %>% select(id1, x1, C1.1, C1.2, C1.3, L1, N1,
                     id2, x2, C2.1, C2.2, C2.3, L2, N2,
                     id3, x3, C3.1, C3.2, C3.3, L3, N3,
                     id4, x4, C4.1, C4.2, C4.3, L4) %>% filter(id4 != 0) %>% distinct()

L2_v = ct_data_level2[,c("C2.1","C2.2","C2.3")]
L2_v = as.matrix(L2_v)

L1_v = ct_data_level2[,c("C1.1","C1.2","C1.3")]
L1_v = as.matrix(L1_v)

u2 = L1_v  # parent
> v2 = L2_v  # child
> 
> ct_dir_l2_vars = ct_data_level2[c("x2")]
> 
> ct_dir_l2 = systematic_fit_direction(ct_dir_l2_vars, u2, v2, T)
> ct_dir_l2
```

### Model for Direction

```
> ## Level 2
>
> L2_v = ct_data_level2[,c("C2.1","C2.2","C2.3")]
> L2_v = as.matrix(L2_v)
> 
> L1_v = ct_data_level2[,c("C1.1","C1.2","C1.3")]
> L1_v = as.matrix(L1_v)
> 
> u2 = L1_v  # parent
> v2 = L2_v  # child
> 
> ct_dir_l2_vars = ct_data_level2[c("x2")]
> 
> ct_dir_l2 = systematic_fit_direction(ct_dir_l2_vars, u2, v2, T)
> ct_dir_l2
>
>
> ## Level 3
>
> L3_v = ct_data_level3[,c("C3.1","C3.2","C3.3")]
> L3_v = as.matrix(L3_v)
> 
> L2_v = ct_data_level3[,c("C2.1","C2.2","C2.3")]
> L2_v = as.matrix(L2_v)
> 
> u3 = L2_v  # parent
> v3 = L3_v  # child
> 
> ct_dir_l3_vars = ct_data_level3[c("x3", "x2", "C2.3", "L2", "N2")]
> 
> ct_dir_l3 = systematic_fit_direction(ct_dir_l3_vars, u3, v3, T)
> ct_dir_l3
>
>
> ## Level 4
> L4_v = tree4[,c("C4.1","C4.2","C4.3")]
> L4_v = as.matrix(L4_v)
> 
> L3_v = tree4[,c("C3.1","C3.2","C3.3")]
> L3_v = as.matrix(L3_v)
> 
> u4 = L3_v  # parent
> v4 = L4_v  # child
> 
> ct_dir_l4_vars = ct_data_level4[c("x4", 
+                                   "x3", "C2.3", "L3", "N3", 
+                                   "x2", "C1.3", "L2", "N2")]
> 
> ct_dir_l4 = systematic_fit_direction(ct_dir_l3_vars, u4, v4, T)
> ct_dir_l4
```


## Help

For questions, please email nishan [dot] mudalige [at] gmail [dot] com .
