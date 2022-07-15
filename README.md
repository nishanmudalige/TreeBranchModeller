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

## Example: Pixie Tree data (also reffered to as the Christmas Tree)

### Read the data into R

```
> ## Read The data set
> ct_data = read.table("https://raw.githubusercontent.com/nishanmudalige/TreeBranchModeller/main/data/CTobsn.txt", header = T)
> 
> ## Level 2 data
> ct_data_level2 = 
+   ct_data %>% select(id1, x1, C1.1, C1.2, C1.3, L1, N1,
+                      id2, x2, C2.1, C2.2, C2.3, L2, N2) %>% distinct()
> 
> ## Level 3 data
> ct_data_level3 =
+   ct_data %>% select(id1, x1, C1.1, C1.2, C1.3, L1, N1,
+                      id2, x2, C2.1, C2.2, C2.3, L2, N2,
+                      id3, x3, C3.1, C3.2, C3.3, L3, N3) %>% filter(id3 != 0) %>% distinct()
> 
> ## Level 4 data
> ct_data_level4 =
+   ct_data %>% select(id1, x1, C1.1, C1.2, C1.3, L1, N1,
+                      id2, x2, C2.1, C2.2, C2.3, L2, N2,
+                      id3, x3, C3.1, C3.2, C3.3, L3, N3,
+                      id4, x4, C4.1, C4.2, C4.3, L4) %>% filter(id4 != 0) %>% distinct()
```

### Models for Direction
```
> ## Run code above to read in the data into R and create the objects which contain relevant data for each level
> 
> ## Model for Level 2
>
> u3 = ct_data_level3 %>% select(C2.1, C2.2, C2.3)  # parent
> v3 = ct_data_level3 %>% select(C3.1, C3.2, C3.3)  # child
> 
> ct_dir_l3_vars = ct_data_level3 %>% select(x3, x2, C2.3, L2, N2)
>  
> ct_dir_l3 = systematic_fit_direction(ct_dir_l3_vars, u3, v3, T)
>
>
> ## Model for Level 3
>
> u3 = ct_data_level3 %>% select(C2.1, C2.2, C2.3)  # parent
> v3 = ct_data_level3 %>% select(C3.1, C3.2, C3.3)  # child
> 
> ct_dir_l3_vars = ct_data_level3 %>% select(x3, x2, C2.3, L2, N2)
>  
> ct_dir_l3 = systematic_fit_direction(ct_dir_l3_vars, u3, v3, T)
> ct_dir_l3
>
>
> ## Model for Level 4
>
> u4 = ct_data_level4 %>% select(C3.1, C3.2, C3.3)  # parent
> v4 = ct_data_level4 %>% select(C4.1, C4.2, C4.3)  # child
> 
> ct_dir_l4_vars = ct_data_level4 %>% 
+                     select(x4, x3, C2.3, L3, N3, x2, C1.3, L2, N2)
>  
> ct_dir_l4 = systematic_fit_direction(ct_dir_l3_vars, u4, v4, T)
> ct_dir_l4
```

### Models for Number of Offspring Branches
```
> ## Run code above to read in the data into R and create the objects which contain relevant data for each level
>
> ## Level 2 data
> ct_n_l2_vars = c("x2", "C2.3", "L2") 
> ct_n_l2 = systematic_fit_full_offspring("N2 ~ 1", 
+                               vars = ct_n_l2_vars, 
+                               data = ct_data_level2, 
+                               linear = T, 
+                               quad_int = T, 
+                               hierarchical = T, 
+                               trace = F)
> ct_n_l2
>
> ## Level 3 data
>
> ct_offspring_l3_vars = c("x2", "C2.3", "L2", "N2",
+                          "x3", "C3.3", "L3")
> ct_offspring_l3 = systematic_fit_full_offspring("N3 ~ 1",
+                                      vars = ct_offspring_l3_vars, 
+                                      data = ct_data_level3, 
+                                      linear = T, quad_int = T, 
+                                      hierarchical = T, trace = F)
> ct_offspring_l3
```

## Help

For questions, please email nishan [dot] mudalige [at] gmail [dot] com .
