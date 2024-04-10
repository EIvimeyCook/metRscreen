<p align="center">
  <img src="https://github.com/EIvimeyCook/metRscreen/blob/master/inst/metRscreen/www/logo/metRscreen.png" width = "200"/>
</p>

<div align="center">
 <h1>metRscreen</h1>
</div>

The metRscreen shiny app allows you to screen papers via their abstracts and titles along with highlighting keywords in multiple colours.

## Installation
Currently the metRscreen package is not on CRAN, but you can install the development version from GitHub using the devtools package:

```{r}
install.packages("devtools")
devtools::install_github("EIvimeyCook/metRscreen")
library(metRscreen)
```

## Running metRscreen
The only function metRscreen is metRscreen(). The most important argument is "screen.file".

```{r}
library(metRscreen)
metRscreen(screen.file = "~/Desktop/Example.csv")
```

## metRscreen arguments
You can give specific rejection reasons as a vector with "reject.list = c()".

```{r}
metRscreen(screen.file = "~/Desktop/Example.csv", reject.list = c("no control", "wrong study system"))
```

The .rds file that's produced allows for reloading of previous screening decisions. This will be automatically reloaded the next instance metRscreen is run and will return screening to the same state (i.e. with the same reject list, hidden or showing paper components, and any previous screening decisions). 


