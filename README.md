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
The only function metRscreen is metRscreen(). This can be run without any arguments.

```{r}
library(metRscreen)
metRscreen()
```

## metRscreen arguments
Alternatively, you can specific rejection reasons as a vector with "reject.list = c()".

```{r}
metRscreen(reject.list = c("no control", "wrong study system"))
```

You can also load a previous screening iteration by providing the file path to a metRDS file "metRDS = 'path/to/.rds/file'". This will reload metRscreen to the same state as in previous screening iterations (i.e. with the same reject list, hidden paper components, and any previous screening decisions). 

```{r}
metRscreen(metRDS = "~/Desktop/example.rds))
```
