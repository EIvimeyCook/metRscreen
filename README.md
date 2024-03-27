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
