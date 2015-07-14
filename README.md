
**Descripción**

Recurso digital para el aprendizaje del concepto de los intervalos de confianza.

**Objetivo**

Esta aplicación intenta representar gráficamente los fundamentos del cálculo de los intervalos de confianza. En particular, se muestra una representación gráfica de las visiones frecuentista y probabilistica del intervalo de confianza para la media de una población, con varianza desconocida, a partir de una muestra aleatoria te tamaño n. 

Para esto, la aplicación genera inicialmente una población de 10.000 observaciones con una distribución normal conocida, y a partir de ahí se crean las muestras para construir los intervalos.

**Requisitos**

La aplicación está desarrollada en R, por lo que se requiere para su ejecución, tener instalado este lenguaje ([www.r-project.org](https://cran.rstudio.com/)) y [RStudio](https://www.rstudio.com/products/RStudio/#Desktop)

Adicionalmente se requiere los paquetes _Shiny_ y _HMisc_, entre otros. A continuación un ejemplo de como instalarlos:

```{r}
install.packages("shiny",dependencies=TRUE)
install.packages("Hmisc",dependencies=TRUE)
install.packages("grid",dependencies=TRUE)
install.packages("lattice",dependencies=TRUE)
install.packages("survival",dependencies=TRUE)
install.packages("splines",dependencies=TRUE)
install.packages("Formula",dependencies=TRUE)
```

**¿Como ejecutar la aplicación?**

A continuación una ejemplo de como ejecutarlo desde el repositorio de _github.com_:

```{r}
library(shiny)
shiny::runGitHub('IntervalosConfianza', 'josersosa')
```

Para ver adecuadamente las ecuaciones, presione el botón **"Open in Browser"** en la parte superior de la aplicación.
