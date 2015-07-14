library(shiny)
library(Hmisc)

# Definición de la aplicación servidor
shinyServer(function(input, output) {
  actPanel <- "0"
  alpha <- 0.1
  mu <- 40
  sigma <- 10
  msize <- 30
  media_m <- 40
  s <- 10
  sd_m <- s/sqrt(msize)
  
  cert <- 1- alpha
  delta <- qt(p=1-alpha/2, df=msize-1) * sd_m
  ic_inf <- media_m - delta
  ic_sup <- media_m + delta
  
  plot_CIs <- function(intervals, true_mean, sample_size, alpha) {
    colnames(intervals) <- c("sample_mean", "l_limit", "u_limit")
    num_means <- nrow(intervals)
    # Plot all the means and intervals:
    errbar(x=1:num_means, 
           y=intervals[, "sample_mean"], 
           ylim = input$RangoX,
           yminus=intervals[, "l_limit"], 
           yplus=intervals[, "u_limit"], 
           pch="+", col="gray", cex.lab=1.0, cex.axis=0.8,
           xlab="# de Intervalos de confianza",
           ylab="Intervalos de confianza",
           errbar.col=ifelse(
             ((intervals[, "l_limit"] <= true_mean) & 	
                (intervals[, "u_limit"] >= true_mean)), "green", "red")
    )
    #	Plot the line through the middle
    abline(h=true_mean,col="black",lty=2)
    #	Add a title
    title(main=paste(num_means, "Intervalos de confianza del ", (1-alpha)*100, "%"), cex.main=1)
  }
  
  calculate_misses <- function(intervals, true_mean){
    misses <- sum(((intervals[, "l_limit"] >= true_mean) | (intervals[, "u_limit"] < true_mean)))
    sample_alpha <- misses/nrow(intervals)
    return(c(misses, sample_alpha))
  }
  
  Poblacion <- reactive({
    rnorm(n=10000, mean=input$mu, sd=input$sigma)
  })

  output$visProb <- renderPlot({
    actPanel <- "visProb"
    alpha <- input$alpha
    mu <- input$mu
    sigma <- input$sigma
    msize <- input$n
    normal_points <- Poblacion()
    samples <- sample(x=normal_points, size=msize, replace=TRUE)
    media_m <- mean(samples)
    s <- sd(samples)
    sd_m <- s/sqrt(msize)
    
    cert <- 1- alpha
    delta <- qt(p=1-alpha/2, df=msize-1) * sd_m
    ic_inf <- media_m - delta
    ic_sup <- media_m + delta
    
    if(input$Mostrar_hist_Pob) hist(normal_points, prob=TRUE, col="light blue", 
                                    main=paste("Intervalo de confianza del ", (1-alpha)*100, "%"),
                                    xlab=paste("Distribución de una muestra de tamaño ", msize))
    fdp <- function(x){ dnorm(x, mean=mu, sd=sigma) }
    hist(samples, prob=TRUE, col="light green",add=T)
    if(input$Mostrar_fdp_Pob) curve(fdp, add=TRUE, col="blue", lwd = 2)
    fdp_muestra <- function(x){ dnorm(x,mean=media_m,sd=s) }
    if(input$Mostrar_fdp_Muest) curve(fdp_muestra, add=TRUE, col="dark green", lwd = 2)
    #fdp_media <- function(x){ dnorm(x,mean=media_m,sd=sd_m) }
    #curve(fdp_media, add=TRUE, col="red", lwd = 2)
    lines(c(ic_inf, ic_sup), c(0, 0), col="red", lwd = 3)
    lines(c(media_m, media_m), c(0.002, 0), col="red", lwd = 3)
    lines(c(ic_inf, ic_inf), c(0.002, 0), col="red", lwd = 3)
    lines(c(ic_sup, ic_sup), c(0.002, 0), col="red", lwd = 3)
  })
  
  output$Medias <- renderPlot({
    actPanel <- "Medias"
    num_CIs <- input$num_CIs
    alpha <- input$alpha
    mu <- input$mu
    sigma <- input$sigma
    msize <- input$n
    normal_points <- Poblacion()
    CIs <- matrix(data=NA, nrow=num_CIs, ncol=3)
    for (i in 1:num_CIs) {
      samples <- sample(x=normal_points, size=msize, replace=TRUE)
      CIs[i, 1] <- mean(samples)
      #  Calculate the sample standard deviations
      sample_sd <- sd(samples)
      sample_mean_sd <- sample_sd/sqrt(msize)
      #  Use the t distribution to calculate the confidence interval:
      delta <- qt(p=1-alpha/2, df=msize-1) * sample_mean_sd
      CIs[i, 2] <- CIs[i, 1] - delta
      CIs[i, 3] <- CIs[i, 1] + delta
    }
    h <- hist(CIs[,1], prob=TRUE, col="yellow", 
              main=paste("Distribución de las medias muestrales de ", num_CIs, " muestras de tamaño ",msize), 
              xlim = input$RangoX,
              ylim = c(0,0.5),
              xlab="Medias muestrales")
    m <- mean(CIs[,1])
    s <- sd(CIs[,1])
    fdp <- function(x){ dnorm(x,m,s) }
    curve(fdp, add=TRUE, col="red", lwd = 2)
  })

  output$visFrec <- renderPlot({
    actPanel <- "visFrec"
    num_CIs <- input$num_CIs
    alpha <- input$alpha
    mu <- input$mu
    sigma <- input$sigma
    msize <- input$n
    normal_points <- Poblacion()
    CIs <- matrix(data=NA, nrow=num_CIs, ncol=3)
    for (i in 1:num_CIs) {
      samples <- sample(x=normal_points, size=msize, replace=TRUE)
      CIs[i, 1] <- mean(samples)
      #  Calculate the sample standard deviations
      sample_sd <- sd(samples)
      sample_mean_sd <- sample_sd/sqrt(msize)
      #	Use the t distribution to calculate the confidence interval:
      delta <- qt(p=1-alpha/2, df=msize-1) * sample_mean_sd
      CIs[i, 2] <- CIs[i, 1] - delta
      CIs[i, 3] <- CIs[i, 1] + delta
    }
    colnames(CIs) <- c("sample_mean", "l_limit", "u_limit")
    calculate_misses(CIs, true_mean=mu)
    plot_CIs(CIs, mu, num_CIs, alpha)
  })
  
  # Despliegue de las ecuaciones
  output$Ecuaciones <- renderUI({
    actPanel <- "Ecuaciones"
    alpha <- input$alpha
    n <- input$n
    if(n<100){
      withMathJax(
        sprintf('Tamaño de la muestra: $$n = %d$$',n),
        sprintf('Nivel de confianza: $$1-\\alpha = %.02f$$', 1-alpha),
        sprintf('Media muestral: $$\\bar{X} = \\frac{\\sum_{i=1}^n x_i}{n}$$'),
        sprintf('Varianza muestral: $$s^2 = \\frac{\\sum_{i=1}^n (x_i-\\bar{X})^2}{n-1} $$'),
        sprintf('Distribución de la Media muestral: $$\\frac{\\bar{X}-\\mu}{\\frac{s}{\\sqrt{n}}} \\sim T \\ de \\ Student(n-1)$$'),
        sprintf('Intervalo de confianza para \\(\\mu\\): $$IC_{1-\\alpha}=[\\bar{X} \\pm T_{1-\\frac{\\alpha}{2},n-1}\\frac{s}{\\sqrt{n}} ]$$')
      )
    }
    else{
      withMathJax(
        sprintf('Tamaño de la muestra: $$n = %d$$',n),
        sprintf('Nivel de confianza: $$1-\\alpha = %.02f$$', 1-alpha),
        sprintf('Media muestral: $$\\bar{X} = \\frac{\\sum_{i=1}^n x_i}{n}$$'),
        sprintf('Varianza muestral: $$s^2 = \\frac{\\sum_{i=1}^n (x_i-\\bar{X})^2}{n-1} $$'),
        sprintf('Distribución de la Media muestral: $$\\frac{\\bar{X}-\\mu}{\\frac{s}{\\sqrt{n}}} \\approx Normal(0,1)$$'),
        sprintf('Intervalo de confianza para \\(\\mu\\): $$IC_{1-\\alpha}=[\\bar{X} \\pm Z_{1-\\frac{\\alpha}{2}}\\frac{s}{\\sqrt{n}} ]$$')
      )
    }
    
  })

})
