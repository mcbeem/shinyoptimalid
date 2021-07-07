# Optimal Gifted Identification

library(shiny)
library(ggplot2)
library(matrixcalc)
library(mnormt)
library(giftedCalcs)

# Define UI for the application
ui <- fluidPage(

   # Application title
   titlePanel("Optimal Gifted Identification"),
   helpText("This app calculates the identification system performance that",
            "is achieveable using optimal identification, as per an upcoming",
            "publication by Matthew T. McBee and Scott J. Peters."),
   fluidRow(
     column(4,
            numericInput("assessments",
                         "Number of assessments to be combined", min=2, max=5, value=2),

            sliderInput("nom.cutoff",
                        "Nomination cutoff (percentile)",
                        min = .00,
                        max = .99,
                        value = .9,
                        step=.005),

            sliderInput("t.cutoff",
                        HTML("Test cutoff (percentile)"),
                        min = .5,
                        max = .99,
                        value = .9,
                        step=.005),

            actionButton("goButton", "Update")
     ),

     column(4,
            conditionalPanel(
              condition = "input.assessments >= 2 & input.assessments <= 5",
              numericInput(inputId="rely1",label="Reliability of assessment 1",
                           min=-1, max=1,step=.01, value=.8)
            ),

            conditionalPanel(
              condition = "input.assessments >= 2 & input.assessments <= 5",
              numericInput(inputId="rely2",label="Reliability of assessment 2",
                           min=-1, max=1,step=.01, value=.8)
            ),

            conditionalPanel(
              condition = "input.assessments >= 3 & input.assessments <= 5",
              numericInput(inputId="rely3",label="Reliability of assessment 3",
                           min=-1, max=1,step=.01, value=.8)
            ),

            conditionalPanel(
              condition = "input.assessments >= 4 & input.assessments <= 5",
              numericInput(inputId="rely4",label="Reliability of assessment 4",
                           min=-1, max=1,step=.01, value=.8)
            ),

            conditionalPanel(
              condition = "input.assessments >= 5 & input.assessments <= 5",
              numericInput(inputId="rely5",label="Reliability of assessment 5",
                           min=-1, max=1,step=.01, value=.8)
            )
     ),

     column(4,
            conditionalPanel(
              condition = "input.assessments >= 2 & input.assessments <= 5",
              numericInput(inputId="r12",label="Correlation between assessments 1 and 2",
                           min=0, max=1,step=.01, value=.7)
            ),

            conditionalPanel(
              condition = "input.assessments >= 3 & input.assessments <= 5",
              numericInput(inputId="r13", label="Correlation between assessments 1 and 3",
                           min=0, max=1, step=.01, value=.7),
              numericInput(inputId="r23",label="Correlation between assessments 2 and 3",
                           min=0, max=1, step=.01, value=.7)

            ),

            conditionalPanel(
              condition = "input.assessments >= 4 & input.assessments <= 5",
              numericInput(inputId="r14", label="Correlation between assessments 1 and 4",
                           min=0, max=1, step=.01, value=.7),
              numericInput(inputId="r24", label="Correlation between assessments 2 and 4",
                           min=0, max=1, step=.01, value=.7),
              numericInput(inputId="r34", label="Correlation between assessments 3 and 4",
                           min=0, max=1, step=.01, value=.7)
            ),

            conditionalPanel(
              condition = "input.assessments >= 5 & input.assessments <= 5",
              numericInput(inputId="r15", label="Correlation between assessments 1 and 5",
                           min=0, max=1, step=.01, value=.7),
              numericInput(inputId="r25", label="Correlation between assessments 2 and 5",
                           min=0, max=1, step=.01, value=.7),
              numericInput(inputId="r35", label="Correlation between assessments 3 and 5",
                           min=0, max=1, step=.01, value=.7),
              numericInput(inputId="r45", label="Correlation between assessments 4 and 5",
                           min=0, max=1, step=.01, value=.7)
            )
     )  # closes column
  ), # closes fluidRow

  fluidRow(
    column(5,
           h4("Identification system parameters"),
           tableOutput('summary.stat'),

           h4("Identification system performance"),
           tableOutput('summary.perf')
           ), # closes column

    column(7,
           plotOutput("performancePlot")
    ) # closes column
    ),  #closes fluidRow

  fluidRow(
    column(12,
           helpText("(c) Matthew T. McBee, Dept. of Psychology, East Tennessee State University."),
           helpText("mcbeem@etsu.edu @TunnelofFire")
    )  # closes column
  ) # closes fluidRow
) # closes ui

# Define server logic
server <- function(input, output) {

  # define the functions that will be used to do the calculations

  #  build a covariance (correlation) matrix from the unique correlation elements
  # vector2corr <- function(assessments, r) {
  #   # convert the vector of corrs to a matrix
  #   cov <- matrix(1, nrow=assessments, ncol=assessments)
  #   cov[lower.tri(cov)] <- r
  #   t.cov <- t(cov)
  #   cov[upper.tri(cov)] <- t.cov[upper.tri(t.cov)]
  #   return(cov)
  # }

  vector2corr <- function(assessments, r) {
    cov <- matrix(1, nrow=assessments, ncol=assessments)
    if (assessments == 2) {

      cov <- matrix(c(1, r[1], r[1], 1), nrow=assessments, ncol=assessments, byrow=T)

    } else if (assessments == 3) {

      cov <- matrix(c( 1,   r[1], r[2],
                      r[1],   1,  r[3],
                      r[2],  r[3],  1), nrow=assessments, ncol=assessments, byrow=T)

    } else if (assessments == 4) {

      #       r <- isolate(c(input$r12, input$r13, input$r23, input$r14, input$r24, input$r34))

      cov <- matrix(c( 1,    r[1], r[2], r[4],
                       r[1],   1,  r[3], r[5],
                       r[2],  r[3],  1,  r[6],
                       r[4],  r[5], r[6],  1), nrow=assessments, ncol=assessments, byrow=T)

    } else if (assessments == 5) {

      cov <- matrix(c( 1,    r[1], r[2], r[4], r[7],
                      r[1],   1,  r[3], r[5], r[8],
                      r[2],  r[3],  1,  r[6], r[9],
                      r[4],  r[5], r[6],  1,  r[10],
                      r[7],  r[8],  r[9],  r[10], 1), nrow=assessments, ncol=assessments, byrow=T)

    }

    return(cov)
  }


  # function: calculate each assessment's correlation with the mean of the assessments
  # cor_x_mean <- function(assessments, cov) {
  #   unique.r <- cov[lower.tri(cov)]
  #   #define vector to hold results
  #   corrs <- vector()
  #   for (i in 1:assessments) {
  #     corrs[i] <- (1/assessments)*sum(cov[i,]) /
  #       sqrt((1/assessments)+(2*((1/assessments)^2))*sum(unique.r))
  #   }
  #  return(corrs)
  # }

  # function: calculate the reliability of the mean
  # reliability_mean <- function(reliabilities) {
  #   mean.r <- mean(reliabilities)
  #   r_mean <- mean.r / (mean.r + ((1-mean.r)/ length(reliabilities)))
  #   return(r_mean)
  # }

  # funtion: calculate the shrinkage-adjusted cutoff on the mean
  # sd.mean <- function(assessments, cov) {
  #   sum.corr <- sum(cov[lower.tri(cov)])
  #   return(sqrt((1/assessments) + (2*((1/assessments)^2)*(sum.corr))))
  # }

  # function: calculate performance and performance curve values
  calc.performance <-  function(r.test, valid, t.cutoff, nom.cutoff, mu=0) {

    nom.mu <- mu*valid

    a11 = sqrt(r.test/(1-r.test))
    b11 = ((qnorm(t.cutoff,0,1)) / sqrt(r.test))
    a21 = sqrt(((valid^2)/r.test)/(1-((valid^2)/r.test)))
    b21 = ((qnorm(nom.cutoff, 0, 1))/(valid/sqrt(r.test)))

    x <- seq(0,3, .025)
    y <- pnorm(a11*(x-b11),0,1)*pnorm(a21*(x-b21),0,1)

    cov <- matrix(c(
      1, valid/sqrt(1*r.test), sqrt(1), valid/sqrt(1),
      valid/sqrt(1*r.test), 1, valid/sqrt(r.test), sqrt(r.test),
      sqrt(1), valid/sqrt(r.test), 1, valid,
      valid/sqrt(1), sqrt(r.test), valid, 1),
      nrow=4, ncol=4)

    # Sensitivity

    # cutoffs for group 1
    tau_n <- qnorm(nom.cutoff, 0, 1)
    tau_t <- qnorm(t.cutoff, 0, 1)

    # order:     n true, t true, n obs, t obs
    means <- c(mu, mu, mu*valid, mu*sqrt(r.test))

    sensitivity <-   sadmvn(lower=c(-5, tau_t, tau_n, tau_t), upper=c(5,5,5,5),
                            mean=means,
                            varcov=cov, maxpts=10*100000)[[1]] /
      sadmvn(lower=c(-5, tau_t, -5, -5), upper=c(5,5,5,5),
             mean=means,
             varcov=cov, maxpts=10*100000)[[1]]

    # incorrect identification rate

    IIR <- sadmvn(lower=c(-5, -5, tau_n, tau_t), upper=c(5, tau_t, 5, 5),
                   mean=means, varcov=cov, maxpts=10*10000)[[1]] /
      sadmvn(lower=c(-5, -5, tau_n, tau_t), upper=c(5, 5, 5, 5),
             mean=means, varcov=cov, maxpts=10*10000)[[1]]

    # proportion identified

    pID <- sadmvn(lower=c(-5, -5, tau_n, tau_t), upper=c(5, 5, 5, 5),
                   mean=means, varcov=cov, maxpts=10*100000)[[1]]

    passrate <-  sadmvn(lower=c(-5, -5, tau_n, tau_t), upper=c(5,5,5,5),
                         mean=means, varcov=cov, maxpts=10*100000)[[1]] /
      sadmvn(lower=c(-5, -5, tau_n, -5), upper=c(5,5,5,5),
             mean=means, varcov=cov, maxpts=10*100000)[[1]]

    return(list(x=x, y=y, sensitivity=round(sensitivity,3),
                IIR=round(IIR, 3), pID=round(pID,3), passrate=round(passrate, 3)))
  }

   output$performancePlot <- renderPlot({

     # Take a dependency on input$goButton
     input$goButton

     # read the inputs
     assessments <- isolate(input$assessments)

     nom.cutoff <- isolate(input$nom.cutoff)

     t.cutoff <- isolate(input$t.cutoff)

     #read the reliability and correlation coefficients
     if (assessments==2) {
       reliabilities <- isolate(c(input$rely1, input$rely2))
       r <- isolate(c(input$r12))

       # check for impossible correlation
       bad1 <-ifelse(r[1]>sqrt(prod(reliabilities)), 1, 0)
       bad2 <- 0
       bad3 <- 0
       bad4 <- 0
       bad5 <- 0
       bad6 <- 0
       bad7 <- 0
       bad8 <- 0
       bad9 <- 0
       bad10 <- 0


     }
     if (assessments==3) {
       reliabilities <- isolate(c(input$rely1, input$rely2, input$rely3))
       r <- isolate(c(input$r12, input$r13, input$r23))
       # check for impossible correlation
       bad1 <-ifelse(r[1]>sqrt(prod(reliabilities[c(1,2)])), 1, 0)
       bad2 <-ifelse(r[2]>sqrt(prod(reliabilities[c(1,3)])), 1, 0)
       bad3 <-ifelse(r[3]>sqrt(prod(reliabilities[c(2,3)])), 1, 0)
       bad4 <- 0
       bad5 <- 0
       bad6 <- 0
       bad7 <- 0
       bad8 <- 0
       bad9 <- 0
       bad10 <- 0

     }
     if (assessments==4) {
       reliabilities <- isolate(c(input$rely1, input$rely2, input$rely3, input$rely4))
       r <- isolate(c(input$r12, input$r13, input$r23, input$r14, input$r24, input$r34))
       # check for impossible correlation
       bad1 <-ifelse(r[1]>sqrt(prod(reliabilities[c(1,2)])), 1, 0)
       bad2 <-ifelse(r[2]>sqrt(prod(reliabilities[c(1,3)])), 1, 0)
       bad3 <-ifelse(r[3]>sqrt(prod(reliabilities[c(2,3)])), 1, 0)

       bad4 <-ifelse(r[4]>sqrt(prod(reliabilities[c(1,4)])), 1, 0)
       bad5 <-ifelse(r[5]>sqrt(prod(reliabilities[c(2,4)])), 1, 0)
       bad6 <-ifelse(r[6]>sqrt(prod(reliabilities[c(3,4)])), 1, 0)

       bad7 <- 0
       bad8 <- 0
       bad9 <- 0
       bad10 <- 0
     }
     if (assessments==5) {
       reliabilities <- isolate(c(input$rely1, input$rely2, input$rely3, input$rely4, input$rely5))
       r <- isolate(c(input$r12, input$r13, input$r23, input$r14, input$r24,
              input$r34, input$r15, input$r25, input$r35, input$r45))

       # check for impossible correlation
       bad1 <-ifelse(r[1]>sqrt(prod(reliabilities[c(1,2)])), 1, 0)
       bad2 <-ifelse(r[2]>sqrt(prod(reliabilities[c(1,3)])), 1, 0)
       bad3 <-ifelse(r[3]>sqrt(prod(reliabilities[c(2,3)])), 1, 0)

       bad4 <-ifelse(r[4]>sqrt(prod(reliabilities[c(1,4)])), 1, 0)
       bad5 <-ifelse(r[5]>sqrt(prod(reliabilities[c(2,4)])), 1, 0)
       bad6 <-ifelse(r[6]>sqrt(prod(reliabilities[c(3,4)])), 1, 0)

       bad7 <-ifelse(r[7]>sqrt(prod(reliabilities[c(1,5)])), 1, 0)
       bad8 <-ifelse(r[8]>sqrt(prod(reliabilities[c(2,5)])), 1, 0)
       bad9 <-ifelse(r[9]>sqrt(prod(reliabilities[c(3,5)])), 1, 0)
       bad10 <-ifelse(r[10]>sqrt(prod(reliabilities[c(4,5)])), 1, 0)
     }


     # error checking
     validate(
       need(input$assessments <= 5, "The maximum number of assessments is 5.")
     )

     validate(
       need(input$assessments >= 2, "The minimum number of assessments is 2.")
     )

     validate(
       need(is.integer(input$assessments)==TRUE, "The number of assessments must be an integer between 2 and 5.")
     )

     validate(
       need(max(reliabilities) <= 1,
            "The maximum possible value for a reliability coefficient is 1.
            Please check the values you entered.")
       )

     validate(
       need(min(reliabilities) >= 0,
            "The minimum possible value for a reliability coefficient is 0.
            Please check the values you entered.")
       )

     validate(
       need(max(r) <= 1,
            "The maximum possible value for a correlation is 1.
            Please check the values you entered.")
       )

     validate(
       need(min(r) >= 0,
            "The minimum possible value for a correlation (in this calculation) is 0.
            Please check the values you entered.")
       )

     # check for impossible correlations

     validate(
       need(bad1 == 0,
            paste("The maximum possible value for the correlation between assessment 1 and 2 is",
                  round(sqrt(prod(reliabilities[c(1,2)])),3))
       )
     )

     validate(
       need(bad2 == 0,
            paste("The maximum possible value for the correlation between assessment 1 and 3 is",
                  round(sqrt(prod(reliabilities[c(1,4)])),3))
       )
     )

     validate(
       need(bad3 == 0,
            paste("The maximum possible value for the correlation between assessment 2 and 3 is",
                  round(sqrt(prod(reliabilities[c(2,3)])),3))
       )
     )

     validate(
       need(bad4 == 0,
            paste("The maximum possible value for the correlation between assessment 1 and 4 is",
                  round(sqrt(prod(reliabilities[c(1,4)])),3))
       )
     )

     validate(
       need(bad5 == 0,
            paste("The maximum possible value for the correlation between assessment 2 and 4 is",
                  round(sqrt(prod(reliabilities[c(2,4)])),3))
       )
     )

     validate(
       need(bad6 == 0,
            paste("The maximum possible value for the correlation between assessment 3 and 4 is",
                  round(sqrt(prod(reliabilities[c(3,4)])),3))
       )
     )

     validate(
       need(bad7 == 0,
            paste("The maximum possible value for the correlation between assessment 1 and 5 is",
                  round(sqrt(prod(reliabilities[c(1,5)])),3))
       )
     )

     validate(
       need(bad8 == 0,
            paste("The maximum possible value for the correlation between assessment 2 and 5 is",
                  round(sqrt(prod(reliabilities[c(2,5)])),3))
       )
     )

     validate(
       need(bad9 == 0,
            paste("The maximum possible value for the correlation between assessment 3 and 5 is",
                  round(sqrt(prod(reliabilities[c(3,5)])),3))
       )
     )

     validate(
       need(bad10 == 0,
            paste("The maximum possible value for the correlation between assessment 4 and 5 is",
                  round(sqrt(prod(reliabilities[c(4,5)])),3))
       )
     )

     # display no results for out-of-range values
     validate(
       need(assessments <= 5, ""),
       need(assessments >= 2, ""),
       need(is.integer(input$assessments)==TRUE, ""),
       need(max(r) <= 1, ""),
       need(min(r) >= 0, ""),
       need(max(reliabilities) <= 1, ""),
       need(min(reliabilities) >= 0, ""),
       need(max(bad1, bad2, bad3, bad4, bad5, bad6, bad7, bad8, bad9, bad10) == 0, "")
     )


     # build the correlation matrix
     cov <- vector2corr(assessments=assessments, r=r)
     pos.def <- is.positive.definite(cov)

     # calculate the correlation between each assessment and the mean of the assessments
     #corrs <- cor_x_mean(assessments=assessments, cov=cov)
     corrs <- giftedCalcs::cor_mean(r=cov)
     valid <- max(corrs)
     validate(
       need(pos.def == TRUE, "The correlations you have entered are impossible. The correlation matrix is not positive definite.")
     )


     best.for.nom <- which(corrs==max(corrs))
     num.for.nom <- length(best.for.nom)

     if (num.for.nom==1) {nom.assessment <- best.for.nom[1]}
     if (num.for.nom==2) {nom.assessment <- paste(best.for.nom[1], " or", best.for.nom[2])}
     if (num.for.nom==3) {nom.assessment <- paste(best.for.nom[1], " or", best.for.nom[2], " or", best.for.nom[3])}
     if (num.for.nom==4) {nom.assessment <- paste(best.for.nom[1], " or", best.for.nom[2], " or", best.for.nom[3], " or", best.for.nom[4])}
     if (num.for.nom==5) {nom.assessment <- paste(best.for.nom[1], " or", best.for.nom[2], " or", best.for.nom[3], " or", best.for.nom[4], " or", best.for.nom[5])}

     # calculate the reliability of the mean
     relyt <- giftedCalcs::reliability_mean(rely=reliabilities, r=cov)

     # calculate shrinkage-adjusted test cutoff
     #test.sd <- sd.mean(assessments=assessments, cov=cov)
     #test.sd <- giftedCalcs::shrinkage_mean(rely=reliabilities, r=cov)
     test.sd <- sqrt(giftedCalcs::var_mean(r=cov, w=rep(1, assessments)))

     # calculate performance
     performance <-   calc.performance(r.test=relyt, valid=valid, t.cutoff=t.cutoff,
        nom.cutoff=nom.cutoff, mu=0)

     data <- data.frame(
       cbind(
         performance$x,
         performance$y
        )
       )

     names(data) <- c("x", "y")

     # make the plot

     p <- ggplot(data=data, aes(x=(x*15)+100, y=y))+geom_line(col="#377eb8")+
       #guides(fill=FALSE, colour=guide_legend(keywidth = 3, keyheight = 1.5))+
       theme_classic()+
       coord_cartesian(xlim=c(100,145), ylim=c(0,1.1))+
       theme(legend.position="bottom",
             text=element_text(size=15),
             plot.title = element_text(hjust = 0.5))+
       scale_color_brewer(palette="Set1")+
       labs(title="Conditional probability of identification",
            col="Group")+
       scale_y_continuous(breaks=seq(0,1, .1))+
       scale_x_continuous(breaks=seq(100,145, 5))+
       xlab("True ability")+ylab("Probability of identification")+
       geom_vline(xintercept=qnorm(c(.6, .7, .8, .9, .95, .975, .99), 100, 15),
                  linetype="dashed", alpha=.25)+
       geom_vline(xintercept=qnorm(t.cutoff, 100, 15),
                  linetype="dashed", alpha=.7, size=.9)+ #  col="#984ea3"

       geom_hline(yintercept=seq(0,1,.1), linetype="dotted", alpha=.15)+
       annotate("text", x = qnorm(.6, 100, 15), y = 1.08, label = "60th\n%ile",
                fontface="bold", size=4)+
       annotate("text", x = qnorm(.7, 100, 15), y = 1.08, label = "70th\n%ile",
                fontface="bold", size=4)+
       annotate("text", x = qnorm(.8, 100, 15), y = 1.08, label = "80th\n%ile",
                fontface="bold", size=4)+
       annotate("text", x = qnorm(.9, 100, 15), y = 1.08, label = "90th\n%ile",
                fontface="bold", size=4)+
       annotate("text", x = qnorm(.95, 100, 15), y = 1.08, label = "95th\n%ile",
                fontface="bold", size=4)+
       annotate("text", x = qnorm(.975, 100, 15), y = 1.08, label = "97.5th\n%ile",
                fontface="bold", size=4)+
       annotate("text", x = qnorm(.99, 100, 15), y =1.08, label = "99th\n%ile",
                fontface="bold", size=4)
     print(p)



    output$summary.stat <- renderTable({

       summary.stat <- data.frame(matrix(
         c("Unadjusted test cutoff", paste("z = ", round(qnorm(t.cutoff, 0, 1),2)),
           "Shrinkage-adjusted test cutoff", paste("z = ", round(qnorm(t.cutoff, 0, test.sd),2)),
           "Effective test reliability", round(relyt, 3),
           "Effective nomination validity", round(valid, 3),
           "Which assessment for nomination",  nom.assessment
         ),
         ncol=2, byrow=T))

       names(summary.stat) <- c("", "")
       # display no results for out-of-range values
       validate(
         need(pos.def == TRUE, ""),
         need(assessments <= 5, ""),
         need(assessments >= 2, ""),
         need(is.integer(input$assessments)==TRUE, ""),
         need(max(r) <= 1, ""),
         need(min(r) >= 0, ""),
         need(max(reliabilities) <= 1, ""),
         need(min(reliabilities) >= 0, ""),
         need(max(bad1, bad2, bad3, bad4, bad5, bad6, bad7, bad8, bad9, bad10) == 0, "")
       )

       summary.stat
     }) # close renderTable

     output$summary.perf <- renderTable({

       # display no results for out-of-range values
       validate(
         need(pos.def == TRUE, ""),
         need(assessments <= 5, ""),
         need(assessments >= 2, ""),
         need(is.integer(input$assessments)==TRUE, ""),
         need(max(r) <= 1, ""),
         need(min(r) >= 0, ""),
         need(max(reliabilities) <= 1, ""),
         need(min(reliabilities) >= 0, ""),
         need(max(bad1, bad2, bad3, bad4, bad5, bad6, bad7, bad8, bad9, bad10) == 0, "")
       )


       summary.perf <- data.frame(matrix(
         c("Sensitivity", performance$sensitivity,
           "Incorrect Identification Rate", performance$IIR,
           "Percent Identified", paste(performance$pID*100, "%", sep=""),
           "Nomination Pass Rate", paste(performance$passrate*100, "%", sep="")
         ),
         ncol=2, byrow=T))
       names(summary.perf) <- c("", "")

       summary.perf


     }) # close renderTable


   }) # close renderPlot
} # close server()

# Run the application
shinyApp(ui = ui, server = server)

