library(readr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(plotly)
library(RColorBrewer)
library(deSolve)
library("devtools")
library(shinySIR)

## Create an SIR function 
##ꞵ = rate of transmission, γ = rate of removal, 
#SI = rate of which susceptible and infectious interact.
#Running Shiny and Stating parameters
#Total population = N
AoN_sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- - beta*I *S - v* S * Veff
    dI <-   beta*I *S - gamma *I
    dR <-    gamma *I
    dV <- v * S * Veff
    return(list(c(dS, dI, dR, dV)))
  })
}

init<- c(S = 1-1e-6-1e-6, I = 1e-6, R = 0.0, v=0.01)
parameters <- c(beta = 1.5, gamma = 0.5, Veff = 0.7)
times      <- seq(0, 70, by = 0.01)
## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init,times = times, func = AoN_sir, parms = parameters)
## change to data frame
out <- as.data.frame(out)

head(out)
names(out) = c("Time","Susceptible","Infected","Removed","Vaccinated")
dat.AoNSIR = melt(out,id="Time",measure = c("Susceptible","Infected","Removed","Vaccinated"))
head(dat.AoNSIR)
names(dat.AoNSIR) = c("Time","Compartment","Value")

pp1 = ggplot(dat.AoNSIR) +
  ggtitle("ꞵ = 1.5, v = 0.01, ae = 0.7")+
  geom_line(aes(x = Time,y = Value,color=Compartment),size=1.2) +
  theme_minimal()+
  xlab ("Time")  +
  ylab("Proportion of Population")+
  theme_classic() + 
  theme(text = element_text(size = 20)) +
  ylim(0,1)+ 
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#03A548")) 

show(pp1)
