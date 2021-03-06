\name{predictionInterval}
\alias{predictionInterval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prediction interval for effect estimate of replication study}
%%  ~~function to do ... ~~
\description{
  Computes a prediction interval for the effect estimate of the replication study
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predictionInterval(thetao, seo, ser, tau = 0, conf.level = 0.95, 
                   designPrior = "predictive")
}
\arguments{
  \item{thetao}{
    A vector of effect estimates from original studies. 
  }
  \item{seo}{
    A vector of standard errors of the original effect estimates.
  }
  \item{ser}{
    A vector of standard errors of the replication effect estimates.
  }
  \item{tau}{
    Between-study heterogeneity standard error. Default is \code{0}
    (no heterogeneity). Is only taken into account when \code{designPrior = "predictive"} or \code{designPrior = "EB"}.
  }
  \item{conf.level}{
    The confidence level of the prediction intervals. Default is 0.95.
  }
  \item{designPrior}{
    Either \code{"conditional"}, \code{"predictive"}, or \code{"EB"}.  
    Defaults to \code{"predictive"}. If \code{"EB"}, the contribution of the original study to the predictive distribution is shrunken towards zero based on the evidence in the original study (with empirical Bayes).
  }
}
  \details{
  This function computes a prediction interval and a mean estimate under a specified 
  predictive distribution of the replication effect estimate. Setting
  \code{designPrior = "conditional"} is not recommended since this ignores the uncertainty of
  the original effect estimate. 
  See Patil, Peng, and Leek (2016) and Pawel and Held (2020) for details.
    %%  ~~ If necessary, more details than the description above ~~
  }
\value{
A data frame with the following columns
  \item{lower}{
    Lower limit of prediction interval
  }
  \item{mean}{
    Mean of predictive distribution
  }
  \item{upper}{
    Upper limit of prediction interval
  }
}
  \references{
  
  Patil, P., Peng, R. D., Leek, J. T. (2016). What should researchers expect when they replicate studies? A statistical view of replicability in psychological science. \url{https://dx.doi.org/10.1177/1745691616646366}
  
  Pawel, S., Held, L. (2020). Probabilistic forecasting of replication studies. PLoS ONE 15(4):e0231416. \url{https://doi.org/10.1371/journal.pone.0231416}

  }
  \author{
    Samuel Pawel
  }
  \examples{
predictionInterval(thetao = c(1.5, 2, 5), seo = 1, ser = 0.5, designPrior = "EB")
  
# plot prediction intervals for different original effect estimates
thetao <- c(2, 2.5, 3)
pi_pred <- predictionInterval(thetao = thetao, seo = 1, ser = 1)
pi_cond <- predictionInterval(thetao = thetao, seo = 1, ser = 1, 
                              designPrior = "conditional")
pi_eb <- predictionInterval(thetao = thetao, seo = 1, ser = 1, designPrior = "EB")
plot(thetao - 0.03, pi_pred$mean, xlim = c(1, 3.5), ylim = c(-2, 7), pch = 20, xaxt = "n",
     xlab = expression(hat(theta)[o]), ylab = expression(hat(theta)[r]), las = 2)
axis(side = 1, at = thetao)
abline(h = 0, lty = 2, col = "gray70")
arrows(thetao - 0.03, pi_pred$lower, thetao - 0.03, pi_pred$upper, 
       length = 0.02, angle = 90, code = 3)
points(thetao, pi_cond$mean, pch = 20, col = "darkred")
arrows(thetao, pi_cond$lower, thetao, pi_cond$upper, length = 0.02, angle = 90, 
       code = 3, col = "darkred")
points(thetao + 0.03, pi_eb$mean, pch = 20, col = "darkblue")
arrows(thetao + 0.03, pi_eb$lower, thetao + 0.03, pi_eb$upper, length = 0.02, angle = 90, 
       code = 3, col = "darkblue")
legend("topleft", c("predictive", "conditional", "EB"), title = "designPrior", 
       pch = 20, col = c("black", "darkred", "darkblue"), bty = "n")
       
# compute prediction intervals for replication projects
data("RProjects", package = "ReplicationSuccess")
par(mfrow = c(2, 2), las = 1, mai = rep(0.65, 4))
for (p in unique(RProjects$project)) {
  data_project <- subset(RProjects, project == p)
  PI <- predictionInterval(thetao = data_project$fiso, seo = data_project$se_fiso, 
                           ser = data_project$se_fisr)
  PI <- tanh(PI) # transforming back to correlation scale
  within <- (data_project$rr < PI$upper) & (data_project$rr > PI$lower)
  coverage <- mean(within)
  color <- ifelse(within == TRUE, "#333333B3", "#8B0000B3")
  study <- seq(1, nrow(data_project))
  plot(data_project$rr, study, col = color, pch = 20, 
       xlim = c(-0.5, 1), xlab = expression(italic(r)[r]), 
       main = paste0(p, ": ", round(coverage*100, 1), "\% coverage"))
  arrows(PI$lower, study, PI$upper, study, length = 0.02, angle = 90, 
         code = 3, col = color)
  abline(v = 0, lty = 3)
}
}