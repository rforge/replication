\name{predictionInterval}
\alias{predictionInterval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prediction interval for test statistic of replication study}
%%  ~~function to do ... ~~
\description{
  Computes prediction interval for test statistic of the replication study
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predictionInterval(po = NULL, to = p2t(po, alternative = alternative), c = 1,
conf.level = 0.95, designPrior = "predictive", alternative = "two.sided", d = 0)
}
\arguments{
  \item{po}{
    A vector of P-values from original studies, default is \code{NULL}
  }
  \item{to}{
    A vector of test statistics from original studies. 
    Is calculated from \code{po}, if necessary. 
  }
  \item{c}{
    A vector of ratios of the variance of the original and replication effect
    estimates. This is usually the ratio of the sample size of the
    replication study to the sample size of the original study. 
  }
  \item{conf.level}{
    The confidence level of the prediction intervals. Default is 0.95.
  }
  \item{designPrior}{
    Either \code{"conditional"}, \code{"predictive"}, or \code{"EB"}.  
    Defaults to \code{"predictive"}. If \code{"EB"}, the contribution of the original study to the predictive distribution is shrunken towards zero based on the evidence in the original study (with empirical Bayes).
  }
  \item{alternative}{
    Either \code{"less"}, \code{"greater"}, or \code{"two.sided"}.  
    Defaults to \code{"two.sided"}.  
    Specifies direction of the alternative for the P-value. If \code{"two.sided"},
    it is assumed that the original test statistic was positive.
  }
  \item{d}{
  The relative between-study heterogeneity, i.e. the ratio of the heterogeneity variance to the variance of the original effect estimate.
  Default is \code{0} (no heterogeneity).
  Is only taken into account when \code{designPrior = "predictive"} or \code{designPrior = "EB"}.
  } 
}
  \details{
  This function computes a prediction interval and a mean estimate under a specified 
  predictive distribution of the replication test statistic. Setting
  \code{designPrior = "conditional"} is not recommended since this ignores the uncertainty of
  the original effect estimate.
  See Patil, Peng, and Leek (2016) and Pawel and Held (2019) for details.
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
  
  Pawel, S., Held, L. (2019). Probabilistic forecasting of replication studies. \url{https://dx.doi.org/10.31234/osf.io/fhwb7}

  }
  \author{
    Samuel Pawel
  }
\seealso{
  \code{\link{sampleSizePI}}, \code{\link{sampleSizePIwidth}}
}
  \examples{
predictionInterval(po = c(0.05, 0.01, 0.001))
predictionInterval(to = c(1.5, 2, 5), c = 2, designPrior = "EB")
  
# plot prediction intervals for different original test statistics
to <- c(2, 2.5, 3)
pi_pred <- predictionInterval(to = to, c = 1)
pi_cond <- predictionInterval(to = to, c = 1, designPrior = "conditional")
pi_eb <- predictionInterval(to = to, c = 1, designPrior = "EB")
plot(to - 0.03, pi_pred$mean, xlim = c(1.5, 3.5), ylim = c(-2, 7), pch = 20, xaxt = "n",
     xlab = expression(italic(t)[o]), ylab = expression(italic(t)[r]), las = 2)
axis(side = 1, at = to)
abline(h = 0, lty = 2, col = "gray70")
arrows(to - 0.03, pi_pred$lower, to - 0.03, pi_pred$upper, 
       length = 0.02, angle = 90, code = 3)
points(to, pi_cond$mean,  pch = 20, col = "darkred")
arrows(to, pi_cond$lower, to, pi_cond$upper, length = 0.02, angle = 90, 
       code = 3, col = "darkred")
points(to + 0.03, pi_eb$mean, pch = 20, col = "darkblue")
arrows(to + 0.03, pi_eb$lower, to + 0.03, pi_eb$upper, length = 0.02, angle = 90, 
       code = 3, col = "darkblue")
legend("topright", c("predictive", "conditional", "EB"), title = "designPrior", 
       pch = 20, col = c("black", "darkred", "darkblue"), bty = "n")
       
# compute prediction intervals for replication projects
data("ReplicationProjects", package = "ReplicationSuccess")
ReplicationProjects$to <- with(ReplicationProjects, z_O/z_se_O)
ReplicationProjects$tr <- with(ReplicationProjects, z_R/z_se_R)
ReplicationProjects$c <- with(ReplicationProjects, z_se_O^2/z_se_R^2)
par(mfrow = c(2, 2), las = 1, mai = rep(0.65, 4))
for (p in unique(ReplicationProjects$project)) {
  data_project <- subset(ReplicationProjects, project == p)
  PI <- predictionInterval(to = data_project$to, c = data_project$c)*
        data_project$z_se_R # multiplying by standard error to go to z-scale
  PI <- tanh(PI) # converting back to correlation scale
  within <- (data_project$r_R < PI$upper) & (data_project$r_R > PI$lower)
  coverage <- mean(within)
  color <- ifelse(within == TRUE, "#333333B3", "#8B0000B3")
  study <- seq(1, nrow(data_project))
  plot(data_project$r_R, study, col = color, pch = 20, 
       xlim = c(-0.5, 1), xlab = expression(italic(r)[r]), 
       main = paste0(p, ": ", round(coverage*100, 1), "\% coverage"))
  arrows(PI$lower, study, PI$upper, study, length = 0.02, angle = 90, code = 3, col = color)
  abline(v = 0, lty = 3)
}
}