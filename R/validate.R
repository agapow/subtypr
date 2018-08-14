# Validate clustering by various means, internal & external

validate_internal <- function(aff_matrix, labels) {
  # XXX: what should the signature of this function look like?
}


validate_internal <- function(labels_internal, labels_external) {
  # XXX: what should the signature of this function look like?
}


#' Plots the survival curves and compute the Cox p-value
#'
#' Plots the survival curves and compute the Cox p-value using the package
#'   \href{https://cran.r-project.org/web/
#'         packages/survival/index.html}{survival}.
#'
#'
#'
#' @param survival_time the follow-up time of the patients.
#' @param death_status integer, 0 = alive, 1 = dead
#' @param patients_partition a partition of the patients
#' @param return logical, TRUE to return the cox p-value
#' @param no_plot logical, set to TRUE to avoid plotting and return
#'   the cox p-value.
#' @return the cox p-value (if return or no_plot is TRUE).
#'
#' @seealso \code{\link[survival]{Surv}}, \code{\link[survival]{coxph}}
#'
#' @export
#'
analyze_survival <- function(survival_time,
                             death_status,
                             patients_partition,
                             return = FALSE,
                             no_plot = FALSE) {

  ## Preconditions & preparation:
  df <- data.frame(
    time = survival_time,
    status = death_status,
    cluster = patients_partition
  )

  ## Main:

  df$surv_obj <- with(df, survival::Surv(time, status))

  # Survival curve
  res_surv_fit <- survival::survfit(
    formula = surv_obj ~ cluster,
    data = df
  )


  # Cox regression

  res_cox_ph <- survival::coxph(surv_obj ~ cluster, df,
    ties = "exact"
  )


  cox_p_value <- summary(res_cox_ph)[["coefficients"]][1, "Pr(>|z|)"]


  # Plot

  if (!no_plot) {
    k <- length(unique(patients_partition))
    pl <- plot(res_surv_fit,
      xlab = "Time (days)", ylab = "Survival percentage",
      main = "Kaplan-Meier survival plot", col = rainbow(k)
    )


    opt <- options("scipen")
    options(scipen = -2)

    legend("topright",
      inset = 0.02,
      legend = c(
        unlist(lapply(1:k, function(i) paste0("subtype ", i))),
        signif(cox_p_value, 2)),
      box.col = rainbow(1, alpha = 1),
      lty = c(rep(1, k), 0),
      col = rainbow(k),
      box.lty = 0,
      title = "subtypes & Cox p-value"
    )
    options(opt)
  }

  ## Post-processing & return:

  if (return) {
    return(cox_p_value)
  }
}
