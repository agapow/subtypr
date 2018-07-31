# Validate clustering by various means, internal & external

validate_internal <- function(aff_matrix, labels) {
    # XXX: what should the signature of this function look like?
}


validate_internal <- function(labels_internal, labels_external) {
    # XXX: what should the signature of this function look like?
}


analyze_survival <- function(survival_time,
                             death_status,
                             samples_partition,
                             return = FALSE,
                             plot = TRUE) {

  ## Preconditions & preparation:
  df <- data.frame(time = survival_time,
                   status = death_status,
                   cluster = samples_partition)

  ## Main:

  df$surv_obj <- with(df, survival::Surv(time, status))

  # Survival curve
  res_surv_fit <- survival::survfit(formula = surv_obj ~ cluster,
                                data = df)


  # Cox regression

  res_cox_ph <- survival::coxph(surv_obj ~ cluster, df,
                                ties = "exact")


  cox_p_value <- summary(res_cox_ph)[["coefficients"]][1, "Pr(>|z|)"]


  # Plot
  if (plot) {
    k <- length(unique(samples_partition))
      pl <- plot(res_surv_fit,
         xlab = "Time (days)", ylab = "Survival percentage",
         main = "Kaplan-Meier survival analysis", col = rainbow(k))

    #xlim <- max(pl$x) for the legend


    legend("topright", inset = 0.02,
           legend = unlist(lapply(1:k,
                                  function(i) paste0("subtype ", i))),
           box.col = rainbow(1, alpha = 1),
           lty = 1,
           col=rainbow(k),
           box.lty = 0)


    legend("bottomleft", inset = 0.02,
           legend = paste0("Cox p-value = ", round(cox_p_value, 6)),
           box.lty = 0)
  }

  ## Post-processing & return:

  if (return) {
    return(cox_p_value)
  }
}

## Tests --------------

load("/data/data_for_review/COAD/COAD_ProcessedData.RData")

partition <- c(1,2,3,4,rep(11, 69), rep(12, 73))

analyze_survival(survival$Survival, survival$Death, partition)





