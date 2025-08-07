
#' Plot Forest
#' Plot a forest plot to represent
#'
#' @param obj object from ps_calc()
#'
#' @returns object with plot representing the results
#' @export
#'
#' @examples
#' plot_forest(obj2)
plot_forest <- function(obj){

  # init
  variable <- balance_prop <- effect <- estimate <-
    lower_ci <- upper_ci <- NULL

  # Example table
  tabletext <- cbind(
    c("variable", obj$var_mapping$variable),
    c("balanced", obj$var_mapping$balance_prop),
    c("direction" , obj$var_mapping$effect),
    c("estimate", round(obj$var_mapping$estimate, 2)),
    c("lower", round(obj$var_mapping$lower_ci,2)),
    c("upper", round(obj$var_mapping$upper_ci,2))
  )


  f <- forestplot::forestplot(labeltext = tabletext,
             mean = c(NA, obj$var_mapping$estimate),
             lower = c(NA, obj$var_mapping$lower_ci),
             upper = c(NA, obj$var_mapping$upper_ci),
             zero = 0, xlab = "Effect size",
             boxsize = 0.2,
             is.summary = c(TRUE, rep(FALSE, nrow(tabletext)-1)))

  obj$plot <- f
  obj$iptw_results <- obj$var_mapping %>%
    dplyr::select(variable, balance_prop, effect, estimate, lower_ci , upper_ci )

  return(obj)
  }
