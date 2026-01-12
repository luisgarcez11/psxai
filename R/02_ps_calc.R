#' Weighted mean different
#'
#' @param weights weights
#' @param treatment treatment vector
#' @param outcome outcome vector
#' @param n_bootstrap bootstrap samples
#'
#' @returns object with dataframe with LR PS calculation for to balance each
#' independent varaible
#' @export
#'
#' @examples
#' ape_calc(rnorm(100,5,0.5),
#' sample(c(0,1), 100, replace = TRUE),
#' sample(c(0,1), 100, replace = TRUE))
ape_calc <- function(weights, treatment, outcome, n_bootstrap = 200){

  treat_names = c(0,1)
  if(is.factor(treatment)){
    treat_names = levels(treatment);
    if(levels(treatment)[1] == "high" &
       levels(treatment)[2] == "low" ){
      treat_names <- c("low", "high");
      treatment <- factor(treatment, levels = treat_names)
    }}



  list("effect" = paste(treat_names[2], "-", treat_names[1]),
       "estimate" =
    stats::weighted.mean(x = as.numeric(outcome[treatment == treat_names[2]]), w = weights[treatment == treat_names[2]])-
    stats::weighted.mean(x = as.numeric(outcome[treatment == treat_names[1]]), w = weights[treatment == treat_names[1]]))}


#' PS calculation
#'
#' @param obj object returned from preprocess_data()
#' @param threshold SMD threshold to be balanced
#' @param n_bootstrap bootstrap samples
#'
#' @returns object with dataframe with LR PS calculation for to balance each
#' independent variable
#' @export
#'
#' @examples
#' ps_calc(obj1)
ps_calc <- function(obj, threshold = 0.10, n_bootstrap = 200){

  outcome <- categorized_var <- var <- weightit_object <-
    balance_table <- balanced_vars <- var <- balance_table <-
    balanced_vars <- total_dep_vars <- cat_option <-
    ps_model <- NULL

  #create IPTW object for each variable
  iptw <-  obj$var_mapping %>%
    # slice(12) %>%
    dplyr::filter(outcome == FALSE) %>%
    dplyr::select(categorized_var, var) %>%
    dplyr::mutate(weightit_object = purrr::map2(.x = categorized_var,
                                   .y = var,
                                  .f = function(.x, .y){
                                    # if(.x == "categorized_chest_pain_type_non_anginal"){ browser()}
                                    WeightIt::weightit(

                                      formula = formula(paste(.x, "~",
                                                      paste(obj$var_mapping$original_var,
                                                            collapse = " + "),
                                                      " - ",
                                                      paste(obj$var_mapping$original_var[which(obj$var_mapping$var == .y)],
                                                            collapse = " - "),
                                                      " - ",
                                                      obj$var_mapping$original_var[which(obj$var_mapping$var == obj$pred)],
                                                      " - ",
                                                      obj$var_mapping$original_var[which(obj$var_mapping$var == obj$y)])),
                                      data = obj$data_preprocessed,
                                      method = "glm",
                                      estimand = "ATE"
                                    )
                                  },
                                  .progress = "ps_estimation")) %>%
    dplyr::mutate(ps_model = purrr::map(.x = weightit_object,
                           .f = function(.x){
                             stats::glm(formula = .x$formula,
                                 family = stats::binomial(),
                                 data = obj$data_preprocessed)
                           })) %>%
    dplyr::mutate(balance_table = purrr::map(.x = weightit_object,
                                      .f = function(.x){
                                        r <- tryCatch(expr = {cobalt::bal.tab(.x)},
                                                      error = function(e){NA})
                                        return(r)
                                        })) %>%
    dplyr::mutate(total_dep_vars = purrr::map_int(.x = balance_table,
                                       .f = function(.x){
                                         length(.x$Balance$Diff.Adj[-1])
                                       }) ) %>%
    dplyr::mutate(balanced_vars = purrr::map_int(.x = balance_table,
                                          .f = function(.x){
                                            if(any(is.na(.x))){return(NA_integer_)}else{
                                              sum(abs(.x$Balance$Diff.Adj[-1]<threshold), na.rm = TRUE)
                                            }
                                            })) %>%
    dplyr::mutate(balance_prop = purrr::map2_chr(.x = balanced_vars,
                                         .y = total_dep_vars,
                                         .f = function(.x, .y){
                                           paste0(.x, "/", .y)
                                         })) %>%
    dplyr::mutate(ape = purrr::map(.x = weightit_object,
                            .f = function(.x){
                              ape_calc(weights = .x$weights,
                                       treatment = .x$treat,
                                       outcome = obj$data_preprocessed[[
                                         obj$var_mapping$categorized_var[which(obj$var_mapping$var == obj$y)]]])
                            }))%>%
    dplyr::mutate(ape_ci = purrr::map(.x = weightit_object,
                            .f = function(.x){
                              # browser();
                              n = n_bootstrap;
                              ape_v = c()
                              for(i in 1:n){
                                s = sample(x = 1:length(.x$weights),
                                         size = length(.x$weights) ,
                                         replace = TRUE)
                                est = ape_calc(weights = .x$weights[s],
                                       treatment = .x$treat[s],
                                       outcome = obj$data_preprocessed[[
                                         obj$var_mapping$categorized_var[which(obj$var_mapping$var == obj$y)]]][s])
                                ape_v <- c(ape_v, est$estimate)
                              }
                              return(list("lower_ci" = stats::quantile(ape_v, 0.05),
                                          "upper_ci" = stats::quantile(ape_v, 0.95)))
                            })) %>%
    tidyr::unnest_auto(col = "ape_ci" ) %>%
    tidyr::unnest_auto(col = "ape" ) %>%
    dplyr::mutate(cat_option = stringr::str_remove(categorized_var, "categorized_")) %>%
    dplyr::mutate(cat_option = stringr::str_remove(cat_option, var )) %>%
    dplyr::mutate(cat_option = stringr::str_squish(stringr::str_replace_all(cat_option, "_", " " ))) %>%
    dplyr::mutate(variable = paste0(var, " (",cat_option, ")" )) %>%
    dplyr::mutate_at("variable", ~stringr::str_replace_all(., "\\(\\)", "")) %>%
    dplyr::mutate_at("variable", ~stringr::str_replace_all(., "_", " ")) %>%
    dplyr::mutate_at("variable", ~stringr::str_squish(.))

  obj$var_mapping <- iptw

  return(obj)

}
