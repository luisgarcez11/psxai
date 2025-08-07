#' Preprocess data to be analysed
#'
#' @param data Model data
#' @param pred String representing Model Predictions
#' @param indeps String representing independent variables
#' @param y String representing Real outcome
#' @returns list with tibble with independent variables, model predictions and
#' real outcome
#' @export
#'
#' @examples
#' preprocess_data(data = heart_preprocess,
#' indeps = NULL,
#' y = "Heart Disease",
#' pred = "predictions")
preprocess_data <- function(data = heart_preprocess,
                            indeps = NULL,
                            y = "Heart Disease",
                            pred = "predictions"){

  # init
  variable <- balance_prop <- effect <- estimate <-
    lower_ci <- upper_ci <- lc <- heart_preprocess <- NULL

  #expect
  if(is.null(indeps)){indeps <- setdiff(names(data), c(pred, y))}
  testthat::expect_true(all(indeps %in% names(data)))
  testthat::expect_true(all(pred %in% names(data)))
  testthat::expect_true(all(y %in% names(data)))

  #dplyr::select/order
  data <- data %>% dplyr::select(dplyr::all_of(c(indeps, pred, y))) %>%
    janitor::clean_names()

  #variable mapping
  var_mapping <- tibble::tibble(var = character(),
                        original_var= character(),
                        var_type = character(),
                        categorized_var = character())

  #set variable types
  no_unique <- sapply(data, function(x){length(unique(x))})
  binary_vars <- names(no_unique)[which(no_unique == 2)]

  numeric_vars <- (data %>% dplyr::select_if(is.numeric) %>% names()) %>%
    setdiff(binary_vars) %>%
    setdiff(pred)

  char_vars <- (data %>% dplyr::select_if( is.character) %>% names()) %>%
    c((data %>% dplyr::select_if( is.factor) %>% names())) %>%
    setdiff(binary_vars)

  #dummify
  data2 <- tibble::tibble(lc = rep(NA, nrow(data))) %>%
    dplyr::select(-lc)

  for( var in c(binary_vars, numeric_vars, char_vars)){
    #binary
    if (var %in% binary_vars){
      data2[[paste0("original_", var)]] <- as.factor(data[[var]])
      data2[[paste0("categorized_", var)]] <- as.factor(data[[var]])
      var_mapping <- var_mapping %>%
        dplyr::add_row(var = var,
                original_var = paste0("original_", var),
                var_type = "binary",
                categorized_var = paste0("categorized_", var))
    }

    #numeric
    if (var %in% numeric_vars){
      data2[[paste0("original_", var)]] <- data[[var]]
      data2[[paste0("categorized_", var)]] <-
        dplyr::case_when(data[[var]] > median(data[[var]]) ~ "high",
                  data[[var]] <= median(data[[var]]) ~ "low") %>%
        as.factor()
      var_mapping <- var_mapping %>%
        dplyr::add_row(var = var,
                original_var = paste0("original_", var),
                var_type = "numeric",
                categorized_var = paste0("categorized_", var))
    }

    #categorical
    if (var %in% char_vars){

      dummy_data <- fastDummies::dummy_cols(.data = data,
                               select_columns = var,
                               remove_selected_columns = TRUE) %>%
        dplyr::select(dplyr::starts_with(var)) %>%
        janitor::clean_names()

      data2 <- dplyr::bind_cols(data2, dummy_data %>%
                           dplyr::rename_all(~ paste0("original_", .)))

      data2 <- dplyr::bind_cols(data2, dummy_data %>%
                           dplyr::rename_all(~ paste0("categorized_", .)))

      var_mapping <- var_mapping %>%
        dplyr::bind_rows(tibble::tibble(var = var,
                         original_var = paste0("original_", names(dummy_data)),
                         var_type = "categorical",
                         categorized_var = paste0("categorized_", names(dummy_data))))
    }


  }

  #set independent and outcome variable
  var_mapping <- var_mapping %>%
    dplyr::mutate(outcome = dplyr::case_when(var == utils::tail(names(data),1) ~ TRUE,
                               TRUE ~ FALSE))

  #return
  return(list("data_preprocessed" = data2,
              "indeps" = setdiff(names(data2), c(pred, y)),
              "pred" = utils::tail(names(data), 2)[1],
              "y" = utils::tail(names(data),1),
              "var_mapping" = var_mapping))



}
