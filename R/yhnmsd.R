#' yhnmsd()
#'
#' @param data  dataset
#' @param mydv  response variable
#' @param est_d number of digits for estimate
#' @param ci_d  number of digits for CI
#' @param propcut  cutpoint to create proportion
#'
#' @return data.frame
#' @export
#'
yhnmsd <- function(data, mydv, est_d = 2, ci_d = 2, propcut){

  if (FALSE) {
  #' summarizes RCT results
  #' estimate subgroup n() and mean-sd
  #' pivot_wider the results (stratified by timepoints)
  #' mydv = string character
  #' TODO: include dtpoint and tgroup args()
  }

  options(dplyr.summarise.inform = FALSE)

  if(!is.character(mydv)) mydv <- deparse(substitute(mydv))


  df_groupby <-
    data %>%
    dplyr::select(dtpoint, tgroup, .data[[mydv]]) %>%
    group_by(dtpoint, tgroup)

  if(missing(propcut)) {
    ## compute mean and SD

    nmsd <-
      df_groupby %>%
      summarise(across(!!mydv, ~list(
        c( sum(!is.na(.)), smean.sd(.x) ) %>%            # create vector
          set_names(c("n", "m", "SD")) %>%    # name vector elements
          as_tibble_row))) %>%                # convert output to wide tibble
      unnest_wider(.data[[mydv]]) %>%
      data.frame() %>%                        # so that yhestci() will work
      mutate(myestci1 = yhestci(mydf=., "m", "SD", est_digit = est_d, ci_digit = ci_d)) %>%
      mutate(myestci = str_glue("{myestci1}; {n}")) %>%
      dplyr::select(dtpoint, tgroup, myestci) %>%
      pivot_wider(names_from = tgroup, values_from = myestci) %>%
      mutate(mydv = mydv) %>%
      dplyr::select(mydv, everything())

  } else {

    nmsd <-
      df_groupby %>%
      summarise(across(c(!!mydv), ~ list(yhprop(.x >= 6))  )) %>%
      unnest_wider(.data[[mydv]]) %>%
      mutate(myestci = str_glue("{num} ({prop}); {total_n}") ) %>%
      dplyr::select(dtpoint, tgroup, myestci) %>%
      pivot_wider(names_from = tgroup, values_from = myestci)
  }

  return(nmsd)
}
