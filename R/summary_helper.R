library(stats)

#' Create a summary table from a polr model.
#' 
#' Calculates and adds p-values, significance flags ('.', '*', '**', '***') and human readable explanations.
#' 
#' In polr, negative coefficients mean decreasing the higher value odds and positive coefficients mean 
#' increasing the higher value odds. This stands for all coefficients except intercepts.
#' 
#' Also adds factor reference levels in the summary for a clearer picture.
#'
#' @param polr.model A model, built with polr.
#' @param factors.list A list of factor variables used in building a model. Used to add reference levels.
#' 
#' @return Summary table with added columns for easier interpretation of results.
#' 
get_summary_table_for_polr_model = function(polr.model, factors.list) {
  
  # all coefficients together, intercepts too, with estimate, std. error and t value
  sumtable = data.frame((summary(polr.model))$coefficients)
  
  # last few coefficients are intercepts, let's emphasize that
  intercepts_rows = nrow(sumtable) - ((length(polr.model$zeta) - 1):0)
  rownames(sumtable)[intercepts_rows] = paste0("Intercept_", rownames(sumtable)[intercepts_rows])
  intercepts_rows = grepl("Intercept_", rownames(sumtable))
  
  # now adding p-values and significance, it is a usual way to display the results, don't know why polr doesn't have it
  sumtable$p.value = pt(abs(sumtable$t.value), polr.model$nobs - polr.model$edf, lower.tail = FALSE) * 2
  sumtable$signif = ifelse(sumtable$p.value < .1, ".", "")
  if (any(sumtable$p.value < .05)) sumtable[sumtable$p.value < .05, ]$signif = "*"
  if (any(sumtable$p.value < .01)) sumtable[sumtable$p.value < .01, ]$signif = "**"
  if (any(sumtable$p.value < .001)) sumtable[sumtable$p.value < .001, ]$signif = "***"

  # adding odds effect
  sumtable$odds_effect = exp(sumtable$Value)
  
  # adding human readable explanation of the odds effect
  sumtable$explanation = "" # create a column
  negative_odds_effect = (sumtable$odds_effect < 1)
  if (any(negative_odds_effect & !intercepts_rows)) {
    sumtable[negative_odds_effect & !intercepts_rows, ]$explanation = paste0("decrease buying odds by factor ", format(abs(sumtable[negative_odds_effect & !intercepts_rows, ]$odds_effect), digits = 6))
  }
  if (any(!negative_odds_effect & !intercepts_rows)) {
    sumtable[!negative_odds_effect & !intercepts_rows, ]$explanation = paste0("increase buying odds by factor ", format(sumtable[!negative_odds_effect & !intercepts_rows, ]$odds_effect, digits = 6))
  }
  # intercepts have a different interpretation
  if (any(negative_odds_effect & intercepts_rows)) {
    sumtable[negative_odds_effect & intercepts_rows, ]$explanation = "reference buying odds < 50%"
  }
  if (any(!negative_odds_effect & intercepts_rows)) {
    sumtable[!negative_odds_effect & intercepts_rows, ]$explanation = "reference buying odds >= 50%"
  }
  
  # add reference levels of factors for a clearer picture
  for (an in names(factors.list)) {
    an_rows = grep(an, rownames(sumtable))
    sumtable = rbind(sumtable[1:(min(an_rows)-1), ], 
                     c(0, 0, 0, 1, "", 1, "reference level, no effect"),
                     sumtable[min(an_rows):nrow(sumtable), ]
    )
    rownames(sumtable)[min(an_rows)] = paste0(an, levels(factors.list[[an]])[1], ", ref. level")
  }
  
  # have to convert back to numeric, don't know why the columns were converted to character in the loop above
  numeric_columns = !(colnames(sumtable) %in% c("signif", "explanation"))
  sumtable[, numeric_columns] = lapply(sumtable[, numeric_columns], as.numeric)
  
  sumtable
}

#' Create a summary table from a glm logistic model.
#' 
#' Adds significance flags ('.', '*', '**', '***') and human readable explanations.
#' 
#' In glm, negative coefficients mean increasing the higher value odds and positive coefficients mean 
#' decreasing the higher value odds. This stands for all coefficients.
#' 
#' Also adds factor reference levels in the summary for a clearer picture.
#'
#' @param glm.model A logistic model, built with glm.
#' @param factors.list A list of factor variables used in building a model. Used to add reference levels.
#' 
#' @return Summary table with added columns for easier interpretation of results.
#' 
get_summary_table_for_glm_logistic_model = function(glm.model, factors.list) {
  
  sumtable.buyer.glm = data.frame((summary(glm.model))$coefficients)
  sumtable.buyer.glm$signif = ifelse(sumtable.buyer.glm$Pr...z.. < .1, ".", "")
  if (any(sumtable.buyer.glm$Pr...z.. < .05)) sumtable.buyer.glm[sumtable.buyer.glm$Pr...z.. < .05, ]$signif = "*"
  if (any(sumtable.buyer.glm$Pr...z.. < .01)) sumtable.buyer.glm[sumtable.buyer.glm$Pr...z.. < .01, ]$signif = "**"
  if (any(sumtable.buyer.glm$Pr...z.. < .001)) sumtable.buyer.glm[sumtable.buyer.glm$Pr...z.. < .001, ]$signif = "***"
  sumtable.buyer.glm$odds_effect = exp(sumtable.buyer.glm$Estimate)
  sumtable.buyer.glm$explanation = "" # create a column
  negative_odds_effect = (sumtable.buyer.glm$odds_effect < 1)
  intercepts_rows = grepl("(Intercept)", rownames(sumtable.buyer.glm))
  
  if (any(negative_odds_effect & !intercepts_rows)) {
    sumtable.buyer.glm[negative_odds_effect & !intercepts_rows, ]$explanation = 
      paste0("decrease buying odds by factor ", 
             format(abs(sumtable.buyer.glm[negative_odds_effect & !intercepts_rows, ]$odds_effect), digits = 6))
  }
  if (any(!negative_odds_effect & !intercepts_rows)) {
    sumtable.buyer.glm[!negative_odds_effect & !intercepts_rows, ]$explanation = 
      paste0("increase buying odds by factor ", 
             format(sumtable.buyer.glm[!negative_odds_effect & !intercepts_rows, ]$odds_effect, digits = 6))
  }
  # intercepts have a different interpretation
  if (any(negative_odds_effect & intercepts_rows)) {
    sumtable.buyer.glm[negative_odds_effect & intercepts_rows, ]$explanation = "reference buying odds < 50%"
  }
  if (any(!negative_odds_effect & intercepts_rows)) {
    sumtable.buyer.glm[!negative_odds_effect & intercepts_rows, ]$explanation = "reference buying odds >= 50%"
  }
  for (an in names(factors.list)) {
    #print(an)
    an_rows = grep(an, rownames(sumtable.buyer.glm))
    sumtable.buyer.glm = rbind(sumtable.buyer.glm[1:(min(an_rows)-1), ], 
                               c(0, 0, 0, 1, "", 1, "reference level, no effect"),
                               sumtable.buyer.glm[min(an_rows):nrow(sumtable.buyer.glm), ]
    )
    rownames(sumtable.buyer.glm)[min(an_rows)] = paste0(an, levels(factors.list[[an]])[1], ", ref. level")
  }
  
  # have to convert back to numeric, don't know why the columns were converted to character in the loop above
  numeric_columns = !(colnames(sumtable.buyer.glm) %in% c("signif", "explanation"))
  sumtable.buyer.glm[, numeric_columns] = lapply(sumtable.buyer.glm[, numeric_columns], as.numeric)
  
  sumtable.buyer.glm
}



