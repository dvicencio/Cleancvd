# R/rename_variables.R

#' Rename Variables in a Data Frame
#'
#' This function renames variables in a data frame based on a named vector or list.
#'
#' @param df A data frame with variables to rename.
#' @param mappings A named vector or list where the names are current variable names and the values are new names.
#' @param guess A logical value indicating whether to use guessing logic for names not specified in mappings.
#' @return A data frame with renamed variables.
#'
#' @examples
#' df <- data.frame(bpbpm = 120, hr = 80)
#' rename_variables(df, c(bpbpm = "bloodpressure", hr = "heartrate"))
rename_variables <- function(df, mappings, guess = FALSE) {
        if (!is.data.frame(df)) {
                stop("The input must be a data frame.")
        }

        if (!is.named(mappings)) {
                stop("Mappings must be a named vector or list.")
        }

        # Rename based on exact mappings
        for (old_name in names(mappings)) {
                new_name <- mappings[[old_name]]
                if (old_name %in% names(df)) {
                        names(df)[names(df) == old_name] <- new_name
                }
        }

        # Guess missing mappings if requested
        if (guess) {
                df <- guess_and_rename(df, mappings)
        }

        return(df)
}

#' Guess and Rename Variables
#'
#' This function attempts to guess and rename variables that are not explicitly mapped.
#'
#' @param df A data frame with variables to rename.
#' @param mappings A named vector or list with known mappings.
#' @return A data frame with guessed variable names.
guess_and_rename <- function(df, mappings) {
        # Example: Implement a simple heuristic-based guessing mechanism
        guessed_mappings <- list(
                "bpbpm" = "bloodpressure",
                "hr" = "heartrate",
                "wt" = "weight",
                "ht" = "height",
                "temp" = "temperature"
        )

        for (var_name in names(df)) {
                if (!(var_name %in% names(mappings))) {
                        # Check if there's a guessed mapping for this variable
                        if (var_name %in% names(guessed_mappings)) {
                                guessed_name <- guessed_mappings[[var_name]]
                                message(sprintf("Guessing: Renaming '%s' to '%s'", var_name, guessed_name))
                                names(df)[names(df) == var_name] <- guessed_name
                        }
                }
        }

        return(df)
}
