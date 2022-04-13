# Workshop Helper Functions

library(haven)
library(tidyverse)
library(dplyr)
library(kableExtra)


naturalize_labelled_df <- function(labelled_df, levels = "default", 
                                   native_numerical_suffix = "", 
                                   fctr_suffix = "_fct", 
                                   char_suffix = ""){
  # all labelled variables as factors
  native_df <- labelled_df %>% 
    rename_with(~ paste0(.x, native_numerical_suffix), .cols = where(is.labelled)) %>%
    mutate(
      across(where(is.labelled), ~.x %>% 
               zap_labels() %>% 
               zap_formats() %>% 
               zap_widths() %>% 
               zap_missing())
    ) 
  
  # all labelled variables transformed to native R
  # user defined missings turned to NA
  # Most variables will become numeric, except for string variables
  
  fctr_df <- labelled_df %>% 
    select(where(is.labelled)) %>% 
    rename_with(~ paste0(.x, fctr_suffix)) %>% 
    mutate(
      across(where(is.labelled), ~ as_factor(.x, levels = "both"))
    )
  
  #lastly, add a suffix for spss character vectors (containing only text)
  
  native_df %>% add_column(fctr_df) %>% 
    rename_with(~ paste0(.x, char_suffix), .cols=where(is.character))
}

register_labels <- function(labelled_df){
  # Collects all variable labels in a data frame
  
  var_labels <- labelled_df %>% 
    map_chr(~ attr(.x, "label")) %>% 
    as_tibble(rownames = NA) %>% 
    rownames_to_column("var_name") %>% 
    rename(variable_label = value)
  
  value_labels <- labelled_df %>% 
    map_chr(~ attr(.x, "labels") %>% paste0("[", ., "] ", names(.), collapse = "; ")) %>% 
    as_tibble(rownames = NA) %>% 
    rownames_to_column("var_name") %>% 
    rename(value_labels = value)
  
  full_join(var_labels, value_labels, by = "var_name")
  
}

fetch_var_lab <- function(var_name_to_look_up, label_table){
  # fetches the correct label from the df, when given a var_name
  # This also works with the suffix var names from naturalize_labelld_df() e.g., v_01_fctr
  label_table %>% 
    filter(var_name_to_look_up == var_name) %>% 
    pull(variable_label) %>% 
    pluck(1)
}

fetch_val_lab <- function(var_name_to_look_up, label_table){
  # fetches the correct label from the df, when given a var_name
  # This also works with the suffix var names from naturalize_labelld_df() e.g., v_01_fctr
  label_table %>% 
    filter(var_name_to_look_up == var_name) %>% 
    pull(value_labels) %>% 
    pluck(1)
}

# Generates an intercorrelation matrix for a dataframe; 
# non-numeric vars are discarded

form_intercorr_matrix <- function(df, discard_non_numeric = TRUE){
  if(discard_non_numeric) {
    df <- df %>% select(where(is.numeric))
  } 
  
  cor_matrix <- tibble(
    variables = names(df),
    variable_b = list(variables)
  ) %>% 
    unnest(cols = c(variable_b)) %>% 
    mutate(correlation = map2_dbl(variables, variable_b, ~ cor(df[[.x]], df[[.y]]))) %>% 
    pivot_wider(values_from = correlation, names_from = variable_b) %>% 
    select(variables, sort(colnames(.))) %>% 
    arrange(variables)
  
  cor_matrix_variables <- cor_matrix[1]
  cor_matrix <- cor_matrix[-1]
  cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
  cor_matrix %>% add_column(cor_matrix_variables, .before = 1) %>% 
    mutate(across(where(is.numeric), round, 3)) 
}
