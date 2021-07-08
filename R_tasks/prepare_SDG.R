##' Prepare SDG
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_SDG -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_SDG
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_SDG <- function(DF_clean, short_name_scale_str) {

  # DEBUG
  # debug_function(prepare_SDG)

  # [ADAPT]: Items to ignore, reverse and dimensions ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  names_dimensions = c("") # If no dimensions, keep names_dimensions = c("")
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  standardized_names(short_name_scale = short_name_scale_str, 
                     dimensions = names_dimensions,
                     help_names = FALSE) # help_names = FALSE once the script is ready
  
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE)
  
  # Show number of items, responses, etc. [uncomment to help prepare the test] 
  # prepare_helper(DF_long_RAW, show_trialid_questiontext = TRUE)
  
  
  # Create long DIR ------------------------------------------------------------
  
  DF_long_DIR = 
    DF_long_RAW %>% 
    select(id, trialid, RAW) %>%
    
    
  # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************

    # Transformations
    mutate(
      DIR = 
        case_when(
          
          # TODO: This give WARNINGS -----
          trialid %in% c("SDG_001", "SDG_002", "SDG_003", "SDG_005", "SDG_006", "SDG_008", "SDG_009", "SDG_010_1", "SDG_011_1") ~ RAW,
          trialid == "SDG_004" & RAW == "Femenino" ~ "0",
          trialid == "SDG_004" & RAW == "Masculino" ~ "1",
          trialid == "SDG_004" & RAW == "No binario" ~ "0",
          
          trialid == "SDG_007" & RAW == "Menos de 100 mil" ~ "1",
          trialid == "SDG_007" & RAW == "100 mil – 199 mil" ~ "2",
          trialid == "SDG_007" & RAW == "200 mil – 399 mil" ~ "3",
          trialid == "SDG_007" & RAW == "400 mil – 599 mil" ~ "4",
          trialid == "SDG_007" & RAW == "600 mil – 799 mil" ~ "5",
          trialid == "SDG_007" & RAW == "800 mil – 999 mil" ~ "6",
          trialid == "SDG_007" & RAW == "1.000.000 – 1.500.000" ~ "7",
          trialid == "SDG_007" & RAW == "Más de 1.500.000" ~ "8",
          
          trialid %in% c("SDG_010", "SDG_011", "SDG_012") & RAW == "No" ~ "0",
          trialid %in% c("SDG_010", "SDG_011", "SDG_012") & RAW == "Si" ~ "1",
          
          is.na(RAW) ~ NA_character_,
          trialid %in% paste0(short_name_scale_str, "_", items_to_ignore) ~ NA_character_,
          TRUE ~ "9999"
        )
    ) 
    
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
    

  # Create DF_wide_RAW_DIR -----------------------------------------------------
  DF_wide_RAW =
    DF_long_DIR %>% 
    pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") %>% 
    
    # NAs for RAW and DIR items
    mutate(!!name_RAW_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$")))),
           !!name_DIR_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$")))))
      
    
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
    # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)

  DF_wide_RAW_DIR =
    DF_wide_RAW 
  

  # [END ADAPT]: ***************************************************************
  # ****************************************************************************


  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
 
}
