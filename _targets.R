# jsPsychHelpeR with {targets}

# Parameters --------------------------------------------------------------

  pid_target = 8


# Libraries ---------------------------------------------------------------

  library(targets) 
  library(tarchetypes) 


# Set options, load packages -----------------------------------------------
  
  # Source all /R files
  lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source)
  lapply(list.files("./R_tasks/", full.names = TRUE, pattern = ".R$"), source)
  options(pillar.sigfig = 5)
  
  
  # Packages to load
  main_packages = c("cli", "crayon", "furrr", "patchwork", "renv", "tarchetypes", "targets", "testthat")
  data_preparation_packages = c("dplyr", "forcats", "here", "janitor", "purrr", "readr", "stringr", "tibble", "tidyr") #"safer", 
  data_analysis_packages = c("broom", "broom.mixed", "emmeans", "gmodels", "gt", "gtsummary", "irr", "lme4", "parameters", "performance", "psych", "sjPlot", "tinytex")
  data_visualization_packages = c("ggalluvial", "ggridges", "viridis")
  non_declared_dependencies = c("qs", "visNetwork", "webshot", "performance")
  extra_packages = c("shrtcts")
  packages_to_load = c(main_packages, data_preparation_packages, data_analysis_packages, data_visualization_packages, non_declared_dependencies, extra_packages)
  
  # target options (packages, errors...)
  tar_option_set(packages = packages_to_load, # Load packages for all targets
                 workspace_on_error = TRUE) # Needed to load workspace on error to debug
  

  # Make sure tests run always
  # if (file.exists("_targets/objects/TESTS") == TRUE ) targets::tar_invalidate(matches("TESTS"))

  
  

# Define targets -------------------------------------------------------------
  
targets <- list(
  
  ## Read files --------------------------------------------------------------
  
  # RAW data
  tar_target(input_files, list.files(path = "data/8", pattern="*.csv", full.names = TRUE)), #, format = "file" (IF files in vault/ first run fails)
  tar_target(DF_raw, read_data(input_files, anonymize = FALSE)),
  
  # Cleaned data
  tar_target(DF_clean, create_clean_data(DF_raw)),
  
  # Diccionary of tasks
  tar_target(DICCIONARY_tasks, create_diccionary_tasks(DF_clean), priority = 1),
  
  
  ## Prepare tasks -----------------------------------------------------------
  
  # Use create_new_task("NAME_OF_TASK") create new preparation_scripts
  tar_target(df_SDG, prepare_SDG(DF_clean, short_name_scale_str = 'SDG')),
  tar_target(df_CAS, prepare_CAS(DF_clean, short_name_scale_str = "CAS")),
  tar_target(df_DASS21, prepare_DASS21(DF_clean, short_name_scale_str = "DASS21")),
  tar_target(df_SRBQP, prepare_SRBQP(DF_clean, short_name_scale_str = "SRBQP")),
  tar_target(df_PVC, prepare_PVC(DF_clean, short_name_scale_str = "PVC")),
  tar_target(df_Report, prepare_Report(DF_clean, short_name_scale_str = "Report")),
  
  # # tar_target(df_Consent, prepare_Consent(DF_clean, short_name_scale_str = 'Consent')),
  # # tar_target(df_Goodbye, prepare_Goodbye(DF_clean, short_name_scale_str = 'Goodbye')),
  tar_target(df_IRS, prepare_IRS(DF_clean, short_name_scale_str = 'IRS')),
  
  tar_target(df_PBS, prepare_PBS(DF_clean, short_name_scale_str = 'PBS')), #rPBS
  tar_target(df_PSS, prepare_PSS(DF_clean, short_name_scale_str = 'PSS')),
  
 
  
  ## Join tasks --------------------------------------------------------------
  
  tar_target(DF_joined, 
             create_joined(
               df_CAS,
               df_DASS21,
               df_IRS,
               df_PBS,
							 df_PSS,
							 df_PVC,
							 df_Report,
							 df_SRBQP,
							 df_SDG
             )),
  

  
  ## Analysis ----------------------------------------------------------------- 
  
  # Prepare a DF ready for the analysis
  tar_target(DF_analysis, create_DF_analysis(DF_joined)),
  
  # [TODO] Descriptive Table 1
  # Important: Should we compare DF_analysis with the final data used in each model? 
  tar_render(descriptives_table1, "doc/descriptives_table1.Rmd", deployment = "main"),

  
  # Models
  # tar_target(model_E1, analysis_model_E1(DF_analysis)),


  # Tables and plots use the model (e.g. model_XXX) as input.  
  # Most model objects in R include the data used to fit the model

  # Tables
  # tar_target(table1_model_E1, analysis_model_E1_table(model_E1)),

  # Plots
  tar_target(plots_descriptive, analysis_descriptive_plots(DF_joined, DF_raw)),
  # tar_target(plot1_model_E1, analysis_model_E1_plot(model_E1)),
  
  
  ## Tests -------------------------------------------------------------------
  
  # [REMEMBER]: Have to manually put every target we have a test for here (except the automatic tests: 'input_files_automatic_tests_str' takes care of that)
  tar_target(input_files_automatic_tests_str, list.files(path = "_targets/objects/", pattern="df_*", full.names = FALSE, ignore.case = FALSE)),

  tar_target(TESTS, test_testhat(input_files_automatic_tests_str = input_files_automatic_tests_str,
                                 input_files,
                                 DF_raw,
                                 DF_clean,
                                 DICCIONARY_tasks,
                                 DF_joined
                                 )
  ),
  
  # Report ------------------------------------------------------------------

  # Automatic report
  tar_render(report_DF_clean, "doc/report_DF_clean.Rmd", 
             params = list(last_task = "Goodbye",
                           pid_report = pid_target),
             output_file = paste0("../outputs/reports/report_DF_clean.html")),
  
  # Progress report
  tar_render(report_PROGRESS, path = "doc/report_PROGRESS.Rmd", 
             params = list(input_files_vector = input_files, 
                           pid_report = pid_target, 
                           last_task = "Goodbye", 
                           goal = 500),
             output_file = paste0("../outputs/reports/report_PROGRESS_", pid_target , ".html")),
  
  # Create array with the id's of participants that want the report and already finished the experiment to use it in the tar_render_rep() below
  tar_target(ids_reports, ids_reports_create(DF_analysis, df_PVC)),

  # Personalized report for each participant
  tar_render_rep(report_Participants, 
                 path = here::here("doc/report_Participants.Rmd"), 
                 params = tibble::tibble(id = ids_reports$id,
                                         output_file = paste0("informe_", ids_reports$id, "_", ids_reports$name)),
                 output_dir = paste0("outputs/reports/personalized_reports"))
  )


# Declare pipeline --------------------------------------------------------

targets
