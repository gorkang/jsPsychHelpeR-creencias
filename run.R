##### TODO ######

# Remember to copy the prepare_NEWTASKS() functions to jsPsychHelper

# SDG y Reports TIENEN DATOS SENSIBLES!!!!



# Initial setup -----------------------------------------------------------

  # Run the fist time or when you have an error
  # targets::tar_renv()
  # source("setup/setup.R")


# Load libraries ---------------------------------------------------------

  invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))



# Sync data from server ---------------------------------------------------

  pid_target = 8
  
  update_data(id_protocol = pid_target)

  # Delete duplicates 
  delete_duplicates(folder = paste0("data/", pid_target, "/"), check = TRUE)


# Destroy cache (_targets folder) -----------------------------------------

  # Destroys cache to force a clean run
  targets::tar_destroy()
  
  # Invalidates input_files to force data preparation
  targets::tar_invalidate(matches("input_files"))

  
# Run project --------------------------------------------------------------

  targets::tar_make()
  
  # IF running megatron
  # TODO : create a test with this? Or maybe just the "manual" test will be enough
  targets::tar_load(DF_joined)
  number_items_tasks(DF_joined)


# Run in parallel ---------------------------------------------------------

  # targets::tar_invalidate(matches("ids_reports"))
  # targets::tar_make_future(workers = 12)
  
    
# Visualize targets networks -----------------------------------------------

  targets::tar_visnetwork(targets_only = TRUE, label = "time") #label = "time"
  # targets::tar_glimpse()
  
  # Global time and time per process
  sum(targets::tar_meta(fields = seconds)$seconds, na.rm = TRUE)
  targets::tar_meta() %>% select(name, seconds) %>% arrange(desc(seconds))


# See warnings in all functions ------------------------------------------
  targets::tar_meta(fields = warnings) %>% tidyr::drop_na(warnings)# See warnings


# Errors ------------------------------------------------------------------

  # If we get an error, load workspace of errored state
  targets::tar_workspace(TESTS)
  targets::tar_undebug() # Delete all the debugging stuff
  
  # WARNINGS
  targets::tar_meta(fields = warnings) %>% drop_na
  
  # Data frame of targets info
  targets::tar_manifest(fields = "command")
