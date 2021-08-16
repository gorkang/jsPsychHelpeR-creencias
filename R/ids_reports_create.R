##' ids_reports_create
##'
##' Get DF with id's of reports to generate
##'
##' @title ids_reports
##'
##' @param DF_analysis 
##' @param df_PVC
##'
##' @return
##' @author gorkang
##' @export
ids_reports_create <- function(DF_analysis, df_PVC) {
  
  # DEBUG
  # debug_function(ids_reports_create)
  
  # Array of id's already sent
  sent_reports = list.files(path = "outputs/reports/personalized_reports/personalized_reports_SENT/", pattern="pdf", full.names = FALSE, ignore.case = FALSE)
  IDs_sent = sent_reports %>% as_tibble() %>% separate(value, into = c("informe", "id", "alias"), extra = "drop") %>% pull(id)
  
  # Create final DF with id's to send
  DF_IDs_tosend = 
    DF_analysis %>% 
    filter(!id %in% IDs_sent) %>% 
    filter(Report_informe_DIRd == 1) %>% # Only those that wanted the report
    filter(id %in% df_PVC$id) %>%   # Only those that finished the experiment
    select(id, Report_alias_DIRd) %>% 
    mutate(name = gsub(" ", "_", gsub(" $|^ |[;@\\.,:]", "", tolower(iconv(Report_alias_DIRd, from = 'UTF-8', to = 'ASCII//TRANSLIT'))))) # Clean up name
  
  # Output of function ---------------------------------------------------------
  return(DF_IDs_tosend) 
  
}
