###############################################################################
### SCRAPES RESULTS FROM YACHTSCORING.COM
###############################################################################

##TEST URL FOR DEV --- COMMENT OUT 
url = "https://www.yachtscoring.com/event_results_cumulative.cfm?eID=4367"


scrape_yacht_scoring = function(row) {
  url  = row$link[[1]]
  name = row$name[[1]]
  year = row$year[[1]]
  ##Pull info from page
  page = read_html(url)
  tables = html_nodes(page, "table")
  raw_results = html_table(tables[[4]], fill = TRUE)
  
  ##Clean up extracted info
  ##Get column names
  first_col = 4
  colnames = raw_results[5, first_col:ncol(raw_results)] %>%
    select_if(~sum(!is.na(.)) > 0) %>%
    as.list()
  names(colnames) = NULL
  
  ##Select rows/columns with data
  results = raw_results[9:nrow(raw_results),first_col:(first_col + length(colnames) - 1)]
  results = results[ ,(colnames != "")]
  colnames(results) = colnames[colnames != ""]
  results %<>%
    filter(!is.na(Bow) & Bow != "") %>%
    mutate(year = year,
           url  = url,
           name = name)
  
  return(results)
}
  
