###############################################################################
### SCRAPES RESULTS FROM ETCHELLSFLEET20.ORG
###############################################################################

##TEST URL FOR DEV --- COMMENT OUT
url = "http://www.etchellsfleet20.org/results/2015/2015Doren_results.html"

scrape_etchells_fleet20 = function(row) {
  ##get info from row
  url  = row$link[[1]]
  name = row$name[[1]]
  year = row$year[[1]]
  
  ##Pull html
  url = str_remove(url, "_results")
  page = try(read_html(url))
  
  ##logic for oddball URLs
  if("try-error" %in% class(page)){
    url = str_remove(url, "[0-9]{4}/")
    page = read_html(url)
  }
  
  ##Pull table from HTML
  tables = html_table(page)
  
  ##Exception for 2010 CRYC Cup
  if(length(tables) == 0) {
    url = "http://www.etchellsfleet20.org/results/2010/2010cryccup.html"
    page = read_html(url)
    tables = html_table(page, fill = TRUE)
  }
  
  ##Get columns with info
  results = tables[[which.max(lapply(tables, ncol))]] %>%
    select_if(~sum(!is.na(.)) > 0)
  
  ##Clean up column names
  if("X1" %in% colnames(results)) {
    colnames = results%>%
      filter(X1 %in% c("Place", "Pos", "PLACE", "Pts", "PL.")) %>%
      as.list() %>%
      tolower()
    results %<>%
      filter(!(X1 %in% c("Place", "Pos", "PLACE", "Pts", "PL.")))
    colnames(results) = colnames
  } else {
    colnames(results) = lapply(colnames(results), tolower)
  }
  
  ##Remove duplicated columns
  results = results[,!duplicated(colnames(results))]
    
  ##Pull in row info
  results %>% 
    mutate(year = year,
           url  = url,
           name = name)
}
