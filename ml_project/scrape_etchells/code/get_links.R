###############################################################################
### PULL LINKS FROM FLEET 20 WEBSITE
###############################################################################

##scrape page for year blocks
summary_url = "http://www.etchellsfleet20.org/results.html"

results_page = read_html(summary_url)
years = html_nodes(results_page, ".textfooter .box table td~ td+ td , .textfooter .box table td:nth-child(1)")

##loop through year blocks and pull links into output table
links = data.frame( year = character(),
                    name = character(),
                    link = character())

for(list in years) {
  year = html_text(html_nodes(list, "p"))
  regattas = html_nodes(list, "a")
  for(regatta in regattas){
    name = html_text(regatta)
    link = html_attr(regatta, "href")
    row  = data.frame(year, name, link)
    links %<>% rbind(row)
  }
}

write.csv(links, "../intermediate/links.csv")
