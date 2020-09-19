library(rvest)

extract_usdves <- function(source="bcv") {
  # Collect usdves data from different sources
  urls <- list("bcv"="http://www.bcv.org.ve/")
  xpaths <- list("bcv"='//*[@id="dolar"]/div/div/div[2]/strong')

  usdves <- read_html(urls[[source]]) %>%
    html_node(xpath=xpaths[[source]]) %>%
    html_text(trim=T)

  list(date=format(Sys.time(), "%Y-%m-%d %H:%M"),
       usdves=sub(",", ".", gsub("\\.", "", usdves))
       )
}

update_usdves <- function() {
  # Update the usdves history file
  file <- "data/usdves.csv"

  new_row <- do.call(extract_usdves, list(source="bcv"))

  write.table(new_row,
              file,
              sep=",",
              append=T,
              col.names=!file.exists(file),
              row.names=F,
              quote=F)
}
