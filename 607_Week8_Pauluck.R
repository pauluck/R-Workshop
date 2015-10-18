# Puneet Auluck - 607 - Week 8 Assignment
library(XML)

# HTML table parse
htmlfile_df <- readHTMLTable("c:\\607_Week8\\geography.html", header=TRUE, as.data.frame = TRUE)[[1]]
str(htmlfile_df)
htmlfile_df


# XML parse
xmlfile <- xmlTreeParse("c:\\607_Week8\\geography.xml")
xmltop <- xmlRoot(xmlfile)
geo <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))
geo_xml <- data.frame(t(geo), row.names=NULL)
str(geo_xml)
geo_xml


# JSON parse
library(jsonlite)
geo_json <- fromJSON("c:\\607_Week8\\geography.json")
str(geo_json)
geo_json

# I would not say the data frames are identical because the structure of each file is different and they are parsed differently as well.
# For example, JSON file contains review of books in array while HTML does not so they contain different rows of data. 

