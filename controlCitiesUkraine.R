#Extract "Cities and towns during the Russo-Ukrainian War" from Wikipedia
# https://en.wikipedia.org/wiki/Cities_and_towns_during_the_Russo-Ukrainian_War

library(rvest)
library(data.table)
library(tidyr)

url <- "https://en.wikipedia.org/wiki/Cities_and_towns_during_the_Russo-Ukrainian_War"
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")
df <- html_table(tbls[grep("Held by",tbls,ignore.case = T)],fill = T)
if(ncol(df[[1]]) < 5){
  df <- df[-1] #remove disclaimer if present
}
df <- rbindlist(df)
df <- separate(data = df, col = "Held by", into = c("Held by", "source"), sep = "\\[")
df$source <- as.numeric(gsub("\\].*","", df$source))
ref <- html_attr(html_node(html_nodes(webpage, 'cite'), "a"), "href")
df$sourceLink <- ref[df$source]
df$`Held by` <- gsub(":","", df$`Held by`)
df <- subset(df, select = -c(Raion) )
df$Population <- gsub("approx. ","", df$Population)
df$Population[df$Name=="Chernobyl"] <- 500

last_edit <- html_text(html_nodes(webpage, "footer #footer-info-lastmod"))
last_edit <- gsub(".*on ","",last_edit)
last_edit <- gsub(", at","",last_edit)
last_edit <- gsub("\\.","",last_edit)
last_edit <- gsub(":","h",last_edit)
last_edit <- gsub("[[:punct:]]", "", last_edit)
last_edit <- gsub("[[:space:]]", " ", last_edit)
df$update <- last_edit
last_edit <- gsub("[[:space:]]", "_", last_edit)


df$NameSimple <- gsub("y", "i", df$Name)

#Coordinates are taken from https://simplemaps.com/ and from Google when missing
coords <- read.csv("ukraineCities.csv")
coords$NameSimple <-  gsub("y", "i", coords$city_ascii)

temp <- merge(df, coords, all.x=T) 
df <- subset(temp, select = -c(NameSimple, city, city_ascii, country,	iso2,	iso3,	admin_name) )

df[is.na(df)] <- ""


write.csv(df, paste0("output/Cities_and_towns_during_the_Russo-Ukrainian_War_", last_edit, ".csv"), 
          row.names = F, 
          fileEncoding = "UTF-8")
write.csv(df, paste0("output/Cities_and_towns_during_the_Russo-Ukrainian_War_", "latest", ".csv"),
          row.names = F,
          fileEncoding = "UTF-8")



