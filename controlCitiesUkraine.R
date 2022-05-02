#Extract "Cities and towns during the Russo-Ukrainian War" from Wikipedia
# https://en.wikipedia.org/wiki/Control_of_cities_during_the_Russo-Ukrainian_War

library(rvest)
library(data.table)
library(tidyr)

#Scrap page ----
url <- "https://en.wikipedia.org/wiki/Control_of_cities_during_the_Russo-Ukrainian_War"
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")
df <- html_table(tbls[grep("Held by",tbls,ignore.case = T)],fill = T)
if(ncol(df[[1]]) < 5){
  df <- df[-1] #remove disclaimer if present
}
df <- rbindlist(df, fill = TRUE)
df$Name <- gsub("\\[.*","", df$Name)

df <- separate(data = df, col = "Held by", into = c("Held by", "source"), sep = "\\[")
df$source <- as.numeric(gsub("\\].*","", df$source))

# References ----
# ref <- html_attr(html_node(html_nodes(webpage, 'cite'), "a"), "href")
ref <- html_nodes(webpage, '.references')
ref <- html_nodes(ref, "li")
ref <- html_nodes(ref, "a")
ref <- html_attr(ref, "href")

ref <- gsub("\\#.*", "\\#", ref)
ref <- c(rle(ref)$values, "#")

ref_sharp <- which(ref=="#")
ref_clean <- rep(NA, length(ref_sharp)-1)

for (i in 1:(length(ref_sharp)-1)){
  temp <- ref[ref_sharp[i]:ref_sharp[i+1]]
  match <- grep("web.archive.org", temp, ignore.case = T)
  
  if(!length(temp[match])){
    ref_clean[i] <- temp[2]
  }else{
    ref_clean[i] <- temp[match]
  }
  
  if(ref_clean[i] == "/wiki/General_Staff_of_the_Ukrainian_Armed_Forces"){
    ref_clean[i] <- temp[3]
  }
}

ref <- ref_clean
ref <- gsub("/wiki/", "https://en.wikipedia.org/wiki/", ref)
#ref <- gsub("\\?.*", "", ref) #bonify links
df$sourceLink <- ref[df$source]


# Control ----
df$`Held by` <- gsub(":","", df$`Held by`)
df$`Held by` <- gsub("Contested.*$", "Contested", df$`Held by`)
df$`Held by` <- gsub("Russia.*$", "Russia", df$`Held by`)
df$`Held by`[df$`Held by`==""] <- "Contested" #temp fix

#Population ----
colnames(df)[colnames(df) == "Pop."] <- "Population"
df$Population <- gsub("[^0-9]+", "", df$Population)
df$Population <- as.numeric(df$Population)

#More info ----
df$`More information` <- gsub("\\[.*","", df$`More information`)
df$`More information` <- gsub("Awarded"," Awarded", df$`More information`)
df$`More information` <- gsub("Present military control in Kyiv","", df$`More information`)
df$`More information` <- gsub("Present control in Mariupol","", df$`More information`)

#Last edit ----
last_edit <- html_text(html_nodes(webpage, "footer #footer-info-lastmod"))
last_edit <- gsub(".*on ","",last_edit)
last_edit <- gsub(", at","",last_edit)
last_edit <- gsub("\\.","",last_edit)
last_edit <- gsub(":","h",last_edit)
last_edit <- gsub("[[:punct:]]", "", last_edit)
last_edit <- gsub("[[:space:]]", " ", last_edit)
last_edit <- paste(gsub("(\\d)[^0-9]+$", "\\1", last_edit), "UTC")
df$update <- last_edit
# last_edit <- gsub("[[:space:]]", "_", last_edit)

#As of
df$`As of` <- gsub("\\[.*","", df$`As of`)
df$`As of`[is.na(df$`As of`)] <- substr(last_edit, 1, nchar(last_edit)-10)
df$`As of`[df$`As of` == ""] <- substr(last_edit, 1, nchar(last_edit)-10)


# Simple name for matching coords
df$NameSimple <- df$Name
df$NameSimple <- gsub("[^[:alpha:]]", "", df$NameSimple)
#Fix cities with same name
df$NameSimple[df$NameSimple=="Shevchenkove"] <- paste(df$NameSimple[df$NameSimple=="Shevchenkove"],
                                                      df$Raion[df$NameSimple=="Shevchenkove"],
                                                      sep = "")
df$NameSimple[df$NameSimple=="Zarichne"] <- paste(df$NameSimple[df$NameSimple=="Zarichne"],
                                                      df$Raion[df$NameSimple=="Zarichne"],
                                                      sep = "")
df$NameSimple[df$NameSimple=="Zarichne"] <- paste(df$NameSimple[df$NameSimple=="Ukrainka"],
                                                      df$Raion[df$NameSimple=="Ukrainka"],
                                                      sep = "")

df$NameSimple <- gsub("y", "i", df$NameSimple)

#Remove Raion
df <- subset(df, select = -c(Raion) )

# Add coordinates ----
#Coordinates are taken from https://simplemaps.com/ and from Google when missing
coords <- read.csv("ukraineCities.csv")
coords$NameSimple <- gsub("y", "i", coords$city_ascii)
coords$NameSimple <- gsub("[^[:alpha:]]", "", coords$NameSimple)


temp <- merge(df, coords, all.x=T) 
df <- subset(temp, select = -c(NameSimple, city, city_ascii, admin_name) )

df[is.na(df)] <- ""


#Write current and latest ----
# write.csv(df, paste0("output/Cities_and_towns_during_the_Russo-Ukrainian_War_", last_edit, ".csv"), 
#           row.names = F, 
#           fileEncoding = "UTF-8")
write.csv(df, paste0("output/Cities_and_towns_during_the_Russo-Ukrainian_War_", "latest", ".csv"),
          row.names = F,
          fileEncoding = "UTF-8")

#Total ----
total <- read.csv("output/Cities_and_towns_during_the_Russo-Ukrainian_War_total.csv", 
                  fileEncoding = "UTF-8", sep = ",", check.names=FALSE)
if(df$update[nrow(df)] != total$update[nrow(total)]){
 df <- rbind(total, df)
 df[is.na(df)] <- ""
 df <- unique(df)
 write.csv(df, paste0("output/Cities_and_towns_during_the_Russo-Ukrainian_War_", "total", ".csv"),
           row.names = F,
           fileEncoding = "UTF-8")
}

