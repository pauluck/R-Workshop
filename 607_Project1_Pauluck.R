
library(dplyr) # Selecting and filtering
library(magrittr) # Pipes
library(knitr) # kable
library(stringr)

RawFile <- paste(readLines("C:\\Users\\Minnie\\Documents\\tournamentinfo.txt"), collapse = "\n")
splitRawFile <- gsub("\n\\s+|--+","",RawFile) %>%
          gsub("\\|\\s*\n", "\n", .) %>%
          strsplit(.,"\n")
      #    gsub("\\s+\\d+\\s/\\sR:\\s+?|\\s+->\\d+\\s+?","",.)
df_chess.in <- textConnection(splitRawFile[[1]])
df_chess <- read.table(textConnection(splitRawFile[[1]]), sep="|", skip = 1)
df_chess_subset <- df_chess[c(1,2,4:12)]
colnames(df_chess_subset) <- c("Num","Player","R1","R2","R3","R4","R5","R6","R7","State","Rating")
df_chess_subset[3:9] <- lapply(df_chess_subset[3:9],function(x) str_extract_all(x,"\\d{1,}"))
df_chess_subset$Rating <- as.character(gsub("^.*R:\\s+(\\d+)(\\D|\\s).*$","\\1",df_chess_subset$Rating))
avg <- calculateAvg(unlist(df_chess_subset[3:9]))
calculateAvg <- function(x){
 # return(mean
 #        (as.numeric(lapply(x[x>0], function(x) df_chess_subset[x,"Rating"])))       )
  return(list(x))
 # player <- str_extract_all(x,"\\d{1,}")
 #   return(c(player,length(player),nchar(player)))
#  lapply(x, )
#  if(!str_detect(x,"\\d")){return(NA)}
  
 #   else return(df_chess_avg[unlist(player),"new"])
  
}
avg<-lapply(df_chess_subset[3:7], function(x) calculateAvg((unlist(x))))
avg <- calculateAvg((df_chess_subset[3:7]))
df_chess_subset$Rating <- as.character(gsub("^.*R:\\s+(\\d+)(\\D|\\s).*$","\\1",df_chess_subset$Rating))
calculateAvg(c("BB","35","rwwje", "60"))
sapply(df_chess_subset[c(3:7)], function(x) calculateAvg(x))

df_chess_avg <- df_chess_subset %>% 
              mutate(new = calculateAvg(c(R1,R2,R3,R4,R5,R6,R7)))
df_chess_add <- df_chess_avg %>%
              mutate(round1 = df_chess_avg[1,"new"])
flags_subset_df <- flags_fulldump_df %>%
  select(landmass, name, bars, stripes) %>%
  filter(landmass %in% c(3,4))6

flags_percent_df <- flags_sum_df %>%
  mutate(percentBars = round(totalBars/totalCountries,digits=2)