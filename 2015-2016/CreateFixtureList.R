library(rvest)
library(stringr)
library(plyr)
library(dplyr)
library(jsonlite)
library(data.table)
library(car)
library(tidyr)
library(xlsx)

################################################################
# Functions
################################################################

#For a particular team and cleaned fixture list, get all of the 
#home and away opponents in order.
#Calls helper function: getTeams
grab_fixtures <- function(team, fixtures){
  team_fixtures_raw <- fixture_table %>% filter(Home == team | Away == team)
  final_fixtures <- ddply(team_fixtures_raw, .(date), getTeams, team)
  names(final_fixtures) <- c("date","team")
  #Use setNames so we can use the variable value as the column name
  return(setNames(data.frame(final_fixtures[[team]]), team))
}

#Returns the corresponding home or away team for a particular week.
getTeams <- function(week, team){
  if(week$Home == team){
    return(str_c(week$Away, " (H)"))
  }else{
    return(str_c(week$Home, " (A)"))
  }
}

#Used to clean the html and create a data.table of fixtures
cleanFixtures <- function(week){
  dt <- as.tbl(data.frame(date=the_breakdown[week], 
                          Fixture=unlist(str_split(the_breakdown[week+1], "\n"))))
  dt <- separate(dt, Fixture, c("Home", "Away"), sep=" v ")
  return(dt)
}

#Modify the table so it's in the correct format to write
formatTable <- function(table){
  transposed_fixtures <- data.frame(t(table))
  transposed_fixtures <- unname(transposed_fixtures)
  colnames(transposed_fixtures) <- unlist(lapply(1:38, function(x){str_c("Week_", x)}))
  transposed_fixtures <- transposed_fixtures[order(rownames(transposed_fixtures)), ]
  return(transposed_fixtures)
}

################################################################

#Fixtures scraped from ESPN
fixtures <- html("http://www.espnfc.us/english-premier-league/"+
                   "story/2494005/premier-league-fixtures-2015-16")

#HTML is kind of weird so we need to grab the paragraph nodes. 
#Pattern is alternating date, fixtures, date, fixtures.
the_breakdown <- fixtures %>% html_nodes("p") %>% html_text

#So figure out which index are the week dates and then pull the 
#fixture list from the next one.
dates <- which(grepl("2015|2016", the_breakdown))

#Clean the fixtures and store everything in a giant data.table
fixture_table <- do.call("rbind", lapply(dates, cleanFixtures))

#Fix a small formatting issue on the away team. 
fixture_table <- fixture_table %>% 
  mutate(Away = str_replace(Away, " \\(8:00\\)", "")) 

#Get all the team.
teams <- unique(fixture_table$Home)

#Grab the fixture list for all teams
cleaned_fixtures <- do.call("cbind", lapply(teams, grab_fixtures, fixture_table))

#Write data to an xlsx file.
formattedTable <- formatTable(cleaned_fixtures)
write.xlsx(formattedTable, file = "2015-2016 Fixtures.xlsx")