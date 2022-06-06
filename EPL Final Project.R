# MSBA 615 Final Project: Cameron, Varun, Sri and Keshav I believe are their names.

#I'd like to start by saying I really enjoyed your class, and had a lot of fun with this project.
#Though it looks sloppy to me, it feels good to have built this mostly from the ground up, with your
#instructions included, of course. 



# libraries
library(readr)
library(lubridate)
library(dplyr)
library(stringr)

EPL_Standings(Date = "04/28/2022", Season = "2021/22") 


EPL_Standings <- function(Date, Season) {
        
 #I'll be honest with you, I'm not certain if this is what you wanted, and I spent a good 
 #6+ hours trying to create some way to dynamically pull from the website using stringr and other
 #methods to try and read, but it resulted in frustration and left me just linking the three 
 #most recent urls and combining it with Ifs.
  
        if (Season == "2021/22") {df <- read_csv(url("https://www.football-data.co.uk/mmz4281/2122/E0.csv"))}
        if (Season == "2019/20") {df <- read_csv(url("https://www.football-data.co.uk/mmz4281/1920/E0.csv"))}
        if (Season == "2020/21") {df <- read_csv(url("https://www.football-data.co.uk/mmz4281/2021/E0.csv"))}
        
               
        
 #Using Select() here lets us pick out the only columns we will need.       
      
      df <- select(df, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
       

 #This is used to convert our mm/dd/yyyy function argument to dd/mm/yyyy which
 #is used in these excel sheets, and then use a filter to only keep the games 
 #played up to the specified date. 
        
                Date_input <- mdy(Date)
        
                df <- df %>% 
                        mutate(Date = dmy(Date)) %>% 
                        filter(Date <= Date_input)
        
      #The dataframe is then broken into two dataframes so we can more easily
      #calculate certain figures, including HomeRec and AwayRec. It also just
      #seemed like an easier approach, as was recommended. 
                
                            Home <- df %>%
                              select(HomeTeam, FTHG, FTAG, FTR) %>%
                              rename(
                                TeamName = HomeTeam,
                                TGoals = FTHG,
                                OGoals = FTAG
                              ) %>%
                              mutate(
                                Win = TGoals > OGoals,
                                Loss = TGoals < OGoals,
                                Draw = TGoals == OGoals
                              ) %>%
                              add_column(Match_Type = "Home")
                            
                            Away <- df %>%
                              select(AwayTeam, FTHG, FTAG, FTR) %>%
                              rename(
                                TeamName = AwayTeam,
                                TGoals = FTAG,
                                OGoals = FTHG
                              ) %>%
                              mutate(
                                Win = TGoals > OGoals,
                                Loss = TGoals < OGoals,
                                Draw = TGoals == OGoals
                              ) %>%
                              add_column(Match_Type = "Away")  
                                
                      #Then we rejoin the two dataframes... 
                            
                                df <- bind_rows(Home, Away)
                      
                      #And go about creating the columns needed for the final result.
                      #The hardest part of this section was figuring out how to properly
                      #count Wins, Losses and Draws when it was a Home and Away game,
                      #until I realized that I missed the %>% just above "add_column(Match_Type = "Away")"
                      #resulting in Match_Type never existing for the Away dataframe and making an 
                      #incredibly frustrating hour, spent thinking of work-arounds, more painful.
                                
                                df <- df %>%
                                  group_by(TeamName) %>%
                                  summarize(
                                    Record = str_c(sum(Win =="TRUE"), sum(Loss == "TRUE"), sum(Draw == "TRUE"), sep =  "-"),
                                    HomeRec = str_c(sum(Win =="TRUE" & FTR == "H"), sum(Loss == "TRUE" & FTR == "A"), sum(Draw == "TRUE" & Match_Type == "Home"), sep =  "-"),
                                    AwayRec = str_c(sum(Win =="TRUE" & FTR == "A"), sum(Loss == "TRUE" & FTR == "H"), sum(Draw == "TRUE" & Match_Type == "Away"), sep =  "-"),
                                    MatchesPlayed = sum(Win, Loss, Draw),
                                    Points = sum(Win == "TRUE")*3 + sum(Draw == "TRUE"),
                                    PPM = Points/MatchesPlayed,
                                    PtPct = (Points/3) / MatchesPlayed,
                                    GS = sum(TGoals,OGoals),
                                    GSM = GS/MatchesPlayed,
                                    GA = sum(OGoals),
                                    GAM = GA/MatchesPlayed
                                    
                                    
                                  )
                              
                       df <- arrange(df, -PPM, Record, -GSM, GAM)
       
        #For the longest time, I couldn't figure out how to make the result be assigned
        #, as return() wouldn't assign the result in the environment window, but
        #using <<- did.               
        
            Results <<- df

}












