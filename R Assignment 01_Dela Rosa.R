# 1. WHO dataset
    WHO = read.csv("WHO.csv")
# d. country with the lowest literacy
    Lowest_literacy<-which.min(WHO$LiteracyRate)
      WHO$Country[Lowest_literacy]
# e. Richest country in Europe based on GNI
    WHO.Europe = subset(WHO, Region == "Europe")
      RichGNI<-which.max(WHO.Europe$GNI)
        WHO.Europe$Country[RichGNI]
# f. Mean Life expectancy of countries in Africa
    WHO.Africa = subset(WHO, Region == "Africa")
      Life_expectancy<-mean(WHO.Africa$LifeExpectancy, na.rm = TRUE)
        Life_expectancy  
# g. Number of countries with population greater than 10M
    Countries_greater10M<-subset(WHO, Population>10000)
      dim(Countries_greater10M)[1]
# h. Top 5 countries in the Americas with the highest child mortality
    WHO.America = subset(WHO, Region == "Americas")
      index_ordered<-order(WHO.America$ChildMortality,decreasing = TRUE)
        americas_ordered<-index_ordered[1:5]
          WHO.America$Country[americas_ordered]
#
# 2. NBA dataset (Historical NBA Performance.xlsx)
    NBA = read.csv("Historical NBA Performance.csv")
# a. using the dataset, Historical NBA Performance,  I simply just need an R script involving Team == 'Bulls' and the Winning Percentage column, that when executed will show 1995-96 as the answer
    Bulls = subset(NBA, Team == "Bulls")
      Most_wins_percentage<-max(Bulls$Winning.Percentage)
        bulls_row<-subset(Bulls,Winning.Percentage == Most_wins_percentage)
          bulls_row$ï..Year  
# b. Teams with an even win-loss record in a year
    Teams_even_records = subset(NBA, Winning.Percentage == "0.5")
      Teams_even_records$Team
#
# 3. Seasons_Stats.csv
    Stat = read.csv("Seasons_Stats.csv")
# 3.a and 3.b, aggregation of data values within a season is needed since some players play in more than one team in a season
# a. Player with the highest 3-pt attempt rate in a season.
    highest_3pa<-max(Stat$X3PAr,na.rm = TRUE)
    players_with_highest3pa<-subset(Stat,X3PAr == highest_3pa)
    players_with_highest3pa
# b. Player with the highest free throw rate in a season.
    highest_freethrow<-max(Stat$FTr, na.rm = TRUE)
    players_with_highestFtr<-subset(Stat,FTr == highest_freethrow)
    print(players_with_highestFtr)
# c. What year/season does Lebron James scored the highest?
    Lebron<-subset(Stat, Player == 'LeBron James')
      Lebron_max_pts<-max(Lebron$PTS, na.rm = TRUE)
        subset(Lebron,PTS == Lebron_max_pts)$Year
#   d. What year/season does Michael Jordan scored the highest?
    M_Jordan<-subset(Stat,Player == 'Michael Jordan*')
      M_Jordan_max_pts<-max(M_Jordan$PTS, na.rm = TRUE)
        subset(M_Jordan,PTS == M_Jordan_max_pts)$Year
#   e. Player efficiency rating of Kobe Bryant in the year where his MP is the lowest?
    Kobe<-subset(Stat,Player == 'Kobe Bryant')
      Kobe_low_MP<-min(Kobe$MP,na.rm = TRUE)
        subset(Kobe,MP == Kobe_low_MP)$PER
#        
# 4. National Universities Rankings.csv
    Ranking = read.csv("National Universities Rankings.csv")
# a. University with the most number of undergrads
    print(Ranking[which.max(Ranking$Undergrad.Enrollment),]$Name)
# b. Average Tuition in the Top 10 University
    top10<-Ranking[order(Ranking$Rank),][1:10,]
      top10$tuition_no_dollar <- gsub(pattern = "\\$|\\,",replacement = "", top10$Tuition.and.fees)
        mean(as.numeric(top10$tuition_no_dollar))
    