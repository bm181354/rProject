

library(ggplot2)
library(dplyr)arr

batting <- read.csv('Batting.csv')
sal <- read.csv('Salaries.csv')


#Removing all data prior to 1985 --players are past their prime. 
batting <- subset(batting,yearID >= 1985)

#combing salary.csv and batting.csv file by primary key as 'playerID' and 'yearID'
combo <- merge (batting,sal, by = c('playerID','yearID'))
# filtering out future and past values
combo <- subset(combo,yearID == 2001)


# Formulae to calculate Batting Average,OBP,1st base and SLG [Source: Wikipedia]
batting$BA <-batting$H / batting$AB 
batting$OBP = (batting$H + batting$BB + batting$HBP)/ (batting$AB + batting$BB + batting$HBP + batting$SF)
batting$X1B = batting$H - batting$X2B - batting$X3B - batting$HR 
batting$SLG = ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB

# creteria is that salary should be less than 8 million and OBP more than 0
combo <- subset(combo,salary < 8000000 & OBP > 0)

# Data of three players lost by OAKLAND in 2001
lost_players <- subset(combo,playerID %in% c('giambja01', 'damonjo01','saenzol01'))

# neglecting all the data after 2001 
lost_players <- subset(lost_players, yearID == 2001)
lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]

# sorting dataframe combo with OBP and showing only 10 data
option <- head(arrange(combo,desc(OBP)),10)

ggplot(combo, aes(x=OBP,y=salary)) + geom_point(size = 2)

#Players that are suitable to replace 3 lost players:
# 2  heltoto01 587 4950000 0.4316547
# 3  berkmla01 577  305000 0.4302326
# 4  gonzalu01 609 4833333 0.4285714
# 5  martied01 470 5500000 0.4234079
# 6  thomeji01 526 7875000 0.4161491
# 7  alomaro01 575 7750000 0.4146707
# 8  edmonji01 500 6333333 0.4102142
# 9  gilesbr02 576 7333333 0.4035608

