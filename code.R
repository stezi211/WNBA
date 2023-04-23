
# Loading data
  wnba <- read.csv("WNBA Stats.csv")
  str(wnba)
  summary(wnba)
  names(wnba)
  dim(wnba)
  head(wnba)

# Correcting variables' types

  wnba$Name <- as.character(wnba$Name)
  
  library(lubridate)
  
  wnba$Birthdate <- mdy(wnba$Birthdate)

#Replacing R with 0 in Experience

  levels(wnba$Experience)
  levels(wnba$Experience)<-c("1","10","11","12","13","14","15","2","3","4","5","6","7","8","9","0" )
 

# Creating a categorical variable for experience

    wnba$exp_level = ifelse(wnba$Experience=="0", "Rookie",
                      ifelse(wnba$Experience=="1" | wnba$Experience=="2" | wnba$Experience=="3", "Little Experience",
                       ifelse(wnba$Experience=="4" | wnba$Experience=="5", "Experienced",
                          ifelse(wnba$Experience=="6" | wnba$Experience=="7" | wnba$Experience=="8" & 
                                   wnba$Experience=="9" | wnba$Experience=="10", "Very Experienced","Veteran"
                             ))))

    
    wnba$exp_level = as.factor(wnba$exp_level)
    wnba$exp_level = ordered(wnba$exp_level, levels=c("Rookie", "Little Experience", "Experienced" ,"Very Experienced","Veteran" ))
    levels(wnba$exp_level)
    
#Making Experience numerical
    
  wnba$Experience= as.numeric(levels(wnba$Experience))[wnba$Experience]
  

#Changing Pos values

    levels(wnba$Pos)
    levels(wnba$Pos) <- c("Center", "Forward", "Forward/Center", "Guard", "Guard/Forward")
    summary(wnba)
    
    head(wnba)
    tail(wnba)

# Correcting variables names
    
    names(wnba) <- c("Name", "Team", "Pos", "Height", "Weight", "BMI", "Birth_Place", "Birthdate", "Age",
                     "College", "Experience", "Games.Played", "MIN", "FGM", "FGA", "FGpercent", "threePM", "threePA",
                     "threePpercent", "FTM", "FTA", "FTpercent", "OREB", "DREB", "REB", "AST", "STL", "BLK", "TO", "PTS", 
                     "DDtwo", "TDthree","exp_level")


    
#I notice that the name of observation 53 is corrupted and I check the source to fix it

    wnba[53,1]<-"Adut Bulgak"

#Checking for missing values

  is.na(wnba)
  any(is.na(wnba))
  sum(is.na(wnba))
  summary(wnba)

#There are 2 rows with missing values, I will remove them from wnba

  complete.cases(wnba)
  wnba <- subset(wnba,complete.cases(wnba)==TRUE)

#Checking for duplicates

  duplicated(wnba)   #no duplicates
 
#I am building a variable that will divide the positions of the players into 2 categories,
#σε αυτές των Guard και σε αυτες των Center και Forward
  
  levels(wnba$Pos)
  pos_level=ifelse(wnba$Pos=="Center"|wnba$Pos=="Forward"|wnba$Pos=="Forward/Center","C-F","G")
  wnba=data.frame(wnba,pos_level)
  str(wnba$pos_level)  

#I define a variable that divides the height into 2 categories
  
  tall=ifelse(wnba$Height<mean(wnba$Height),"NO","YES")
  wnba=data.frame(wnba,tall)
  str(wnba$tall)
  summary(wnba$tall)
  levels(wnba$tall)  
  
#looking at data's form
  summary(wnba)
  names(wnba)
  dim(wnba)
  head(wnba)


#Descriptive Statistics
  library(DescTools)
  library(DescriptiveStats.OBeu)

  #players' age

  summary(wnba$Age)
  Range(wnba$Age)
  var(wnba$Age)
  sd(wnba$Age)
  Mode(wnba$Age)
  IQR(wnba$Age)
  quantile(wnba$Age)
  CV(wnba$Age)
  ds.skewness(wnba$Age)
  ds.kurtosis(wnba$Age)
  
  boxplot(wnba$Age, col="darkgreen", xlab="Boxplot of Age")
  #players' height
  
  summary(wnba$Height)
  var(wnba$Height)
  sd(wnba$Height)
  Mode(wnba$Height)
  Range(wnba$Height)
  IQR(wnba$Height)
  quantile(wnba$Height)
  CV(wnba$Height)
  ds.skewness(wnba$Height)
  ds.kurtosis(wnba$Height)
  
  boxplot(wnba$Height, col="darkgreen", xlab="Boxplot of Height")
  #players' weight
  
  summary(wnba$Weight)
  var(wnba$Weight)
  sd(wnba$Weight)
  Mode(wnba$Weight)
  IQR(wnba$Weight)
  quantile(wnba$Weight)
  CV(wnba$Weight)
  ds.skewness(wnba$Weight)
  ds.kurtosis(wnba$Weight)
  
  boxplot(wnba$Weight, col="darkgreen", xlab="Boxplot of Weight")
  #players' bmi
  
  summary(wnba$BMI)
  var(wnba$BMI)
  sd(wnba$BMI)
  Mode(wnba$BMI)
  IQR(wnba$BMI)
  quantile(wnba$BMI)
  CV(wnba$BMI)
  ds.skewness(wnba$BMI)
  ds.kurtosis(wnba$BMI)
  
  boxplot(wnba$BMI,col="darkgreen", xlab="Boxplot of BMI")
  
  #experience
  
  summary(wnba$Experience)
  var(wnba$Experience)
  sd(wnba$Experience)
  Mode(wnba$Experience)
  IQR(wnba$Experience)
  quantile(wnba$Experience)
  CV(wnba$Experience)
  ds.skewness(wnba$Experience)
  ds.kurtosis(wnba$Experience)
  
  boxplot(wnba$Experience,col="darkgreen", xlab="Boxplot of Experience")
  #games played by each player
  
  summary(wnba$Games.Played)
  var(wnba$Games.Played)
  sd(wnba$Games.Played)
  Mode(wnba$Games.Played)
  IQR(wnba$Games.Played)
  quantile(wnba$Games.Played)
  CV(wnba$Games.Played)
  ds.skewness(wnba$Games.Played)
  ds.kurtosis(wnba$Games.Played)
  
  boxplot(wnba$Games.Played,col="darkgreen", xlab="Boxplot of Played Games")
  #minutes played by each player
  
  summary(wnba$MIN)
  var(wnba$MIN)
  sd(wnba$MIN)
  Mode(wnba$MIN)
  IQR(wnba$MIN)
  quantile(wnba$MIN)
  CV(wnba$MIN)
  ds.skewness(wnba$MIN)
  ds.kurtosis(wnba$MIN)
  
  boxplot(wnba$MIN,col="darkgreen", xlab="Boxplot of Minutes Played")
  #FGM
  
  summary(wnba$FGM)
  var(wnba$FGM)
  sd(wnba$FGM)
  Mode(wnba$FGM)
  IQR(wnba$FGM)
  quantile(wnba$FGM)
  CV(wnba$FGM)
  ds.skewness(wnba$FGM)
  ds.kurtosis(wnba$FGM)
  
  boxplot(wnba$FGM,col="darkgreen", xlab="Boxplot of 2-points shouts made")
  #FGA
  
  summary(wnba$FGA)
  var(wnba$FGA)
  sd(wnba$FGA)
  Mode(wnba$FGA)
  IQR(wnba$FGA)
  quantile(wnba$FGA)
  CV(wnba$FGA)
  ds.skewness(wnba$FGA)
  ds.kurtosis(wnba$FGA)
  
  boxplot(wnba$FGA,col="darkgreen", xlab="Boxplot of 2-points shouts attempted")
  #FGpercent
  
  summary(wnba$`FGpercent`)
  var(wnba$`FGpercent`)
  sd(wnba$`FGpercent`)
  Mode(wnba$`FGpercent`)
  IQR(wnba$`FGpercent`)
  quantile(wnba$`FGpercent`)
  CV(wnba$`FGpercent`)
  ds.skewness(wnba$`FGpercent`)
  ds.kurtosis(wnba$`FGpercent`)
  
  boxplot(wnba$`FGpercent`,col="darkgreen", xlab="Boxplot of 2-points shouts success percentence")
  #threePM
  
  summary(wnba$`threePM`)
  var(wnba$`threePM`)
  sd(wnba$`threePM`)
  Mode(wnba$`threePM`)
  IQR(wnba$`threePM`)
  quantile(wnba$`threePM`)
  CV(wnba$`threePM`)
  ds.skewness(wnba$`threePM`)
  ds.kurtosis(wnba$`threePM`)
  
  boxplot(wnba$`threePM`,col="darkgreen", xlab="Boxplot of 3-points shouts made")
  #threePA
  
  summary(wnba$`threePA`)
  var(wnba$`threePA`)
  sd(wnba$`threePA`)
  Mode(wnba$`threePA`)
  IQR(wnba$`threePA`)
  quantile(wnba$`threePA`)
  CV(wnba$`threePA`)
  ds.skewness(wnba$`threePA`)
  ds.kurtosis(wnba$`threePA`)
  
  boxplot(wnba$`threePA`,col="darkgreen", xlab="Boxplot of 3-points shouts attempted")
  #threePpercent
  
  summary(wnba$`threePpercent`)
  var(wnba$`threePpercent`)
  sd(wnba$`threePpercent`)
  Mode(wnba$`threePpercent`)
  IQR(wnba$`threePpercent`)
  quantile(wnba$`threePpercent`)
  CV(wnba$`threePpercent`)
  ds.skewness(wnba$`threePpercent`)
  ds.kurtosis(wnba$`threePpercent`)
  
  boxplot(wnba$`threePpercent`,col="darkgreen", xlab="Boxplot of 3-points shouts success percentence")
  #FTM
  
  summary(wnba$FTM)
  var(wnba$FTM)
  sd(wnba$FTM)
  Mode(wnba$FTM)
  IQR(wnba$FTM)
  quantile(wnba$FTM)
  CV(wnba$FTM)
  ds.skewness(wnba$FTM)
  ds.kurtosis(wnba$FTM)
  
  boxplot(wnba$FTM,col="darkgreen", xlab="Boxplot of Free Throws made")
  #FTA
  
  summary(wnba$FTA)
  var(wnba$FTA)
  sd(wnba$FTA)
  Mode(wnba$FTA)
  IQR(wnba$FTA)
  quantile(wnba$FTA)
  CV(wnba$FTA)
  ds.skewness(wnba$FTA)
  ds.kurtosis(wnba$FTA)
 
  boxplot(wnba$FTA,col="darkgreen", xlab="Boxplot of Free Throws attempted") 
  #FTpercent
  
  summary(wnba$`FTpercent`)
  var(wnba$`FTpercent`)
  sd(wnba$`FTpercent`)
  Mode(wnba$`FTpercent`)
  IQR(wnba$`FTpercent`)
  quantile(wnba$`FTpercent`)
  CV(wnba$`FTpercent`)
  ds.skewness(wnba$`FTpercent`)
  ds.kurtosis(wnba$`FTpercent`)
  
  boxplot(wnba$`FTpercent`,col="darkgreen", xlab="Boxplot of Free Throws success percentence")
  #OREB
  
  summary(wnba$OREB)
  var(wnba$OREB)
  sd(wnba$OREB)
  Mode(wnba$OREB)
  IQR(wnba$OREB)
  quantile(wnba$OREB)
  CV(wnba$OREB)
  ds.skewness(wnba$OREB)
  ds.kurtosis(wnba$OREB)
  
  boxplot(wnba$OREB,col="darkgreen", xlab="Boxplot of Offensive Rebounds")
  #DREB
  
  summary(wnba$DREB)
  var(wnba$DREB)
  sd(wnba$DREB)
  Mode(wnba$DREB)
  IQR(wnba$DREB)
  quantile(wnba$DREB)
  CV(wnba$DREB)
  ds.skewness(wnba$DREB)
  ds.kurtosis(wnba$DREB)
  
  boxplot(wnba$DREB,col="darkgreen", xlab="Boxplot of Defensive Rebounds")
  #REB
  
  summary(wnba$REB)
  var(wnba$REB)
  sd(wnba$REB)
  Mode(wnba$REB)
  IQR(wnba$REB)
  quantile(wnba$REB)
  CV(wnba$REB)
  ds.skewness(wnba$REB)
  ds.kurtosis(wnba$REB)
  
  boxplot(wnba$REB,col="darkgreen", xlab="Boxplot of Rebounds")
  #AST  
  
  summary(wnba$AST)
  var(wnba$AST)
  sd(wnba$AST)
  Mode(wnba$AST)
  IQR(wnba$AST)
  quantile(wnba$AST)
  CV(wnba$AST)
  ds.skewness(wnba$AST)
  ds.kurtosis(wnba$AST)
  
  boxplot(wnba$AST,col="darkgreen", xlab="Boxplot of Assists")
  #STL
  
  summary(wnba$STL)
  var(wnba$STL)
  sd(wnba$STL)
  Mode(wnba$STL)
  IQR(wnba$STL)
  quantile(wnba$STL)
  CV(wnba$STL)
  ds.skewness(wnba$STL)
  ds.kurtosis(wnba$STL)
  
  boxplot(wnba$STL,col="darkgreen", xlab="Boxplot of Steals")
  #BLK
  
  summary(wnba$BLK)
  var(wnba$BLK)
  sd(wnba$BLK)
  Mode(wnba$BLK)
  IQR(wnba$BLK)
  quantile(wnba$BLK)
  CV(wnba$BLK)
  ds.skewness(wnba$BLK)
  ds.kurtosis(wnba$BLK)
  
  boxplot(wnba$BLK,col="darkgreen", xlab="Boxplot of Blocks")
  #TO
  
  summary(wnba$TO)
  var(wnba$TO)
  sd(wnba$TO)
  Mode(wnba$TO)
  IQR(wnba$TO)
  quantile(wnba$TO)
  CV(wnba$TO)
  ds.skewness(wnba$TO)
  ds.kurtosis(wnba$TO)
  
  boxplot(wnba$TO,col="darkgreen", xlab="Boxplot of Turnovers")
  #PTS
  
  summary(wnba$PTS)
  var(wnba$PTS)
  sd(wnba$PTS)
  Mode(wnba$PTS)
  IQR(wnba$PTS)
  quantile(wnba$PTS)
  CV(wnba$PTS)
  ds.skewness(wnba$PTS)
  ds.kurtosis(wnba$PTS)
  
  boxplot(wnba$PTS,col="darkgreen", xlab="Boxplot of Total Points")
  #DDtwo
  
  summary(wnba$DDtwo)
  var(wnba$DDtwo)
  sd(wnba$DDtwo)
  Mode(wnba$DDtwo)
  IQR(wnba$DDtwo)
  quantile(wnba$DDtwo)
  CV(wnba$DDtwo)
  ds.skewness(wnba$DDtwo)
  ds.kurtosis(wnba$DDtwo)
  
  boxplot(wnba$DDtwo,col="darkgreen", xlab="Boxplot of Double Doubles")
  #TDthree
  
  summary(wnba$TDthree)
  var(wnba$TDthree)
  sd(wnba$TDthree)
  Mode(wnba$TDthree)
  IQR(wnba$TDthree)
  quantile(wnba$TDthree)
  CV(wnba$TDthree)
  ds.skewness(wnba$TDthree)
  ds.kurtosis(wnba$TDthree)
  
  boxplot(wnba$TDthree,col="darkgreen", xlab="Boxplot of Triple Doubles")
  
#Descriptive Statistics of qualitative variables
  
  #birth_place
  
  nlevels(wnba$Birth_Place)
  levels(wnba$Birth_Place)
  
  ftab.birth_place = table(wnba$Birth_Place)
  ftab.birth_place.df = as.data.frame(ftab.birth_place)
  
  colnames(ftab.birth_place.df) = c("Birth Place","Frequency")
  ftab.birth_place.df
  
  rftab.birth_place = prop.table(ftab.birth_place)
  rftab.birth_place = round(rftab.birth_place,3)
  rftab.birth_place
  rftab.birth_place.df = as.data.frame(rftab.birth_place)
  colnames(rftab.birth_place.df) = c("Birth Place","Relative Frequency")
  rftab.birth_place.df
  
    #birthplace barplot
    
    barplot(ftab.birth_place,main="DIstribution of BirthPlaces between WNBA players",
                             xlab = "Birth Places",
                             ylab = "Frequency",
                             horiz = FALSE, 
                             cex.names = 0.8,
                             col="darkgreen")
                          
  #team

  levels(wnba$Team)
  nlevels(wnba$Team)
  
  ftab.Team = table(wnba$Team)
  ftab.Team.df = as.data.frame(ftab.Team)
  
  colnames(ftab.Team.df) = c("Team","Frequency")
  ftab.Team.df
  
  rftab.Team = prop.table(ftab.Team)
  rftab.Team = round(rftab.Team,2)
  rftab.Team
  rftab.Team.df = as.data.frame(rftab.Team)
  colnames(rftab.Team.df) = c("Team","Relative Frequency")
  rftab.Team.df
  
    #teams' barplot
    
    barplot(ftab.Team,main="DIstribution of Players in Teams",
                             xlab = "Teams",
                             ylab = "Number of Players",
                             horiz = FALSE, 
                             cex.names = 0.8,
                             col="darkgreen")
                           
  #Position
  
  levels(wnba$Pos)
  
  ftab.pos = table(wnba$Pos)
  ftab.pos.df = as.data.frame(ftab.pos)
  
  colnames(ftab.pos.df) = c("Position","Frequency")
  ftab.pos.df
  
  rftab.pos = prop.table(ftab.pos)
  rftab.pos = round(rftab.pos,2)
  rftab.pos
  rftab.pos.df = as.data.frame(rftab.pos)
  colnames(rftab.pos.df) = c("Position","Relative Frequency")
  rftab.pos.df
  

    #positions' barplot
  
    barplot(ftab.pos,main="Distribution of Players by position",
                       xlab = "Positions",
                       ylab = "Number of Players",
                       horiz = FALSE, 
                       cex.names = 0.8,
                       col=rainbow(length(ftab.pos)),
                       legend.text =  ftab.pos.df$Frequency)
 
   #exp_level
  
  levels(wnba$exp_level)
  
  ftab.exp = table(wnba$exp_level)
  ftab.exp.df = as.data.frame(ftab.exp)
  
  colnames(ftab.exp.df) = c("Experience","Frequency")
  ftab.exp.df
  
  rftab.exp = prop.table(ftab.exp)
  rftab.exp = round(rftab.exp,2)
  rftab.exp
  rftab.exp.df = as.data.frame(rftab.exp)
  colnames(rftab.exp.df) = c("Position","Relative Frequency")
  rftab.exp.df
  
  
    #experiences' barplot
    
    barplot(ftab.exp,main="Distribution of Players's Experience",
                      xlab = "Level of Experience",
                      ylab = "Number of Players",
                      horiz = FALSE, 
                      cex.names = 0.8,
                      col=rainbow(length(ftab.exp)),
                      legend.text =  ftab.exp.df$Frequency)
  
  #tall
  
  ftab.tall = table(wnba$tall)
  ftab.tall.df = as.data.frame(ftab.tall)
  
  colnames(ftab.tall.df) = c("Tall","Frequency")
  ftab.tall.df
  
  rftab.tall = prop.table(ftab.tall)
  rftab.tall = round(rftab.tall,2)
  rftab.tall
  rftab.tall.df = as.data.frame(rftab.tall)
  colnames(rftab.tall.df) = c("Tall","Relative Frequency")
  rftab.tall.df
  
  
    #height barplot
    
    barplot(ftab.tall,main="Distribution of Players's Height",
            xlab = "Is the Player considered tall among other",
            ylab = "Number of Players",
            horiz = FALSE, 
            cex.names = 0.8,
            col=rainbow(length(ftab.tall)),
            legend.text =  ftab.tall.df$Frequency)
    
  #pos_level

  ftab.pos_level = table(wnba$pos_level)
  ftab.pos_level.df = as.data.frame(ftab.pos_level)
  
  colnames(ftab.pos_level.df) = c("pos_level","Frequency")
  ftab.pos_level.df
  
  rftab.pos_level = prop.table(ftab.pos_level)
  rftab.pos_level = round(rftab.pos_level,2)
  rftab.pos_level
  rftab.pos_level.df = as.data.frame(rftab.pos_level)
  colnames(rftab.pos_level.df) = c("pos_level","Relative Frequency")
  rftab.pos_level.df
  
  
    #positions barplot
    
    barplot(ftab.pos_level,main="Distribution of Position",
            xlab = "Position Level",
            ylab = "Number of Players",
            horiz = FALSE, 
            cex.names = 0.8,
            col=rainbow(length(ftab.pos_level)),
            legend.text =  ftab.pos_level.df$Frequency)
  
  #College
  
  nlevels(wnba$College)
  
  ftab.college = table(wnba$College)
  ftab.college.df = as.data.frame(ftab.college)
  
  colnames(ftab.college.df) = c("College","Frequency")
  ftab.college.df
  
  rftab.college = prop.table(ftab.college)
  rftab.college = round(rftab.college,3)
  rftab.college
  rftab.college.df = as.data.frame(rftab.college)
  colnames(rftab.college.df) = c("College","Relative Frequency")
  rftab.college.df
  
    #college barplot
    
  
    barplot(ftab.college,main="Distribution of Players in COlleges",
                      xlab = "Colleges",
                      ylab = "Number of Players",
                      horiz = FALSE, 
                      cex.names = 0.7,
                      col=rainbow(length(ftab.college)))

#Visualisations-Graphs
  
  library(ggplot2)
  
  #Scatter plot BMI, Height
    ggplot(data = wnba, aes(x=BMI,y=Height)) +
      geom_jitter()+
      labs(x="BMI",
           y="Height",
           title="Scatter Plot of Heght ~ BMI")+
      theme_minimal()
    
  #Scatter plot BMI, Weight
    ggplot(data = wnba, aes(x=BMI,y=Weight)) +
      geom_jitter()+
      labs(x="BMI",
           y="Weight",
           title="Scatter Plot of Weght ~ BMI")+
      geom_smooth(method = "lm")+
      theme_minimal()
    
  #Histogram age
  ggplot(data = wnba, 
         aes(x = Age)) +
    geom_histogram(bins = 10,
                   fill=I("green"), 
                   col=I("darkgreen"), 
                   alpha=I(.2))+
    labs(x="Age", y="Frequency", title="Histogram of Age" )+
    theme_minimal()
  
  #Histogram points
  
  ggplot(data = wnba, aes(PTS))+
    geom_histogram(binwidth = 10, fill=I("green"), col=I("darkgreen"))+
    labs(x="Points",
         y="Frequency",
         title="Histogram of Total Points")+
    theme_minimal()


  #histogram min
  
  ggplot(data = wnba, aes(MIN))+
    geom_histogram(binwidth = 15, fill=I("green"), col=I("darkgreen"))+
    labs(x="Minutes",
         y="Frequency",
         title="Histogram of Minutes")+
    theme_minimal()
  

  #histogram weight
  
  ggplot(data = wnba, aes(Weight))+
    geom_histogram(binwidth = 5, fill=I("darkgreen"), col=I("black"))+
    labs(x="Weight",
         y="Number of Players",
         title="Histogram of Weight")+
    theme_minimal()
  
  #histogram 3P
  
  ggplot(data = wnba, aes(`threePM`))+
    geom_histogram(binwidth = 10, fill=I("darkgreen"), col=I("black"))+
    labs(x="3Points Made",
         y="Frequency",
         title="Histogram of 3Points Made")+
    theme_minimal()
  
  #Graph 3P-positions
  ggplot(data = wnba,aes(x=Pos, y=`threePM`))+
    geom_bar(stat = "identity", fill="darkgreen")+
    labs(x="Position", y="3Points Made", title="Barplot of 3Points Made ~ Positions")+
    theme_minimal()
  
  
  #Graph positions - points
    ggplot(data = wnba, aes(x=Pos,y=PTS)) +
    geom_bar(stat = "identity",fill="darkgreen")+
      labs(x="Position",
           y="Points",
           title="Barplot of Points per Position")+
      theme_minimal()

    #boxplot pos - points
    
      ggplot(wnba,aes(x=Pos,y=PTS))+
      geom_boxplot(color="darkgreen", fill="green")+
        labs(x="Position ",
             y="Total Points",
             title = "Comperative Boxplots of Points between Posistion")+
        theme_minimal()
   
   
     
  #Scatter plot heigth-points 
  
      ggplot(data = wnba, aes(x=Height, y=PTS))+
      geom_jitter() +
      labs(x = "Height",
           y = "Points",
           title = "Scatter-Plot of Player's height ~ Points")+
      theme_minimal()
    
    cor(wnba$Height, wnba$PTS, method = "pearson")
  
  #games - points
  
       ggplot(data = wnba, aes(x=Games.Played, y=PTS))+
         geom_jitter() +
         geom_smooth(method = "lm")+
          labs(x = "Games Played",
               y = "Total Points",
               title = "Scatter-Plot of Games Played ~ Total Points")+
        theme_minimal()

  
  cor(wnba$Games.Played, wnba$PTS, method = "pearson")
  
  #age - points
  
    ggplot(data = wnba, aes(x=PTS, y=Age))+
      geom_jitter() +
      geom_smooth(method = "lm")+
      labs(x = "Total Points",
           y = "Age",
           title = "Scatter-Plot of Points ~ Age")+
      theme_minimal()
    
  
  cor(wnba$Age, wnba$PTS, method = "pearson")
  
  #gameplay minutes - points
  
    ggplot(data = wnba, aes(x=MIN, y=PTS))+
         geom_jitter() +
         geom_smooth(method = "lm")+
         labs(x="Minutes Played", y="Total Points", title="Scatter-Plot of Minutes ~ Points")+
        theme_minimal()
  
  cor(wnba$MIN,wnba$PTS,method="pearson")
  
  #clustered bar graph points - empiric~position
  
  ggplot(data=wnba, aes(x=exp_level, y=PTS, fill=Pos))+
        geom_bar(stat = "identity",position = position_dodge(),color="black")+
    labs(x="Experience", y="Points", title = "Comparing Barplot of Points per Position in Experience level")+
    theme_minimal()
    
  #reverse
  
  ggplot(data=wnba, aes(x=Pos, y=PTS, fill=exp_level))+
    geom_bar(stat = "identity",position = position_dodge(),color="black")+
    labs(x="Positions", y="Points", title = "Comparing Barplot of Points per Experience Level in Positions")+
    theme_minimal()

  
  #Scatter plot experience - position
  
  ggplot(data=wnba, aes(x=exp_level, y=Pos))+
        geom_jitter()+
        labs(x = "Experience",
         y = "Posistion",
         title = "Players's Experience by Position")+
        theme_minimal()

  #Scatter plot REB~Height
  
  ggplot(data = wnba, aes(x=REB,y=Height))+
    geom_jitter()+
    labs(x="Total Rebounds", y="Height", title = "Scatter-Plot of Rebounds ~ Height")+
    geom_smooth(method = "lm")+
    theme_minimal()
  
  cor(wnba$REB,wnba$Height,method = "pearson")
  
  #Scatter plot PTS~FGA
  
  ggplot(data = wnba, aes(x=PTS,y=FGA))+
    geom_jitter()+
    labs(x="Total Points", y="Total 2-points shouts attempted", title = "Scatter-Plot of Points ~ 2-points shouts ")+
    geom_smooth(method = "lm")+
    theme_minimal()
  
  cor(wnba$PTS,wnba$FGA,method = "pearson")
  
  #Scatter plot
  
  ggplot(wnba,aes(x=Height,y=PTS,colour=pos_level))+
    geom_jitter()+
    labs(x="Height",
         y="Total Points",
         ttitle = "Scatterplot of Height ~ PTS coloured by Position Category")+
    theme_minimal()

  
#statistical hypothesis testing

  
  library(nortest)

  #test of normality

    lillie.test(wnba$Height)
    lillie.test(wnba$Weight)
    lillie.test(wnba$BMI)
    lillie.test(wnba$Age)
    lillie.test(wnba$Experience)
    lillie.test(wnba$Games.Played)
    lillie.test(wnba$MIN)
    lillie.test(wnba$FGM)
    lillie.test(wnba$FGA)
    lillie.test(wnba$`FGpercent`)
    lillie.test(wnba$`threePM`)
    lillie.test(wnba$`threePA`)
    lillie.test(wnba$`threePpercent`)
    lillie.test(wnba$FTM)
    lillie.test(wnba$FTA)
    lillie.test(wnba$`FTpercent`)
    lillie.test(wnba$OREB)
    lillie.test(wnba$DREB)
    lillie.test(wnba$REB)
    lillie.test(wnba$AST)
    lillie.test(wnba$STL)
    lillie.test(wnba$BLK)
    lillie.test(wnba$TO)
    lillie.test(wnba$PTS)
    lillie.test(wnba$DDtwo)
    lillie.test(wnba$TDthree)
    
    
  #t-test
    
  
    
    weight.t.test = t.test(x=wnba$Weight,
                           alternative = "two.sided",
                           mu=mean(wnba$Weight),
                           conf.level = 0.95)
    weight.t.test
    
    
  #non-parametric tests
    
    #Do the guards have more points than the centers?
    
      wilcox.test(
      x=wnba[wnba$Pos=="Guard","PTS"],
      y=wnba[wnba$Pos=="Center","PTS"],
      alternative = "less",
      paired = FALSE,
      conf.level = 0.95
      )
  
    #are G points more than C-F?
      
      wilcox.test(
      x=wnba[wnba$pos_level=="G","PTS"],
      y=wnba[wnba$pos_level=="C-F","PTS"],
      alternative = "less",
      paired = FALSE,
      conf.level = 0.95
      )
    
    
    #more rebounds G than C-F;
      
      wilcox.test(
      x=wnba[wnba$pos_level=="C-F","REB"],
      y=wnba[wnba$pos_level=="G","REB"],
      alternative = "less",
      paired = FALSE,
      conf.level = 0.95
      )
    
    
    #equal average weight for G and C-F?
      wilcox.test(
      x=wnba[wnba$pos_level=="C-F","Weight"],
      y=wnba[wnba$pos_level=="G","Weight"],
      alternative = "two.sided",
      paired = FALSE,
      conf.level = 0.95
       )
    

    
    #do to tall players score more points?
      wilcox.test(
      x=wnba[wnba$tall=="YES","PTS"],
      y=wnba[wnba$tall=="NO","PTS"],
      alternative = "less",
      paired = FALSE,
      conf.level = 0.95
       )


    
#Classification Trees
    library(tree)

 #remove variable with names, date of birth, university, place of birth
 # because they are categorical with many categories
   # and I exclude triple doubles because they create a problem later in svm, because they are rare events
    
    wnba.tree<-wnba[,-c(1,7,8,10,32)]
    str(wnba.tree)
  #Spliting data in training and test, 80% - 20%
    
    set.seed(1)
    wnba_size=floor(0.8 * nrow(wnba.tree))
    
    train_index=sample(1:nrow(wnba.tree), size = wnba_size)
    
    #defining train data frame
    
    train= wnba.tree[train_index, ]
    
    #defining  test data frame
    
    test= wnba.tree[-train_index,]   
    

  #construct the Classification tree
    tree.wnba = tree(pos_level~.-Pos,train)
    summary(tree.wnba)
    
    plot(tree.wnba)
    text(tree.wnba,pretty=0,cex=0.5)
    
    #I look at its performance
    
    tree.wnba.pred = predict(tree.wnba,test,type = "class")
    con.mat<-table(tree.wnba.pred,pos_level[-train_index])
    
    #assessment measures
    library(caret)
    
    confusionMatrix(con.mat)    
    
    #Model optimisation
    
    
    cv.tree.wnba = cv.tree(tree.wnba,FUN = prune.misclass)#classification error as a criterion in cross validation procedures
                                                          #and pruning
    names(cv.tree.wnba)
    cv.tree.wnba    
    
#plot of error rate and complexity cost as a function of tree size    
    plot(cv.tree.wnba$size,cv.tree.wnba$dev,type = "b",
         ylab = "cross-validation error rate", xlab = "size")
    
    plot(cv.tree.wnba$k, cv.tree.wnba$dev, type = "b",
         ylab = "cost-complexity parameter k", xlab = "size")
    
    #tree with 2 nodes and tree with 5 gives us the lowest cross validation error rate
    
     #I first take the pruned one with 5 nodes
    
    prune.tree.wnba=prune.misclass(tree.wnba,best = 5)
    
    plot(prune.tree.wnba)    
    text(prune.tree.wnba,pretty = 0,cex=0.85)    
    
#evaluation of pruned with 5 nodes    
    tree.prune.pred=predict(prune.tree.wnba,test,type = "class")
    con.mat.2<-table(tree.prune.pred,pos_level[-train_index])    
    con.mat.2    
    
    confusionMatrix(con.mat.2)    
       #pruning increased accuracy rate
    
#I'm also trying the other tree, the one with 2 nodes    
    prune.tree.wnba2=prune.misclass(tree.wnba,best=2)
    
    plot(prune.tree.wnba2)
    text(prune.tree.wnba2,pretty = 0,cex=0.85)   
    
#evaluation of pruned with 2 nodes    
    
    tree.prune.pred.2=predict(prune.tree.wnba2,test,type = "class")
    con.mat.3<-table(tree.prune.pred.2,pos_level[-train_index])
    con.mat.3    
    
    confusionMatrix(con.mat.3)
    
   # the tree with 2 nodes has worse accuracy than the one with 5
     #and equal to the 7-node tree
    
     # ara optimal model the model of 5 knots

    
#Support Vector Machine
    
#I will use the SVM method as a 2nd statistical learning method    
    library(e1071)
    

    #Trying first with a linear kernel
    
    svmfit = svm(pos_level~., data = wnba.tree, kernel="linear",cost=10,scale = FALSE)  
    summary(svmfit)

    
    #Visualising for PTS and BMI
    plot(svmfit,wnba.tree, Height~PTS)

    #Looking for optimal cost
    
    set.seed(1)
    tune.out=tune(svm, pos_level~., data=train, kernel ="linear",
                  ranges=list(cost=c(0.00001,0.0001,0.001,0.01,0.1,1,5,10,100)))
                    
    summary(tune.out)
  
    #best linear model with cost 1
    bestmod=tune.out$best.model

    summary(bestmod)    

    plot(bestmod,wnba.tree,Height~PTS)
    
    best.pred=predict(bestmod,test)    

    xtab2=table(predict=best.pred,truth=test$pos_level)    

    confusionMatrix(xtab2)    

    
    #trying with radial kernel
    
    svmfit_radial=svm(pos_level~.,data = train,kernel="radial",gamma=1,cost=1)
    
    plot(svmfit_radial,train,Height~PTS)    

    summary(svmfit_radial)    

    #trying with different cost
    
    svmfit_radial=svm(pos_level~.,data = train,kernel="radial",gamma=1,cost=1e-10)
    
    plot(svmfit_radial,train,Height~PTS)    
    
    summary(svmfit_radial)
    
    #looking to find optimal cost and optimal gamma
    
    set.seed(1)

    tune.out.radial=tune(svm,pos_level~.,data = train,kernel="radial",
                         ranges = list(cost=c(1e-10,1e-8,1e-5,1e-3,1e-2,1e-1,1,5,10,100),gamma=c(0.5,1,2,3,4)))    

    summary(tune.out.radial)    

 #looking at what the best model gives me    
    bestmode_rad<-tune.out.radial$best.model
    
    summary(bestmode_rad)
    
    plot(bestmode_rad,train,Height~PTS)

    bestmode_rad_pred <-predict(bestmode_rad,test)    
    
    xtab_rad<-table(predict=bestmode_rad_pred,truth=test$pos_level)

    
    confusionMatrix(xtab_rad)
    
#here svm with linear kernel simulates the problem better    
    
