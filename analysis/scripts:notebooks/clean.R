#load data
dat <- season2017

dat2 <- read.csv('2016-17_playerBoxScore.csv')


#remove punctuation to allow for merging
dat2$m_NAME <- stri_trans_general(gsub("[.]", "", dat2$playDispNm), "Latin-ASCII")


#merge data
df <- merge(dat,dat2,by.x = c('PLAYER_NAME', 'GAME_DATE'), by.y = c('m_NAME', 'gmDate'), all.x = TRUE)


#add advanced statistics
df$GameScore <- df$playPTS + 0.4 * df$playFGM - 0.7 * df$playFGA - 0.4*(df$playFTA - df$playFTM) + 0.7 * df$playORB + 0.3 * df$playDRB + df$playSTL + 0.7 * df$playAST + 0.7 * df$playBLK - 0.4 * df$playPF - df$playTO

df$TS <- df$playPTS / (2 * (df$playFGA + 0.44 * df$playFTA))

df$eFG <- (df$playFGM + 0.5 * df$play3PM) / df$playFGA


#add relative rest, group rest vars by category
df$relativeOff <- ifelse(abs(df$teamDayOff - df$opptDayOff) > 1, ifelse(df$teamDayOff - df$opptDayOff > 0, '2+', '-2-'), df$teamDayOff - df$opptDayOff)

df$offType <- ifelse(df$teamDayOff > 2, '3+', df$teamDayOff)


#set default order, remove rownames
df <- df[order(df$TEAM_ID),]

df <- df[order(df$GAME_ID),]

rownames(df) <- NULL


#clean up existing vars (order, type) 
df$SHOT_ZONE_BASIC <- factor(df$SHOT_ZONE_BASIC, levels = c("Restricted Area", "In The Paint (Non-RA)", "Mid-Range", "Right Corner 3", "Above the Break 3", "Left Corner 3", "Backcourt"))

df$SHOT_ZONE_RANGE <- factor(df$SHOT_ZONE_RANGE, levels = c("Less Than 8 ft.", "8-16 ft.", "16-24 ft.", "24+ ft.", "Back Court Shot"))

df$PERIOD <- as.numeric(df$PERIOD)

#Remove NAs
df <- na.omit(df)

#list teams
team <- unique(df['TEAM_NAME'])
