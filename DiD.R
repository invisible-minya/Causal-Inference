library(data.table)
library(lubridate)
library(ggplot2)
library(rddtools)
library(rdrobust)
library(rdd)
library(plm)

new_d1 <- read.csv('/Users/miana/Downloads/new_d1.csv',header = T)
new_d2 <- read.csv('/Users/miana/Downloads/new_d2.csv',header = T)
avg_d <- merge(new_d1,new_d2, by = c("X","State","year","month"))
d <- fread('/Users/miana/Downloads/US_Accidents_Dec19.csv',stringsAsFactors = F)

me <- d[d$State == 'ME',]


me$month <- month(me$Start_Time)
me$year <- year(me$Start_Time)
view(me)

me_crash <- me %>%
  select(year,month,State)%>%
  group_by(year,month)%>%
  count()

avg_me <- avg_d[avg_d$State =="ME",]
me <- merge(avg_me,me_crash, by = c("year","month"))

nv <- d[d$State == 'NV',]
nv$month <- month(nv$Start_Time)
nv$year <- year(nv$Start_Time)
nv_crash <- nv %>%
  select(year,month,State)%>%
  group_by(year,month)%>%
  count()
avg_nv<- avg_d[avg_d$State =="NV",]
nv <- merge(avg_nv,nv_crash, by = c("year","month"))

mn <- d[d$State == 'MN',]
mn$month <- month(mn$Start_Time)
mn$year <- year(mn$Start_Time)
mn_crash <- mn %>%
  select(year,month,State)%>%
  group_by(year,month)%>%
  count()
avg_mn<- avg_d[avg_d$State =="MN",]
mn <- merge(avg_mn,mn_crash, by = c("year","month"))

tx <- d[d$State == 'TX',]
tx$month <- month(tx$Start_Time)
tx$year <- year(tx$Start_Time)
tx_crash <- tx %>%
  select(year,month,State)%>%
  group_by(year,month)%>%
  count()
avg_tx<- avg_d[avg_d$State =="TX",]
tx <- merge(avg_tx,tx_crash, by = c("year","month"))

{ri <- d[d$State == 'RI',]
ri$month <- month(ri$Start_Time)
ri$year <- year(ri$Start_Time)
ri_crash <- ri %>%
  select(year,month,State)%>%
  group_by(year,month)%>%
  count()
avg_ri<- avg_d[avg_d$State =="RI",]
ri <- merge(avg_ri,ri_crash, by = c("year","month"))}

f <- rbind(me,nv)
f <- rbind(f,mn)
f <- rbind(f,tx)
colnames(f)
f<-f %>%
  select(X,State,year,month,n,avg_severity:major_astro)%>%
  arrange(X,State,year,month,n)

write.csv(f,'/Users/miana/Downloads/4_states_crashes_weather.csv')

f <- read.csv('/Users/miana/Downloads/4_states_crashes_weather.csv',header = T)
# differences in difference

f$y_m <- ymd(paste(f$year,f$month,1,sep=" "))
colnames(f)
f$after <- ifelse(f$y_m>=as.POSIXct("2017-1-1"),1,0)
f$legalization <- ifelse(f$State=='ME' |f$State=='NV',1,0)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#f %>%
#  select(legalization, year, month,n, major_weather_condition)%>%
#  group_by(legalization,year,month) %>%
#  summarise(crashs = mean(n), weather = Mode(major_weather_condition))%>%
#  filter(States == "MN" & States =='ME')

filter_f <- f %>%
  filter(y_m <as.POSIXct("2017-11-1"))%>%
  filter(State == "ME" | State == "MN")

filter_f$week <- c(1:2,4:20,1:20)

ggplot(filter_f, aes(x = week, y = log(n+1),group = State, color = factor(State))) + 
  geom_line()+
  geom_vline(xintercept = 9)

colnames(normalized_m)

view(f)
did_basic = lm(log(n+1) ~ legalization + after + legalization*after +as.factor(major_weather_condition), data=filter_f)
summary(did_basic)

library(plm)
did_sfe_tfe = plm(log(n+1) ~ legalization + after + legalization*after + as.factor(major_weather_condition), data = filter_f, index=c("State", "week"), effect="twoway", model="within")
summary(did_sfe_tfe)



# dynamic failed
did_dyn_sfe_tfe <- lm(log(n+1) ~ legalization + as.factor(week) + legalization*as.factor(week)+as.factor(major_weather_condition), data = filter_f)
summary(did_dyn_sfe_tfe)

model = summary(did_dyn_sfe_tfe)
coefs_ses = as.data.frame(model$coefficients[27:39,c("Estimate", "Std. Error")])
colnames(coefs_ses) = c("beta", "se")
coefs_ses = coefs_ses %>%
  mutate(ub90 = beta + 1.96*se,
         lb90 = beta - 1.96*se,
         week = 1:nrow(coefs_ses))


ggplot(coefs_ses, aes(x = week, y = beta)) + 
  geom_line() + 
  geom_hline(yintercept=0,linetype="dashed") + 
  geom_vline(xintercept=6,linetype="dashed") + 
  geom_ribbon(aes(ymin = lb90, ymax = ub90), alpha = 0.3) + 
  theme_bw()




library(readxl)
##
rev <- read_excel('/Users/miana/Downloads/clean_data_causal/rev_expenses.xlsx')
rev <- rev %>%
  select(State,Year,Highways_Expense)
rev <- rev %>%
  filter(Year %in% c(2017))
view(rev)

rev_high <- rev%>%
  spread(key = Year,value = Highways_Expense) %>%
  setnames(old = '2017',
           new = 'X2017_highway')

rev_high <- rev_high%>%
  filter(State == "Minnesota" | State == "Maine")

rev_high <- rev_high%>%
  mutate(State_ab = ifelse(State == "Minnesota","MN","ME"))
write.csv(rev_high, '/Users/miana/Downloads/high_way_MN_ME.csv')
write.csv(filter_f,'/Users/miana/Downloads/weather_high_way_MN_ME.csv')

filter_f<- merge(filter_f, rev_high, by.x = "State", by.y = "State_ab")
filter_f$X2017_highway <- filter_f$X2017_highway/12
highway <- data.frame(rowMeans(subset(m, select = c(Highway1, prop_male_2015 , prop_male_2016)),na.rm = TRUE))
colnames(m)
