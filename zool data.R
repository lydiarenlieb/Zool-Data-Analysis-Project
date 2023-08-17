install.packages("dplyr")
library(dplyr)
library(tidyr)
df<-read_csv("data/Zool Month 1.csv")
#Clean data--change form

##change player_id into number
df$player_id <- as.integer(factor(df$player_id, levels = unique(df$player_id)))

##delate useless list
df <- df[, -c(2, 3, 4,5,6,7,10)]

##put time together
df <- df %>%
  unite(time, year, month, day, hour, sep = "-")

##put same players information together
df <- df %>%
   group_by(event_timestamp) %>%
   pivot_wider(names_from = event_name, values_from = event_value, names_prefix = "event_")


##rename list
df <- df %>%
rename(warning=event_health_warning, 
       deaths = event_number_of_deaths, 
       times = event_total_seconds_in_level,
       inputs=event_total_number_of_inputs,
       world=event_world_number,
       level=event_level_number,
       health_pickup=`event_number_of_health_pick-ups_collected`,
       health_recovered=event_amount_of_health_recovered,
       fh_recovered=event_amount_of_health_recovered_when_on_final_health,
       fh_time=event_number_of_time_player_entered_final_health,
       fh_time_spend=event_total_seconds_spend_on_final_health,
       fh_inputs=event_total_number_of_inputs_on_final_health,
       score=event_score_collected,
       collected=event_total_number_of_giant_collectibles_collected,
       collected_badge=event_collectible_badge_obtained,
       time_badge=event_time_badge_obtained,
       gameover=event_game_over,
       health_pickup_spawned=`event_number_of_health_pick-ups_spawned`)

#Clean data--delete data

df$gameover[is.na(df$gameover)] <- 0
df$collected_badge[is.na(df$collected_badge)] <- 0
df$time_badge[is.na(df$time_badge)] <- 0


df <- df[!is.na(df$warning), ]

df$times[df$times >= 2000] <- NA

df<-df[df$player_id!=358,]

colSums(is.na(df))


#Add Varabies

## degree
convert <- function(x, y) {
  return((x-1)*4 + y)
}

df$degree <- mapply(convert, df$world, df$level)

##Churn Rate
###status
df <- df %>%
  group_by(player_id) %>%
  mutate(status = ifelse(degree == max(degree), 1, 0))
###churn
df <- df %>%
  group_by(player_id) %>%
  mutate(max_date_for_status_1 = max(ifelse(status == 1, event_timestamp, as.Date(NA)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(churn = ifelse(status == 1 & event_timestamp == max_date_for_status_1, 1, 0)) %>%
  select(-max_date_for_status_1)  

###natural-churn
df$natural_churn <- ifelse(df$degree == 28 & df$churn == 1, 2, df$churn)

##dup_flag
df$dup_flag <- ifelse(duplicated(df[c("player_id", "degree")]) | duplicated(df[c("player_id", "degree")], fromLast = TRUE), 1, 0)
##dup_count

df <- df %>%
  arrange(time) %>% 
  group_by(player_id, degree) %>%
  mutate(dup_count = row_number() - 1) %>% 
  ungroup()

##total time
df <- df %>%
  group_by(player_id) %>%
  mutate(total_time = sum(times))

#Analysis

##cox
install.packages("survival")
library(survival)
surv_obj <- with(df, Surv(degree, churn))
cox_model <- coxph(surv_obj ~ deaths+times+inputs+warning+total_time+dup_count+gameover, data = df)
summary(cox_model)

##分层cox
df$natural_churn <- as.factor(df$natural_churn)
surv_obj1 <- Surv(time = df$degree, event = df$natural_churn)
model1 <- coxph(surv_obj ~ deaths+times+inputs+warning+total_time+dup_count+gameover + strata(natural_churn), data = df)
summary(model1)
##crr

install.packages("cmprsk")
library(cmprsk)
cov.mat <- cbind(df$inputs, df$deaths, df$dup_count, df$warning, df$total_time, df$times)
fit <- crr(ftime = df$degree, fstatus = df$natural_churn, cov.mat)

fit1 <- crr(ftime = df$degree, fstatus = df$natural_churn, 
            cov1 = df[, c("inputs", "deaths","collected","collected_badge","time_badge","dup_count","warning","total_time","times","gameover")],
            failcode=1,cencode=0)
fit_na1 <- crr(ftime = df$degree, fstatus = df$natural_churn, 
            cov1 = df[, c("inputs", "deaths","collected","collected_badge","time_badge","dup_count","warning","total_time","times","gameover")],
            failcode=2,cencode=0)
fit2 <- crr(ftime = df$degree, fstatus = df$natural_churn, 
            cov1 = df[, c("collected_badge","time_badge","dup_count","total_time","warning","gameover")],
            failcode=1,cencode=0)
fit_na2 <- crr(ftime = df$degree, fstatus = df$natural_churn, 
            cov1 = df[, c("collected_badge","time_badge","dup_count","total_time","warning","gameover")],
            failcode=2,cencode=0)
summary(fit1)
summary(fit2)
##warning

library(stats)

model_deaths <- lm(deaths ~ warning, data = df)
model_times <- lm(times ~ warning, data = df)
model_inputs <- lm(inputs ~ warning, data = df)
model_score <- lm(score ~ warning, data = df)
model_dup_count <- lm(dup_count ~ warning, data = df)
model_gameover <- glm(gameover~ warning, data = df, family = binomial)


summary(model)


#Save data
 write.csv(df, file = "zool.csv", row.names = FALSE)