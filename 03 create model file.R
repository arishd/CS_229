
# Join tables together

############
# Join attack and defense
pl19_a_d <- merge(pl_19, pl_19_d, by = c("match_no","H_or_A"))
pl18_a_d <- merge(pl_18, pl_18_d, by = c("match_no","H_or_A"))
pl17_a_d <- merge(pl_17, pl_17_d, by = c("match_no","H_or_A"))
pl16_a_d <- merge(pl_16, pl_16_d, by = c("match_no","H_or_A"))
pl15_a_d <- merge(pl_15, pl_15_d, by = c("match_no","H_or_A"))

############
# Union all seasons
pl_a_d <- unique(rbind(pl19_a_d, pl18_a_d, pl17_a_d, pl16_a_d, pl15_a_d))
ncol(pl_a_d)
nrow(pl_a_d)

############
# Make indicator for home game
pl_a_d$home <- ifelse(pl_a_d$H_or_A == "H",1,0)

############
# Add id to each column
row_id <- as.numeric(rownames(pl_a_d))
pl_a_d <- cbind(row_id=row_id, pl_a_d)

############
# Drop columns that won't be used for modelling
pl_a_d <- subset(pl_a_d, select = -c(match_no,date,H_or_A,team,opp_team))
tail(pl_a_d,3)

############
# Split into training, validation and test
vald_size <- floor(0.2 * nrow(pl_a_d))
test_size <- floor(0.2 * nrow(pl_a_d))

# validation set first
set.seed(123)
vald_ind <- sample(seq_len(nrow(pl_a_d)), size = vald_size)
vald <- pl_a_d[vald_ind, ]

# test set
train_test <- pl_a_d[-vald_ind, ]
set.seed(321)
test_ind <- sample(seq_len(nrow(train_test)), size = test_size)
test <- train_test[test_ind, ]

# training set
train <- train_test[-test_ind, ]

# so we have
vald
test
train

############

# 3 data splits
df_vald_pre = vald
df_test_pre = test
df_train_pre = train

# split response from train
df_train = subset(df_train_pre, select = -c(row_id))
df_test = subset(df_test_pre, select = -c(row_id))
df_vald = subset(df_vald_pre, select = -c(row_id))


df_train$goals_h_a_l3 <- ifelse(is.na(df_train$goals_h_l3),df_train$goals_a_l3,df_train$goals_h_l3)
df_train$shots_h_a_l3 <- ifelse(is.na(df_train$shots_h_l3),df_train$shots_a_l3,df_train$shots_h_l3)
df_train$shots_ot_h_a_l3 <- ifelse(is.na(df_train$shots_ot_h_l3),df_train$shots_ot_a_l3,df_train$shots_ot_h_l3)
df_train$corners_h_a_l3 <- ifelse(is.na(df_train$corners_h_l3),df_train$corners_a_l3,df_train$corners_h_l3)
df_test$goals_h_a_l3 <- ifelse(is.na(df_test$goals_h_l3),df_test$goals_a_l3,df_test$goals_h_l3)
df_test$shots_h_a_l3 <- ifelse(is.na(df_test$shots_h_l3),df_test$shots_a_l3,df_test$shots_h_l3)
df_test$shots_ot_h_a_l3 <- ifelse(is.na(df_test$shots_ot_h_l3),df_test$shots_ot_a_l3,df_test$shots_ot_h_l3)
df_test$corners_h_a_l3 <- ifelse(is.na(df_test$corners_h_l3),df_test$corners_a_l3,df_test$corners_h_l3)
df_vald$goals_h_a_l3 <- ifelse(is.na(df_vald$goals_h_l3),df_vald$goals_a_l3,df_vald$goals_h_l3)
df_vald$shots_h_a_l3 <- ifelse(is.na(df_vald$shots_h_l3),df_vald$shots_a_l3,df_vald$shots_h_l3)
df_vald$shots_ot_h_a_l3 <- ifelse(is.na(df_vald$shots_ot_h_l3),df_vald$shots_ot_a_l3,df_vald$shots_ot_h_l3)
df_vald$corners_h_a_l3 <- ifelse(is.na(df_vald$corners_h_l3),df_vald$corners_a_l3,df_vald$corners_h_l3)


df_train$goals_c_h_a_l3 <- ifelse(is.na(df_train$goals_c_h_l3),df_train$goals_c_a_l3,df_train$goals_c_h_l3)
df_train$shots_c_h_a_l3 <- ifelse(is.na(df_train$shots_c_h_l3),df_train$shots_c_a_l3,df_train$shots_c_h_l3)
df_train$shots_ot_c_h_a_l3 <- ifelse(is.na(df_train$shots_ot_c_h_l3),df_train$shots_ot_c_a_l3,df_train$shots_ot_c_h_l3)
df_train$corners_c_h_a_l3 <- ifelse(is.na(df_train$corners_c_h_l3),df_train$corners_c_a_l3,df_train$corners_c_h_l3)
df_test$goals_c_h_a_l3 <- ifelse(is.na(df_test$goals_c_h_l3),df_test$goals_c_a_l3,df_test$goals_c_h_l3)
df_test$shots_c_h_a_l3 <- ifelse(is.na(df_test$shots_c_h_l3),df_test$shots_c_a_l3,df_test$shots_c_h_l3)
df_test$shots_ot_c_h_a_l3 <- ifelse(is.na(df_test$shots_ot_c_h_l3),df_test$shots_ot_c_a_l3,df_test$shots_ot_c_h_l3)
df_test$corners_c_h_a_l3 <- ifelse(is.na(df_test$corners_c_h_l3),df_test$corners_c_a_l3,df_test$corners_c_h_l3)
df_vald$goals_c_h_a_l3 <- ifelse(is.na(df_vald$goals_c_h_l3),df_vald$goals_c_a_l3,df_vald$goals_c_h_l3)
df_vald$shots_c_h_a_l3 <- ifelse(is.na(df_vald$shots_c_h_l3),df_vald$shots_c_a_l3,df_vald$shots_c_h_l3)
df_vald$shots_ot_c_h_a_l3 <- ifelse(is.na(df_vald$shots_ot_c_h_l3),df_vald$shots_ot_c_a_l3,df_vald$shots_ot_c_h_l3)
df_vald$corners_c_h_a_l3 <- ifelse(is.na(df_vald$corners_c_h_l3),df_vald$corners_c_a_l3,df_vald$corners_c_h_l3)



df_train <- df_train[df_train$shots_l5 > -1]
df_train <- df_train[df_train$shots_h_a_l3 > -1]
df_train <- df_train[df_train$shots_ot_c_h_a_l3 > -1]
df_test <- df_test[df_test$shots_l5 > -1]
df_test <- df_test[df_test$shots_h_a_l3 > -1]
df_test <- df_test[df_test$shots_ot_c_h_a_l3 > -1]
df_vald <- df_vald[df_vald$shots_l5 > -1]
df_vald <- df_vald[df_vald$shots_h_a_l3 > -1]
df_vald <- df_vald[df_vald$shots_ot_c_h_a_l3 > -1]
nrow(df_train)
nrow(df_test)
nrow(df_vald)

df_train2 <- df_train[,c("goals","goals_l1","goals_l5","shots_l1","shots_l5","shots_ot_l1","shots_ot_l5",
                         "corners_l1","corners_l5","goals_c_l1","goals_c_l5","shots_c_l1","shots_c_l5",
                         "shots_ot_c_l1","shots_ot_c_l5","corners_c_l1","corners_c_l5","home","shots_ot_c_h_a_l3", "corners_c_h_a_l3",
                         "goals_h_a_l3", "shots_h_a_l3", "shots_ot_h_a_l3", "corners_h_a_l3", "goals_c_h_a_l3", "shots_c_h_a_l3"
)]
df_test2 <- df_test[,c("goals","goals_l1","goals_l5","shots_l1","shots_l5","shots_ot_l1","shots_ot_l5",
                       "corners_l1","corners_l5","goals_c_l1","goals_c_l5","shots_c_l1","shots_c_l5",
                       "shots_ot_c_l1","shots_ot_c_l5","corners_c_l1","corners_c_l5","home","shots_ot_c_h_a_l3", "corners_c_h_a_l3",
                       "goals_h_a_l3", "shots_h_a_l3", "shots_ot_h_a_l3", "corners_h_a_l3", "goals_c_h_a_l3", "shots_c_h_a_l3"
)]
df_vald2 <- df_vald[,c("goals","goals_l1","goals_l5","shots_l1","shots_l5","shots_ot_l1","shots_ot_l5",
                       "corners_l1","corners_l5","goals_c_l1","goals_c_l5","shots_c_l1","shots_c_l5",
                       "shots_ot_c_l1","shots_ot_c_l5","corners_c_l1","corners_c_l5","home","shots_ot_c_h_a_l3", "corners_c_h_a_l3",
                       "goals_h_a_l3", "shots_h_a_l3", "shots_ot_h_a_l3", "corners_h_a_l3", "goals_c_h_a_l3", "shots_c_h_a_l3"
)]

#apply(df_train2,2,function(x) sum(is.na(x)))
#apply(df_test2,2,function(x) sum(is.na(x)))
#apply(df_vald2,2,function(x) sum(is.na(x)))


maxs_train <- apply(df_train2, 2, max) 
mins_train <- apply(df_train2, 2, min)
df_train3 <- as.data.frame(scale(df_train2, center = mins_train, scale = maxs_train - mins_train))
#maxs_test <- apply(df_train2, 2, max) 
#mins_test <- apply(df_train2, 2, min)
df_test3 <- as.data.frame(scale(df_test2, center = mins_train, scale = maxs_train - mins_train))
df_vald3 <- as.data.frame(scale(df_vald2, center = mins_train, scale = maxs_train - mins_train))



