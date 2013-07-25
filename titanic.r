
######
# load data
#####

train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')


#####
# Some basic cleaning
#####

colnames(train) <- tolower(colnames(train))
colnames(test) <- tolower(colnames(test))

# Parse the cabins
# Some rows have cabins, indicated by a letter and a number (except Cabin T, which is unique)
# However, some rows have multiple cabins, or have malformed cabins (maybe only a letter)
# We'll ignore the additional cabins, since it looks like they tend to be on the same deck
# So we process like this to create two new columns, deck and cabin_num

parse_cabins <- function(df) {
    cabins <- df$cabin
    deck <- substr(cabins, 1,1)
    deck[deck == ""] <- NA

    cabin_num <- substr(cabins, 2, 3)
    # Handles single digit cabins fine, but sometimes will catch a space and a letter.  We'll just set these to NA
    cabin_num[substr(cabin_num, 1, 1)==" "] <- NA
    cabin_num[cabin_num == ""] <- NA

    df$cabin <- NULL
    df$deck <- deck
    df$cabin_num <- as.numeric(cabin_num)
    return(df)
}

# Assign a class
# Decks are split into classes
# First Class: A, B, C, D, T
# Second Class: E
# Third Class: F, G

# oops, this is already in the dataset

assign_class <- function(df){
    deck <- df$deck
    deck_class <- rep(NA, length(deck))
    deck_class[deck %in% c('A', 'B', 'C', 'D', 'T')] <- 1
    deck_class[deck %in% c('E')] <- 2
    deck_class[deck %in% c('F', 'G')] <- 3
    df$deck_class <- deck_class
    return(df)
}

# 0 for female, 1 for male

code_gender <- function(df){
    sex <- df$sex
    coded_sex  <- rep(NA, length(sex))
    coded_sex[sex=='male'] <- 1
    coded_sex[sex=='female'] <- 0
    df$sex <- coded_sex
    return(df)
}

code_embarkation <- function(df){
    df$embarked <- as.numeric(as.factor(df$embarked))
    return(df)
}

########
# Modeling
########

glm_binomial <- function(df){
    # simple linear model
    # Can't use age, since not all of them have age.
    # Removed a few factors for nonsignificance.
    fm <- as.formula('survived ~ pclass + sibsp + embarked + sex')
    mdl <- glm(fm, data=df, family='binomial')
    return(mdl)
}

if (FALSE) {
    # Skip this, already run
    train_df <- code_gender(code_embarkation(train))
    test_df <- code_gender(code_embarkation(test))
    linear_mdl <- glm_binomial(train_df)
    linear_predict <- predict(linear_mdl, test_df, type='response')
    linear_predict[linear_predict < 0.5] <- 0
    linear_predict[linear_predict > 0.5] <- 1
    out <- data.frame(PassengerId=test_df$passengerid, Survived=linear_predict)
    write.csv(out, file="prediction.csv", row.names=FALSE)
}
library(randomForest)
rforest <- function(df) {
    # Random Forests model
    # Manually select the ones that are fully populated
    fm <- as.formula('survived ~ pclass + sex + sibsp + parch + embarked + fare')
    # Requires response var to be categorical
    if (class(df$survived) != 'factor') {
        df$survived <- as.factor(df$survived)
    }
    mdl <- randomForest(fm, data=df)
    return(mdl)
}

if (TRUE) {
    train_df <- code_gender(code_embarkation(train))
    test_df <- code_gender(code_embarkation(test))
    rf_mdl <- rforest(train_df)
    rf_predict <- predict(rf_mdl, test_df)
    out <- data.frame(PassengerId=test_df$passengerid, Survived=rf_predict)
    write.csv(out, file="prediction.csv", row.names=FALSE)
}

