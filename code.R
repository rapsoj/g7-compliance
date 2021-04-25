#### PREPARE WORKSPACE ####

# Set working directory
setwd("~/Desktop")

# Import libraries
library(tidyr)
library(dplyr)
library(MASS)
library(pscl)


#### CLEAN COMPLIANCE DATA ####

# Read in data
df_compliance <- read.csv("compliance.csv")

# Make commitments and conclusions year numeric
df_compliance$year <- as.numeric(df_compliance$year)

# Convert data to long format
df_compliance <- gather(df_compliance, country, score, canada:eu, factor_key=TRUE)

# Replace incorrectly written scores and convert to integer
df_compliance$score <- ifelse(df_compliance$score == "", NA, df_compliance$score)
df_compliance$score <- ifelse(df_compliance$score == "-", -1, df_compliance$score)
df_compliance$score <- as.factor(df_compliance$score)


#### A) ANALYZE COMPLIANCE BY MINISTERIAL MEETING ####

# Read in summit and ministerial dates
df_summit.dates <- read.csv("summit_dates.csv")
df_ministerial.dates <- read.csv("ministerial_dates.csv", na.strings=c("","NA"))

# Merge summit dates with compliance data and ministerial dates
df_compliance <- merge(df_compliance, df_summit.dates)
df_ministerial.dates <- merge(df_ministerial.dates, df_summit.dates)

# Count number of ministerials for a given year-subject
df_ministerial.dates$min.number <- rowSums(!is.na(df_ministerial.dates[3:8]))

# Calculate number of days between ministerial meetings and summit
for (i in colnames(df_ministerial.dates[3:8])) {
  name <- gsub(" ", "", paste(i, ".dist"))
  df_ministerial.dates[[name]] <- as.integer(
    as.Date(df_ministerial.dates[[i]]) - as.Date(df_ministerial.dates$date))
}

# Create column for any ministerial before
df_ministerial.dates$min.before <- ifelse(is.na(apply(
  df_ministerial.dates[12:17], 1, function(row) any(row < 0))), 0, 1)

# Create column for any ministerial after
df_ministerial.dates$min.after <- ifelse(is.na(apply(
  df_ministerial.dates[12:17], 1, function(row) any(row > 0))), 0, 1)

# Create column for closest ministerial
df_ministerial.dates$min.closest <-
  apply(abs(df_ministerial.dates[12:17]), 1, absolute.min)

# Merge closest ministerials with compliance data
cols <- c("year", "area", "min.number", "min.before", "min.after", "min.closest")
df_compliance <- left_join(df_compliance, df_ministerial.dates[cols])

# Format missing values in ministerial columns
cols <- c("min.number", "min.before", "min.after")
df_compliance[cols][is.na(df_compliance[cols])] <- 0

# Add column for any ministerial
df_compliance$min.any <- ifelse(is.na(df_compliance$min.closest), 0, 1)

# Test effect of number of ministerials on compliance
fit <- polr(score ~ min.number + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))

# Test effect of before-summit ministerial on compliance
fit <- polr(score ~ min.before + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))

# Test effect of after-summit ministerial on compliance
fit <- polr(score ~ min.after + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))

# Test effect of closest ministerial on compliance
fit <- polr(score ~ min.closest + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))

# Test effect of any ministerial on compliance
fit <- polr(score ~ min.any + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))

#### B) ANALYZE COMPLIANCE BY SAME-SUBJECT COMMITMENTS ####

# Read in commitment data
df_commitments <- read.csv("total_commitments.csv")

# Convert data to long format
df_commitments <-
  gather(df_commitments, year, commitments,X1975:X2019, factor_key=TRUE)

# Format year column
df_commitments$year <- as.numeric(substring(df_commitments$year, 2))

# Replace missing values with zeroes
df_commitments$commitments <-
  ifelse(is.na(df_commitments$commitments), 0, df_commitments$commitments)

# Merge with compliance data
df_compliance <- left_join(df_compliance, df_commitments)

# Make column for commitments squared
df_compliance$commitments2 <- df_compliance$commitments^2

# Test effect of total commitments on compliance
fit <- polr(score ~ total + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))

# Test effect of same-subject commitments on compliance
fit <- polr(score ~ commitments + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### C) ANALYZE COMPLIANCE BY BINDING LEVEL ####

# Set high binding level
high = c('commit','agree','endorse','pledge','promise','must renew','seek',
         'reaffirm commitment','intend to','create','will','are determined to',
         'continue to','insist on','remain determined','affirm our intention',
         'ensure','complete','reaffirm the need','reaffirmed the importance',
         'remain focussed','we request','ratify','publish','implement','look to',
         'strongly advocate','more ambitious','we shall','pursue','immediate action',
         'investment','decide to','plan of action','resolve to','hold a','mobilize',
         'determination to','undertake to mobilize','mandate','preventing',
         'confirmed additional contributions','further efforts','raising',
         'take sustainable measures')

# Set medium binding level
mid = c('encourage','promote','support','stand ready to','shall ourselves object',
        'need to','must ensure','necessary','need to address','ought to',
        'we emphasize the need','foster','help','strengthen','working to',
        'making available', 'facilitate','strive for','cooperate','should',
        'action is required','stimulate','aim to','necessity')

# Set low binding level
low = c('welcome','urge','call on','reflected upon','discussed','are aware',
        'look forward to','emphasize','call for','recognize the importance',
        'we gave particular emphasis to','united in determination','wishes to',
        'should stand ready to','express confidence in', 'consider',
        'reaffirm the need for advocate','call for','ready to','ask')

# Remove blank and single word commitment text
df_compliance$text <-
  ifelse(df_compliance$text == "" | df_compliance$text == "Trade",
         NA, df_compliance$text)

# Code commitment text by binding level
df_compliance$binding.level <- NA
df_compliance$binding.level <-
  ifelse(str_detect(tolower(df_compliance$text), paste(high, collapse = '|')),
         "High", df_compliance$binding.level)
df_compliance$binding.level <-
  ifelse(str_detect(tolower(df_compliance$text), paste(mid, collapse = '|')),
         "Mid", df_compliance$binding.level)
df_compliance$binding.level <-
  ifelse(str_detect(tolower(df_compliance$text), paste(low, collapse = '|')),
         "Low", df_compliance$binding.level)

# Combine medium and low biding levels
df_compliance$binding.level <- ifelse(df_compliance$binding.level == "High", 1, 0)

# Test effect of binding level on compliance
fit <- polr(score ~ binding.level + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### D) ANALYZE COMPLIANCE BY REFERENCE TO ANOTHER SUMMIT ####

# Set terms to exclude
exclude = c('world summit', 'johannesburg summit', 'observation summit',
            'anti-corruption summit', 'nato lisbon summit')

# Identtify commitments that reference a summit
df_compliance$summit.ref <- NA
df_compliance$summit.ref <-
  ifelse(str_detect(tolower(df_compliance$text), "summit"), 1, 0)

# Exclude terms that do not reference G7/8 summit
df_compliance$summit.ref <-
  ifelse(str_detect(tolower(df_compliance$text), paste(exclude, collapse = '|')),
         0, df_compliance$summit.ref)

# Test effect of summit reference on compliance
fit <- polr(score ~ summit.ref + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))



#### E) ANALYZE COMPLIANCE BY REFERENCE TO INTERNATIONAL ORGANIZATION ####

# Set international organization terms
orgs = c('united nations', 'oecd', ' un ', 'g20', 'world health organization',
         'unaids', 'who plan', 'with who', 'global action plan', 'the who',
         'global health security agenda')

# Identtify commitments that reference an international organization
df_compliance$org.ref <-
  ifelse(str_detect(tolower(df_compliance$text), paste(orgs, collapse = '|')), 1, 0)

# Test effect of international organization reference on compliance
fit <- polr(score ~ org.ref + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### F) ANALYZE COMPLIANCE BY REFERENCE TO TIMETABLE ####

# Identify commitments that mention single year timetable
match <- c("19|20\\d{2}", "\\d+/\\d+/\\d+", "next year",
           "end of the year", "this year")
df_compliance$mentions.date <-
  ifelse(grepl(paste(match, collapse = "|"), df_compliance$text), 1, 0)

# Identify commitments that mention multi-year timetable
match <- c("four year", "19|20\\d{2}-19|20\\d{2}", "5 years", "five years",
           "ten years", "three years", "four-year")
df_compliance$mentions.multi.year <-
  ifelse(grepl(paste(match, collapse = "|"), df_compliance$text), 1, 0)

# Test effect of date reference on compliance
fit <- polr(score ~ mentions.date + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))

# Test effect of multiyear reference on compliance
fit <- polr(score ~ mentions.multi.year + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### G) ANALYZE COMPLIANCE BY REFERENCE TO MONEY MOBILIZED ####

# Identify commitments that mention money
match <- c("\\$", "billion", "million")
df_compliance$mentions.money <-
  ifelse(grepl(paste(match, collapse = "|"), df_compliance$text), 1, 0)

# Test effect of money reference on compliance
fit <- polr(score ~ mentions.money + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### F) ANALYZE COMPLIANCE BY REFERENCE TO DEMOCRACY OR HUMAN RIGHTS ####

# Identify commitments that mentions democracy or human rights
match <- c("democra", "human rights")
df_compliance$mentions.values <-
  ifelse(grepl(paste(match, collapse = "|"), df_compliance$text), 1, 0)

# Test effect of value reference on compliance
fit <- polr(score ~ mentions.values + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### BUILD MULTIVARIABLE MODEL TO EXPLAIN COMPLIANCE ####

# Read in GDP data
df_gdp <- read.csv("gdp.csv")
df_gdp.per.cap <- read.csv("gdp_per_capita.csv")
df_gdp.growth <- read.csv("gdp_per_capita_growth.csv")

# Convert data to long format
df_gdp <- gather(df_gdp, year, gdp, X1960:X2019, factor_key=TRUE)
df_gdp.per.cap <-
  gather(df_gdp.per.cap, year, gdp.per.cap, X1960:X2019, factor_key=TRUE)
df_gdp.growth <-
  gather(df_gdp.growth, year, gdp.growth, X1960:X2019, factor_key=TRUE)

# Format year column
df_gdp$year <- as.numeric(substring(df_gdp$year, 2))
df_gdp.per.cap$year <- as.numeric(substring(df_gdp.per.cap$year, 2))
df_gdp.growth$year <- as.numeric(substring(df_gdp.growth$year, 2))

# Merge with compliance data
df_compliance <- left_join(df_compliance, df_gdp)
df_compliance <- left_join(df_compliance, df_gdp.per.cap)
df_compliance <- left_join(df_compliance, df_gdp.growth)

# Convert GDP to billions and GDP per capita into thousands
df_compliance$gdp <- df_compliance$gdp / 1000000000
df_compliance$gdp.per.cap <- df_compliance$gdp.per.cap / 10000

# Create new columns for countries of interest
df_compliance$is.italy <- ifelse(df_compliance$country == "italy", 1, 0)
df_compliance$is.uk <- ifelse(df_compliance$country == "uk", 1, 0)
df_compliance$is.canada <- ifelse(df_compliance$country == "canada", 1, 0)
df_compliance$is.eu <- ifelse(df_compliance$country == "eu", 1, 0)
df_compliance$is.france <- ifelse(df_compliance$country == "france", 1, 0)
df_compliance$is.germany <- ifelse(df_compliance$country == "germany", 1, 0)
df_compliance$is.japan <- ifelse(df_compliance$country == "japan", 1, 0)
df_compliance$is.russia <- ifelse(df_compliance$country == "russia", 1, 0)

# Create new columns for areas of interest
df_compliance$is.climate <- ifelse(df_compliance$area == "Climate Change", 1, 0)
df_compliance$is.conflict <- ifelse(df_compliance$area == "Conflict Prevention", 1, 0)
df_compliance$is.crime <- ifelse(df_compliance$area == "Crime and Corruption", 1, 0)
df_compliance$is.development <- ifelse(df_compliance$area == "Development", 1, 0)
df_compliance$is.democracy <- ifelse(df_compliance$area == "Democracy", 1, 0)
df_compliance$is.education <- ifelse(df_compliance$area == "Education", 1, 0)
df_compliance$is.energy <- ifelse(df_compliance$area == "Energy", 1, 0)
df_compliance$is.food <- ifelse(df_compliance$area == "Food and Agriculture", 1, 0)
df_compliance$is.gender <- ifelse(df_compliance$area == "Gender", 1, 0)
df_compliance$is.ict <- ifelse(df_compliance$area == "ICT and Digitization", 1, 0)
df_compliance$is.labor <- ifelse(df_compliance$area == "Labour and Employment", 1, 0)
df_compliance$is.macro <- ifelse(df_compliance$area == "Macroeconomics", 1, 0)
df_compliance$is.security <- ifelse(df_compliance$area == "Regional Security", 1, 0)
df_compliance$is.trade <- ifelse(df_compliance$area == "Trade", 1, 0)

# Build model with potentially significant variables
fit <- polr(score ~ year + min.before + mentions.money + mentions.values +
              is.italy + is.uk + is.canada + is.eu + is.japan + is.russia +
              is.france + is.climate + is.conflict + is.crime + is.development +
              is.democracy + is.education + is.energy + is.food + is.gender +
              is.ict + is.labor + is.macro + is.security + is.trade,
            data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))
pR2(fit)