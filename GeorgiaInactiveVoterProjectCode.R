library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(ggpubr)
library(scales)

# We need to change our dates to Date types 
setAs("character","myDate", function(from) ymd(from))

# We ar only given birth year, so we need to convert that to their age
setAs("character", "age", function(from) as.integer(2017 - as.numeric(from)))

# We need to skip some columns that are not useful to us
# as well as read in the correct type for the column 
readcolumns <- c(rep("character", 3), rep("NULL", 9), rep("character", 2), "age", "myDate", rep("character", 2), rep("NULL",2), rep("character", 2), rep("NULL",25), rep("myDate", 4), "NULL", "character", "myDate", rep("NULL", 9))

# Sept 2017 GA Voter File 
working.dir <- "C:/Users/UserName/Desktop/Election Research/Georgia_Daily_VoterBase/"
ga_voter.file <- paste(working.dir, "Georgia_Daily_VoterBase.txt", sep="")
ga_voter.sept17 <- as.tibble(read.csv(ga_voter.file, header = TRUE, sep = "|", quote = "", dec = ".", colClasses = readcolumns, fill = TRUE, stringsAsFactors = FALSE))

colnames(ga_voter.sept17)[6] <- "AGE"

###################################
# First let's look at frequency of 
# DATE_CHANGED of inacitve voters 
###################################

# All Inactive voters from the Sept 2017 voter file 
inactive.sept17 <- ga_voter.sept17 %>% 
  filter(VOTER_STATUS == "I" & DATE_CHANGED <= "2017-12-31")

# Freq of date changed dates in Sept 17 VF
inactive.sept17 %>%
  group_by(DATE_CHANGED) %>%
  summarise(Freq = n()) %>%
  arrange(desc(Freq))

# Plot to point out peaks in Date Changed for Sept 2017
date_changed.plot <- ggplot(data = inactive.sept17) + 
  geom_freqpoly(mapping = aes(x = DATE_CHANGED), binwidth = 1) + 
  scale_x_date(date_labels="%Y", date_breaks  = "1 year") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(title = "Voter Status 'Inactive' from September 2017 GA Voter File", 
       x = "Date Changed", y = "Registrations Changed", subtitles = "Total Voters: 6,421,489\nInactive Voters: 606,205") + 
  annotate("text",
           x = as.Date(c("2015-08-08", "2015-10-05", "2017-08-09"), "%Y-%m-%d"),
           y = c(131000, 111000, 138000),
           label = c("Aug 8, 2015\n129,361", "Oct 5, 2015\n109,915", "Aug 9, 2017\n137,158"),
           color = "red",
           size = 2.75,
           fontface = "bold") +
  scale_y_continuous(labels = comma)
date_changed.plot <- annotate_figure(date_changed.plot, bottom = text_grob("Data source: \n September 2017 GA Voter File", color = "blue",
                                                                   hjust = 1, x = 1, face = "italic", size = 10))
date_changed.plot

# There are 3 large spikes: 8/8/2015, 10/5/2015, and 8/9/2015. 
# I have more data and voter files for 2017 so I will look at 
# the 8/9/2017 spike which I will called "the spike" for now on
# Also, there is an interesting article surrounding this date that 
# is related to the anaylsis. 
# https://www.nytimes.com/2017/08/08/us/politics/justice-department-ohio-voter-rolls.html

#############
# THE SPIKE #
#############

### TOTAL BREAKDOWN ###

# Inactive voters in the spike
inactive.spike <- inactive.sept17 %>% 
  filter(DATE_CHANGED == "2017-08-09")

# Percent of inactive voters in the spike
spike_by_inactive <- nrow(inactive.spike) / nrow(inactive.sept17) * 100

# Percent of over all voters in the spike
spike_by_total <- nrow(inactive.spike) / nrow(ga_voter.sept17) * 100

spike <- tibble(Voters = c('Total', 'Inactive'), 
                Percent = c(spike_by_total, spike_by_inactive))
spike

spike.bar <- ggplot(data = spike) + 
  geom_bar(mapping = aes(x = Voters, y = Percent, fill = Voters), stat = "identity") +
  geom_text(aes(x = Voters, y = Percent + 1,
                label = paste0(format(Percent,digits=1, nsmall=2) , '%')),  
            position = position_dodge(width = .9), 
            size = 4) +
  labs(title = "Voters in August 9th, 2017 Spike", y = "Percent", 
       subtitle = "Total voters: 6,421,489\nInactive voters: 606,205\nVoters in spike: 137,158") + 
  scale_fill_manual(values = c("blue", "orange"), guide = FALSE) +
  scale_x_discrete(limits = c("Total", "Inactive")) +
  scale_y_continuous(labels = percent_format(scale = 1), breaks = c(0,10,20))

spike.bar <- annotate_figure(spike.bar, bottom = text_grob("Data source: \n September 2017 GA Voter File", color = "blue",
                                                                       hjust = 1, x = 1, face = "italic", size = 10))

spike.bar

### RACIAL BREAKDOWN ###

# Total percent by race
total.race <- ga_voter.sept17 %>%
  group_by(RACE) %>%
  summarise(Total = n()/nrow(ga_voter.sept17) * 100) %>%
  arrange(desc(Total))
total.race

# Spike percent by race
spike.race <- inactive.spike %>%
  group_by(RACE) %>%
  summarise(Spike = n()/nrow(inactive.spike) * 100) %>%
  arrange(desc(Spike))
spike.race

# Inactive percent by race
inactive.race <- inactive.sept17 %>%
  group_by(RACE) %>%
  summarise(Inactive = n()/nrow(inactive.sept17) * 100) %>%
  arrange(desc(Inactive))
inactive.race

# Make a table for all results to compare proportions
total_inactive <- merge(x = total.race, y = inactive.race, by = "RACE")
total_inactive_spike.1 <- merge(x = total_inactive, y = spike.race, by = "RACE") %>% 
  arrange(desc(RACE))
format(total_inactive_spike.1, digits=1, nsmall=2)

# Using gather, we can make the data more friendlier to work with in a graph
total_inactive_spike.2 <- total_inactive_spike.1 %>% 
  gather(Total, Inactive, Spike, key = "Voters", value = "Percent") %>%
  arrange(RACE)
format(total_inactive_spike.2, digits=2, nsmall=2)

 
# Make a bar graph for total_inactive_spike
total_inactive_spike.bar <- ggplot(data = total_inactive_spike.2, mapping = aes(x = Voters, y = Percent, fill = RACE)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(y = Percent + 1.3,    # nudge above top of bar
                label = paste0(format(Percent,digits = 0, nsmall = 1), '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 2) +
  labs(title = "Racial Breakdown by Subsets of Entire Voter File", y = "Percent",
       subtitle = "Total voters: 6,421,489\nInactive voters: 606,205\nVoters in spike: 137,158") +
  scale_fill_discrete(name = "Race",
                      labels= c("American Indian or\nAlaskan Native", "Asian or\nPacific Islander", 
                                "Black Not of\nHispanic Origin", "Hispanic", "Other", "Unknown", "White")) +
    scale_y_continuous(labels=percent_format(scale = 1)) +
  scale_x_discrete(limits = c("Spike", "Inactive", "Total"))

total_inactive_spike.bar <- annotate_figure(total_inactive_spike.bar, bottom = text_grob("Data source: \n September 2017 GA Voter File", color = "blue",
                                                                              hjust = 1, x = 1, face = "italic", size = 10))

total_inactive_spike.bar

#########################
# ACTIVITY IN THE SPIKE #
#########################

### Deactivated voters ### 

# The most recent voter file I have before the 
# September 2017 voter file is from Marh 17

# March 2017 Voter File 
working.dir <- "C:/Users/UserName/Desktop/Election Research/Georgia_Daily_VoterBase/Statewide Voter File 3-13-17/"
ga_voter.file <- paste(working.dir, "Georgia_Daily_VoterBase.txt", sep="")
ga_voter.march17 <- as.tibble(read.csv(ga_voter.file, header = TRUE, sep = "|", quote = "", dec = ".", colClasses = readcolumns, fill = TRUE, stringsAsFactors = FALSE))

colnames(ga_voter.march17)[6] <- "AGE"

active.march17 <- ga_voter.march17 %>% 
  filter(VOTER_STATUS == "A" & DATE_CHANGED <= "2017-12-31") 

# Deactivated voters in the spike
deactivated <- merge(x = active.march17, y = inactive.spike, by = "REGISTRATION_NUMBER", all = FALSE)

# Percent of deactivated voters in the spike
deactivated_by_spike <- nrow(deactivated) / nrow(inactive.spike) * 100

# Percent of deactivated voters in the spike by inactive
deactivated_by_inactive <- nrow(deactivated) / nrow(inactive.sept17) * 100

# Percent of over all deactivated voters in the spike
deactivated_by_total <- nrow(inactive.spike) / nrow(ga_voter.sept17) * 100

spike.deactivated <- tibble(Voters = c('Total', 'Deactive', 'In Spike'), 
                Percent = c(deactivated_by_total, deactivated_by_inactive, deactivated_by_spike))
spike.deactivated

# Looking at the code above tells us using the March 2017 voter file
# doesn't give us any real new information. The voters status were changed
# on 8/9/2017 so it makes sense that 99.9 % of voters in the spike
# would have been active in March. Were the missing or other 0.1 % removed between March and Sept?

###### Group 1: People who voted in 2016 GE ######

### TOTAL BREAKDOWN ###

#Total voted
all.voted <- ga_voter.sept17 %>% filter(DATE_LAST_VOTED == "2016-11-08")
inactive.voted <- inactive.sept17 %>% filter(DATE_LAST_VOTED == "2016-11-08")

# Spike voted
spike.voted <- inactive.sept17 %>% 
  filter(DATE_CHANGED == "2017-08-09" & DATE_LAST_VOTED == "2016-11-08")

voted_by_spike <- nrow(spike.voted) / nrow(inactive.spike) * 100
voted_by_inactive <- nrow(spike.voted) / nrow(inactive.sept17) * 100
voted_by_total <- nrow(spike.voted) / nrow(ga_voter.sept17) * 100

voted <- tibble(Voted = c('Total', 'Inactive', 'In Spike'), 
                Percent = c(voted_by_total, voted_by_inactive, voted_by_spike))
voted

voted.bar <- ggplot(data = voted) + 
  geom_bar(mapping = aes(x = Voted, y = Percent, fill = Voted), stat = "identity") +
  geom_text(aes(x = Voted, y = Percent + 1.2,
                label = paste0(format(Percent,digits=1, nsmall=2) , '%')),  
            position = position_dodge(width = .9), 
            size = 4) +
  labs(title = "Voted in 2016 General Election and in The Spike", y = "Percent", 
       subtitle = "Total voted: 3,727,094Voted inactive: 64,104\nVoted in spike: 47,931") + 
  scale_fill_manual(values = c("blue", "orange", "light blue"), guide = FALSE) +
  scale_x_discrete(limits = c("Total", "Inactive", "In Spike")) +
  scale_y_continuous(labels = percent_format(scale = 1))
  
voted.bar <- annotate_figure(voted.bar, bottom = text_grob("Data source: \n September 2017 GA Voter File", color = "blue",
                                                                                   hjust = 1, x = 1, face = "italic", size = 10))
voted.bar

### RACIAL BREAKDOWN ###
voted_overall.race <- all.voted %>%
  group_by(RACE) %>%
  summarise(Total = n()/nrow(all.voted) * 100) %>%
  arrange(desc(Total))
voted_overall.race

voted_in_spike.race <- spike.voted %>%
  group_by(RACE) %>%
  summarise(Spike = n() / nrow(spike.voted) * 100) %>%
  arrange(desc(Spike))
voted_in_spike.race

voted_by_inactive.race <- inactive.voted %>%
  group_by(RACE) %>%
  summarise(Inactive = n() / nrow(inactive.voted) * 100) %>%
  arrange(desc(Inactive))
voted_by_inactive.race

# Make a table for all results to compare proportions
total_inactive.voted <- merge(x = voted_overall.race, y = voted_by_inactive.race, by = "RACE")
total_inactive_spike.voted.1 <- merge(x = total_inactive.voted, y = voted_in_spike.race, by = "RACE") %>% 
  arrange(desc(RACE))
format(total_inactive_spike.voted.1, digits=1, nsmall=2)

# Using gather, we can make the data more friendlier to work with in a graph
total_inactive_spike.voted.2 <- total_inactive_spike.voted.1 %>% 
  gather(Total, Inactive, Spike, key = "Voters", value = "Percent") %>%
  arrange(RACE)
format(total_inactive_spike.voted.2, digits=1, nsmall=2)

total_inactive_spike.voted.bar <- ggplot(data = total_inactive_spike.voted.2, mapping = aes(x = Voters, y = Percent, fill = RACE)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(y = Percent + 1.3,    # nudge above top of bar
                label = paste0(format(Percent,digits = 0, nsmall = 1), '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 2) +
  labs(title = "Racial Breakdown of 2016 General Election Day Voters\nby Subsets of Entire Voter File", y = "Percent",
       subtitle = "Total voted: 3,727,094\nVoted inactive: 64,104\nVoted in spike: 47,931") +
  scale_x_discrete(limits = c("Spike", "Inactive", "Total")) +
  scale_fill_discrete(name = "Race",
                      labels= c("American Indian or\nAlaskan Native", "Asian or\nPacific Islander", 
                                "Black Not of\nHispanic Origin", "Hispanic", "Other", "Unknown", "White")) +
  scale_y_continuous(labels = percent_format(scale = 1)) 

total_inactive_spike.voted.bar <- annotate_figure(total_inactive_spike.voted.bar, bottom = text_grob("Data source: \n September 2017 GA Voter File", color = "blue",
                                                             hjust = 1, x = 1, face = "italic", size = 10))

total_inactive_spike.voted.bar
##### Group 2: People removed between spike and November 2017 #####

# Nov 2017 Voter File
working.dir <- "C:/Users/UserName/Desktop/Election Research/Georgia_Daily_VoterBase_Nov2017/"
ga_voter.file <- paste(working.dir, "Georgia_Daily_VoterBase.txt", sep ="")
ga_voter.nov17 <- as.tibble(read.csv(ga_voter.file, header = TRUE, sep = "|", quote = "", dec = ".", colClasses = readcolumns, fill = TRUE, stringsAsFactors = FALSE))

colnames(ga_voter.nov17)[6] <- "AGE"

### TOTAL BREAKDOWN ###

# How many were purged from the spike?
purged.all <- ga_voter.sept17 %>% filter(!(ga_voter.sept17$REGISTRATION_NUMBER %in% ga_voter.nov17$REGISTRATION_NUMBER))
purged.spike <- inactive.spike %>% filter(!(inactive.spike$REGISTRATION_NUMBER %in% ga_voter.nov17$REGISTRATION_NUMBER))
purged.inactive <- inactive.sept17 %>% filter(!(inactive.sept17$REGISTRATION_NUMBER %in% ga_voter.nov17$REGISTRATION_NUMBER))

purged_by_spike <- nrow(purged.spike) / nrow(inactive.spike) * 100

purged_by_inactive <- nrow(purged.spike) / nrow(inactive.sept17) * 100

purged_by_total <- nrow(purged.spike) / nrow(ga_voter.sept17) * 100

purged <- tibble(Voters = c('Total', 'Inactive', 'In Spike'), 
                Percent = c(purged_by_total, purged_by_inactive, purged_by_spike))
purged

purged.bar <- ggplot(data = purged) + 
  geom_bar(mapping = aes(x = Voters, y = Percent, fill = Voters), stat = "identity") +
  geom_text(aes(x = Voters, y = Percent + .05,
                label = paste0(format(Percent,digits=1, nsmall=2) , '%')),  
            position = position_dodge(width = .9), 
            size = 4) +
  labs(title = "Purged between Sept. and Nov. 2017 and in the Spike", y = "Percent", 
       subtitle = "Total purged: 24,021\nInactive and purged: 2,676\nPurged in spike: 919") + 
  scale_fill_manual(values = c("blue", "orange", "light blue"), guide = FALSE) +
  scale_x_discrete(limits = c("Total", "Inactive", "In Spike")) +
  scale_y_continuous(labels = percent_format(scale = 1, accuracy = 0.1))

purged.bar <- annotate_figure(purged.bar, bottom = text_grob("Data source: \n September 2017 GA Voter File\nNovember 2017 GA Voter File", color = "blue",
                                                           hjust = 1, x = 1, face = "italic", size = 10))
purged.bar

### RACIAL BREAKDOWN ### 

# Purged break down by race
purged_total.race <- purged.all %>%
  group_by(RACE) %>%
  summarise(Total = n() / nrow(purged.all) * 100) %>%
  arrange(desc(Total))
purged_total.race

purged_by_spike.race <- purged.spike %>%
  group_by(RACE) %>%
  summarise(Spike = n() / nrow(purged.spike) * 100) %>%
  arrange(desc(Spike))
purged_by_spike.race

# Purged / All inactive
purged_by_inactive.race <- purged.inactive %>%
  group_by(RACE) %>%
  summarise(Inactive = n() / nrow(purged.inactive) * 100) %>%
  arrange(desc(Inactive))
purged_by_inactive.race

# Make a table for all results to compare proportions
purged_inactive <- merge(x = purged_total.race, y = purged_by_inactive.race, by = "RACE")
purged_inactive_spike.1 <- merge(x = purged_inactive, y = purged_by_spike.race, by = "RACE") %>% 
  arrange(desc(RACE))
format(purged_inactive_spike.1, digits=2, nsmall=2)

# Using gather, we can make the data more friendlier to work with in a graph
purged_inactive_spike.2 <- purged_inactive_spike.1 %>% 
  gather(Total, Inactive, Spike, key = "Voters", value = "Percent") %>%
  arrange(RACE)
format(purged_inactive_spike.2, digits=2, nsmall=2)

purged_inactive_spike.bar <- ggplot(data = purged_inactive_spike.2, mapping = aes(x = Voters, y = Percent, fill = RACE)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(y = Percent + 1.3,    # nudge above top of bar
                label = paste0(format(Percent,digits = 0, nsmall = 1), '%')),   
            position = position_dodge(width = .9), 
            size = 2) +
  labs(title = "Racial Breakdown of Purged Voters by Subsets of Entire Voter File", y = "Percent",
       subtitle = "Total purged: 24,021\nTotal inactive purged: 2,676\nPurged in spike: 919") +
  scale_x_discrete(limits = c("Spike", "Inactive", "Total")) +
  scale_fill_discrete(name = "Race",
                      labels= c("American Indian or\nAlaskan Native", "Asian or\nPacific Islander", 
                                "Black Not of\nHispanic Origin", "Hispanic", "Other", "Unknown", "White"))+
  scale_y_continuous(labels = percent_format(scale = 1))

purged_inactive_spike.bar <- annotate_figure(purged_inactive_spike.bar, bottom = text_grob("Data source: \n September 2017 GA Voter File\nNovember 2017 GA Voter File", color = "blue",
                                                                            hjust = 1, x = 1, face = "italic", size = 10))
purged_inactive_spike.bar

#####  Group 3: People who voted in 2016 GE and were removed between spike and November 2017 #####

### TOTAL BREAKDOWN ###

purged_all.voted <- purged.all %>%
  filter(DATE_LAST_VOTED == "2016-11-08")

purged_inactive.voted <- purged.inactive %>%
  filter(DATE_LAST_VOTED == "2016-11-08")

purged_spike.voted <- purged.spike %>%
  filter(DATE_LAST_VOTED == "2016-11-08")


voted_purged_by_spike <- nrow(purged_spike.voted) / nrow(inactive.spike) * 100

voted_purged_by_inactive <- nrow(purged_spike.voted) / nrow(inactive.sept17) * 100

voted_purged_by_total <- nrow(purged_spike.voted) / nrow(ga_voter.sept17) * 100

voted_and_purged <- tibble(Voters = c('Total', 'Inactive', 'In Spike'), 
                        Percent = c(voted_purged_by_total, voted_purged_by_inactive, 
                                    voted_purged_by_spike))

voted_and_purged

voted_and_purged.bar <- ggplot(data = voted_and_purged) + 
  geom_bar(mapping = aes(x = Voters, y = Percent, fill = Voters), stat = "identity") +
  geom_text(aes(x = Voters, y = Percent + .01,
                label = paste0(format(Percent,digits=1, nsmall=2) , '%')),  
            position = position_dodge(width = .9), 
            size = 4) +
  labs(title = "Purged, Voted on Election Day 2016, and in the Spike", 
       y = "Percent", subtitle = "Total voted and purged: 8,429\nInactive, voted, and purged: 295\nIn spike, voted, and purged: 243") + 
  scale_fill_manual(values = c("blue", "orange", "light blue"), guide = FALSE) +
  scale_x_discrete(limits = c("Total", "Inactive", "In Spike")) +
  scale_y_continuous(labels = percent_format(scale = 1, accuracy = 0.01))

voted_and_purged.bar <- annotate_figure(voted_and_purged.bar, bottom = text_grob("Data source: \n September 2017 GA Voter File\nNovember 2017 GA Voter File", color = "blue",
                                                             hjust = 1, x = 1, face = "italic", size = 10))
voted_and_purged.bar

### RACIAL BREAKDOWN ###

voted_purged_all.race <- purged_all.voted %>%
  group_by(RACE) %>%
  summarise(Total = n() / nrow(purged_all.voted) * 100) %>%
  arrange(desc(Total))
voted_purged_all.race

voted_purged_inactive.race <- purged_inactive.voted %>%
  group_by(RACE) %>%
  summarise(Inactive = n() / nrow(purged_inactive.voted) * 100) %>%
  arrange(desc(Inactive))
voted_purged_inactive.race

voted_purged_spike.race <- purged_spike.voted %>%
  group_by(RACE) %>%
  summarise(Spike = n() / nrow(purged_spike.voted) * 100) %>%
  arrange(desc(Spike))
voted_purged_spike.race

# Make a table for all results to compare proportions
voted_purged_inactive <- merge(x = voted_purged_all.race, y = voted_purged_inactive.race, by = "RACE")
voted_purged_inactive.1 <- merge(x = voted_purged_inactive, y = voted_purged_spike.race, by = "RACE") %>% 
  arrange(desc(RACE))
format(voted_purged_inactive.1, digits=2, nsmall=2)

# Using gather, we can make the data more friendlier to work with in a graph
voted_purged_inactive.2 <- voted_purged_inactive.1 %>% 
  gather(Total, Inactive, Spike, key = "Voters", value = "Percent") %>%
  arrange(RACE)
format(voted_purged_inactive.2, digits=2, nsmall=2)

voted_purged_inactive.bar <- ggplot(data = voted_purged_inactive.2, mapping = aes(x = Voters, y = Percent, fill = RACE)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(y = Percent + 2,    
                label = paste0(format(Percent,digits = 0, nsmall = 1), '%')),   
            position = position_dodge(width = .9), 
            size = 2) +
  labs(title = "Racial Breakdown of 2016 General Election Day Voters\nand Purged by Subsets of Entire Voter File", y = "Percent",
       subtitle = "Total voted and purged: 8,429\nInactive, voted, and purged: 295\nIn spike, voted, and purged: 243") +
  scale_x_discrete(limits = c("Spike", "Inactive","Total")) +
  scale_fill_discrete(name = "Race",
                      labels= c("American Indian or\nAlaskan Native", "Asian or\nPacific Islander", 
                                "Black Not of\nHispanic Origin", "Hispanic", "Other", "Unknown", "White")) +
  scale_y_continuous(labels = percent_format(scale = 1)) 

voted_purged_inactive.bar <- annotate_figure(voted_purged_inactive.bar, bottom = text_grob("Data source: \n September 2017 GA Voter File\nNovember 2017 GA Voter File", color = "blue",
                                                                                      hjust = 1, x = 1, face = "italic", size = 10))
voted_purged_inactive.bar

# Compare total populations of each group and overall
total_populations.1 <- tibble(Race = c("WH", "U", "OT", "HP", "BH", "AP", "AI"), 
                            Overall = total_inactive_spike.1$Total, 
                            Voted = total_inactive_spike.voted.1$Total, 
                            Purged = purged_inactive_spike.1$Total, 
                            Voted_And_Purged = voted_purged_inactive.1$Total)

total_populations.2 <- total_populations.1 %>%
  gather(Overall, Voted, Purged, Voted_And_Purged, key = "Total_Type", value = "Percent") 
total_populations.2

total_pops.bar <- ggplot(data = total_populations.2) +
  geom_bar(mapping = aes(x = Total_Type, y = Percent, fill = Race), stat = 'identity') +
  scale_fill_brewer(name = "Race",
                      labels= c("American Indian or\nAlaskan Native", "Asian or\nPacific Islander", 
                                "Black Not of\nHispanic Origin", "Hispanic", "Other", "Unknown", "White"),
                      palette = "PuOr") +
  scale_x_discrete(limits = c("Voted_And_Purged","Purged","Voted","Overall"),
                   labels = c("Voted and Purged","Purged","Voted GE '16","Total")) +
  scale_y_continuous(labels = percent_format(accuracy = 1, scale = 1)) +
  labs(
       subtitles = "Total voters: 6,421,489\nTotal voted: 3,727,094\nTotal purged: 24,021\nTotal voted and purged: 8,429",
       x = "") + 
  coord_flip()

total_pops.bar <-  annotate_figure(total_pops.bar, bottom = text_grob("Data source: \n September 2017 GA Voter File\nNovember 2017 GA Voter File", color = "blue",
                                                                                 hjust = 1, x = 1, face = "italic", size = 10))

total_pops.bar

race.names <- c("White", "Unknown", "Other", "Hispanic", "Black Not of\nHispanic Origin", "Asian or\nPacific Islander", "American Indian or\nAlaskan Native")
col.names <- c("Race", "Total", "Voted GE '16", "Purged", "Voted and Purged")

total_populations.1$Race <- race.names

total_populations.1 <- total_populations.1 %>%
  arrange(desc(Overall))

total_populations.1 <- total_populations.1 %>% mutate_if(is.numeric, ~round(., 2))

gridExtra::grid.table(total_populations.1, row = NULL, cols = col.names)

