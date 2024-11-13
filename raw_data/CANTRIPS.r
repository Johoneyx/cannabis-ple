library(psych)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)
library(openxlsx)

#########################################################################
#                  Functions I created                                  #
#########################################################################


#function to read in all the data files from different versions, combine them, and handle duplicate columns
read_combine_convert_allversions <- function(file_name_ending, df_name) {
  data_frames <- list()
  
  for (version in c(40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16)) {
    file_name <- paste0("data_exp_74063-v", version, file_name_ending)
    if (file.exists(file_name)) {
      data_frames[[paste0(df_name, version)]] <- read.csv(file_name, sep=";")
    } else {
      warning(paste("File not found:", file_name))
    }
  }
  
  if (length(data_frames) == 0) {
    stop("No files were found.")
  }
  
  # Convert all capes to character in each data frame
  data_frames <- lapply(data_frames, function(df) {
    df[] <- lapply(df, as.character)
    return(df)
  })
  
  column_names_list <- lapply(data_frames, names)
  common_columns <- Reduce(intersect, column_names_list)
  
  combined_data <- data_frames[[1]]
  
  for (i in 2:length(data_frames)) {
    combined_data <- full_join(combined_data, data_frames[[i]], by = common_columns)
  }
  
  # here i remove some duplicate columns. Although I prespecified overlapping columns for the by parameter, the SQL full_join made a cartesian product with suffixes which is weird. But maybe its better if I coalesce them later in the cleaning process when I select the capes as I want to avoid dataloss
 #combined_data <- combined_data %>%
  # select(!matches("\\.x$|\\.y$|\\.x\\.x$|\\.y\\.y$"))
  
  return(combined_data)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~version-specific~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


read_combine_convert_versions31to40 <- function(file_name_ending, df_name) {
  data_frames <- list()
  
  for (version in c(40, 39, 38, 37, 36, 35, 34, 33, 32, 31)) {
    file_name <- paste0("data_exp_74063-v", version, file_name_ending)
    data_frames[[paste0(df_name, version)]] <- read.csv(file_name, sep=";")
  }
  
  # i converted everything to character because R has problems with different datatypes
  data_frames <- lapply(data_frames, function(df) {
    df[] <- lapply(df, as.character)
    return(df)
  })
  
  column_names_list <- lapply(data_frames, names)
  
  common_columns <- Reduce(intersect, column_names_list)
  
  combined_data <- data_frames[[1]]
  
  for (i in 2:length(data_frames)) {
    combined_data <- full_join(combined_data, data_frames[[i]], by = common_columns)
  }
  
  return(combined_data)
}


read_combine_convert_versions16to30 <- function(file_name_ending, df_name) {
  data_frames <- list()
  
  for (version in c(30, 29, 28, 27, 26, 25, 24, 23, 21, 20, 19, 18, 17, 16)) {
    file_name <- paste0("data_exp_74063-v", version, file_name_ending)
    data_frames[[paste0(df_name, version)]] <- read.csv(file_name, sep=";")
  }
  
  data_frames <- lapply(data_frames, function(df) {
    df[] <- lapply(df, as.character)
    return(df)
  })
  
  column_names_list <- lapply(data_frames, names)
  
  common_columns <- Reduce(intersect, column_names_list)
  
  combined_data <- data_frames[[1]]
  
  for (i in 2:length(data_frames)) {
    combined_data <- full_join(combined_data, data_frames[[i]], by = common_columns)
  }
  
  return(combined_data)
}


read_combine_convert_versions22to40 <- function(file_name_ending, df_name) {
  data_frames <- list()
  
  for (version in c(40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22)) {
    file_name <- paste0("data_exp_74063-v", version, file_name_ending)
    data_frames[[paste0(df_name, version)]] <- read.csv(file_name, sep=";")
  }
  
  data_frames <- lapply(data_frames, function(df) {
    df[] <- lapply(df, as.character)
    return(df)
  })
  
  column_names_list <- lapply(data_frames, names)
  

  common_columns <- Reduce(intersect, column_names_list)
  
  combined_data <- data_frames[[1]]
  
  for (i in 2:length(data_frames)) {
    combined_data <- full_join(combined_data, data_frames[[i]], by = common_columns)
  }
  
  return(combined_data)
}





read_combine_convert_versions16to21 <- function(file_name_ending, df_name) {
  data_frames <- list()
  
  for (version in c(21, 20, 19, 18, 17, 16)) {
    file_name <- paste0("data_exp_74063-v", version, file_name_ending)
    data_frames[[paste0(df_name, version)]] <- read.csv(file_name, sep=";")
  }
  

  data_frames <- lapply(data_frames, function(df) {
    df[] <- lapply(df, as.character)
    return(df)
  })
  

  column_names_list <- lapply(data_frames, names)
  
  common_columns <- Reduce(intersect, column_names_list)
  
  combined_data <- data_frames[[1]]
  
  for (i in 2:length(data_frames)) {
    combined_data <- full_join(combined_data, data_frames[[i]], by = common_columns)
  }
  
  return(combined_data)
}


find_common_variables<-function(dflist){
column_names_list <- lapply(dflist, names)

common_columns <- Reduce(intersect, column_names_list)

print(common_columns)
}




#this function I made because R  (SQL in this case), apparently creates different columns with suffixes when full_joining, i want to coalesce those later in the cleaning process and can reuse this function 

coalesce_columns_suffixes <- function(df, base_name) {
  coalesce(df[[base_name]], df[[paste0(base_name, ".x")]], df[[paste0(base_name, ".y")]])
}


#this function i made to recode the capes into numeric 
# Define the recode function
recode_cape <- function(df, column_name) {
  df %>%
    mutate(
      !!paste0(column_name, "_quantized") := case_when(
        .[[column_name]] == "never" ~ 1,
        .[[column_name]] == "sometimes" ~ 2,
        .[[column_name]] == "often" ~ 3,
        .[[column_name]] == "nearly always" ~ 4,
        TRUE ~ NA_real_
      )
    )
}


recode_distress <- function(df, column_name) {
  df %>%
    mutate(
      !!paste0(column_name, "_quantized") := case_when(
        .[[column_name]] == "not at all" ~ 1,
        .[[column_name]] == "a bit distressed" ~ 2,
        .[[column_name]] == "quite distressed" ~ 3,
        .[[column_name]] == "very distressed" ~ 4,
        TRUE ~ NA_real_
      )
    )
}


########################################################################
#   Reading in Questionnaires and combining all versions               #
########################################################################

df_consent_all <-read_combine_convert_allversions("_questionnaire-vlry.csv","df_consent")
View(df_consent_all)
print(names(df_consent_all))

#e-mailadress:ykle
df_email_combined<- read_combine_convert_allversions("_questionnaire-ykle.csv","df_email")
print(names(df_email_combined))
View(df_email_combined)

#Demographics: s5wi
df_demographics_combined <- read_combine_convert_allversions("_questionnaire-s5wi.csv","df_demographics")
print(names(df_demographics_combined))
View(df_demographics_combined)

#CAPEP15
df_CAPEP15_combined<- read_combine_convert_allversions("_questionnaire-t3t7.csv","df_CAPEP15")
print(names(df_CAPEP15_combined))
View(df_CAPEP15_combined)


#Cannabisuse: 3ga9
df_cannabisuse_combined<- read_combine_convert_allversions("_questionnaire-3ga9.csv","df_cannabisuse")
print(names(df_cannabisuse_combined))
View(df_cannabisuse_combined)

#CEQ_PART1: 6uhi
df_CEQ_Part1_combined<- read_combine_convert_allversions("_questionnaire-6uhi.csv","df_CEQ_Part1")
print(names(df_CEQ_Part1_combined))
View(df_CEQ_Part1_combined)


df_regularuse_combined<-read_combine_convert_allversions("_questionnaire-a3ow.csv", "df_regularuse")
print(names(df_regularuse_combined))
View(df_regularuse_combined)

#current regular use 5j5a
df_currentregularuse_combined<-read_combine_convert_allversions("_questionnaire-5j5a.csv", "df_currentregularuse")
print(names(df_currentregularuse_combined))
View(df_currentregularuse_combined)

#cannabis use stop: go9y
df_cannabisusestop_combined<-read_combine_convert_allversions("_questionnaire-go9y.csv", "df_cannabisusestop")
print(names(df_cannabisusestop_combined))
View(df_cannabisusestop_combined)

#CEQ_End:q9xg 
df_CEQ_end_combined<-read_combine_convert_allversions("_questionnaire-q9xg.csv", "df_CEQ_end")
print(names(df_CEQ_end_combined))
View(df_CEQ_end_combined)

#Questionnaire end:moh2
df_Questionnaire_end_combined<-read_combine_convert_allversions("_questionnaire-moh2.csv", "df_Questionnaire_end")
print(names(df_Questionnaire_end_combined))
View(df_Questionnaire_end_combined)




#**************************************************************************#
#         select relevant capes, clean and tidy questionnaires         #
#                                                                          #
#**************************************************************************#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Consent~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_consent_clean <- df_consent_all %>%
select(Participant.Private.ID, Experiment.ID,Experiment.Version,Participant.Public.ID,
 Participant.Device.Type,Participant.Device, Participant.OS,
Participant.Browser,Participant.Monitor.Size, Participant.Viewport.Size,      
Understanding,Reading_Information,Inclusion_Criteria,Withdraw_Keep_Info,
Understand_dataprotection, understand_GDPR, Understanding_Not_Identifiable, Use_data_future_research) %>% View()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~E-mail~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_email_clean <- df_email_combined %>%
select(Participant.Private.ID, e.mail_adress) %>%View()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Demographics~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#i need to use na_if which fills with NA if there is an empty string as otherwise the coalesce function does not work 
df_demographics_clean <- df_demographics_combined %>%
mutate(
country = coalesce(na_if(country.1, ""), na_if(country.text, "")),
sex = coalesce(na_if(sex.1, ""), na_if(sex.2, ""), na_if(sex.3, ""), na_if(sex.other, "")),
education = coalesce(na_if(education.1, ""), na_if(education.2, ""), na_if(education.3, ""), na_if(education.4, ""), na_if(education.5, ""), na_if(education.text, "")),
occupation = coalesce(na_if(occupation.1, ""), na_if(occupation.2, ""), na_if(occupation.3, ""), na_if(occupation.4, ""), na_if(occupation.5, ""), na_if(occupation.text, "")),
mental_health_disorder = coalesce(na_if(mental_health_disorder_yes_no.1, ""), na_if(mental_health_disorder_yes_no.text, "")),
ethnicity = coalesce(na_if(ethnicity.1, ""), na_if(ethnicity.2, ""), na_if(ethnicity.3, ""), na_if(ethnicity.4, ""), na_if(ethnicity.text, ""))) %>%
select(UTC.Timestamp,UTC.Date.and.Time, Local.Timestamp, Local.Timezone, Local.Date.and.Time,, age, sex, country, education, occupation, mental_health_disorder, ethnicity, Participant.Private.ID)


#####################################################################
#                   CAPE P15                                        #                
####################################################################


#ever
#past_3_months
#found the naming in the CAPEP15 paper 
#PI1_drop_hints
#I2_seem_to_be
#PI3_persecuted
#PI4_conspiracy
#PI5_look_oddly
#BE1_devices
#BE2_thought_read
#BE3_thought_own
#BE4_thought_vivid
#BE5_thought_echo
#BE6_control_force
#BE7_double_place
#PA1_heard_voices
#PA2_heard_talking
#PA3_seen_things

#distress
#PLE_before_cannabis
#PLE_before_drugs

df_CAPE15_clean <- df_CAPEP15_combined %>%
  mutate(
    PI1_drop_hints_ever = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPE_15_ever.1"),
    PI2_seem_to_be_ever = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPE_15_ever.2"),
    PI3_persecuted_ever = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPE_15_ever.3"),
    PI4_conspiracy_ever = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPE_15_ever.4"),
    PI5_look_oddly_ever = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPE_15_ever.5"),
    BE1_devices_ever = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPE_15_ever.6"),
    BE2_thought_read_ever = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPE_15_ever.7"),
    BE3_thought_own_ever = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPE_15_ever.8"),
    BE4_thought_vivid_ever = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPE_15_ever.9"),
    BE5_thought_echo_ever = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPE_15_ever.10"),
    BE6_control_force_ever = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPE_15_ever.11"),
    BE7_double_place_ever = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPE_15_ever.12"),
    PA1_heard_voices_ever = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPE_15_ever.13"),
    PA2_heard_talking_ever = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPE_15_ever.14"),
    PA3_seen_things_ever = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPE_15_ever.15"),
    PI1_drop_hints_3m = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_1"),
    PI1_drop_hints_3m_distress = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_1_distress"),
    PI2_seem_to_be_3m = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_2"),
    PI2_seem_to_be_3m_distress = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_2_distress"),
    PI3_persecuted_3m = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_3"),
    PI3_persecuted_3m_distress = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_3_distress"),
    PI4_conspiracy_3m = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_4"),
    PI4_conspiracy_3m_distress = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_4_distress"),
    PI5_look_oddly_3m = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_5"),
    PI5_look_oddly_3m_distress = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_5_distress"),
    BE1_devices_3m = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_6"),
    BE1_devices_3m_distress = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_6_distress"),
    BE2_thought_read_3m = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_7"),
    BE2_thought_read_3m_distress = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_7_distress"),
    BE3_thought_own_3m = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_8"),
    BE3_thought_own_3m_distress = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_8_distress"),
    BE4_thought_vivid_3m = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_9"),
    BE4_thought_vivid_3m_distress = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_9_distress"),
    BE5_thought_echo_3m = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_10"),
    BE5_thought_echo_3m_distress = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_10_distress"),
    BE6_control_force_3m = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_11"),
    BE6_control_force_3m_distress = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_11_distress"),
    BE7_double_place_3m = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_12"),
    BE7_double_place_3m_distress = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_12_distress"),
    PA1_heard_voices_3m = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_13"),
    PA1_heard_voices_3m_distress = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_13_distress"),
    PA2_heard_talking_3m = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_14"),
    PA2_heard_talking_3m_distress = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_14_distress"),
    PA3_seen_things_3m = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_15"),
    PA3_seen_things_3m_distress = coalesce_columns_suffixes(df_CAPEP15_combined, "CAPEP15_3m_15_distress"),
    PLE_before_cannabis = coalesce_columns_suffixes(df_CAPEP15_combined, "PLE_before_cannabis"),
    PLE_before_cannabis_which = coalesce_columns_suffixes(df_CAPEP15_combined, "PLE_before_cannabis.text"),
    PLE_before_other_drug = coalesce_columns_suffixes(df_CAPEP15_combined, "PLE_before_other_substance"),
    PLE_before_other_drug_which = coalesce_columns_suffixes(df_CAPEP15_combined, "PLE_before_other_substance.text")
  ) %>%
  select(starts_with("PI"), starts_with("BE"), starts_with("PA"), PLE_before_cannabis,PLE_before_cannabis_which,PLE_before_other_drug,PLE_before_other_drug_which, -starts_with("Participant"),Participant.Private.ID) 

View(df_CAPE15_clean)
#ich muss noch alle endoffile rows entfernen 
#there are also some old versions of cape-threemonths columns that I still need to coalesce. I think they are called cape_three_months or something 
#the old version has also yes/no instead of the levels. some of the participants data must be available as interview questionnaire though - ask sagnik what to do 
#i will do this later 

#recode levels into numeric 
  df_CAPE15_clean <- df_CAPE15_clean %>%
  recode_cape("PI1_drop_hints_ever") %>%
  recode_cape("PI2_seem_to_be_ever") %>%
  recode_cape("PI3_persecuted_ever") %>%
  recode_cape("PI4_conspiracy_ever") %>%
  recode_cape("PI5_look_oddly_ever") %>%
  recode_cape("BE1_devices_ever") %>%
  recode_cape("BE2_thought_read_ever") %>%
  recode_cape("BE3_thought_own_ever") %>%
  recode_cape("BE4_thought_vivid_ever") %>%
  recode_cape("BE5_thought_echo_ever") %>%
  recode_cape("BE6_control_force_ever") %>%
  recode_cape("BE7_double_place_ever") %>%
  recode_cape("PA1_heard_voices_ever") %>%
  recode_cape("PA2_heard_talking_ever") %>%
  recode_cape("PA3_seen_things_ever") %>%
  recode_cape("PI1_drop_hints_3m") %>%
  recode_distress("PI1_drop_hints_3m_distress") %>%
  recode_cape("PI2_seem_to_be_3m") %>%
  recode_distress("PI2_seem_to_be_3m_distress") %>%
  recode_cape("PI3_persecuted_3m") %>%
  recode_distress("PI3_persecuted_3m_distress") %>%
  recode_cape("PI4_conspiracy_3m") %>%
  recode_distress("PI4_conspiracy_3m_distress") %>%
  recode_cape("PI5_look_oddly_3m") %>%
  recode_distress("PI5_look_oddly_3m_distress") %>%
  recode_cape("BE1_devices_3m") %>%
  recode_distress("BE1_devices_3m_distress") %>%
  recode_cape("BE2_thought_read_3m") %>%
  recode_distress("BE2_thought_read_3m_distress") %>%
  recode_cape("BE3_thought_own_3m") %>%
  recode_distress("BE3_thought_own_3m_distress") %>%
  recode_cape("BE4_thought_vivid_3m") %>%
  recode_distress("BE4_thought_vivid_3m_distress") %>%
  recode_cape("BE5_thought_echo_3m") %>%
  recode_distress("BE5_thought_echo_3m_distress") %>%
  recode_cape("BE6_control_force_3m") %>%
  recode_distress("BE6_control_force_3m_distress") %>%
  recode_cape("BE7_double_place_3m") %>%
  recode_distress("BE7_double_place_3m_distress") %>%
  recode_cape("PA1_heard_voices_3m") %>%
  recode_distress("PA1_heard_voices_3m_distress") %>%
  recode_cape("PA2_heard_talking_3m") %>%
  recode_distress("PA2_heard_talking_3m_distress") %>%
  recode_cape("PA3_seen_things_3m") %>%
  recode_distress("PA3_seen_things_3m_distress")

#recode invalid number to NA
df_CAPE15_clean <- df_CAPE15_clean %>%
filter(!is.na(Participant.Private.ID))%>%
  mutate_all(~ str_replace_all(., "Invalid Number", NA_character_))


#convert the vasriables i just recoded to numeric and factorize the levels
df_CAPE15_clean <- df_CAPE15_clean %>%
  mutate_at(vars(PI1_drop_hints_ever_quantized:PA3_seen_things_3m_distress_quantized), ~ as.numeric(.)) %>%
  mutate_at(vars(PI1_drop_hints_ever:PA3_seen_things_3m_distress), ~ as.factor(.))

#create sumscores
#ever 
df_CAPE15_clean <- df_CAPE15_clean %>%
  mutate(
  total_CAPE_ever = rowSums(select(.,PI1_drop_hints_ever_quantized:PA3_seen_things_ever_quantized), na.rm = TRUE),
  n_answered_ever =rowSums(!is.na(select(., PI1_drop_hints_ever_quantized:PA3_seen_things_ever_quantized))))


#3m +distress
#n_answered_3m+distress


df_CAPE15_clean <- df_CAPE15_clean %>%
  mutate(
  total_CAPE_3m_withdistress = rowSums(select(.,PI1_drop_hints_3m_quantized:PA3_seen_things_3m_distress_quantized), na.rm = TRUE),
  n_answered_3m_withdistress =rowSums(!is.na(select(., PI1_drop_hints_3m_quantized:PA3_seen_things_3m_distress_quantized))))



#weightedsum_CAPE_ever and #weightedsum_CAPE_3m +distress
df_CAPE15_clean <- df_CAPE15_clean %>%
  mutate(
  weightedsum_CAPE_ever = total_CAPE_ever/n_answered_ever,
  weightedsum_CAPE_3mdistress =total_CAPE_3m_withdistress/n_answered_3m_withdistress)

df_CAPE15_clean %>%
  ggplot(aes(x = weightedsum_CAPE_ever)) +
  geom_histogram() 

df_CAPE15_clean %>%
  ggplot(aes(x = weightedsum_CAPE_ever)) +
  geom_histogram() +
  scale_x_log10()

df_CAPE15_clean %>%
  ggplot(aes(x = weightedsum_CAPE_3mdistress)) +
  geom_histogram()
  
df_CAPE15_clean %>%
  ggplot(aes(x = total_CAPE_3m_withdistress)) +
  geom_histogram()

df_CAPE15_clean %>%
  ggplot(aes(x = total_CAPE_ever)) +
  geom_histogram()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data-Exploration CAPE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
View(df_CAPE15_clean)

View(summary(df_CAPE15_clean))



# Exclude the column "Participant.Private.ID" from the iteration
columns_to_plot <- setdiff(names(df_CAPE15_clean), "Participant.Private.ID")

# Create a list to store the plots
plots <- list()

# Iterate over the columns in df_CAPE15_clean, excluding "Participant.Private.ID", and create a bar plot for each column
for (column in columns_to_plot) {
  # Ensure the column is a factor
  df_CAPE15_clean[[column]] <- factor(df_CAPE15_clean[[column]])
  
  # Create the bar plot
  p <- ggplot(df_CAPE15_clean, aes_string(x = column)) +
    geom_bar() +
    labs(x = column, y = "Count", title = paste("Count of", column)) +
    theme_minimal()
  
  # Add the plot to the list
  plots[[column]] <- p
}

# Combine all plots into an overall plot
overall_plot <- wrap_plots(plots)

# Print the overall plot
print(overall_plot)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~CANNABIS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#have you ever used cannabis (cannabis_use)

#age_of_first_use
#ever_regular

#how long period lasted 
#age_regular                                                  
#grams
#current regular 

#current_regular_use_how_many_grams
#how_long_lasts


#df_cannabis_clean
#df_CEQ_Part1_clean
#df_regularuse_clean
#df_currentregularuse_clean
#df_cannabisusestop_clean
#df_CEQ_clean
#df_cu_experiences
#df_otherdruguse


df_cannabis_clean <- df_cannabisuse_combined %>%
filter(!is.na(Participant.Private.ID))%>%
select(Participant.Private.ID,cannabis_use,cannabis_use.quantised)%>%
mutate_at(vars(cannabis_use,cannabis_use.quantised), ~ as.factor(.))

View((df_cannabis_clean))
View(summary(df_cannabis_clean))

df_CEQ_Part1_clean <- df_CEQ_Part1_combined%>%
filter(!is.na(Participant.Private.ID))%>%
select(Participant.Private.ID,age_first_use, regular_cannabis_use_past, regular_cannabis_use_past.quantised)%>%
mutate_at(vars(regular_cannabis_use_past, regular_cannabis_use_past.quantised), ~ as.factor(.))



View(df_CEQ_Part1_clean)
View(summary(df_CEQ_Part1_clean))



#Regular use: a3ow

df_regularuse_clean <- df_regularuse_combined %>%
filter(!is.na(Participant.Private.ID))%>%
select(Participant.Private.ID,regular_cannabis_use_past_period, age_cannabis_regular_use, grams_cannabis_single, current_use_regular, current_use_regular.quantised) %>%
mutate_at(vars(current_use_regular, current_use_regular.quantised), ~ as.factor(.))


View(df_regularuse_clean)
View(summary(df_regularuse_clean))


#current regular use 5j5a
df_currentregularuse_clean <-df_currentregularuse_combined %>%
filter(!is.na(Participant.Private.ID))%>%
select(Participant.Private.ID, current_grams_cannabis, current_cannabis_amount_last)


View(df_currentregularuse_clean)

#cannabis use stop: go9y
df_cannabisusestop_clean <-df_cannabisusestop_combined %>%
filter(!is.na(Participant.Private.ID))%>%
select(Participant.Private.ID,age_stop_cannabis)

View(df_cannabisusestop_clean)

#CEQ_End:q9xg 
 df_CEQ_clean <- df_CEQ_end_combined %>%
 filter(!is.na(Participant.Private.ID))%>%
 select(Participant.Private.ID, cannabis_affect_health, cannabis_affect_health.text, cannabis_affect_health.quantised,use_past_year, use_past_year.quantised,shared, shared.quantised,kind_use_pastyear.text,kind_use_pastyear.quantised, quantity_use_occasion, quantity_use_occasion.text, quantity_use_occasion.quantised,money_week, money_week.quantised, money_week_otherthanpounds, type_cannabis_used,type_cannabis_used.text, type_cannabis_used.quantised) %>%
mutate_at(vars(use_past_year, use_past_year.quantised,shared, shared.quantised,kind_use_pastyear.text,kind_use_pastyear.quantised, quantity_use_occasion, quantity_use_occasion.quantised,money_week, money_week.quantised,  type_cannabis_used, type_cannabis_used.quantised), ~ as.factor(.))


View(df_CEQ_clean)
View(summary(df_CEQ_clean))

#####################################################################
#                    Experiences from smoking cannabis              #                
#####################################################################


# I need to bring the values from the dotgrid capes in the right format, right now they are all over the place

#"positive_symptom_cannabis.1.1" ,"positive_symptom_cannabis.1.1.quantised", "positive_symptom_cannabis.1.2","positive_symptom_cannabis.1.2.quantised","positive_symptom_cannabis.1.3","positive_symptom_cannabis.1.3.quantised","positive_symptom_cannabis.1.4","positive_symptom_cannabis.1.4.quantised","positive_symptom_cannabis.1.5","positive_symptom_cannabis.1.5.quantised","positive_symptom_cannabis.1.6","positive_symptom_cannabis.1.6.quantised", "positive_symptom_cannabis.1.7","positive_symptom_cannabis.1.7.quantised","positive_symptom_cannabis.1.8" ,"positive_symptom_cannabis.1.8.quantised"

#How often have you had these experiences when you used cannabis? (1-Rarely or never, 2-From time to time, 3-Sometimes, 4-More often than not, 5-Almost always)
#Please rate whether it was a 6-good 7-Bad or 8-neutral experience 
#1.1 = cu_fearful
#1.2 = cu_crazy 
#1.3 = cu_nervy 
#1.4 = cu_suspicious 
#1.5 = cu_happy 
#1.6 = cu_ideaful 
#1.7 = cu_understand
#1.8 = cu_visions 

#*********************************positive&negative experiences**************************************

#1. no_motivation
#2. suspicious
#3 slow_thinking
#4 difficulty_focus
#4. not_clear_thinking 

df_cu_experiences <- df_CEQ_end_combined %>%
  filter(Participant.Private.ID != "Invalid Number") %>%
  mutate(
    cu_fearful = coalesce(positive_symptom_cannabis.1.1, positive_symptom_cannabis.1.2, positive_symptom_cannabis.1.3, positive_symptom_cannabis.1.4, positive_symptom_cannabis.1.5),
    cu_fearful_eval = coalesce(positive_symptom_cannabis.1.6, positive_symptom_cannabis.1.7, positive_symptom_cannabis.1.8),
    cu_crazy = coalesce(positive_symptom_cannabis.2.1, positive_symptom_cannabis.2.2, positive_symptom_cannabis.2.3, positive_symptom_cannabis.2.4, positive_symptom_cannabis.2.5),
    cu_crazy_eval = coalesce(positive_symptom_cannabis.2.6, positive_symptom_cannabis.2.7, positive_symptom_cannabis.2.8),
    cu_nervy = coalesce(positive_symptom_cannabis.3.1, positive_symptom_cannabis.3.2, positive_symptom_cannabis.3.3, positive_symptom_cannabis.3.4, positive_symptom_cannabis.3.5),
    cu_nervy_eval = coalesce(positive_symptom_cannabis.3.6, positive_symptom_cannabis.3.7, positive_symptom_cannabis.3.8),
    cu_suspicious = coalesce(positive_symptom_cannabis.4.1, positive_symptom_cannabis.4.2, positive_symptom_cannabis.4.3, positive_symptom_cannabis.4.4, positive_symptom_cannabis.4.5),
    cu_suspicious_eval = coalesce(positive_symptom_cannabis.4.6, positive_symptom_cannabis.4.7, positive_symptom_cannabis.4.8),
    cu_happy = coalesce(positive_symptom_cannabis.5.1, positive_symptom_cannabis.5.2, positive_symptom_cannabis.5.3, positive_symptom_cannabis.5.4, positive_symptom_cannabis.5.5),
    cu_happy_eval = coalesce(positive_symptom_cannabis.5.6, positive_symptom_cannabis.5.7, positive_symptom_cannabis.5.8),
    cu_ideaful = coalesce(positive_symptom_cannabis.6.1, positive_symptom_cannabis.6.2, positive_symptom_cannabis.6.3, positive_symptom_cannabis.6.4, positive_symptom_cannabis.6.5),
    cu_ideaful_eval = coalesce(positive_symptom_cannabis.6.6, positive_symptom_cannabis.6.7, positive_symptom_cannabis.6.8),
    cu_understand = coalesce(positive_symptom_cannabis.7.1, positive_symptom_cannabis.7.2, positive_symptom_cannabis.7.3, positive_symptom_cannabis.7.4, positive_symptom_cannabis.7.5),
    cu_understand_eval = coalesce(positive_symptom_cannabis.7.6, positive_symptom_cannabis.7.7, positive_symptom_cannabis.7.8),
    cu_visions = coalesce(positive_symptom_cannabis.8.1, positive_symptom_cannabis.8.2, positive_symptom_cannabis.8.3, positive_symptom_cannabis.8.4, positive_symptom_cannabis.8.5),
    cu_visions_eval = coalesce(positive_symptom_cannabis.8.6, positive_symptom_cannabis.8.7, positive_symptom_cannabis.8.8), 
    cu_no_motivation = coalesce(negative_symptom_cannabis.1.1, negative_symptom_cannabis.1.2, negative_symptom_cannabis.1.3, negative_symptom_cannabis.1.4, negative_symptom_cannabis.1.5),
    cu_no_motivation_eval = coalesce(negative_symptom_cannabis.1.6, negative_symptom_cannabis.1.7, negative_symptom_cannabis.1.8),
    cu_suspicious = coalesce(negative_symptom_cannabis.2.1, negative_symptom_cannabis.2.2, negative_symptom_cannabis.2.3, negative_symptom_cannabis.2.4, negative_symptom_cannabis.2.5),
    cu_suspicious_eval = coalesce(negative_symptom_cannabis.2.6, negative_symptom_cannabis.2.7, negative_symptom_cannabis.2.8),
    cu_slow_thinking = coalesce(negative_symptom_cannabis.3.1, negative_symptom_cannabis.3.2, negative_symptom_cannabis.3.3, negative_symptom_cannabis.3.4, negative_symptom_cannabis.3.5),
    cu_slow_thinking_eval = coalesce(negative_symptom_cannabis.3.6, negative_symptom_cannabis.3.7, negative_symptom_cannabis.3.8),
    cu_difficulty_focus = coalesce(negative_symptom_cannabis.4.1, negative_symptom_cannabis.4.2, negative_symptom_cannabis.4.3, negative_symptom_cannabis.4.4, negative_symptom_cannabis.4.5),
    cu_difficulty_focus_eval = coalesce(negative_symptom_cannabis.4.6, negative_symptom_cannabis.4.7, negative_symptom_cannabis.4.8),
    cu_not_clear_thinking = coalesce(negative_symptom_cannabis.5.1, negative_symptom_cannabis.5.2, negative_symptom_cannabis.5.3, negative_symptom_cannabis.5.4, negative_symptom_cannabis.5.5),
    cu_not_clear_thinking_eval = coalesce(negative_symptom_cannabis.5.6, negative_symptom_cannabis.5.7, negative_symptom_cannabis.5.8)
  )%>% select(Participant.Private.ID,cu_fearful, cu_fearful_eval, cu_crazy, cu_crazy_eval, cu_nervy, cu_nervy_eval, cu_suspicious, cu_suspicious_eval, cu_happy, cu_happy_eval, cu_ideaful, cu_ideaful_eval, cu_understand, cu_understand_eval, cu_visions, cu_visions_eval,cu_no_motivation, cu_no_motivation_eval, cu_slow_thinking, cu_slow_thinking_eval, cu_difficulty_focus, cu_difficulty_focus_eval, cu_not_clear_thinking, cu_not_clear_thinking_eval)

View(df_cu_experiences)
summary(df_cu_experiences)






############################################################################################
#                                        other drug use                                   #
############################################################################################


#1;Tobacco
#2: Alcohol 
#3: non-prescripbed medications
#4: cocaine
#5: crack
#6: opiates
#7: amphetamines 
#8: hallucinogenes 
#9:ketamine 
#10: legal Highs/bath salts


df_otherdruguse <- df_CEQ_end_combined %>%
  mutate(
    tobacco_use = coalesce(other_drug.1.1, other_drug.1.2, other_drug.1.3, other_drug.1.4),
    alcohol_use = coalesce(other_drug.2.1, other_drug.2.2, other_drug.2.3, other_drug.2.4),
    non_prescribed_medications_use = coalesce(other_drug.3.1, other_drug.3.2, other_drug.3.3, other_drug.3.4),
    cocaine_use = coalesce(other_drug.4.1, other_drug.4.2, other_drug.4.3, other_drug.4.4),
    crack_use = coalesce(other_drug.5.1, other_drug.5.2, other_drug.5.3, other_drug.5.4),
    opiates_use = coalesce(other_drug.6.1, other_drug.6.2, other_drug.6.3, other_drug.6.4),
    amphetamines_use = coalesce(other_drug.7.1, other_drug.7.2, other_drug.7.3, other_drug.7.4),
    hallucinogens_use = coalesce(other_drug.8.1, other_drug.8.2, other_drug.8.3, other_drug.8.4),
    ketamine_use = coalesce(other_drug.9.1, other_drug.9.2, other_drug.9.3, other_drug.9.4),
    legal_highs_use = coalesce(other_drug.10.1, other_drug.10.2, other_drug.10.3, other_drug.10.4)
  )  %>% select(Participant.Private.ID,tobacco_use, alcohol_use, non_prescribed_medications_use, cocaine_use, crack_use, opiates_use, amphetamines_use, hallucinogens_use, ketamine_use, legal_highs_use)

View(df_otherdruguse)



#########################################################################
#                 Combine Questionnaires                                #
#########################################################################

# Combine the data frames using the key Participant.Private.ID
df_all_Questionnaires_combined <- df_demographics_clean %>%
left_join(df_CAPE15_clean, by ="Participant.Private.ID") %>%
left_join(df_cannabis_clean, by= "Participant.Private.ID")%>%
left_join(df_CEQ_Part1_clean, by = "Participant.Private.ID") %>%
left_join(df_regularuse_clean, by = "Participant.Private.ID")%>%
left_join(df_currentregularuse_clean, by = "Participant.Private.ID") %>%
left_join(df_cannabisusestop_clean, by = "Participant.Private.ID") %>%
left_join(df_CEQ_clean, by = "Participant.Private.ID") %>%
left_join(df_cu_experiences, by = "Participant.Private.ID") %>%
left_join(df_CEQ_clean, by = "Participant.Private.ID")

# View the combined data frame
df_all_Questionnaires_combined%>%
mutate_all(~ str_replace_all(., "Invalid Number", NA_character_))

write.xlsx(df_all_Questionnaires_combined, "df_all_Questionnaires_combined.xlsx")
write.csv(df_all_Questionnaires_combined, "df_all_Questionnaires_combined.csv", row.names = FALSE)

View(df_demographics_clean)

##########################################################################
#                       Data Exploration                                 #
##########################################################################


plot1 <- df_CEQ_CAPE_combined %>%
  ggplot(aes(x = weightedsum_CAPE_ever, fill = cannabis_use)) +
  geom_histogram(position = "dodge", bins = 10) +
  scale_x_log10() +
  labs(title = "Weighted Sum CAPE Ever by Cannabis Use")

plot2 <- df_CEQ_CAPE_combined %>%
  ggplot(aes(x = weightedsum_CAPE_3mdistress, fill = cannabis_use)) +
  geom_histogram(position = "dodge", bins = 10) +
  scale_x_log10() +
  labs(title = "Weighted Sum CAPE 3m Distress by Cannabis Use")

plot3 <- df_CEQ_CAPE_combined %>%
  ggplot(aes(x = weightedsum_CAPE_ever, fill = regular_cannabis_use_past)) +
  geom_histogram(position = "dodge", bins = 10) +
  scale_x_log10() +
  labs(title = "Weighted Sum CAPE Ever by Regular Cannabis Use Past")

plot4 <- df_CEQ_CAPE_combined %>%
  ggplot(aes(x = weightedsum_CAPE_3mdistress, fill = regular_cannabis_use_past)) +
  geom_histogram(position = "dodge", bins = 10) +
  scale_x_log10() +
  labs(title = "Weighted Sum CAPE 3m Distress by Regular Cannabis Use Past")

plot5 <- df_CEQ_CAPE_combined %>%
  ggplot(aes(x = weightedsum_CAPE_ever, fill = use_past_year)) +
  geom_histogram(position = "dodge", bins = 10) +
  scale_x_log10() +
  labs(title = "Weighted Sum CAPE Ever by Use Past Year")

plot6 <- df_CEQ_CAPE_combined %>%
  ggplot(aes(x = weightedsum_CAPE_3mdistress, fill = use_past_year)) +
  geom_histogram(position = "dodge", bins = 10) +
  scale_x_log10() +
  labs(title = "Weighted Sum CAPE 3m Distress by Use Past Year")


combined_plot <- (plot1 | plot2) / (plot3 | plot4) / (plot5 | plot6)

# Print the combined plot
print(combined_plot)

# Create individual plots
p1 <- df_CEQ_CAPE_combined %>%
  ggplot(aes(x = total_CAPE_ever, fill = cannabis_use)) +
  geom_histogram(position = "dodge", bins = 10) +
  scale_x_log10() +
  labs(title = "Total CAPE Ever by Cannabis Use")

p2 <- df_CEQ_CAPE_combined %>%
  ggplot(aes(x = total_CAPE_3m_withdistress, fill = cannabis_use)) +
  geom_histogram(position = "dodge", bins = 10) +
  scale_x_log10() +
  labs(title = "Total CAPE 3m with Distress by Cannabis Use")

p3 <- df_CEQ_CAPE_combined %>%
  ggplot(aes(x = total_CAPE_ever, fill = regular_cannabis_use_past)) +
  geom_histogram(position = "dodge", bins = 10) +
  scale_x_log10() +
  labs(title = "Total CAPE Ever by Regular Cannabis Use Past")

p4 <- df_CEQ_CAPE_combined %>%
  ggplot(aes(x = total_CAPE_3m_withdistress, fill = regular_cannabis_use_past)) +
  geom_histogram(position = "dodge", bins = 10) +
  scale_x_log10() +
  labs(title = "Total CAPE 3m with Distress by Regular Cannabis Use Past")

p5 <- df_CEQ_CAPE_combined %>%
  ggplot(aes(x = total_CAPE_ever, fill = use_past_year)) +
  geom_histogram(position = "dodge", bins = 10) +
  scale_x_log10() +
  labs(title = "Total CAPE Ever by Use Past Year")

p6 <- df_CEQ_CAPE_combined %>%
  ggplot(aes(x = total_CAPE_3m_withdistress, fill = use_past_year)) +
  geom_histogram(position = "dodge", bins = 10) +
  scale_x_log10() +
  labs(title = "Total CAPE 3m with Distress by Use Past Year")

# Combine all plots into one big plot
combined <- (p1 | p2) / (p3 | p4) / (p5 | p6)

# Print the combined plot
print(combined)

################################################################################
#                               Model                                         #
###############################################################################


# Run the linear model
model <- lm(total_CAPE_ever ~ cannabis_use, data = df_CEQ_CAPE_combined)

# Summarize the model
print(summary(model))

df_CEQ_CAPE_combined <- df_CEQ_CAPE_combined %>%
filter(!is.na(use_past_year))


# Run the linear model
model_use_past_year <- lm(total_CAPE_ever ~ use_past_year, data = df_CEQ_CAPE_combined)

# Summarize the model
summary(model_use_past_year)




###############################################################################
        Spacetask
###############################################################################




#Space Task R1: vigd
Spacetask_R1_v31to40 <- read_combine_convert_versions31to40("_task-vigd.csv", "Space_Task_R1")
print(names(Spacetask_R1_v31to40))



Spacetask_R1_v16to30 <-read_combine_convert_versions16to30("_task-uyaf.csv", "Spacetask_R1")
print(names(Spacetask_R1_v16to30))

#Space Task R2: g9qm
Spacetask_R2_v31to40<-read_combine_convert_versions31to40("_task-g9qm.csv", "Spacetask_R2")
print(names(Spacetask_R2_v31to40))


#Space Task R2: g9qm
Spacetask_R2_v16to30<-read_combine_convert_versions16to30("_task-6sm9.csv", "Spacetask_R2")
print(names(Spacetask_R2_v16to30))



Spacetasklist <- list(Spacetask_R1_v16to30,Spacetask_R1_v31to40,Spacetask_R2_v16to30,Spacetask_R2_v31to40)


common_columns_spacetask <- find_common_variables(Spacetasklist)
column_names_spacetask <- lapply(Spacetasklist, names)
  
  # Find the common columns using Reduce and intersect
  common_columns <- Reduce(intersect, column_names_spacetask)
  
  combined_data <- Spacetasklist[[1]]
  
  for (i in 2:length(Spacetasklist)) {
    Spacetask_allcombined <- full_join(combined_data, Spacetasklist[[i]], by = common_columns_spacetask)
  }
  

print(names(Spacetask_allcombined))

View(Spacetask_allcombined)

write.xlsx(Spacetask_allcombined, "Spacetask_allcombined.xlsx")
write.csv(Spacetask_allcombined, "Spacetask_allcombined.csv", row.names = FALSE)

################################################################################
             Socialtask
################################################################################


#Social Task R1: fch3
Socialtask_R1_all<-read_combine_convert_allversions("_task-fch3.csv", "Socialtask_R1")
print(names(Socialtask_R1_all))

#Social Task R2: j2h4
Socialtask_R2_v22to40<-read_combine_convert_versions22to40("_task-j2h4.csv", "Social_Task_R2")
print(names(Socialtask_R2_v22to40))

#Social Task R2: j2h4
Socialtask_R2_v16to21<-read_combine_convert_versions16to21("_task-gtz3.csv", "Social_Task_R2")
print(names(Socialtask_R2_v16to21))

Socialtasklist<-list(Socialtask_R1_all,Socialtask_R2_v16to21,Socialtask_R2_v22to40)

find_common_variables(Socialtasklist)

common_columns_socialtask <- find_common_variables(Socialtasklist)
column_names_list <- lapply(Socialtasklist, names)
  
  # Find the common columns using Reduce and intersect
  common_columns <- Reduce(intersect, column_names_list)
  
  combined_data <- Socialtasklist[[1]]
  
  for (i in 2:length(Socialtasklist)) {
    Socialtask_allcombined <- full_join(combined_data, Socialtasklist[[i]], by = common_columns_socialtask)
  }
  
  
print(names(Socialtask_allcombined))

write.xlsx(df_all_Questionnaires_combined, "Socialtask_allcombined")
write.csv(df_all_Questionnaires_combined, "Socialtask_allcombined.csv", row.names = FALSE)