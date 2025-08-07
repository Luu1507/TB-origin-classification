rm(list=ls()) #clear environment



#=============================================import data
getwd() #- check working directory
setwd("C:/Users/27728/Desktop") #- set working directory
Xpert_data<-read.csv("GeneXpert_unique.csv", sep=",")#- import data

# Load necessary libraries
library(tokenizers)
library(tm)
library(SnowballC)
library(dplyr)
library(ggplot2)

#==============================================Understanding SPECIMEN TYPE

print(Xpert_data$SPECIMEN_TYPE)

# Count and list the unique SPECIMEN_TYPE entries
  # -------count:
  specimen_types_number <- length(unique(Xpert_data$SPECIMEN_TYPE))
  print(specimen_types_number)

  # -------list:
  specimen_types_list <- unique(Xpert_data$SPECIMEN_TYPE)
  print(specimen_types_list)

# Check for records where SPECIMEN_TYPE is 'NA'
missing_specimen_type <- Xpert_data %>% 
  filter(is.na(SPECIMEN_TYPE)) 

nrow(missing_specimen_type)
View(missing_specimen_type)

# Convert SPECIMEN_TYPE to character and handle 'NA' entries
Xpert_data$SPECIMEN_TYPE <- as.character(Xpert_data$SPECIMEN_TYPE)
Xpert_data$SPECIMEN_TYPE[is.na(Xpert_data$SPECIMEN_TYPE)] #- Replace 'NA' with empty string



#==============================================Text-preprocessing

# Creating preprocessing function to apply to SPECIMEN_TYPE
text_preprocess <- function(text) {
  # Convert all text to lower case
  text <- tolower(text)
  
  # Remove punctuation, numbers, and special characters (NOTE: retain "?" because this is a possible SPECIMEN TYPE entry)
  text <- gsub("([^a-zA-Z\\?])", " \\1 ", text)  # "?" is preserved
  
  # Remove extra spaces
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  
  # Tokenize text 
  tokens <- tokenize_words(text)
  tokens <- unlist(tokens)
  
  # This ensures that any "?" stays as "?"
  tokens <- ifelse(tokens == "?", "?", tokens)
  
  # Apply stemming to tokens
  stemmed_tokens <- wordStem(tokens, language = "en")
  
  # Remove stopwords 
  no_stop_words_tokens <- stemmed_tokens[!stemmed_tokens %in% stopwords("en")]
  
  # Put tokens back together into one string
  cleaned_text <- paste(no_stop_words_tokens, collapse = " ")
  
  return(cleaned_text)
}

# Apply preprocessing function to the SPECIMEN_TYPE column
Xpert_data$SPECIMEN_TYPE_preprocessed <- sapply(Xpert_data$SPECIMEN_TYPE, text_preprocess)

#View preprocessed specimen types
print(Xpert_data$SPECIMEN_TYPE_preprocessed)
unique_preprocessed_specimen_types <- unique(Xpert_data$SPECIMEN_TYPE_preprocessed) #- list unique preprocessed specimen types
print(unique_preprocessed_specimen_types) 



# Organize the preprocessed text into a Corpus
corpus <- Corpus(VectorSource(Xpert_data$SPECIMEN_TYPE_preprocessed))

# Create a Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(1, Inf))) # Include every word, including those with only one letter up to infinity

# Extraction of keywords from the dtm
keywords <- findFreqTerms(dtm, lowfreq = 1) # List all the terms in the dtm that show up at least once in the whole corpus
print("Extracted Keywords:")
print(keywords)

# Generate a frequency table
keyword_frequency <- colSums(as.matrix(dtm))
frequency_table <- data.frame(term = names(keyword_frequency), frequency = keyword_frequency) %>%
  arrange(desc(frequency))

# Clean up the frequency table
frequency_table$term <- gsub("\"|c\\(|\\)", "", frequency_table$term ) # Remove unwanted patterns of characters
frequency_table$term <- gsub(",$", "", frequency_table$term) # Remove trailing commas
frequency_table$term <- trimws(frequency_table$term) # Remove white spaces at the beginning and end of keyword

print(frequency_table)


# Check if no entries were removed during preprocessing
if (nrow(Xpert_data) == length(Xpert_data$SPECIMEN_TYPE_preprocessed)) {
  print("No records were removed during preprocessing")
} else {
  print("Warning: Some records might have been removed")
}



#==============================================Keyword Definition
# Define PTB and EPTB categories by assigining keywords (from the preprocessing results above) to the most appropriate category

ptb_keywords <- c("sputum", "washing", "wash", "day", "gastric", "bronchial", "alveolar", "lavag", "nasopharyng", "spu", "spt", "respiratori", "brush", "utum", 
                  "tract", "ta", "sp", "eta", "induced")

eptb_keywords <- c("pericardi",  "csf", "tissu", "fine", "needl", "biopsi", "deep", "pus", "superfici", 
                   "fna", "urin", "lymphnod", "neck", "caviti", "drain", "steril", "bone", 
                   "synovi", "joint", "node", "cervic", "wound", "periton", "dialysi", 
                   "midstream", "cyst", "inguin", "ascit", "axilla", "mass", 
                   "right", "axillari", "plain", "stool", "intraop", "anterior", "fnab", 
                   "lympnod", "submandibular", "tap", "skin", "absess", "supraclavicular", 
                   "pusw", "hip", "wall", "pleura", "chechk", "icd", "scrotal", "bx", "lad", "chest", 
                   "tissue t7/8 tuberculoma",  "gland", "leg", "synovium", "subclavicular", "cold", 
                   "lymhnod", "thigh", "psaa", "abdomin", "psoa", "pleural", "spine", "left", 
                   "semen", "cathet", "marrow", "mesenteric lymphnode", "lymphnd", "endometri", "pigtail", 
                   "breast", "suprascapula", "arm", "subnet", "lymphat", "lyphnod", "angl", 
                   "jaw", "fluid", "imprint", "submand", "asir", "hydrocoel", "pubic",  
                   "lumph", "shunt", "ventriculoperiton", "pericardi", "bak", "anal", "peri", 
                   "effus", "scalp", "post", "clavicular", "supra", "lowerback", "lower", "larg", 
                   "lump", "side", "salivari", "cervial", "submandibul", "lymphno", "groin", 
                   "serous", "albow", "spinal", "intra", "supravicular", "lynhnod", "lymphod", 
                   "ventricular", "superficial", "abscess", "ascitic", "peritoneal", "needle", 
                   "tbna", "cervical", "node", "lymph", "nos", "ln", "apir",
                   "urine", "biopsy", "lymphnode", "lymphnode tissue", "lympnode", "lymphnode aspirate", "sinovial" , "endometrial", "lymhnode", "lyphnode")

ambiguous_keywords <- c("aspir", "smear", "swab", "other", "tube", "tracheal", "lung") #keywords that appear in both ptb and eptb specimens- these will later be used to define the rule-based logic

unknown_keywords <- c("unknown" , "endotrach", "slide", "myctc", "cultur", "sampl",
                      "bacteri", "not stated", "s", "r", "?", "c", "cf", "ssfl", "ss", "ssflf", "blc", "ble", "blp","bl", "bc", "ppt") #keywords from specimens that cannot be explicitly categorised as ptb or eptb due to lack of context




#==============================================Rule-based Matching and Classification

# Creating the classification function to handle context and ambiguity

#-----Rule-based classification:
classify_specimen <- function(specimen) {
  # Context-specific rules for certain keywords
  specific_cases <- list(
    "lung" = function(specimen) {
      if (grepl("abscess", specimen, ignore.case = TRUE)) {
        return("PTB")
      } else if (grepl("fluid", specimen, ignore.case = TRUE)) {
        return("EPTB")
      } 
    },  
    "aspir" = function(specimen) {
      if (grepl("bronchial|gastric|nasopharyngeal|respiratory|tracheal", specimen, ignore.case = TRUE)) {
        return("PTB")
      } else {
        return("EPTB")
      }
    },
    "smear" = function(specimen) {
      if (grepl("sputum", specimen, ignore.case = TRUE)) {
        return("PTB")
      } else {
        return("EPTB")
      }
    },
    "swab" = function(specimen) {
      if (grepl("nasopharyngeal", specimen, ignore.case = TRUE)) {
        return("PTB")
      } else {
        return("EPTB")
      }
    },
    "other" = function(specimen) {
      if (grepl("aspiration|nos", specimen, ignore.case = TRUE)) {
        return("EPTB")
      } else {
        return("Unknown")
      }
    },
    "tube" = function(specimen) {
      if (grepl("fluid|plain", specimen, ignore.case = TRUE)) {
        return("EPTB")
      } else if (grepl("endotracheal", specimen, ignore.case = TRUE)) {
        return("Unknown")
      }
    },
    "tracheal" = function(specimen) {
      if (grepl("aspirate", specimen, ignore.case = TRUE)) {
        return("PTB")
      } else if (grepl("endotracheal", specimen, ignore.case = TRUE)) {
        return("Unknown")
      } 
    },
    "ssfl" = function(specimen) {
      if (grepl("pusw", specimen, ignore.case = TRUE)) {
        return("EPTB")
      } else {
        return("Unknown")
      }
    },
    "tissue" = function(specimen) {
      if (grepl("tuberculoma", specimen, ignore.case = TRUE)) {
        return("EPTB")
      } else {
        return("EPTB")  
      }
    }
  )
  if (grepl("tuberculoma.*tissue|tissue.*tuberculoma", specimen, ignore.case = TRUE)) {
    return("EPTB")
  }
  
  # checking to see if the specimen text fits any patterns or rules defined above before using the generic classification criteria from the keyword definition
  for (term in names(specific_cases)) {
    if (grepl(term, specimen, ignore.case = TRUE)) { #if specimen contains a pattern defined in the rules above, it will return the associated rule-based classification
      return(specific_cases[[term]](specimen))
    }
  } #this ensures priority classifications for special or ambiguous cases before applying the general logic of keyword matching
  
  # Fallback to general keyword checks
  ptb_flag <- any(grepl(paste0("\\b", ptb_keywords, "\\b", collapse = "|"), specimen, ignore.case = TRUE))
  eptb_flag <- any(grepl(paste0("\\b", eptb_keywords, "\\b", collapse = "|"), specimen, ignore.case = TRUE))
  ambiguous_flag <- any(grepl(paste0("\\b", ambiguous_keywords, "\\b", collapse = "|"), specimen, ignore.case = TRUE))
  unknown_flag <- any(grepl(paste0("\\b", unknown_keywords, "\\b", collapse = "|"), specimen, ignore.case = TRUE))
  
  # Default classifications with a fallback
  if (ptb_flag && !eptb_flag) {
    return("PTB")
  } else if (!ptb_flag && eptb_flag) {
    return("EPTB")
  } else if (!ptb_flag && !eptb_flag) {
    return("Unknown")
  } else if (ambiguous_flag) {
    return("Ambiguous")
  } else {
    cat("No match for specimen:", specimen, "\n")  # Identify unmatched specimens
    return("Unclassified")  # To ensure no NULL values exist
  }
}

# Apply classification function to the dataset
Xpert_data$class <- sapply(Xpert_data$SPECIMEN_TYPE, classify_specimen)


# Display the classification results
table(Xpert_data$class)



#____________________________________________________________________________________________________________________________________________________________________________________________________________

#_____________________________________________________________________________Validation (Xpert)____________________________________________

#==============================================Validating Classification against manual coding (Classification vs. PULMONARY_FLAG)

#Load necessary library
library(caret)

# Analyze the distribution of Pulmonary Flag (PF)
table(Xpert_data$PULMONARY_FLAG)

# Filter to only include necessary categories for validation
validation_xpert <- Xpert_data %>% 
  filter(PULMONARY_FLAG %in% c(1, 2)) %>% # include PF=1 (PTB), or PF= 2 (EPTB) only
  filter(class %in% c("PTB", "EPTB")) # include know classifications only ("Unknown" classification is excluded)

# Recode PF values to either PTB and EPTB
validation_xpert <- validation_xpert %>%
  mutate(PF_recode = case_when(
    PULMONARY_FLAG == 1 ~ "PTB",
    PULMONARY_FLAG == 2 ~ "EPTB",
  ))

# Factorise both catagorical variables 
validation_xpert$class <- factor(validation_xpert$class, levels = c("PTB", "EPTB"))
validation_xpert$PF_recode <- factor(validation_xpert$PF_recode, levels = c("PTB", "EPTB"))

# Ensuring that categories are the same for classification and PF
levels(validation_xpert$PF_recode)
levels(validation_xpert$class)

# Generate confusion matrix between classification and PF
PF_confusion_matrix_xpert <- confusionMatrix(validation_xpert$class, validation_xpert$PF_recode)
print(PF_confusion_matrix_xpert)

#-----------------------------------Agreement Analysis
# Extract Cohen's Kappa
PF_kappa_xpert <- PF_confusion_matrix_xpert$overall["Kappa"]
print(PF_kappa_xpert)


#==============================================Validating Classification using Lookup Table (Classification vs. EPTB_CLEAN)

#Load the Lookup table
library(readxl)
lookup <- read_excel("Lookup.xlsx", sheet ="lkup_SpecTypesF") 
View(lookup)

# clean up the Specimen Type variables in both our dataset and the lookup table to make them comparable
lookup <- lookup %>%
  mutate(SpecimenType = tolower(trimws(SpecimenType)))

lookup_validation_xpert <- Xpert_data %>%
  mutate(SPECIMEN_TYPE = tolower(trimws(SPECIMEN_TYPE)))


# We use the combinations of specimen type and its respective classification in our data and compare it to the same combination in the lookup table
lookup_validation_xpert <- lookup_validation_xpert %>%
  mutate(
    validation = ifelse( # creating the "validation" variable to capture if there is a match or no match between the two comparisons
      paste(SPECIMEN_TYPE, class) %in% paste(lookup$SpecimenType, lookup$EPTB_CLEAN),
      "Match",
      "No Match"
    )
  )

# Count the number of matches and mismatches
table(lookup_validation_xpert$validation)


# Ensure that we retain only unique specimen types and its classification (stored in EPTB_CLEAN) in the lookup table
unique_lookup <- lookup %>% select(SpecimenType, EPTB_CLEAN) %>% distinct()
View(unique_lookup)

#Join the dataset to the lookup table with the unique specimen types using the Specimen type variable
lookup_validation_xpert <- lookup_validation_xpert %>%
  left_join(unique_lookup, by = c("SPECIMEN_TYPE" = "SpecimenType")) 
View(lookup_validation_xpert)

#Retain PTB and EPTB specimens (Remove cases classified as "Unknown")
lookup_validation_xpert <- lookup_validation_xpert %>%
  filter(class %in% c("PTB", "EPTB") & EPTB_CLEAN %in% c("PTB", "EPTB"))

# Factorise both catagorical variables 
lookup_validation_xpert$class<-factor(lookup_validation_xpert$class, levels=c("PTB","EPTB"))
lookup_validation_xpert$EPTB_CLEAN<-factor(lookup_validation_xpert$EPTB_CLEAN, levels=c("PTB","EPTB"))

# Ensuring that categories are the same for classification and PF
levels(lookup_validation_xpert$class)
levels(lookup_validation_xpert$EPTB_CLEAN)

#Creating confusion matrix between classification and EPTB_CLEAN from the lookup table
lookup_confusion_matrix_xpert <- confusionMatrix(lookup_validation_xpert$class, lookup_validation_xpert$EPTB_CLEAN)
print(lookup_confusion_matrix_xpert)

lookup_confusion_matrix_xpert_table <- lookup_confusion_matrix_xpert$table
lookup_confusion_matrix_percent <- prop.table(lookup_confusion_matrix_xpert_table) * 100
round(lookup_confusion_matrix_percent, 3) 

#-----------------------------------Agreement Analysis
# Extract Cohen's Kappa
lookup_kappa_xpert <- lookup_confusion_matrix_xpert$overall["Kappa"]
print(lookup_kappa_xpert)



#==============================================Validating Classification using Specimen Group (Classification vs. SPECIMEN_GROUP)

# Define the mapping of specimen groups to PTB, EPTB, and Unknown categories
ptb_group <- c("RESP OR SPUTUM", "SPUTUM", "INDUCED SPUTUM", "GASTRIC ASPIRATE", 
               "BRONCHIAL WASHING", "NASOPHARYNGEAL ASPIRATE", "TRACHEAL ASPIRATE", 
               "SPU", "SPT", "ETA", "SP[UTUM", "SPURUM", "BRONCHIAL ALVEOLAR LAVAGE", "GASTRIC ASPIRATE (WASHING) (>DAY 1)",
               "FNA, RESPIRATORY TRACT", "RSP", "TA", "GASTRIC WASHING", "INDUCED", "GASTRIC ASPIRATE (WAS ING) ( DA  1)", "SP", "NON-GYNAE, GIT")

eptb_group <- c("PLEURAL FLUID", "CSF", "URINE", "TISSUE", "FNA, LYMPH NODES", 
                "FLUID / ASPIRATE", "PUS", "TISSUE:SPINE", "ASCITIC FLUID", 
                "FNA, OTHER", "FNA, HEAD AND NECK", "ABSCESS", "PERICARDIAL FLUID", 
                "FNA CERVICAL", "PUS SWAB", "BONE MARROW ASPIRATE", "PLEURAL TAP", 
                "PERITONEAL FLUID", "SYNOVIAL TISSUE", "ENDOMETRIAL BIOPSY", 
                "SUBMANDIBLE ABSCESS", "GIT", "INTRA-ABDOMINAL FLUID", "PSOAS ABSCESS", 
                "ASCITIC TAP", "TISSUE (L) HIP", "ABSCESS (SUPERFICIAL) ASPIRATE", "FINE NEEDLE ASPIRATE BIOPSY",
                "SWAB (SUPERFICIAL)", "FNA", "PUSW", "SEMEN", "FLUID", "ABSCESS (SUPERFICIAL) SWAB", "FNA, L MPH NODES", "LYMPH NODES",
                "HISTOLOGY", "GYNAE, FEMALE GENITAL TRACT", "LYMPH NODE", "LYMPHNODE ASPIRATE", "MESENTERIC LYMPHNODE", "LYMPHNODE",
                "AXILLA ABSCESS FNA PUS", "LYMPHNODE ASPIRATE FROM THE NECK", "FNA INGUINAL", "NECK MASS ASPIRATE", "LYMPHNODE PUS", "FNA, GIT",
                "TISSUE ANTERIOR WALL", "(R)CERVICAL LYMPHNODE", "NECK PUS","PUS ASPIRATE", "PLEURAL ASPIRATE", "(L) CERVICAL BX", " PUS (ABSCESS",
                "(R) AXILLA PUS", "LYMPNODE ASPIRATE", "FNA (L) MASS ON BAK", "COLD ABSCESS ASPIRATE", "(L) NECK ASPIRATE", "PERI-ANAL BIOPSY",
                "TISSUE(R)NECK", "PIGTAIL FLUID", "ASPIRATE (R) BREAST", "(R)CERVICAL ASPIRATE", "PLEURAL EFFUSION", "ASPIRATE SCALP", "ABSCESS NECK",
                "PUS CHEST WALL", "SUPRASCAPULA ABSCESS ASPIRATE", "LYMPHNODE (L)SUPR CLAVICULAR", "TISSUE T7/8 TUBERCULOMA", "FLUID ASPIRATE",
                "FNA ABSCESS", "FNA LYMPHNODE(L)SUBMANDIBULAR", "ABSCESS (R)CERVICAL", "FNA SUBNENTAL LN", "LOWER NECK PUS ASPIRATE", "PUS NECK",
                "(L)AXILLA ASPIRATE", "LYMPHNODE ABSCESS", "NECK ASPIRATE", "PLEURAL FLUID- (R) PLEURA", "LYMPHNODE ASPIRATE (R) NECK", 
                "RIGHT CERVICAL ABSCESS", "FNA AXILLA", "PUS(PSAAS ABSCESS)", "PLEURA TAP", "PUS CERVICAL LYMPHNODE", "SPINE ABSCESS", "(R) NECK CERVICAL NODE",
                "(L) LEG SYNOVIUM TISSUE", "ABDOMINAL ASCITIC FLUID", "PUS(L)PSAAS", "ABSCESS PUS", "PUS ALBOW", "FNA LYMPHNODE NECK MASS", "CERVICAL FNA",
                "SPINAL ABSCESS", "(R) PLEUARAL ASPIRATE", "(R) NECK FNA", "LYMPH NODE ASPIRATE", "LYPHNODE BIOPSY", "RIGHT PSOAS", "NON-GYNAE, LYMPH NODES", "PUS (ABSCESS")

unknown_group <- c("UNKNOWN", "UN NOWN", "?", "SAMPLE", "FORENSIC CHEMISTRY", 
                   "SMEAR", "NICD: SWAB", "BACTERIAL CULTURE", "C,CF", "OTHER", 
                   "CATHETER TIP", "MYCTC", "S", "STERILE SITES", "PATHOLOGY", "SSFL", "BC,BL,BLC,BLE", "C,CF,SSFL", "SSFL,SSFLF",
                   "BLC,BLP", "BLC,BLE,BLP", "NON-GYNAE, OTHER", "BLP", "BLC,C,CF", "BLC,SSFL", "NON-GYNAE", "NON-GYNAE, BODY CAVITIES", "NON-GYNAE, OTHER")


# Classify the specimens into PTB, EPTB, and Unknown
group_validation_xpert <- Xpert_data %>%
  mutate(
    group_class = case_when(
      SPECIMEN_GROUP %in% ptb_group ~ "PTB",
      SPECIMEN_GROUP %in% eptb_group ~ "EPTB",
      SPECIMEN_GROUP %in% unknown_group ~ "Unknown",
      TRUE ~ "Unclassified"
    )
  )
# View counts for each category
table(group_validation_xpert$group_class)


#Retain PTB and EPTB specimens (Remove cases classified as "Unknown")
group_validation_xpert <- group_validation_xpert %>% 
  filter(class %in% c("PTB", "EPTB")) %>%
  filter(group_class %in% c("PTB", "EPTB"))

# Ensure both columns are factors with the same levels
group_validation_xpert$class <-factor(group_validation_xpert$class, levels = c("PTB", "EPTB"))
group_validation_xpert$group_class <-factor(group_validation_xpert$group_class, levels = c("PTB", "EPTB"))

# Check levels of the relevant factors
levels(group_validation_xpert$group_class)
levels(group_validation_xpert$class)

# Generate confusion matrix
group_confusion_matrix <- confusionMatrix(group_validation_xpert$class, group_validation_xpert$group_class)
print(group_confusion_matrix)

#-----------------------------------Agreement Analysis
# Extract Cohen's Kappa
group_kappa_xpert <- group_confusion_matrix$overall["Kappa"]
print(group_kappa_xpert)


#------------------------------------
