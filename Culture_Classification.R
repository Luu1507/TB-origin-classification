rm(list=ls()) #clear environment



#=============================================import data
getwd() #- check working directory
setwd("C:/Users/27728/Desktop") #- set working directory
Culture_data<-read.csv("Culture_unique.csv", sep=",")#- import data



#==============================================Understanding SPECIMEN TYPE

# Load necessary libraries
library(tokenizers)
library(tm)
library(SnowballC)
library(dplyr)

print(Culture_data$SPECIMEN_TYPE_1)

# Count and list the unique SPECIMEN_TYPE entries
# -------count:
specimen_types_number <- length(unique(Culture_data$SPECIMEN_TYPE_1))
print(specimen_types_number)

# -------list:
specimen_types_list <- unique(Culture_data$SPECIMEN_TYPE_1)
print(specimen_types_list)

# Check for records where SPECIMEN_TYPE is 'NA'
missing_specimen_type <- Culture_data %>% 
  filter(is.na(SPECIMEN_TYPE_1)) 

nrow(missing_specimen_type)
View(missing_specimen_type)

# Convert SPECIMEN_TYPE to character and handle 'NA' entries
Culture_data$SPECIMEN_TYPE_1 <- as.character(Culture_data$SPECIMEN_TYPE_1)
Culture_data$SPECIMEN_TYPE_1[is.na(Culture_data$SPECIMEN_TYPE_1)] #- Replace 'NA' with empty string


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
Culture_data$SPECIMEN_TYPE_preprocessed <- sapply(Culture_data$SPECIMEN_TYPE_1, text_preprocess)

#View preprocessed specimen types
print(Culture_data$SPECIMEN_TYPE_preprocessed)
unique_preprocessed_specimen_types <- unique(Culture_data$SPECIMEN_TYPE_preprocessed) #- list unique preprocessed specimen types
print(unique_preprocessed_specimen_types) 



# Organize the preprocessed text into a Corpus
corpus <- Corpus(VectorSource(Culture_data$SPECIMEN_TYPE_preprocessed))

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
if (nrow(Culture_data) == length(Culture_data$SPECIMEN_TYPE_preprocessed)) {
  print("No records were removed during preprocessing")
} else {
  print("Warning: Some records might have been removed")
}



#==============================================Keyword Definition
# Define PTB and EPTB categories by assigining keywords (from the preprocessing results above) to the most appropriate category

ptb_keywords_1 <- c("sputum", "washing", "wash", "day", "gastric", "bronchial", "alveolar", "lavag", "nasopharyng", "spu", "spt", "respiratori", "brush", "utum", 
                    "tract", "ta", "sp", 
                    "sutum", "sputumq", "sputim", "sputumpcr", "nga", "sptuum", "spuutm", "soutum", "bronchiolar", "sputm", "sputu", "sputam", "suptum", "sputumn",
                    "aputum", "sputupm", "resp", 'sputum2', "1sputum", "eta", "induced", "endobronchial")
eptb_keywords_1 <- c("csf", "tissu", "fine", "needl", "biopsi", "deep", "pus", "superfici", 
                     "fna", "urin", "lymphnod", "neck", "caviti", "drain", "steril", "bone", 
                     "synovi", "joint", "node", "cervic", "wound", "periton", "dialysi", 
                     "midstream", "cyst", "inguin", "ascit", "axilla",  
                     "right", "axillari", "plain", "stool", "intraop", "anterior", "fnab", 
                     "lympnod", "submandibular", "tap", "skin", "absess", "supraclavicular", 
                     "pusw", "hip", "wall", "pleura", "chechk", "icd", "scrotal", "bx", "lad", "chest", 
                     "tissue t7/8 tuberculoma",  "gland", "leg", "synovium", "subclavicular", "cold", 
                     "lymhnod", "thigh", "psaa", "abdomin", "psoa", "pleural", "spine", "left", 
                     "semen", "cathet", "marrow", "mesenteric lymphnode", "lymphnd", "endometri", "pigtail", 
                     "breast", "suprascapula", "arm", "subnet", "lymphat", "lyphnod", "angl", 
                     "jaw", "fluid", "imprint", "submand", "asir", "hydrocoel", "pubic",  
                     "lumph", "shunt", "ventriculoperiton", "pericardial", "bak", "anal", "peri", 
                     "effus", "scalp", "post", "clavicular", "supra", "lowerback", "lower", "larg", 
                     "lump", "side", "salivari", "cervial", "submandibul", "lymphno", "groin", 
                     "serous", "albow", "spinal", "intra", "supravicular", "lynhnod", "lymphod", 
                     "ventricular", "superficial", "abscess", "ascitic", "peritoneal", "needle", 
                     "tbna", "transbronchial", "cervical", "node", "lymph", "nos", "ln", "apir",
                     "urine", "biopsy", "lymphnode", "lymphnode tissue", "lympnode", "lymphnode aspirate", "sinovial" , "endometrial", "lymhnode", "lyphnode",
                     "knee", "ear", "collect", "blister", "perian ", "perinot", "urc", "puss", "antral", "sinus", "fld", "trephin", "medium", "transport", "viral", "asit", "wrist", "empyema", "exud",
                     "time", "ulcer", "fistula", "colon", "subnent", "lumbar", "ingan", "pleuaral", "upperarm", "aspit", "lyphm", "abdo", "ankl", "intraabodni", "armpit", "upper", "cerciv", "masss",
                     "foot", "irrig", "pericadial", "ednometrus", "space", "pleual", "site", "cystic", "ovarian", "salin", "ascites", "fl", "lymhpnode", "fliud", "asp")

ambiguous_keywords_1 <- c("aspir", "smear", "swab", "blood", "other", "tube", "tracheal")

unknown_keywords_1 <- c("unknown" , "endotrach", "blc", "ble", "blp", "slide", "myctc", "ppt", "cultur", "sampl",
                        "bacteri", "not stated", "s", "r", "?", "c", "cf", "ssfl", "ss", "bc", "bl", "ssflf", 
                        "culss", "specimen", "/", "not indicated", "pu", "p", "citrat", "blue top", "y", "cb", "opther", "cheek")


#==============================================Rule-based Matching and Classification

# Creating the classification function to handle context and ambiguity

#-----Rule-based classification:
classify_specimen_1 <- function(specimen_1) {
  # Context-specific rules for terms
  specific_cases_1 <- list(
    "lung" = function(specimen_1) {
      if (grepl("abscess", specimen_1, ignore.case = TRUE)) {
        return("PTB")
      } else if (grepl("fluid|pus", specimen_1, ignore.case = TRUE)) {
        return("EPTB")
      } 
    },  
    "mass" = function(specimen_1) {
      if (grepl("endobronchial", specimen_1, ignore.case = TRUE)) {
        return("PTB")
      } else {
        return("EPTB")
      } 
    },  
    "aspir" = function(specimen_1) {
      if (grepl("bronchial|gastric|nasopharyngeal|respiratory|tracheal", specimen_1, ignore.case = TRUE)) {
        return("PTB")
      } else {
        return("EPTB")
      }
    },
    "smear" = function(specimen_1) {
      if (grepl("sputum", specimen_1, ignore.case = TRUE)) {
        return("PTB")
      } else {
        return("EPTB")
      }
    },
    "swab" = function(specimen_1) {
      if (grepl("nasopharyngeal", specimen_1, ignore.case = TRUE)) {
        return("PTB")
      } else {
        return("EPTB")
      }
    },
    "other" = function(specimen_1) {
      if (grepl("aspiration|nos", specimen_1, ignore.case = TRUE)) {
        return("EPTB")
      } else {
        return("Unknown")
      }
    },
    "tube" = function(specimen_1) {
      if (grepl("fluid|plain", specimen_1, ignore.case = TRUE)) {
        return("EPTB")
      } else if (grepl("endotracheal", specimen_1, ignore.case = TRUE)) {
        return("Unknown")
      }
    },
    "tracheal" = function(specimen_1) {
      if (grepl("aspirate", specimen_1, ignore.case = TRUE)) {
        return("PTB")
      } else if (grepl("endotracheal", specimen_1, ignore.case = TRUE)) {
        return("Unknown")
      } 
    },
    "ssfl" = function(specimen_1) {
      if (grepl("pusw", specimen_1, ignore.case = TRUE)) {
        return("EPTB")
      } else {
        return("Unknown")
      }
    },
    "asp" = function(specimen_1) {
      if (grepl("gastric", specimen_1, ignore.case = TRUE)) {
        return("PTB")
      } else {
        return("EPTB")
      }
    },
    "tissue" = function(specimen_1) {
      if (grepl("tuberculoma", specimen_1, ignore.case = TRUE)) {
        return("EPTB")
      } else {
        return("EPTB")  
      }
    }
  )
  if (grepl("tuberculoma.*tissue|tissue.*tuberculoma", specimen_1, ignore.case = TRUE)) {
    return("EPTB")
  }
  if (grepl("blood.*citrate|citrate.*blood", specimen_1, ignore.case = TRUE)) {
    return("Unknown")
  }
  
  # checking to see if the specimen text fits any patterns or rules defined above before using the generic classification criteria from the keyword definition
  for (term in names(specific_cases_1)) {
    if (grepl(term, specimen_1, ignore.case = TRUE)) { #if specimen contains a pattern defined in the rules above, it will return the associated rule-based classification
      return(specific_cases_1[[term]](specimen_1))
    }
  } #this ensures priority classifications for special or ambiguous cases before applying the general logic of keyword matching
  
  # Fallback to general keyword checks
  ptb_flag_1 <- any(grepl(paste0("\\b", ptb_keywords_1, "\\b", collapse = "|"), specimen_1, ignore.case = TRUE))
  eptb_flag_1 <- any(grepl(paste0("\\b", eptb_keywords_1, "\\b", collapse = "|"), specimen_1, ignore.case = TRUE))
  ambiguous_flag_1 <- any(grepl(paste0("\\b", ambiguous_keywords_1, "\\b", collapse = "|"), specimen_1, ignore.case = TRUE))
  unknown_flag_1 <- any(grepl(paste0("\\b", unknown_keywords_1, "\\b", collapse = "|"), specimen_1, ignore.case = TRUE))
  
  # Default classifications with a fallback
  if (ptb_flag_1 && !eptb_flag_1) {
    return("PTB")
  } else if (!ptb_flag_1 && eptb_flag_1) {
    return("EPTB")
  } else if (!ptb_flag_1 && !eptb_flag_1) {
    return("Unknown")
  } else if (ambiguous_flag_1) {
    return("Ambiguous")
  } else {
    cat("No match for specimen:", specimen_1, "\n")  # Log unmatched specimens
    return("Unclassified")  # Ensure no NULL values
  }
}

# Apply classification function to the dataset
Culture_data$class <- sapply(Culture_data$SPECIMEN_TYPE_1, classify_specimen_1)


# Display the classification results
table(Culture_data$class)



#____________________________________________________________________________________________________________________________________________________________________________________________________________

#_____________________________________________________________________________Validation (Culture)____________________________________________

#==============================================Validating Classification against manual coding (Classification vs. PULMONARY_FLAG)

#Load necessary library
library(caret)

# Analyze the distribution of Pulmonary Flag (PF)
table(Culture_data$PULMONARY_FLAG)

# Filter to only include necessary categories for validation
validation_culture <- Culture_data %>% 
  filter(PULMONARY_FLAG %in% c(1, 2)) %>% # include PF=1 (PTB), or PF= 2 (EPTB) only
  filter(class %in% c("PTB", "EPTB")) # include know classifications only ("Unknown" classification is excluded)

# Recode PF values to either PTB and EPTB
validation_culture <- validation_culture %>%
  mutate(PF_recode = case_when(
    PULMONARY_FLAG == 1 ~ "PTB",
    PULMONARY_FLAG == 2 ~ "EPTB",
  ))

# Factorise both catagorical variables 
validation_culture$class <- factor(validation_culture$class, levels = c("PTB", "EPTB"))
validation_culture$PF_recode <- factor(validation_culture$PF_recode, levels = c("PTB", "EPTB"))

# Ensuring that categories are the same for classification and PF
levels(validation_culture$PF_recode)
levels(validation_culture$class)

# Generate confusion matrix between classification and PF
PF_confusion_matrix_culture <- confusionMatrix(validation_culture$class, validation_culture$PF_recode)
print(PF_confusion_matrix_culture)

#-----------------------------------Agreement Analysis
# Extract Cohen's Kappa
PF_kappa_culture <- PF_confusion_matrix_culture$overall["Kappa"]
print(PF_kappa_culture)


#==============================================Validating Classification using Lookup Table (Classification vs. EPTB_CLEAN)

#Load the Lookup table
library(readxl)
lookup <- read_excel("Lookup.xlsx", sheet ="lkup_SpecTypesF") 
View(lookup)

# clean up the Specimen Type variables in both our dataset and the lookup table to make them comparable
lookup <- lookup %>%
  mutate(SpecimenType = tolower(trimws(SpecimenType)))

lookup_validation_culture <- Culture_data %>%
  mutate(SPECIMEN_TYPE_1 = tolower(trimws(SPECIMEN_TYPE_1)))


# Ensure that we retain only unique specimen types and its classification (stored in EPTB_CLEAN) in the lookup table
unique_lookup <- lookup %>% select(SpecimenType, EPTB_CLEAN) %>% distinct()
View(unique_lookup)

#Join the dataset to the lookup table with the unique specimen types using the Specimen type variable
lookup_validation_culture <- lookup_validation_culture %>%
  left_join(unique_lookup, by = c("SPECIMEN_TYPE_1" = "SpecimenType")) 
View(lookup_validation_culture)

#Retain PTB and EPTB specimens (Remove cases classified as "Unknown")
lookup_validation_culture <- lookup_validation_culture %>%
  filter(class %in% c("PTB", "EPTB") & EPTB_CLEAN %in% c("PTB", "EPTB"))

# Factorise both catagorical variables 
lookup_validation_culture$class<-factor(lookup_validation_culture$class, levels=c("PTB","EPTB"))
lookup_validation_culture$EPTB_CLEAN<-factor(lookup_validation_culture$EPTB_CLEAN, levels=c("PTB","EPTB"))

# Ensuring that categories are the same for classification and PF
levels(lookup_validation_culture$class)
levels(lookup_validation_culture$EPTB_CLEAN)

#Creating confusion matrix between classification and EPTB_CLEAN from the lookup table
lookup_confusion_matrix_culture <- confusionMatrix(lookup_validation_culture$class, lookup_validation_culture$EPTB_CLEAN)
print(lookup_confusion_matrix_culture)

lookup_confusion_matrix_culture_table <- lookup_confusion_matrix_culture$table
lookup_confusion_matrix_percent <- prop.table(lookup_confusion_matrix_culture_table) * 100
round(lookup_confusion_matrix_percent, 3) 

#-----------------------------------Agreement Analysis
# Extract Cohen's Kappa
lookup_kappa_culture <- lookup_confusion_matrix_culture$overall["Kappa"]
print(lookup_kappa_culture)



#==============================================Validating Classification using Specimen Group (Classification vs. SPECIMEN_GROUP)

# Define the mapping of specimen groups to PTB, EPTB, and Unknown categories
ptb_group_1 <-c("RESP OR SPUTUM", "SPUTUM", "INDUCED SPUTUM", "GASTRIC ASPIRATE", 
                 "BRONCHIAL WASHING", "NASOPHARYNGEAL ASPIRATE", "TRACHEAL ASPIRATE", 
                 "SPU", "SPT", "ETA", "SP[UTUM", "SPURUM", "BRONCHIAL ALVEOLAR LAVAGE", "GASTRIC ASPIRATE (WASHING) (>DAY 1)",
                 "FNA, RESPIRATORY TRACT", "RSP", "TA", "GASTRIC WASHING", "INDUCED", "GASTRIC ASPIRATE (WAS ING) ( DA  1)", "SP", "NON-GYNAE, GIT",
                 "SOUTUM", "RESP", "GASTRIC WASH", "?SPT", "GASTRIC ASP", "SPUTM", "SPUTU", "SPUTAM", "NGA", "SUPTUM", "SPUTUMN", "ENDOTRACHEAL ASPIRATEE",
                 "SPTUUM", "SPUTUPM", "SUTUM", "SPUTUM2", "SPUTUM 1", "SPUTUMPCR", "BRONCHIOLAR ALVEOLAR LAVAGE", "? SPT", "?SPUTUM", "SPUTUMQ", "SPUTIM",
                 "1SPUTUM", "APUTUM", "SPUUTM", "ENDOBRONCHIAL MASS")

eptb_group_1 <-c("PLEURAL FLUID", "CSF", "URINE", "TISSUE", "FNA, LYMPH NODES", 
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
                  "SPINAL ABSCESS", "(R) PLEUARAL ASPIRATE", "(R) NECK FNA", "LYMPH NODE ASPIRATE", "LYPHNODE BIOPSY", "RIGHT PSOAS", "NON-GYNAE, LYMPH NODES", "PUS (ABSCESS",
                  "FLUID (R) BREAST/AXILLA", "PUS ABSCESS", "FLUID/ASPIRATE", "BONE MARROW", "FL", "MIDSTREAM URINE", "FLUID, PLAIN TUBE", "FLUID /ASPIRATE", "ASP",
                  "STOOL", "(L) KNEE FLUID", "ASCITES (R)ARM", "NECK BIOPSY", "FISTULA ABDO FLUID", "ASPIRATE", "(R)PLEURAL FLUID", "(R) GROIN FNA", "PLEURAL FLUID (L)LUNG EFFUSION",
                  "COLON BIOPSY", "BIOPSY", "INGUINAL LYMPHNODE PUS", "SYNOVIAL BIOPSY", "ASITIC FLUID", "SYNOVIAL FLUID", "CERVICAL NECK MASS", "(R  EAR SWAB",
                  "TISSUE BIOPSY", "BONE BIOPSY", "(R) CERVICAL LYMPHNODE", "EMPYEMA FLUID","(L)PLEURAL FLUID", "(L) AXILLA", "FNA(R)NECK ABSCESS", "PLEURAL FLUID LEFT",
                  "(L) KNEE ASPIRATE", "KNEE ASPIRATE", "URETHRU LUTANES FISTULA", "KNEE EFFUSION", "RIGHT AXILLARY PUS SWAB", "(R) PLEURAL FLUID", "PUS - (L) AXILLARY ABSCESS", "(L) EAR SWAB",
                  "TISSUE - LN BIOPSY", "NECK ABSCESS", "BIOPSY (R) NECK", "FNA L CERCIVAL NECK", "FLUID, JOINT (SYNOVIAL)", "ASPIRATE NECK ABSCESS", " KNEE ASPIRATE", "FLUID (R) FOOT",
                  "PLEURAL FLUID (R) LUNG", "WOUND EXUDATE FLUID", " ASPIRATE NECK", "RIGHT KNEE EFFUSION", "PLEURAL FLUID - (R) LUNG", "WOUND EXUDATE FLUID", "ASPIRATE NECK",
                  "PERICADIAL EFFUSION", "LEFT BREAST ABSCESS", "PUS CERVICAL FLUID", "(R)PLEURAL EFFUSION", "PITAIL DRAIN", "(R) AXILLA SWAB", "(R) NECK ABSCESS", "FNA, BREAST",
                  "ANTERIOR NECK PUS", "ABSCESS (ON BACK)", "PERICARDIAL TAP", "RIGHT PLEURAL FLUID", "PIGTAIL ABSCESS", "ICD FLUID", "BIOPSY (EDNOMETRUS)", "ENDOMETRIAL SAMPLE",
                  "PUS ABSCESS NECK", "FNA LYMPHNODE", "CSF - VENTRICULAR", "L PLEURAL FLUID", "ABSCESS (DEEP) ASPIRATE", "PLEURAL FLUID(R)PLEURAL SPACE", "PUS ABSCESS PSOAS",
                  "PLUERAL FLUID", "FNA AXILLARY LYMPHNDOE", "BREAST ASPIRATE RIGHT", "SEROUS FLUID", "PERIANAL PUS SWAB", "AXILLARY LYMPHNODE", "LYMHNODE", "(R) KNEE TISSUE",
                  "PUS NECK ABSCESS", "PUS SWAB (GROIN)", "ABSCESS FLUID", "ABSCESS THIGH", "(R) SUBMANDIBULAR LYMPHNODE", "ABDOMINAL FLUID", "(L) NECK LYMPHNODE", "SWAB",
                  "FNA, SOFT TISSUE", "RIGHT NECK FNA", "PUS SWAB NECK", "ASPIRATE NO SITE", "(R) PUS FROM AXILLARY ABSCESS", "PLEURAL  FLUID", "SCROTAL ASPIRATE", "LEFT UPPERARM PUS",
                  "LYMPHNODE RIGHT NECK", "(R) THIGH FNA", "FNA- (L) CERVICAL GLAND", "PUS CHEST WALL ABSCESS", "CYSTIC FLUID (R) OVARIAN CYST", "NECK- NECK ANTERIOR", "BIOPSY PERICARDIAL",
                  "(R) NECK GLAND FNA", "ICD DRAIN FLUID", "PUS LYNHNODE NECK (R)", "PUS ASPIRATE CHEST WALL", "PLEURAL FUID", "SUBMANDIBULAR ASPIRATE", "(R)SUBMANDIBULAR GLAND FNA",
                  "LYMPHNODE BIOPSY", "INTRACLAVICULAR ABSCESS", "SYNOVIAL FLUID (L)KNEE", "FNA - LYMPHNODE", "(R)CHEST WALL PUS FLUID", "PUS (L)SUPRACLAVICULER", "(R)HIP TISSUE",
                  "PUS LYMPHNODE", "(R) ARMPIT ASPIRATE", "TISSUE LYMPNODE", "FNAB", "SKIN BIOPSY (R) ARM", "(L) PLEURAL FLUID",
                  "PUS:SPINE", "MIDSTREAM URINE[URC]", "ABSCESS ASPIRATE", "(L) CHEEK MASS", "(R)EAR ASPIRATE", "FLUID / ASPIRATE ANKLE", "FNA OF LYMPHNODE", "INTRAABODNIAL PUS",
                  "JOINT FLUID", "PUS-NECK MASS", "(R)WRIST PUS", "PUS ABSCESS (R) NECK", "ASCITES", "COLD ABSCESS", "FNA SLIDES", "(R)KNEE SYNOVIAL ASPIRATE", "ABSCESS PUS FROM NECK",
                  "FNA (L) NECK", "PUS RIGHT SUBCLAVICULAR LYMPHNODE", "(R)HIP BONE TISSUE", "ABDOMINAL ABSCESS", "FNA, CHEST", "ANTERIOR CERVICAL MASS", "FINE NEEDLE ASPIRATE", "FNA SUBMANDIBULAR ASPIRATE (L) PUS",
                  "PERICARDIAL EFFUSION", "LYMPHATIC FLUID", "(L) KNEE PUS", "CERVICAL LYMPHODE ASPIRATE", "(R) EAR SWAB", "PLEUAL FLUID", "FNA SUBMANDIBULAR", "PERITONEAL TISSUE", "(L)PLEURAL EFFUSION", "BONE",
                  "(R) NECK ASPIRATE", "PUS PERIANAL", "PUS SWAB EAR", "LEFT TIBIA ABSCESS", "ABSCESS (SUPERFICIAL) ASPIRATE (R)BACK", "FNA LYMPNODE", "NECK ABSCESS PUS", "PUS SWAB GROIN")

unknown_group_1<- c("UNKNOWN", "UN NOWN", "?", "SAMPLE", "FORENSIC CHEMISTRY", 
                     "SMEAR", "NICD: SWAB", "BACTERIAL CULTURE", "C,CF", "OTHER", 
                     "CATHETER TIP", "MYCTC", "S", "STERILE SITES", "PATHOLOGY", "SSFL", "BC,BL,BLC,BLE", "C,CF,SSFL", "SSFL,SSFLF",
                     "BLC,BLP", "BLC,BLE,BLP", "NON-GYNAE, OTHER", "BLP", "BLC,C,CF", "BLC,SSFL", "NON-GYNAE", "NON-GYNAE, BODY CAVITIES", "NON-GYNAE, OTHER",
                     "BLOOD CULTURE", "<NA>", "NOT STATED", "N/A", "BLOOD", ">", "F L", "NOT INDICATED", "??", "/", "CULSS", "FN", "PUBLIC HEALTH", ",", ".", "FLD",
                     "OPTHER", "SPECIMEN", "`", "FNA  OTHER", "FNA, BODY CAVITIES", "\021", "Y", "CB")


# Classify the specimens into PTB, EPTB, and Unknown
group_validation_culture <- Culture_data %>%
  mutate(
    group_class_1 = case_when(
      SPECIMEN_GROUP_1 %in% ptb_group_1 ~ "PTB",
      SPECIMEN_GROUP_1 %in% eptb_group_1 ~ "EPTB",
      SPECIMEN_GROUP_1 %in% unknown_group_1 ~ "Unknown",
      is.na(SPECIMEN_GROUP_1) ~ "Unknown",
      TRUE ~ "Unclassified"
    )
  )

# View counts for each category
table(group_validation_culture$group_class_1)


#Retain PTB and EPTB specimens (Remove cases classified as "Unknown")
group_validation_culture <- group_validation_culture%>% 
  filter(class %in% c("PTB", "EPTB")) %>%
  filter(group_class_1 %in% c("PTB", "EPTB"))

# Ensure both columns are factors with the same levels
group_validation_culture$class <-factor(group_validation_culture$class, levels = c("PTB", "EPTB"))
group_validation_culture$group_class_1 <-factor(group_validation_culture$group_class_1, levels = c("PTB", "EPTB"))

# Check levels of the relevant factors
levels(group_validation_culture$group_class_1)
levels(group_validation_culture$class)

# Generate confusion matrix
group_confusion_matrix <- confusionMatrix(group_validation_culture$class, group_validation_culture$group_class)
print(group_confusion_matrix)

#-----------------------------------Agreement Analysis
# Extract Cohen's Kappa
group_kappa_culture <- group_confusion_matrix$overall["Kappa"]
print(group_kappa_culture)
