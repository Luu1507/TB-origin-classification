# TB-origin-classification
Classification algorithm designed to use specimen type information to categorize TB into PTB (Pulmonary TB) and EPTB (Extra-pulmonary TB) based on the anatomical origin of the specimen used for testing.
## Overview
This classification algorithm was developed with aim to automate the process of TB classification into PTB and EPTB categories. The approach used leveraged rule-based keyword matching techniques to assign laboratory specimen type data to either category. The algorithm was developed using data from two TB testing methods, Xpert MTB/RIF Ultra and TB culture. Hence, two separate scripts have been included in this repository for each testing method. While the structure of the scripts is the same, this approach was taken to account for minimal differences in the datasets, such as certain specimen types captured in one and not the other.
The classification algorithm was validated against multiple sources using agreement analyses (Cohen's Kappa).

## TABLE OF CONTENTS
### 1. Setup and Exploration
- Import data
- Load necessary libraries
- Understanding the 'Specimen Type' field

### 2. Text-preprocessing

### 3. Keyword Definition

### 4. Rule-based Matching and Classification

### 5. Validation
- Validation against manual coding
- Validation against a predefined lookup table from previous study [^1]
  [^1]: da Silva MP, Cassim N, Ndlovu S, Marokane PS, Radebe M, Shapiro A, et al. More Than a Decade of
GeneXpert Mycobacterium tuberculosis/Rifampicin (Ultra) Testing in South Africa: Laboratory Insights
from Twenty-Three Million Tests. Diagnostics. 2023 10;13.
- Validation against 'Specimen Group' variable

