# Set Working Directory
setwd("C:/Users/kzd5561/OneDrive - The Pennsylvania State University/Documents/Studies/What Works - HHD CoP/2025-2026/CHPS/April CoP")

# Import SPSS file from Downloads
#library(haven)
#CHPS_25_26_April_9_2026_09_41 <- read_sav("C:/Users/kzd5561/Downloads/CHPS+25-26_April+9,+2026_09.41.sav")

# Save imported data frame as CSV
# write.csv(CHPS_25_26_April_9_2026_09_41, "CHPS April 9 AM.csv", row.names=FALSE)

# Open the CSV
#library(readr)
#CHPS_April_9_AM <- read_csv("CHPS April 9 AM.csv")
CHPS_April_9_PM <- read_csv("CHPS April 9 PM.csv")

# Rename imported data frame
#imported_df <- CHPS_April_9_AM
imported_df <- CHPS_April_9_PM

############# Clean up consent question
# Print unique values of Q35 in imported data frame
unique(imported_df$Q35)

# Keep only cases where respondent agreed to participate (Q35 = 1)
library(dplyr)

df_clean <- imported_df %>%
  filter(Q35 == 1)

# Print unique values of Q35 in cleaned data frame (should only see 1)
unique(df_clean$Q35)


############# Clean Up School Names
# Print unique values of School
unique(imported_df$School)

# Examine cases where School is NA - create a data frame of these cases

school_na_df <- imported_df %>%
  filter(is.na(School))

  # This time they were all UTK, so that's easy-ish
  # Also adding in the cleaning of UTK without a comma

df_clean <- df_clean %>%
  mutate(School = case_when(
    is.na(School) ~ "University of Tennessee, Knoxville",
    School == "University of Tennessee Knoxville" ~ "University of Tennessee, Knoxville",
    TRUE ~ School
  ))

  # Now run the unique values of School in the clean data frame
    unique(df_clean$School)
    
############ Remove Villanova and WSU from df_clean (CoP campuses only)
    df_clean <- df_clean %>%
      filter(!School %in% c("Villanova University", "Washington State University"))
    
    # Now run the unique values of School in the clean data frame
    unique(df_clean$School)

############ Make sure everyone has a SchoolCode
    
unique(df_clean$SchoolCode)

# Show NA School Codes
schoolcode_na_df <- imported_df %>%
  filter(is.na(SchoolCode))

# Show pairs of School and SchoolCode

school_code_pairs <- df_clean %>%
  select(School, SchoolCode) %>%
  distinct() %>%
  arrange(School)

# Create a lookup data frame for schools and SchoolCode
school_lookup <- tibble::tribble(
  ~School, ~SchoolCode,
  "Auburn University", 201,
  "Baylor University", 101,
  "Bowling Green State University", 202,
  "Cal Poly San Luis Obispo", 203,
  "James Madison University", 301,
  "Lehigh University", 302,
  "Louisiana State University", 103,
  "Mississippi State University", 204,
  "Ohio University", 303,
  "Penn State University", 304,
  "Stockton University", 305,
  "The University of Alabama", 205,
  "University of Kentucky", 206,
  "University of North Carolina at Chapel Hill", 306,
  "University of Tennessee, Knoxville", 208,
  "Villanova University", 170,
  "Virginia Polytechnic Institute and State University", 209,
  "Washington State University", 105,
  "West Virginia University", 106
)

# Assign SchoolCode where it is NA
df_clean <- df_clean %>%
  left_join(school_lookup, by = "School", suffix = c("", "_lookup")) %>%
  mutate(
    SchoolCode = coalesce(SchoolCode, SchoolCode_lookup)
  ) %>%
  select(-SchoolCode_lookup)

# Show unique SchoolCode again
unique(df_clean$SchoolCode)

########### Create CSV and data frame of CoP Agree to Participate
#write.csv(df_clean, "COP Agree to Participate April 9 AM.csv", row.names=FALSE)
write.csv(df_clean, "COP Agree to Participate April 9 PM.csv", row.names=FALSE)

########### Clean Up JMU AKA Hot Mess

# 1 - Delete Chapter ID and Chapter Name Out of "JMU AKA" cases

df_clean <- df_clean %>%
  mutate(
    Chapter = if_else(
      SchoolCode == 301 & ChapterID == 201,
      NA_character_,
      Chapter
    ),
    ChapterID = if_else(
      SchoolCode == 301 & ChapterID == 201,
      NA_integer_,
      ChapterID
    ),
    Council - if_else(
      SchoolCode == 301 & ChapterID ==201,
      NA_integer_,
      Council
    )
  )

# See where I could still plug in an OrgType/Org_Type
#fs_table_301_nochapter <- df_clean %>%
#  filter(
#    SchoolCode == 301,
#    is.na(Chapter),
#    is.na(ChapterID)
#  ) %>%
#  count(
#    Fraternity_Sorority_1,
#    Fraternity_Sorority_2,
#    Fraternity_Sorority_3,
#    Fraternity_Sorority_4,
#    Fraternity_Sorority_5,
#    Council,
#    sort = TRUE
#  )

# Rename variables to change spaces to underscores
  # Apparently this sometimes happens when importing from Excel
  # I am doing all instances here
colnames(df_clean) <- gsub(" ", "_", colnames(df_clean))

# Recode FraternitySorority into OrgType/Org_Type for these cases
df_clean <- df_clean %>%
  mutate(
    OrgType = case_when(
      # NEW RULE: all FS variables are NA → OrgType = NA
      SchoolCode == 301 &
        is.na(Chapter) &
        is.na(ChapterID) &
        is.na(Fraternity_Sorority_1) &
        is.na(Fraternity_Sorority_2) &
        is.na(Fraternity_Sorority_3) &
        is.na(Fraternity_Sorority_4) &
        is.na(Fraternity_Sorority_5) ~ NA_character_,
      
      SchoolCode == 301 &
        is.na(Chapter) &
        is.na(ChapterID) &
        Fraternity_Sorority_1 == 1 ~ "Fraternity",
      
      SchoolCode == 301 &
        is.na(Chapter) &
        is.na(ChapterID) &
        Fraternity_Sorority_2 == 1 ~ "Sorority",
      
      SchoolCode == 301 &
        is.na(Chapter) &
        is.na(ChapterID) &
        Fraternity_Sorority_3 == 1 &
        is.na(Fraternity_Sorority_1) &
        is.na(Fraternity_Sorority_2) ~ "Co-Ed",
      
      SchoolCode == 301 &
        is.na(Chapter) &
        is.na(ChapterID) &
        Fraternity_Sorority_4 == 1 ~ "Unaffiliated",
      
      TRUE ~ OrgType
    ),
    
    Org_Type = case_when(
      # NEW RULE: all FS variables are NA → Org_Type = NA
      SchoolCode == 301 &
        is.na(Chapter) &
        is.na(ChapterID) &
        is.na(Fraternity_Sorority_1) &
        is.na(Fraternity_Sorority_2) &
        is.na(Fraternity_Sorority_3) &
        is.na(Fraternity_Sorority_4) &
        is.na(Fraternity_Sorority_5) ~ NA_character_,
      
      SchoolCode == 301 &
        is.na(Chapter) &
        is.na(ChapterID) &
        Fraternity_Sorority_1 == 1 ~ "Fraternity",
      
      SchoolCode == 301 &
        is.na(Chapter) &
        is.na(ChapterID) &
        Fraternity_Sorority_2 == 1 ~ "Sorority",
      
      SchoolCode == 301 &
        is.na(Chapter) &
        is.na(ChapterID) &
        Fraternity_Sorority_3 == 1 &
        is.na(Fraternity_Sorority_1) &
        is.na(Fraternity_Sorority_2) ~ "Co-Ed",
      
      SchoolCode == 301 &
        is.na(Chapter) &
        is.na(ChapterID) &
        Fraternity_Sorority_4 == 1 ~ "Unaffiliated",
      
      TRUE ~ Org_Type
    )
  )
# Check recoding
df_clean %>%
  filter(
    SchoolCode == 301,
    is.na(Chapter),
    is.na(ChapterID)
  ) %>%
  count(
    Fraternity_Sorority_1,
    Fraternity_Sorority_2,
    Fraternity_Sorority_3,
    Fraternity_Sorority_4,
    OrgType,
    Org_Type,
    sort = TRUE
  )


########### ChapterID
# Create a Chapter lookup table
chapter_lookup <- tribble(
  ~ChapterID, ~Chapter, ~Council, ~OrgType,
  101, "Alpha Chi Omega", "PHC", "Sorority",
  102, "Alpha Delta Pi", "PHC", "Sorority",
  103, "Alpha Epsilon Phi", "PHC", "Sorority",
  104, "Alpha Gamma Delta", "PHC", "Sorority",
  105, "Alpha Omicron Pi", "PHC", "Sorority",
  106, "Alpha Phi", "PHC", "Sorority",
  107, "Alpha Sigma Alpha", "PHC", "Sorority",
  108, "Alpha Sigma Tau", "PHC", "Sorority",
  109, "Alpha Xi Delta", "PHC", "Sorority",
  110, "Chi Omega", "PHC", "Sorority",
  111, "Delta Delta Delta", "PHC", "Sorority",
  112, "Delta Gamma", "PHC", "Sorority",
  113, "Delta Phi Epsilon", "PHC", "Sorority",
  114, "Delta Zeta", "PHC", "Sorority",
  115, "Gamma Phi Beta", "PHC", "Sorority",
  116, "Kappa Alpha Theta", "PHC", "Sorority",
  117, "Kappa Delta", "PHC", "Sorority",
  118, "Kappa Kappa Gamma", "PHC", "Sorority",
  119, "Phi Mu", "PHC", "Sorority",
  120, "Phi Sigma Sigma", "PHC", "Sorority",
  121, "Pi Beta Phi", "PHC", "Sorority",
  122, "Sigma Delta Tau", "PHC", "Sorority",
  123, "Sigma Kappa", "PHC", "Sorority",
  124, "Sigma Sigma Sigma", "PHC", "Sorority",
  125, "Theta Phi Alpha", "PHC", "Sorority",
  126, "Zeta Tau Alpha", "PHC", "Sorority",
  127, "Alpha Delta Chi", "PHC", "Sorority",
  128, "Alpha Omega Epsilon", "PHC", "Sorority",
  129, "Alpha Sigma Kappa", "Other", "Sorority",
  134, "Kappa Beta Gamma", "PHC", "Sorority",
  138, "Phi Sigma Rho", "PHC", "Sorority",
  139, "Sigma Alpha", "PHC", "Sorority",
  201, "Alpha Kappa Alpha Sorority, Inc.", "NPHC", "Sorority",
  202, "Alpha Phi Alpha Fraternity, Inc.", "NPHC", "Fraternity",
  203, "Delta Sigma Theta Sorority, Inc.", "NPHC", "Sorority",
  204, "Iota Phi Theta Fraternity, Inc.", "NPHC", "Fraternity",
  205, "Kappa Alpha Psi Fraternity, Inc.", "NPHC", "Fraternity",
  206, "Omega Psi Phi Fraternity, Inc.", "NPHC", "Fraternity",
  207, "Phi Beta Sigma Fraternity, Inc.", "NPHC", "Fraternity",
  208, "Sigma Gamma Rho Sorority, Inc.", "NPHC", "Sorority",
  209, "Zeta Phi Beta Sorority, Inc.", "NPHC", "Sorority",
  301, "alpha Kappa Delta Phi International Sorority, Inc.", "MGC", "Sorority",
  303, "Alpha Pi Omega Sorority, Inc.", "MGC", "Sorority",
  305, "Alpha Psi Lambda", "MGC", "Fraternity",
  314, "Chi Upsilon Sigma National Latin Sorority, Inc.", "MGC", "Sorority",
  316, "Delta Lambda Phi", "MGC", "Fraternity",
  317, "Delta Phi Lambda Sorority, Inc.", "MGC", "Sorority",
  324, "Delta Xi Phi", "MGC", "Sorority",
  328, "Gamma Rho Lambda", "MGC", "Sorority",
  329, "Iota Nu Delta Fraternity, Inc.", "MGC", "Fraternity",
  335, "Kappa Phi Lambda Sorority, Inc.", "MGC", "Sorority",
  340, "Lambda Phi Epsilon International Fraternity, Inc.", "MGC", "Fraternity",
  341, "Latinas Promoviendo Communidad/Lambda Pi Chi Sorority, Inc.", "MGC", "Sorority",
  342, "Lambda Sigma Upsilon Latino Fraternity, Inc.", "MGC", "Fraternity",
  343, "Lambda Theta Alpha Latin Sorority, Inc.", "MGC", "Sorority",
  345, "Lambda Theta Phi Latin Fraternity, Inc.", "MGC", "Fraternity",
  346, "La Unidad Latina, Lambda Upsilon Lambda Fraternity, Inc.", "MGC", "Fraternity",
  348, "Mu Sigma Upsilon Sorority, Inc.", "MGC", "Sorority",
  349, "Nu Alpha Kappa Fraternity, Inc.", "MGC", "Fraternity",
  352, "Omega Phi Alpha", "MGC", "Sorority",
  353, "Omega Phi Beta Sorority, Inc.", "MGC", "Sorority",
  359, "Phi Sigma Nu Fraternity, Inc.", "MGC", "Fraternity",
  364, "Sigma Beta Rho", "MGC", "Fraternity",
  365, "Hermandad de Sigma Iota Alpha, Inc.", "MGC", "Sorority",
  367, "Sigma Lambda Beta International Fraternity, Inc.", "MGC", "Fraternity",
  368, "Sigma Lambda Gamma National Sorority, Inc.", "MGC", "Sorority",
  369, "Sigma Lambda Upsilon/Señoritas Latinas Unidas Sorority, Inc.", "MGC", "Sorority",
  371, "Sigma Omega Nu Latina Interest Sorority, Inc.", "MGC", "Sorority",
  372, "Sigma Omicron Pi", "MGC", "Sorority",
  374, "Sigma Psi Zeta Sorority, Inc.", "MGC", "Sorority",
  375, "Sigma Sigma Rho", "MGC", "Sorority",
  379, "Theta Nu Xi Multicultural Sorority, Inc.", "MGC", "Sorority",
  385, "Gamma Zeta Alpha Fraternity, Inc.", "MGC", "Fraternity",
  396, "Lambda Sigma Gamma", "MGC", "Sorority",
  401, "ACACIA", "IFC", "Fraternity",
  403, "Alpha Chi Rho", "IFC", "Fraternity",
  405, "Alpha Delta Phi", "IFC", "Fraternity",
  406, "Alpha Epsilon Pi", "IFC", "Fraternity",
  409, "Alpha Gamma Rho", "IFC", "Fraternity",
  410, "Alpha Kappa Lambda", "IFC", "Fraternity",
  413, "Alpha Phi Delta", "IFC", "Fraternity",
  415, "Alpha Rho Chi", "IFC", "Fraternity",
  416, "Alpha Sigma Phi", "IFC", "Fraternity",
  418, "Alpha Tau Omega", "IFC", "Fraternity",
  419, "Alpha Zeta", "IFC", "Fraternity",
  420, "Beta Sigma Beta", "IFC", "Fraternity",
  422, "Beta Theta Pi", "IFC", "Fraternity",
  423, "Beta Upsilon Chi", "IFC", "Fraternity",
  424, "Chi Phi", "IFC", "Fraternity",
  425, "Chi Psi", "IFC", "Fraternity",
  426, "Delta Chi", "IFC", "Fraternity",
  428, "Delta Kappa Epsilon", "IFC", "Fraternity",
  431, "Delta Sigma Phi", "IFC", "Fraternity",
  432, "Delta Tau Delta", "IFC", "Fraternity",
  433, "Delta Theta Sigma", "IFC", "Fraternity",
  434, "Delta Upsilon", "IFC", "Fraternity",
  436, "FarmHouse", "IFC", "Fraternity",
  438, "Kappa Alpha Order", "IFC", "Fraternity",
  440, "Kappa Delta Rho", "IFC", "Fraternity",
  441, "Kappa Sigma", "IFC", "Fraternity",
  442, "Lambda Chi Alpha", "IFC", "Fraternity",
  446, "Phi Delta Theta", "IFC", "Fraternity",
  448, "Phi Gamma Delta (FIJI)", "IFC", "Fraternity",
  450, "Phi Kappa Psi", "IFC", "Fraternity",
  451, "Phi Kappa Sigma", "IFC", "Fraternity",
  452, "Phi Kappa Tau", "IFC", "Fraternity",
  453, "Phi Kappa Theta", "IFC", "Fraternity",
  454, "Phi Mu Alpha Sinfonia", "IFC", "Fraternity",
  456, "Phi Sigma Kappa", "IFC", "Fraternity",
  458, "Pi Kappa Alpha", "IFC", "Fraternity",
  459, "Pi Kappa Phi", "IFC", "Fraternity",
  462, "Psi Upsilon", "IFC", "Fraternity",
  463, "Sigma Alpha Epsilon", "IFC", "Fraternity",
  464, "Sigma Alpha Mu", "IFC", "Fraternity",
  465, "Sigma Chi", "IFC", "Fraternity",
  467, "Sigma Nu", "IFC", "Fraternity",
  468, "Sigma Phi Delta", "IFC", "Fraternity",
  469, "Sigma Phi Epsilon", "IFC", "Fraternity",
  470, "Sigma Pi", "IFC", "Fraternity",
  471, "Sigma Tau Gamma", "IFC", "Fraternity",
  472, "Tau Epsilon Phi", "IFC", "Fraternity",
  473, "Tau Kappa Epsilon", "IFC", "Fraternity",
  474, "Tau Phi Delta", "IFC", "Fraternity",
  475, "Theta Chi", "IFC", "Fraternity",
  476, "Theta Delta Chi", "IFC", "Fraternity",
  478, "Theta Xi", "IFC", "Fraternity",
  479, "Triangle", "IFC", "Fraternity",
  480, "Zeta Beta Tau", "IFC", "Fraternity",
  481, "Zeta Psi", "IFC", "Fraternity",
  501, "Alpha Kappa Psi", "Other", "Co-Ed",
  502, "Alpha Phi Omega", "Other", "Co-Ed",
  503, "Delta Sigma Pi", "Other", "Co-Ed",
  506, "Kappa Kappa Psi", "Other", "Fraternity",
  508, "Phi Sigma Pi", "Other", "Co-Ed",
  509, "Sigma Alpha Iota", "Other", "Sorority",
  511, "Sigma Phi Lambda", "Other", "Sorority",
  515, "Theta Tau", "Other", "Fraternity",
  543, "Delta Epsilon Mu", "Other", "Co-Ed",
  560, "Kappa Alpha Pi", "Other", "Co-Ed",
  561, "Mu Epsilon Delta Pre-Health Fraternity", "Other", "Co-Ed",
  603, "Chi Delta Theta", "MGC", "Sorority",
  634, "Sigma Omega Phi Multicultural Sorority Inc.", "MGC", "Sorority",
  636, "Sigma Rho Lambda Sorority", "MGC", "Sorority",
  637, "St. Anthony Hall/Fraternity of Delta Psi", "MGC", "Fraternity",
  641, "Phi Sigma Chi", "MGC", "Fraternity",
  999, "Other organization not listed", NA, NA
)

# Assign ChapterID where it is NA

df_clean <- df_clean %>%
  left_join(
    chapter_lookup %>% select(Chapter, ChapterID),
    by = "Chapter",
    suffix = c("", "_lookup")
  ) %>%
  mutate(
    ChapterID = coalesce(ChapterID, ChapterID_lookup)
  ) %>%
  select(-ChapterID_lookup)

# Back-fill Council and OrgType using ChapterID

df_clean <- df_clean %>%
  left_join(
    chapter_lookup %>% select(ChapterID, Council, OrgType),
    by = "ChapterID",
    suffix = c("", "_lookup")
  ) %>%
  mutate(
    Council = coalesce(Council, Council_lookup),
    OrgType = coalesce(OrgType, OrgType_lookup)
  ) %>%
  select(-Council_lookup, -OrgType_lookup)

# Check remaining missing ChapterIDs
missing_id <- df_clean %>%
  filter(is.na(ChapterID)) %>%
  distinct(Chapter) %>%
  arrange(Chapter)

# View them
view(missing_id)

# Compare against valid chapter names
valid_chapters <- chapter_lookup$Chapter

# Use string distance to find near-duplicate names
library(stringdist)

near_matches <- expand.grid(
  observed = missing_id$Chapter,
  reference = valid_chapters,
  stringsAsFactors = FALSE
) %>%
  mutate(distance = stringdist(observed, reference, method = "lv")) %>%
  filter(distance <= 5) %>%
  arrange(observed, distance)

# View near-matches
#view(near_matches)

# Create a corrections table
chapter_corrections <- tibble(
  bad_name=c(
    "Acacia",
    "Alpha Kappa Alpha",
    "alpha Kappa Delta Phi",
    "Alpha Phi Alpha",
    "Delta Sigma Theta",
    "Farmhouse",
    "Iota Phi Theta",
    "Kappa Alpha",
    "Kappa Alpha Psi",
    "Lambda Sigma Upsilon",
    "Lambda Theta Alpha",
    "Mu Sigma Upsilon",
    "Omega Phi Beta",
    "Omega Psi Phi",
    "Phi Beta Sigma",
    "Phi Gamma Delta",
    "Phi Sigma Chi Multicultural Fraternity Inc.",
    "Sigma Gamma Rho",
    "Sigma Lambda Gamma",
    "Sigma Omega Phi",
    "Zeta Phi Beta",
    "Zeta tau Alpha"
  ),
  correct_name=c(
        "ACACIA",
        "Alpha Kappa Alpha Sorority, Inc.",
        "alpha Kappa Delta Phi International Sorority, Inc.",
        "Alpha Phi Alpha Fraternity, Inc.",
        "Delta Sigma Theta Sorority, Inc.",
        "FarmHouse",
        "Iota Phi Theta Fraternity, Inc.",
        "Kappa Alpha Order",
        "Kappa Alpha Psi Fraternity, Inc.",
        "Lambda Sigma Upsilon Latino Fraternity, Inc.",
        "Lambda Theta Alpha Latin Sorority, Inc.",
        "Mu Sigma Upsilon Sorority, Inc.",
        "Omega Phi Beta Sorority, Inc.",
        "Omega Psi Phi Fraternity, Inc.",
        "Phi Beta Sigma Fraternity, Inc.",
        "Phi Gamma Delta (FIJI)",
        "Phi Sigma Chi",
        "Sigma Gamma Rho Sorority, Inc.",
        "Sigma Lambda Gamma National Sorority, Inc.",
        "Sigma Omega Phi Multicultural Sorority Inc.",
        "Zeta Phi Beta Sorority, Inc.",
        "Zeta Tau Alpha"
  )
)

# Apply the corrections

df_clean <- df_clean %>%
  left_join(chapter_corrections, by = c("Chapter" = "bad_name")) %>%
  mutate(
    Chapter = coalesce(correct_name, Chapter)
  ) %>%
  select(-correct_name)


# Re-assign ChapterID -- line 409 (for now)
# Re-run the ID/Council/OrgType Assignment -- line 422 (for now)

df_clean <- df_clean %>%
  left_join(
    chapter_lookup %>% select(Chapter, ChapterID),
    by = "Chapter",
    suffix = c("", "_lookup")
  ) %>%
  mutate(ChapterID = coalesce(ChapterID, ChapterID_lookup)) %>%
  select(-ChapterID_lookup)

# Back-fill Council and OrgType
df_clean <- df_clean %>%
  left_join(
    chapter_lookup %>% select(ChapterID, Council, OrgType),
    by = "ChapterID",
    suffix = c("", "_lookup")
  ) %>%
  mutate(
    Council = coalesce(Council, Council_lookup),
    OrgType = coalesce(OrgType, OrgType_lookup)
  ) %>%
  select(-Council_lookup, -OrgType_lookup)

# Return to check missing IDs (line 435 at last update)

# Final validation - want this to be NA
df_clean %>%
filter(is.na(ChapterID)) %>%
  distinct(Chapter)

# Check unique chapter names where ChapterID is NA
df_clean %>%
  filter(is.na(ChapterID)) %>%
  distinct(Chapter) %>%
  arrange(Chapter)

  # How many affected cases?
sum(is.na(df_clean$ChapterID))

# Look at NA Chapter IDs with response to Fraternity_Sorority

missing_chapter_fs <- df_clean %>%
  filter(is.na(ChapterID)) %>%
  select(
    Chapter,
    Fraternity_Sorority_1,
    Fraternity_Sorority_2,
    Fraternity_Sorority_3,
    Fraternity_Sorority_4,
    Fraternity_Sorority_5
  )

#View(missing_chapter_fs)

# Back-fill chapter names using ChapterID

df_clean <- df_clean %>%
  left_join(
    chapter_lookup %>% select(ChapterID, Chapter),
    by = "ChapterID",
    suffix = c("", "_lookup")
  ) %>%
  mutate(
    Chapter = coalesce(Chapter, Chapter_lookup)
  ) %>%
  select(-Chapter_lookup)

# Check missing chapters now
sum(is.na(df_clean$Chapter))


########### Fraternity/Sorority Responses

# See cases where these questions were not answered

fs_all_na <- df_clean %>%
  filter(
    is.na(Fraternity_Sorority_1) &
      is.na(Fraternity_Sorority_2) &
      is.na(Fraternity_Sorority_3) &
      is.na(Fraternity_Sorority_4) &
      is.na(Fraternity_Sorority_5)
  )

  # How many?
nrow(fs_all_na)

# Unique missing situations
fs_all_na %>%
  distinct(Chapter, ChapterID, Council, OrgType)

# Recode sororities as "Yes - Sorority or Women's Fraternity" in FraternitySorority

df_clean <- df_clean %>%
  mutate(
    Fraternity_Sorority_2 = if_else(
      ChapterID %in% c(138, 139, 201, 328, 352),
      1,
      Fraternity_Sorority_2
    )
  )

# Check that it worked
df_clean %>%
  filter(ChapterID %in% c(138, 139, 201, 328, 352)) %>%
  count(Fraternity_Sorority_2)

########### Council
# Move one weird case where OrgType is IGC 
  #(probably my error updating data in Qualtrics)


df_clean <- df_clean %>%
  mutate(
    Council = if_else(
      is.na(Council) & OrgType == "IGC",
      "IGC",
      Council
    ),
    OrgType = if_else(
      OrgType == "IGC",
      NA_character_,
      OrgType
    )
  )

# Unique Council values
unique(df_clean$Council)

# Bucket Council in new variable called Council2

df_clean <- df_clean %>%
  mutate(
    Council2 = case_when(
      Council %in% c("PHC") ~ "PHC",
      
      Council %in% c(
        "NPHC",
        "National Pan-Hellenic Council (NPHC)",
        "National Pan-Hellenic Council (NPHC), Other (Professional, Service, Co-Ed)"
      ) ~ "NPHC",
      
      Council %in% c("IFC") ~ "IFC",
      
      Council %in% c(
        "Professional",
        "Other",
        "Other (Professional, Service, Co-Ed)",
        "United Greek Council (UGC)",
        "UGC",
        "None",
        "Professional Fraternity Council"
      ) ~ "Other",
      
      Council %in% c(
        "CGC",
        "IGC",
        "MGC",
        "Multicultural Greek Council (MGC, NMGC, NALFO, NAPA)",
        "ICGC",
        "USFC",
        "UCGC"
      ) ~ "MGC",
      
      TRUE ~ NA_character_
    )
  )

# Sanity Check - distribution
table(df_clean$Council2, useNA = "ifany")

# Identify Council values that didn't get bucketed
df_clean %>%
  filter(is.na(Council2)) %>%
  distinct(Council)

# Cross-check Council2 against Council
df_clean %>%
  count(Council, Council2) %>%
  arrange(Council)

# How many cases have missing Council2?
sum(is.na(df_clean$Council2))

# Among these, how many are No for FraternitySorority?
df_clean %>%
  filter(is.na(Council2)) %>%
  summarise(
    total_missing_council2 = n(),
    not_in_fsl = sum(Fraternity_Sorority_4 == 1, na.rm = TRUE)
  )

# Calculate the proportion explained
df_clean %>%
  filter(is.na(Council2)) %>%
  summarise(
    proportion_not_in_fsl =
      mean(Fraternity_Sorority_4 == 1, na.rm = TRUE)
  )

# Cross Tabulation
df_clean %>%
  filter(is.na(Council2)) %>%
  count(Fraternity_Sorority_4)

# Identify Problematic Cases
df_clean %>%
  filter(
    is.na(Council2),
    Fraternity_Sorority_4 != 1 | is.na(Fraternity_Sorority_4)
  ) %>%
  distinct(Chapter, ChapterID, Council)

# Quantify missing Council2 cases
df_clean %>%
  filter(is.na(Council2)) %>%
  summarise(
    total_missing = n(),
    not_in_fsl = sum(Fraternity_Sorority_4 == 1, na.rm = TRUE),
    in_fsl_or_unclear = sum(Fraternity_Sorority_4 != 1 | is.na(Fraternity_Sorority_4))
  )

# Isolate the problem cases then view them
council2_problem_cases <- df_clean %>%
  filter(
    is.na(Council2),
    Fraternity_Sorority_4 != 1 | is.na(Fraternity_Sorority_4)
  )

#View(council2_problem_cases)

# See what's going on in problem cases
# Men's Fraternity?
council2_problem_cases %>%
  select(Chapter, ChapterID, Council, OrgType, Fraternity_Sorority_1) %>%
  distinct()

# Sorority/Women's Fraternity?
council2_problem_cases %>%
  select(Chapter, ChapterID, Council, OrgType, Fraternity_Sorority_2) %>%
  distinct()

# Co-Ed Organization?
council2_problem_cases %>%
  select(Chapter, ChapterID, Council, OrgType, Fraternity_Sorority_3) %>%
  distinct()

# Grad student reporting alum?
council2_problem_cases %>%
  select(Chapter, ChapterID, Council, OrgType, Fraternity_Sorority_5) %>%
  distinct()

# Make Council2 "Unknown" if they indicated F/S membership
df_clean <- df_clean %>%
  mutate(
    Council2 = if_else(
      is.na(Council2) &
        (
          Fraternity_Sorority_1 == 1 |
            Fraternity_Sorority_2 == 1 |
            Fraternity_Sorority_3 == 1 |
            Fraternity_Sorority_5 == 1
        ),
      "Unknown",
      Council2
    )
  )

  # How many cases were set to unknown?
sum(df_clean$Council2 == "Unknown", na.rm = TRUE)

# (optional) Flag for Council2 status
df_clean <- df_clean %>%
  mutate(
    Council2_status = case_when(
      Fraternity_Sorority_4 == 1 ~ "Not in FSL",
      Council2 == "Unknown" ~ "Greek – Council Unknown",
      !is.na(Council2) ~ "Greek – Council Known",
      TRUE ~ NA_character_
    )
  )

# Combination table with counts

chapter_orgtype_fs_table <- df_clean %>%
  count(
    ChapterID,
    Chapter,
    OrgType,
    Org_Type,
    Fraternity_Sorority_1,
    Fraternity_Sorority_2,
    Fraternity_Sorority_3,
    Fraternity_Sorority_4,
    Fraternity_Sorority_5,
    sort = TRUE
  )
View(chapter_orgtype_fs_table)

############## OrgType and Org_Type

# Make a data frame of combinations and view it
orgtype_combinations <- df_clean %>%
  select(OrgType, Org_Type) %>%
  distinct()

#View(orgtype_combinations)

# Look at chapters that have "Unaffiliated" in either variable
unaffiliated_chapters <- df_clean %>%
  filter(
    OrgType == "Unaffiliated" |
      Org_Type == "Unaffiliated" |
      is.na(OrgType) |
      is.na(Org_Type)
  ) %>%
  select(
    Chapter,
    ChapterID,
    OrgType,
    Org_Type
  ) %>%
  distinct()

# Get counts of each pattern and view
unaffiliated_chapter_counts <- df_clean %>%
  filter(
    OrgType == "Unaffiliated" |
      Org_Type == "Unaffiliated"
  ) %>%
  count(Chapter, ChapterID, OrgType, Org_Type, sort = TRUE)

#View(unaffiliated_chapter_counts)

# If one is Unaffiliated and Chapter is NA, both are Unaffiliated.
library(stringr)

df_clean <- df_clean %>%
  mutate(
    OrgType  = na_if(str_trim(OrgType), ""),
    Org_Type = na_if(str_trim(Org_Type), "")
  )

df_clean <- df_clean %>%
  mutate(
    unaffiliated_fix =
      Chapter %>% is.na() &
      (
        OrgType == "Unaffiliated" |
          Org_Type == "Unaffiliated"
      ) &
      (
        is.na(OrgType) | is.na(Org_Type)
      ),
    
    OrgType  = if_else(unaffiliated_fix, "Unaffiliated", OrgType),
    Org_Type = if_else(unaffiliated_fix, "Unaffiliated", Org_Type)
  ) %>%
  select(-unaffiliated_fix)

# Update errors for things improperly coded as "Unaffiliated" 
  # Can add to this
df_clean <- df_clean %>%
  mutate(
    OrgType = if_else(
      ChapterID %in% c(123, 201),
      "Sorority",
      OrgType
    ),
    Org_Type = if_else(
      ChapterID %in% c(123, 201),
      "Sorority",
      Org_Type
    )
  )

# Verify the fix
df_clean %>%
  filter(
    Chapter %>% is.na(),
    (OrgType == "Unaffiliated" & is.na(Org_Type)) |
      (Org_Type == "Unaffiliated" & is.na(OrgType))
  )

# Check remaining Unaffiliated cases
df_clean %>%
  filter(OrgType == "Unaffiliated" | Org_Type == "Unaffiliated") %>%
  count(OrgType, Org_Type)

# Match capitalization in Sorority

df_clean <- df_clean %>%
  mutate(
    OrgType = if_else(
      OrgType == "sorority",
      "Sorority",
      OrgType
    ),
    Org_Type = if_else(
      Org_Type == "sorority",
      "Sorority",
      Org_Type
    )
  )

# Sorority and Blank

sorority_mismatch <- df_clean %>%
  filter(
    (OrgType == "Sorority" & is.na(Org_Type)) |
      (Org_Type == "Sorority" & is.na(OrgType))
  ) %>%
  select(
    Chapter,
    ChapterID,
    OrgType,
    Org_Type
  ) %>%
  distinct() %>%
  arrange(Chapter)

#View(sorority_mismatch)

# Fix Sorority Mismatch

df_clean <- df_clean %>%
  mutate(
    OrgType = case_when(
      ChapterID %in% 101:139 ~ "Sorority",
      ChapterID %in% c(203, 208, 209) ~ "Sorority",
      ChapterID %in% c(301, 328, 343, 348, 352, 353, 365, 368, 369, 371, 375, 396, 603, 634) ~ "Sorority",
      
      ChapterID == 202 ~ "Fraternity",
      ChapterID %in% 204:207 ~ "Fraternity",
      
      ChapterID == 501 ~ "Co-Ed",
      
      TRUE ~ OrgType
    ),
    
    Org_Type = case_when(
      ChapterID %in% 101:139 ~ "Sorority",
      ChapterID %in% c(203, 208, 209) ~ "Sorority",
      ChapterID %in% c(301, 328, 343, 348, 352, 353, 365, 368, 369, 371, 375, 396, 603, 634) ~ "Sorority",
      
      ChapterID == 202 ~ "Fraternity",
      ChapterID %in% 204:207 ~ "Fraternity",
      
      ChapterID == 501 ~ "Co-Ed",
      
      TRUE ~ Org_Type
    )
  )

# Clean cases where FraternitySorority was sorority but OrgType/Org_Type is Fraternity

df_clean <- df_clean %>%
  mutate(
    OrgType = if_else(
      Fraternity_Sorority_2 == 1 & OrgType == "Fraternity",
      "Sorority",
      OrgType
    ),
    Org_Type = if_else(
      Fraternity_Sorority_2 == 1 & Org_Type == "Fraternity",
      "Sorority",
      Org_Type
    )
  )

# PHC Fix - Council2=PHC for all NPC orgs

df_clean <- df_clean %>%
  mutate(
    Council2 = if_else(
      ChapterID %in% 101:126,
      "PHC",
      Council2
    ),
    OrgType = if_else(
      ChapterID %in% 101:126,
      "Sorority",
      OrgType
    ),
    Org_Type = if_else(
      ChapterID %in% 101:126,
      "Sorority",
      Org_Type
    )
  )

# Match IFC and PHC coding to OrgType/Org_Type

df_clean <- df_clean %>%
  mutate(
    OrgType = case_when(
      Council2 == "PHC" ~ "Sorority",
      Council2 == "IFC" ~ "Fraternity",
      TRUE ~ OrgType
    ),
    Org_Type = case_when(
      Council2 == "PHC" ~ "Sorority",
      Council2 == "IFC" ~ "Fraternity",
      TRUE ~ Org_Type
    )
  )


# Check "Other org not listed" sororities were marked as Sorority in FraternitySorority

other_org_combinations <- df_clean %>%
  filter(ChapterID == 999) %>%
  count(Chapter, OrgType, Org_Type, Fraternity_Sorority_1, Fraternity_Sorority_2, sort = TRUE)

# View(other_org_combinations)

# Match "Other org not listed" sororities and blanks
df_clean <- df_clean %>%
  mutate(
    Org_Type = if_else(
      ChapterID == 999 &
        OrgType == "Sorority" &
        is.na(Org_Type),
      "Sorority",
      Org_Type
    )
  )

### Clean up Fraternities
# Fix fraternities marked as sororities

df_clean <- df_clean %>%
  mutate(
    OrgType = if_else(
      ChapterID %in% c(458, 470),
      "Fraternity",
      OrgType
    ),
    Org_Type = if_else(
      ChapterID %in% c(458, 470),
      "Fraternity",
      Org_Type
    )
  )

# Compile and view combinations of fraternity cases
# Look at chapters that have "Unaffiliated" in either variable
unaffiliated_chapters <- df_clean %>%
  filter(
    OrgType == "Unaffiliated" |
      Org_Type == "Unaffiliated" |
      is.na(OrgType) |
      is.na(Org_Type)
  ) %>%
  select(
    Chapter,
    ChapterID,
    OrgType,
    Org_Type
  ) %>%
  distinct()

# Get counts of each pattern and view

fraternity_cases <- df_clean %>%
  filter(
    OrgType == "Fraternity" |
      Org_Type == "Fraternity"
  ) %>%
  count(
    Chapter,
    ChapterID,
    OrgType,
    Org_Type,
    sort = TRUE
  )

# view(fraternity_cases)


df_clean %>%
  filter(
    OrgType == "Fraternity" |
      Org_Type == "Fraternity"
  ) %>%
  filter(
    is.na(OrgType) |
      is.na(Org_Type) |
      OrgType != Org_Type
  )

###### Chapter OrgType Table

chapter_orgtype_table <- df_clean %>%
  count(
    ChapterID,
    Chapter,
    Council2,
    OrgType,
    Org_Type,
    sort = TRUE
  )

# View(chapter_orgtype_table)

# Fix/Fill In Incorrectly Coded Fraternities
df_clean <- df_clean %>%
  mutate(
    OrgType = if_else(
      ChapterID %in% c(
        202,
        204:207,
        305,
        340,
        342,
        345,
        349,
        364,
        367,
        385,
        401:499,
        641
      ),
      "Fraternity",
      OrgType
    ),
    Org_Type = if_else(
      ChapterID %in% c(
        202,
        204:207,
        305,
        340,
        342,
        345,
        349,
        364,
        367,
        385,
        401:499,
        641
      ),
      "Fraternity",
      Org_Type
    )
  )

chapter_orgtype_table <- df_clean %>%
  count(
    ChapterID,
    Chapter,
    Council2,
    OrgType,
    Org_Type,
    sort = TRUE
  )

view(chapter_orgtype_table)

# Clean up co-ed Orgs

df_clean <- df_clean %>%
  mutate(
    OrgType = if_else(
      OrgType == "Co-Ed" | Org_Type == "Co-Ed",
      "Co-Ed",
      OrgType
    ),
    Org_Type = if_else(
      OrgType == "Co-Ed" | Org_Type == "Co-Ed",
      "Co-Ed",
      Org_Type
    )
  )
chapter_orgtype_table <- df_clean %>%
  count(
    ChapterID,
    Chapter,
    Council2,
    OrgType,
    Org_Type,
    sort = TRUE
  )

view(chapter_orgtype_table)

########## Bucket Primary Organizations

# Lookup table for primary orgs
library(tibble)

dem_orgs_lookup <- tribble(
  ~Dem_Orgs_Primary, ~Dem_Orgs_Primary_Label,
  16, "Academic Fraternity or Sorority",
  17, "Academic or Honor Society",
  1,  "Athletics/Recreation: Intercollegiate Athletics",
  25, "Athletics/Recreation: Men's Varsity Intercollegiate Sport",
  32, "Athletics/Recreation: Women's Varsity Intercollegiate Sport",
  2,  "Athletics/Recreation: Club and Intramural Sports",
  24, "Athletics/Recreation: Intramural Sports",
  3,  "Athletics/Recreation: Spirit and Performance Teams",
  31, "Athletics/Recreation: Sport Clubs",
  18, "Campus Employment",
  4,  "Cultural Heritage, Language, and Identity Organizations",
  5,  "Fraternity and Sorority Life",
  19, "Fraternity and Sorority Life: Intercultural Greek Council (IGC)",
  20, "Fraternity and Sorority Life: Interfraternity Council (IFC)",
  21, "Fraternity and Sorority Life: National Pan-Hellenic Council (NPHC)",
  22, "Fraternity and Sorority Life: Panhellenic Association",
  6,  "Honors Programs and Scholarly Societies",
  23, "Graduate and Professional Student",
  7,  "Military or Reserve Officers' Training Corps (ROTC)",
  9,  "Performing Arts (Band, Choir, Dance, Theater)",
  8,  "Pre-Professional, Graduate, and Professional Student Associations",
  26, "Pre-Professional",
  27, "Religious, Spiritual, and Faith",
  10, "Residence Life Associations",
  28, "Secret Society",
  11, "Social Concern, Advocacy, and Awareness",
  30, "Special Interest and Hobby",
  29, "Student Governance and Leadership",
  12, "Student Government",
  13, "Volunteering, Service, and Philanthropy",
  14, "Other Club/Organization",
  33, "Other Registered Club or Organization",
  15, "Not yet a member of an organization"
)

# Join labels into data
df_clean <- df_clean %>%
  left_join(dem_orgs_lookup, by = "Dem_Orgs_Primary")

# Collapse into 15 categories and call it Dem_Orgs_Primary2
df_clean <- df_clean %>%
  mutate(
    Dem_Orgs_Primary2 = case_when(
      
      # Athletics / Recreation
      Dem_Orgs_Primary %in% c(1, 25, 32) ~ 
        "Athletics/Recreation: Intercollegiate Athletics",
      
      Dem_Orgs_Primary %in% c(2, 24) ~ 
        "Athletics/Recreation: Club and Intramural Sports",
      
      Dem_Orgs_Primary == 3 ~ 
        "Athletics/Recreation: Spirit and Performance Teams",
      
      # Cultural / Identity Organizations
      Dem_Orgs_Primary == 4 ~ 
        "Cultural Heritage, Language, and Identity Organizations",
      
      # Fraternity and Sorority Life
      Dem_Orgs_Primary %in% c(5, 19, 20, 21, 22) ~ 
        "Fraternity and Sorority Life",
      
      # Honors / Academic
      Dem_Orgs_Primary %in% c(6, 16, 17) ~ 
        "Honors Programs and Scholarly Societies",
      
      # Military / ROTC
      Dem_Orgs_Primary == 7 ~ 
        "Military or Reserve Officers' Training Corps (ROTC)",
      
      # Performing Arts
      Dem_Orgs_Primary == 9 ~ 
        "Performing Arts (Band, Choir, Dance, Theater)",
      
      # Pre‑Professional / Graduate
      Dem_Orgs_Primary %in% c(8, 23, 26) ~ 
        "Pre-Professional, Graduate, and Professional Student Associations",
      
      # Residence Life (UPDATED LABEL)
      Dem_Orgs_Primary == 10 ~ 
        "Residence Life Associations (Hall Council, Residence Hall Association)",
      
      # Social Concern / Advocacy
      Dem_Orgs_Primary == 11 ~ 
        "Social Concern, Advocacy, and Awareness",
      
      # Student Government & Leadership
      Dem_Orgs_Primary %in% c(12, 29) ~ 
        "Student Government",
      
      # Volunteering / Service
      Dem_Orgs_Primary == 13 ~ 
        "Volunteering, Service, and Philanthropy",
      
      # Other / Miscellaneous
      Dem_Orgs_Primary %in% c(14, 18, 27, 28, 30, 31, 33) ~ 
        "Other Club/Organization",
      
      # Not Yet a Member
      Dem_Orgs_Primary == 15 ~ 
        "Not yet a member of an organization",
      
      # Catch-all
      TRUE ~ NA_character_
    )
  )
# Check distribution and confirm nothing was missed
table(df_clean$Dem_Orgs_Primary2, useNA = "ifany")

df_clean %>%
  filter(is.na(Dem_Orgs_Primary2)) %>%
  distinct(Dem_Orgs_Primary)

# Show rows where Dem_Orgs_Primary2 is NA
  # If Dem_Orgs_Primary is also NA, there is nothing left to clean up
df_clean %>%
  filter(is.na(Dem_Orgs_Primary2)) %>%
  select(Dem_Orgs_Primary, Dem_Orgs_Primary2)

########### Save CSV of cleaned data
# Save cleaned data frame as CSV
write.csv(df_clean, "CHPS April 9 PM_cleaned.csv", row.names=FALSE)
