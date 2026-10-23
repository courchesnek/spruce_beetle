#load packages
source("Scripts/00-packages.R")

#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

#pull in tree cone data
tree_cones <- tbl(con, "cones") %>%
  collect()

#filter for 1996 - 2010
tree_cones <- tree_cones %>%
  filter(Year >= 1996 & Year <= 2010)

#only SU data
# SU_cones <- tree_cones %>%
#   filter(Grid == "SU")

#save
#write_csv(SU_cones, "Output/SU_cones.csv", row.names = FALSE)

# combine locx and locy to produce reflo ----------------------------------
num_to_letter <- setNames(LETTERS, as.character(1:26))

numeric_to_locx <- function(x) {
  x_chr <- as.character(x)
  
  # A) If it already starts with a letter, return as is
  if (grepl("^[A-Za-z]", x_chr)) return(x_chr)
  
  # B) If it's zero or negative, return as is
  if (grepl("^0", x_chr) || grepl("^-", x_chr)) return(x_chr)
  
  # C) Look up letter directly (assuming integer input now)
  letter <- num_to_letter[x_chr]
  
  # D) If lookup failed (e.g. number > 26), just return original number
  if (is.na(letter)) {
    return(x_chr)}
  
  # E) Return the found letter
  return(letter)}

numeric_to_locx_vec <- Vectorize(numeric_to_locx)

tree_cones <- tree_cones %>%
  mutate(
    Stake = paste0(numeric_to_locx_vec(LocX), LocY))

#save
#write_csv(tree_cones, "Output/tree_cones.csv")

# how many new trees? -----------------------------------------------------
sum(tree_cones$New_Tree == "Y", na.rm = TRUE)

# changes in DBH = new tree -------------------------------------------------
dbh_drop_threshold <- 2.0
dbh_increase_threshold <- 5.0

DBH_changes <- tree_cones %>%
  # 1) filter out empty stakes and force DBH to be a number
  filter(Stake != "" & !is.na(Stake)) %>% 
  mutate(DBH = as.numeric(DBH)) %>%
  arrange(Grid, Stake, Year) %>%  # sort by year and stake
  group_by(Grid, Stake) %>%
  # "only keep this group if it has 2 or more valid DBH numbers"
  filter(sum(!is.na(DBH)) >= 2) %>%
  # ONLY keep stakes that span the full 15-year period (1996-2010)
  # this ensures every tree has the same "exposure time"
  filter(min(Year) == 1996, max(Year) == 2010) %>%
  # 2) fill the gaps so we always compare to the last KNOWN measurement
  mutate(last_known_dbh = DBH) %>%
  fill(last_known_dbh, .direction = "down") %>%
  mutate(
    # 3) calculate the specific drop (positive number = shrinkage)
    # logic: previous - current
    # example: prev=10, curr=4 -> drop=6 (flag!)
    # example: prev=10, curr=12 -> drop=-2 (ignore)
    dbh_drop = lag(last_known_dbh) - DBH,
    # 4) flag the specific rows
    new_tree_event = case_when(
      is.na(DBH) ~ "No Measure",              # didn't measure this year
      is.na(lag(last_known_dbh)) ~ "First Record", # start of timeline
      # Condition A: Sudden Drop (Shrinkage > 2cm)
      dbh_drop > dbh_drop_threshold ~ "YES",
      
      # Condition B: Impossible Growth (Growth >= 5cm)
      # Note: dbh_drop is negative when growing, so we check if it's "more negative" than -5
      dbh_drop <= -dbh_increase_threshold ~ "YES",
      TRUE ~ "NO")) %>%
  # identify the time a tree dies, and remove all subsequent years.
  # we keep the "YES" row (to record the death) but drop everything after it.
  group_by(Grid, Stake) %>%
  filter(lag(cumsum(new_tree_event == "YES"), default = 0) < 1) %>%
  ungroup() %>%
  dplyr::select(Grid, Year, Stake, LocX, LocY, DBH, last_known_dbh, dbh_drop, new_tree_event)

#save
write_csv(DBH_changes, "Output/DBH_changes.csv")

# calculate mortality of only the new cohort ---------------------------------------------------------------
# 1) summarize the data by Grid + Stake
# was there ever a death event at that grid-stake?
death_stats <- DBH_changes %>%
  group_by(Grid, Stake) %>%
  summarize(
    # check if "YES" appears anywhere in this tree's history at least once
    tree_died = any(new_tree_event == "YES"))

# 2) calculate the final percentage
total_tree_stakes <- nrow(death_stats)
dead_trees  <- sum(death_stats$tree_died)
percentage  <- (dead_trees / total_tree_stakes) * 100

# print results
total_tree_stakes
dead_trees
percentage

#how many grids?
(unique(DBH_changes$Grid))
(unique(DBH_changes$new_tree_event))

#how many have new_tree_event = "No Measure"?
sum(DBH_changes$new_tree_event == "No Measure", na.rm = TRUE)
