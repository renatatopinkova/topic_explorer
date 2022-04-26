
# This is a script that exports sample texts for topics into an Excel file
# one topic == one sheet

# Load libraries ----------------------------------------------------------


library(XLConnect)
library(stm)
library(dplyr)
library(stringr)
library(purrr)


# Load topic objects ------------------------------------------------------

stmmodel <- readRDS("stmmodel")     # NOTE: These object are exported from the 
stmdata <- readRDS("stmdata")       # STM R script to avoid re-running the model (saves time)


# get labels for the topics to use later
labels <- labelTopics(stmmodel, n = 10)
# saveRDS(labels, "labels")

# Getting topic texts ---------------------------------------------------

# set number of topics
topicnr <- 70

# clean encoding issues 
stmdata$meta$alltext <- str_replace_all(stmdata$meta$alltext, "â\u0080\u0099", "'")
stmdata$meta$alltext <- str_replace_all(stmdata$meta$alltext, "&amp;", "&")
stmdata$meta$alltext <- str_replace_all(stmdata$meta$alltext, "&#x200b;", " ")


# function for getting example texts to iterate over
get_thoughts <- function(i) {
  findThoughts(stmmodel,
               texts = stmdata$meta$alltext, n = 25,
               topics = i
  )$docs
}



# iterate the function over each topic, convert list to a dataframe
df_thoughts <- as.data.frame(lapply(seq(1:topicnr), get_thoughts))
#saveRDS(df_thoughts, "df_thoughts")


# Creating Excel workbook -------------------------------------------------



# load/create empty excel workbook 
db <- loadWorkbook("topic_examples.xlsx", create = TRUE )

# function to iterate over later
make_excel <- function(i) {
  # create sheet with topic number
  createSheet(db, paste("Topic", i))
  # write the data for the topic into the sheet (& split text into title and text)
  writeWorksheet(db, str_split_fixed(df_thoughts[, i], fixed("//"), 2),
    sheet = paste("Topic", i), startRow = 3, startCol = 1,
    header = TRUE
  )
  # give the sheet a header (A1) with the topic's frex terms
  writeWorksheet(db, paste("Topic", i, ":", str_c(labels$frex[i, 1:10], collapse = ", ")),
    sheet = paste("Topic", i), startRow = 1, startCol = 1,
    header = F
  )
}


# run function over each topic - create the final Excel file for topic validation
map(seq(1:topicnr), make_excel)


# Styling the excel -------------------------------------------------------


# add styling (widen cells to make them readable)
setColumnWidth(db, seq(1:topicnr), 1, 10000)
setColumnWidth(db, seq(1:topicnr), 2, 30000)

# create an empty style
cs <- createCellStyle(db)

# Specify to wrap the text 
setWrapText(cs, wrap = TRUE)


# apply wrapping-style to all 
## NOTE: applying in two steps because XLConnects gets weird if done in one go
map(seq_len(topicnr), ~setCellStyle(db, sheet = .x, row = 4:30, col = 2, 
                                           cellstyle = cs))

map(seq_len(topicnr), ~setCellStyle(db, sheet = .x, row = 4:30, col = 1, 
                                    cellstyle = cs))



# making the heading (A1) yellow
get_yellow <- createCellStyle(db)

# Specify the fill foreground color
setFillForegroundColor(get_yellow, color = XLC$"COLOR.GOLD")
# specify fill type
setFillPattern(get_yellow, fill = XLC$FILL.SOLID_FOREGROUND)
# make it wrap
setWrapText(get_yellow, wrap = TRUE)

# Set the cell style created above for the top left cell (A1) for all sheets
map(seq_len(topicnr), ~setCellStyle(db, sheet = seq_len(topicnr), row = 1, col = 1, cellstyle = get_yellow))


# Saving the Excel workbook -----------------------------------------------


# save the result
saveWorkbook(db, "topic_examples.xlsx")



# Alternative solution: long format (used for shiny app) ------------------

# ## iterate over all topics
# df_thoughts_long <- as.data.frame(lapply(seq(1:topicnr), get_thoughts)) %>%
#   # pivoting to longer format
#   pivot_longer(cols = starts_with("Topic"), names_to = "topic", values_to = "text") %>%
#   mutate(topic = as.numeric(str_remove(topic, "Topic."))) %>%
#   separate(text, into = c("title", "text"), sep = "//") %>%
#   arrange(topic)
# 
# saveRDS(df_thoughts_long, "topic_explorer/df_thoughts_long")

