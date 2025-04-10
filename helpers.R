library(telegram.bot)
library(httr)
library(jsonlite)
library(data.table)
library(maditr)
library(magick)
updates_path = "updates/"
options(scipen = 999)

#### wbot helpers ####
warthunder_save_path = "updates/wbot/"

win <- function(bot, update) {
     user_message <- as.character(update$message$text) %>% gsub("/win ", "", .) %>% as.integer()
     user_id <- update$message$from_user
     user_name <- update$message$from$username
     message_date <- as.Date.POSIXct(update$message$date)
     
     if (is.na(user_message)) {
          bot$sendMessage(chat_id = update$message$chat_id,  text ="Выигрыш или поражение, одна цифра, 0 или 1.")
          
     } else if (user_message >= 0 & user_message <= 1) {
          user_profile <- rows(warthunder_user_profile,user_id==user_id & is.na(win_link))
          if (nrow(user_profile) > 0) {
               bot$sendMessage(chat_id = update$message$chat_id,  text ="Заполните недостающие данные в предыдущей записи, прежде чем создавать новую")
          } else {
               
          user_profile <- create_user_profile(user_id, user_name, user_message) 
          
          warthunder_user_profile <<- rbind(warthunder_user_profile, user_profile)
          
          bot$sendMessage(chat_id = update$message$chat_id,  text = paste0("Вы указали win=", user_message, " теперь укажите дату окончания боя командой /date xxxx"))
          
          }
     } else {
          
          bot$sendMessage(chat_id = update$message$chat_id,  text = "Некорректное значение. Укажите выигрыш или поражение, цифра 1 или 0.")
          
     }
}
date <- function(bot, update) {
     user_message <- as.character(update$message$text)
     user_id <- update$message$from_user
     user_name <- update$message$from$username
     
     
}
w_bot_timestamp <- function(bot, update) {
     user_message <- as.character(update$message$text)
     user_id <- update$message$from_user
     user_name <- update$message$from$username
     message_date <- as.Date.POSIXct(update$message$date)
     
}
link <- function(bot, update) {
     user_message <- as.character(update$message$text)
     user_id <- update$message$from_user
     user_name <- update$message$from$username
     message_date <- as.Date.POSIXct(update$message$date)
     
}
analyse <- function(bot, update) {
     user_message <- as.character(update$message$text)
     user_id <- update$message$from_user
     user_name <- update$message$from$username
     message_date <- as.Date.POSIXct(update$message$date)
     
}
show_data <- function(bot, update) {
     user_message <- as.character(update$message$text)
     user_id <- update$message$from_user
     user_name <- update$message$from$username
     message_date <- as.Date.POSIXct(update$message$date)
     
}


#### dbot helpers ####
dushno_choises_save_path = "dushnila_polls_choises/"

dushno_congratulations = data.table(
     
 phrase = c(
      
   "настоящий чемпион в душении всех вокруг	",
   "непревзойденный мастер душного искусства	",
   "душнение - это твое второе имя	",
   "твои душные способности покорили всех	",
   "ты заслуживаешь золотой медали за свою душноту	",
   "настоящий герой на поле битвы душневых	",
   "ты настоящий мастер в душных делах	",
   "в своем стремлении душнить 	",
   "король душных баталий	",
   "заслуживает звания Мистер Душнитель	",
   "настоящий лидер в душных играх	",
   "своей жажде душнить всех вокруг	",
   "настоящий герой на поле душных битв	",
   "твои умения в душнении просто поразительны	",
   "заслуживаешь звания Король Душнитель	",
   "настоящий чемпион в душных играх	",
   "в своих попытках душить всех вокруг	",
   "главный душитель в нашей команде	",
   "заслуживает звания Мистер Душительность	",
   "настоящий гуру в душных делах	",
   "в своих стремлениях душнить всех вокруг	",
   "настоящий герой на поле душных битв	",
   "твои умения в душении просто поразительны	",
   "заслуживает звания Душнитель № 1	",
   "настоящий мастер в душных играх	",
   "в своих попытках душинить всех вокруг	",
   "главный душнитель в нашей команде	",
   "заслуживает звания Мистер Душнительный	",
   "настоящий чемпион в душных играх	",
   "в своих стремлениях душнить всех вокруг	",
   "король душных баталий	",
   "заслуживает звание Душнитель Года	",
   "настоящий мастер в душных играх	",
   "в своих желаниях душнить всех вокруг	",
   "настоящий герой на поле душных битв	",
   "заслуживает звания Мистер Душительность	",
   "настоящий гуру в душных делах	",
   "в своих стремлениях душнить всех вокруг	",
   "настоящий герой на поле душных баталий	",
   "твои умения в душении поразительны	",
   "заслуживает звания Король Душительности	",
   "настоящий чемпион в душных играх	",
   "в своих попытках душнить всех вокруг	",
   "главный душнитель в нашей команде	",
   "заслуживает звания Мистер Душнительный	",
   "настоящий мастер в душных играх	",
   "в своих желаниях душнить всех вокруг	",
   "настоящий герой душнитель на поле	"
         
     )
)


get_random_number <- function() {
     # Get current date
     today <- as.Date(Sys.time())
     
     # Check if today's date is already stored in the environment
     if (exists("last_date") && last_date == today) {
          # If today's date is already stored, check if 1 has been returned 3 times
          if (exists("count_ones") && count_ones >= 5) {
               # If 1 has been returned 3 times, return 0
               return(0)
          } else {
               # If 1 has not been returned 3 times, randomly return 0 or 1
               random_number <- sample(1:random_number_multiplicatior, 1)
               if (random_number == 1) {
                    # If 1 is returned, increment the count of 1s returned today
                    count_ones <<- count_ones + 1
                    random_number_multiplicatior <<- random_number_multiplicatior + (random_number_multiplicatior + count_ones)
                    return(random_number)
               }
               return(0)
          }
     } else {
          # If today's date is not stored, reset the count of 1s returned and store today's date
          count_ones <<- 0
          last_date <<- today
          random_number_multiplicatior <<- 5
          # Randomly return 0 or 1
          random_number <- sample(0:random_number_multiplicatior, 1)
          if (random_number == 1) {
               # If 1 is returned, increment the count of 1s returned today
               count_ones <<- count_ones + 1
               return(random_number)
          }
          return(0)
     }
}


#### sstbot helpers ####
sst_save_path = "updates/sst/"
# /list command 
list <- function(bot, update) {
     if(update$message$from$id %in% sst_bot_users_ids_list) {
     
          url <- "https://api.survey-studio.com/projects"
          headers <- c(
               accept = "application/json",
               `SS-Token` = SurveyStudioAPItoken
          )
          headers
     
          query <- paste0(url,"?","State=2")
          
          response <- GET(query, add_headers(.headers=headers))
          content <- content(response, "text")
          parsed_content <- fromJSON(content)
          bot$sendMessage(chat_id = update$message$chat$id, text = "Список открытых проектов:")  
          t<-parsed_content$body %>% as.data.table
          t[, row_id := .I] # номер проекта по порядку.
          # browser()
          messages <- paste0(t$row_id,
                            " - ",
                            t$name,
                            " id проекта: ",
                            t$id
          )
          for (message in messages) {
               
         
               bot$sendMessage(chat_id = update$message$chat$id, text = message)  
          }
          bot$sendMessage(chat_id = update$message$chat$id, text = "Для управления проектом используйте id проекта из команды /list")  
  
     }
     
}

data <- function(bot, update) {
     if(update$message$from$id %in% sst_bot_users_ids_list) {
          
          GETurl <- "https://api.survey-studio.com/projects"
          GETheaders <- c(
               accept = "application/json",
               `SS-Token` = SurveyStudioAPItoken
          )
          GETheaders
          project_id <- gsub("/data", "", update$message$text) %>% as.integer()
          if (is.na(project_id)) {
               
               bot$sendMessage(chat_id = update$message$chat$id, text = "Укажите ID проекта после команды /data") 
               
          } else {
               
               query <- paste0(GETurl,"/",project_id,"/counters")
          
          
               response <- GET(query, add_headers(.headers = GETheaders))
               content <- content(response, "text")
               parsed_content <- fromJSON(content)
               
               
               url <- "https://api.survey-studio.com/projects"
               headers <- c(
                    accept = "application/json",
                    `SS-Token` = SurveyStudioAPItoken,
                    `Content-Type` = "application/json-patch+json"
               )
               headers
               
               Request_body <- as.list(c(
                    counterId = parsed_content$body$id[1],
                    exportFormat = 1,
                    spssEncoding = 0,
                    dateFrom = NULL,
                    dateTo = NULL,
                    includeAll = FALSE,
                    addNumericPublicId = FALSE,
                    allowFullSizeStrings = FALSE,
                    exportQuestionText = FALSE,
                    exportLabelsInsteadValues = FALSE,
                    exportLabelsAndCodeValues = FALSE,
                    ignoreErrors = FALSE,
                    exportHostAddress = FALSE,
                    exportUserAgent = FALSE,
                    exportInterviewDumpUrl = FALSE,
                    exportInterviewResult = FALSE,
                    exportContactData = FALSE,
                    exportValidationComments = FALSE,
                    exportValidationDetails = FALSE,
                    includeTotalDurations = FALSE,
                    exportEndedCreatedDifference = FALSE,
                    exportContractorInfo = FALSE,
                    convertMultiLineTextToSingleLine = FALSE,
                    exportSpoofingDataFields = FALSE,
                    exportMobileAppId = FALSE,
                    exportDurationInMinutes = FALSE,
                    exportQuestionsDuration = FALSE,
                    exportUpdatedAt = FALSE,
                    archiveSingleXlsxResultFile = TRUE,
                    easyTabsIntegration = FALSE
               ))
               
               Request_body
               
               query <- paste0(url,"/",project_id,"/results/data")
               
               response <- POST(query, add_headers(.headers = headers), body = Request_body, encode = "json")
               content <- content(response, "text")
               parsed_content <- fromJSON(content)
               if (parsed_content$isSuccess) {
                    
                    request_ID <- parsed_content$body
                    
               } else {
                    
                    bot$sendMessage(chat_id = update$message$chat$id, text = parsed_content$errors$description) 
                    
               }
               
               query <- paste0(GETurl,"/",project_id,"/results/data/",request_ID)
               response <- GET(query, add_headers(.headers = GETheaders))
               content <- content(response, "text")
               parsed_content <- fromJSON(content)
               
               while (parsed_content$body$state < 3){
                    response <- GET(query, add_headers(.headers = GETheaders))
                    content <- content(response, "text")
                    parsed_content <- fromJSON(content)
                    
                    
                    Sys.sleep(5)
                    
               }
               
               bot$sendMessage(chat_id = update$message$chat$id, text = paste0("Ссылку на скачивание выгруженной базы смотрите по ссылке https://my.survey-studio.com/project/results?pId=",
                                                                               project_id)
               )
               
          }
          
     }
}

quota <- function(bot, update) {
     if(update$message$from$id %in% sst_bot_users_ids_list) {
          
          GETurl <- "https://api.survey-studio.com/projects"
          GETheaders <- c(
               accept = "application/json",
               `SS-Token` = SurveyStudioAPItoken
          )
          GETheaders
          project_id <- gsub("/quota", "", update$message$text) %>% as.integer()
          if (is.na(project_id)) {
               
               bot$sendMessage(chat_id = update$message$chat$id, text = "Укажите ID проекта после команды /quota") 
               
          } else {
               
               query <- paste0(GETurl,"/",project_id,"/counters")
               
               
               response <- GET(query, add_headers(.headers = GETheaders))
               content <- content(response, "text")
               parsed_content <- fromJSON(content)
               
               response_text <- as.data.frame(parsed_content$body)
               
               messages <- ifelse(!is.na(response_text$quota),
                                  paste0(response_text$name,
                                  " - Квота: ",
                                  response_text$quota
               ),NA)
               
               messages <-  na.omit(messages)
               
               for (message in messages) {
                    
                   
                    bot$sendMessage(chat_id = update$message$chat$id, text = message)
                    Sys.sleep(0.25)
                    
                    
               }
               
          }
          
     }
}



#### common functions ####
start <- function(bot, update){
     saveRDS(update[["message"]][["from"]][["id"]],paste0(updates_path, update[["message"]][["from"]][["id"]], "_user_start_request_id.rds")) 
     bot$sendMessage(chat_id = update$message$chat_id,
                     text = sprintf("Hello %s!", update$message$from$first_name))
}


# Define a function to get updates from Telegram
get_updates <- function(tbot) {
     
     if (exists("last_update_id.rds")) {
          last_update_id <- readRDS(paste0(update_path,"last_update_id.rds"))
     }
     
     # Get updates from Telegram
     updates <- tbot$getUpdates()
     
     # Save the last update ID
     if (length(updates) > 0) {
          last_update_id <- max(sapply(updates, function(update) update$update_id))
          saveRDS(last_update_id, paste0(update_path,"last_update_id.rds"))
     }
     
     return(updates)
}

create_user_profile <- function(user_id, user_name, user_message) {
     user_profile <- data.table(
          user_id = user_id,
          user_name = user_name,
          win = user_message,
          win_date = "",
          win_timestamp = NA,
          win_link = NA,
          message_date = as.Date.POSIXct(Sys.time())
     )
     user_profile
}


#### mia bot helpers ####
mia_path <- "mia_bot/"

mia_image_pool <- data.table(
   mia_file_user_id = as.integer(1),
   mia_file_user_name = "",
   mia_file_id = as.integer(1),
   mia_file_path = "",
   mia_file_info = "",
   mia_file_timestamp = as.integer(1),
   message_date = as.Date.POSIXct(Sys.time()),
   mia_file_name = "",
   mia_file_price = as.integer(1)
)

mia_image_pool <- mia_image_pool[-1]
mia_image_pool <- readRDS(paste0(mia_path, "mia_image_pool.rds"))
# data_part <- readRDS(paste0(mia_path, "mia_image_pool.rds"))
# data_part$mia_file_name = ""
# data_part$mia_file_price = as.integer(1)

# Create a data.table to manage user access to files
mia_user_access <- data.table(
   user_id = integer(),
   user_name = character(),
   accessible_file_id = "" # This column will store a list of file_ids
)

# saveRDS(mia_user_access,paste0(mia_path, "mia_user_access.rds"))
mia_user_access <- readRDS(paste0(mia_path, "mia_user_access.rds"))

# Define the function to handle incoming messages and save images
save_image_to_local <- function(bot, update) {
   # Get the message object
   message <- update$message
   
   # Check if the message contains a photo
   if (!is.null(message$photo)) {
      # Get the highest resolution photo
      file_id <- message$photo[[length(message$photo)]]$file_id
      
      # Download the photo file
      file_info <- bot$getFile(file_id)
      file_path <- file_info$file_path
      
      # Define the local path to save the image
      local_file_path <- paste0("mia_bot/images/", basename(file_path))
      
      # Download the file to the local directory
      bot$getFile(file_id, destfile = local_file_path)
      
      # Append the information to the data.table
      mia_image_pool <<- rbind(
         mia_image_pool,
         data.table(
            mia_file_user_id = message$from$id,
            mia_file_user_name = ifelse(!is.null(message$from$username), message$from$username, ""),
            mia_file_id = file_id,
            mia_file_path = local_file_path,
            mia_file_info = file_info$file_path,
            mia_file_timestamp = as.integer(Sys.time()),
            message_date = as.Date(Sys.time()),
            mia_file_name = "",
            mia_file_price = as.integer(1)
            
         )
      )
      
      saveRDS(mia_image_pool,paste0(mia_path, "mia_image_pool.rds"))
      
      grant_access(417704252, "Admin", file_id)
      grant_access(1304572225, "Owner", file_id)
      
      Sys.sleep(60)
      # Notify the user
      bot$sendMessage(chat_id = message$chat_id, 
                      text = "Image saved")
   } else {
      # Notify the user if there's no photo
      bot$sendMessage(chat_id = message$chat_id, 
                      text = "No photo found in the message!")
   }
   # updater$stop_polling()
}

# Function to get image details from mia_image_pool by file_id
get_image_by_file_id <- function(file_id) {
   # Check if the file_id exists in the mia_image_pool
   result <- mia_image_pool[mia_file_id == file_id]
   
   # Return the result
   if (nrow(result) == 0) {
      return("No record found for the given file_id.")
   } else {
      return(result)
   }
}

# Example usage:
# Assuming mia_image_pool has some records and you have a valid file_id
# file_id <- "example_file_id"
# print(get_image_by_file_id(file_id))


preview_pictures <- function(bot, update) {
   # Check if there are any pictures
   if (nrow(mia_image_pool) == 0) {
      bot$sendMessage(chat_id = update$message$chat_id, 
                      text = "No pictures available to preview.")
      return(NULL)
   }
   
   # Prepare preview for each picture
   for (i in seq_len(nrow(mia_image_pool))) {
      # Read and resize image to 10x10 pixels
      
      img <- image_read(mia_image_pool$mia_file_path[i])
      img <- image_resize(img, "15x15!")
      
      # Save resized image temporarily
      temp_path <- tempfile(fileext = ".png")
      image_write(img, path = temp_path, format = "png")
      
      # Send the image with its name and price
      bot$sendPhoto(chat_id = update$message$chat_id, 
                    photo = temp_path,
                    caption = paste("Name:", mia_image_pool$mia_file_name[i], 
                                    "\nPrice:", mia_image_pool$mia_file_price[i]))
   }
}

show_pictures <- function(bot, update) {
   message <- update$message
   
   user_id <- message$from$id
   # Check if there are any pictures
   if (nrow(mia_image_pool) == 0) {
      bot$sendMessage(chat_id = update$message$chat_id, 
                      text = "No pictures available to preview.")
      return(NULL)
   }
   
   # Prepare preview for each picture
   for (i in seq_len(nrow(mia_image_pool))) {
 
      img_id <- mia_image_pool$mia_file_id[i]
           
      if(has_access(user_id,img_id)) {
                # img <- image_read(mia_image_pool$mia_file_path[i])
                # img <- image_resize(img, "15x15!")
                
                # Save resized image temporarily
                # temp_path <- tempfile(fileext = ".png")
                # image_write(img, path = temp_path, format = "png")
                
                # Send the image with its name and price
                bot$sendPhoto(chat_id = update$message$chat_id, 
                              photo = mia_image_pool$mia_file_path[i],
                              caption = paste("Name:", mia_image_pool$mia_file_name[i], 
                                              "\nPrice:", mia_image_pool$mia_file_price[i]))
      }
   }
}


save_picture_info <- function(bot, update) {
   # Parse the user command
   message <- update$message
   text <- message$text
   
   # Command format: /image_name_save <id> <name> <price>
   cmd_parts <- strsplit(text, " ")[[1]]
   if (length(cmd_parts) != 4) {
      bot$sendMessage(chat_id = message$chat_id, 
                      text = "команда: /image_name_save <id> <name> <price>")
      return(NULL)
   }
   
   # Extract name and price
   picture_id <- as.numeric(cmd_parts[2])
   picture_name <- cmd_parts[3]
   picture_price <- as.numeric(cmd_parts[4])
   
   # Check for a photo in the message
   if (!is.null(picture_id) & is.numeric(picture_id) & picture_id>0 & picture_id%%1 == 0 & nrow(mia_image_pool[picture_id]) == 1) {
 
      
      # Save to data.table
      mia_image_pool[picture_id]$mia_file_name <<- picture_name
      mia_image_pool[picture_id]$mia_file_price <<- picture_price
      saveRDS(mia_image_pool,paste0(mia_path, "mia_image_pool.rds"))
      
      bot$sendMessage(chat_id = message$chat_id, 
                      text = paste("Имя картинки:", picture_name, 
                                   "Цена:", picture_price,
                                   " Сохранено."))
   } else {
      bot$sendMessage(chat_id = message$chat_id, 
                      text = "Неправильно набрана команда /image_name_save <id> <name> <price>")
   }
}



# Function to grant a user access to a file
grant_access <- function(user_id, user_name, file_id) {

      # Add a new row for the user with the file_id
      mia_user_access <<- rbind(
         mia_user_access,
         data.table(user_id = user_id, user_name = user_name, accessible_file_id = file_id)
      )
      saveRDS(mia_user_access,paste0(mia_path, "mia_user_access.rds"))
   
}

# Function to check a user's access to a specific file
has_access <- function(user_id, file_id) {
   user_row <- mia_user_access[user_id == user_id]
   
   if (nrow(user_row) == 0) {
      return(FALSE) # User not found
   }
   
   # Check if the file_id is in the user's list of accessible files
   return(file_id %in% user_row$accessible_file_id)
}

# Function to list all files a user has access to
list_user_files <- function(user_id) {
   user_row <- mia_user_access[user_id == user_id]
   
   if (nrow(user_row) == 0) {
      return("User not found or no files accessible.")
   }
   
   return(user_row$accessible_file_id)
}

# # Example Usage:
# # Grant access
# grant_access(101, "user_1", "file_123")
# grant_access(101, "user_1", "file_456")
# grant_access(102, "user_2", "file_123")
# 
# # Check access
# print(has_access(101, "file_123")) # TRUE
# print(has_access(102, "file_456")) # FALSE
# 
# # List user files
# print(list_user_files(101)) # "file_123", "file_456"
# print(list_user_files(102)) # "file_123"