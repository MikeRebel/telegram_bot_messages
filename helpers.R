library(telegram.bot)
library(httr)
library(jsonlite)
library(data.table)
library(maditr)
updates_path = "updates/"

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
