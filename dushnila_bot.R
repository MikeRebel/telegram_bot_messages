# Set up the bot
source("secret.R")
source("helpers.R")
dbot <- Bot(token = dbot_token)
print(dbot$getMe())
updates <- dbot$getUpdates()

if (length(updates) > 0) {
     group_chat_id <- chat_id <- updates[[1]][["message"]][["chat"]][["id"]]
} else {
     print("Напишите что-то в группе бота")
}
print(group_chat_id)
print(chat_id)

# Set up the channel
channel_name <- "Чат душнил"

# Set up the message
message_text <- ' Tы заcлуженно становишься почётным душнилой дня номер '
message_text2 <- ' ты заcлуженно становишься почётным душнилой дня номер '

# change i to update group_chat_id from correct update number in updates[[i]]
if (updates[[1]][["message"]][["chat"]][["title"]] == channel_name) {
     group_chat_id <- updates[[1]][["message"]][["chat"]][["id"]]
} else {
     print(paste0("Напишите что-то в группе ",channel_name))
}
print(group_chat_id)
# data.table to store messages
dt_messages <- data.table(
     user_id = as.integer(1),
     user_message = "",
     name = "",
     message_date = as.Date.POSIXct(Sys.time())
     
)
dt_messages <- dt_messages[-1]
rm(last_date)

# Store messages to dt_messages and select dushnila dnya 3 times a day if get_random_number() return 1
handle_message <- function(dbot, update) {
     user_message <- as.character(update$message$text)
     chat_id <- update$message$chat_id
     user_id <- update$message$from_user
     user_name <- update$message$from$username
     message_date <- as.Date.POSIXct(update$message$date)
     random_message_seed <- sample(1:100,1)
     
     dt_messages <<- rows(dt_messages, message_date == as.Date.POSIXct(Sys.time()))

     
     if (random_message_seed < nrow(dt_messages)) {
          is.dushnila.chosen <- get_random_number()
          print("randomize")
     } else {
          is.dushnila.chosen <- 0
     }
     
     if (length(user_message) == 0) {user_message = " "}
     if (length(user_name) == 0) {user_name = " "}
     if (length(message_date) == 0) {message_date = as.Date.POSIXct(Sys.time())}
     
     # Do something with the message, chat_id, and user_id
     
     print(paste0(user_message, 
                  " это ", 
                  nrow(dt_messages), 
                  " строка в массиве сообщений. ",
                  "Зерно ",
                  random_message_seed,
                  " Душнила выбран = ", 
                  is.dushnila.chosen
                  )
           )
     
     if (chat_id == group_chat_id) {
          # dbot$sendMessage(chat_id = chat_id, text = message)
          DT_m_current = data.table(
               user_id = user_id,
               user_message = user_message,
               name = user_name,
               message_date = message_date
          )
          
          dt_messages <<- rbind(dt_messages, DT_m_current)
          
          
          if (is.dushnila.chosen == 1) {
               
               
               bot_message <- dt_messages[sample(nrow(dt_messages), 1)]
               if (nchar(bot_message$user_message) <= 1 | nchar(bot_message$name) <= 1) {
                    while (nchar(bot_message$user_message) <= 1 | nchar(bot_message$name) <= 1) {
                         bot_message <- dt_messages[sample(nrow(dt_messages), 1)]
                    }
               }
               saveRDS(bot_message,file = paste0(dushno_choises_save_path,
                                                 "winner_poll_", 
                                                 count_ones, 
                                                 "_", 
                                                 format(Sys.time(), 
                                                        "%a %b %d"
                                                        ), 
                                                 ".rds"
                                                 )
                       )
               
               dbot$sendMessage(chat_id = chat_id, text = paste0("@",
                                                                 bot_message$name, 
                                                                 " ", 
                                                                 gsub("\t", 
                                                                      "", 
                                                                      dushno_congratulations[sample(1:48,1),phrase]
                                                                      ), 
                                                                 ' и cчитая, что ', 
                                                                 bot_message$user_message,
                                                                 
                                                                 ifelse(grepl("$[[:punct:]]", bot_message$user_message),
                                                                        message_text,
                                                                        message_text2
                                                                        ),
                                                                 count_ones
  
                                                                 )
                                )
               saveRDS(dt_messages,file = paste0(dushno_choises_save_path,
                                                 "winner_poll_", 
                                                 count_ones, 
                                                 "_candidates_", 
                                                 format(Sys.time(), 
                                                        "%a %b %d"
                                                 ), 
                                                 ".rds"
               )
               
               )
               dt_messages <<- data.table(
                    user_id = as.integer(1),
                    user_message = "",
                    name = "",
                    message_date = as.Date.POSIXct(Sys.time())
               )
               dt_messages <<- dt_messages[-1]
               
          }
     }
}

rm(message_handler)
message_handler <- MessageHandler(handle_message, MessageFilters$all)

rm(updater)
updater <- Updater(token = dbot_token)
updater <- updater + message_handler

# start bot 
updater$start_polling()

# dbot$sendMessage(chat_id = chat_id, text = "Вика, поправляйся скорее")

# dbot$sendMessage(chat_id = chat_id, text = "")
