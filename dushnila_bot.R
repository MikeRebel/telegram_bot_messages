library(telegram.bot)
library(data.table)

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

# Set up the channel
channel_name <- "Чат душнил"

# Set up the message
message_text <- " ты точно душнила дня "

# change i to update group_chat_id from correct update number in updates[[i]]
if (updates[[1]][["message"]][["chat"]][["title"]] == channel_name) {
     group_chat_id <- updates[[1]][["message"]][["chat"]][["id"]]
} else {
     print(paste0("Напишите что-то в группе ",channel_name))
}

dt_messages <- data.table(
     user_id = as.integer(1),
     user_message = "",
     name = ""
     
)
dt_messages <- dt_messages[-1]

rm(last_date)
handle_message <- function(dbot, update) {
     user_message <- as.character(update$message$text)
     chat_id <- update$message$chat_id
     user_id <- update$message$from_user
     user_name <- update$message$from$username
     if (length(user_message) == 0) {user_message = " "}
     if (length(user_name) == 0) {user_name = " "}
     # Do something with the message, chat_id, and user_id
     print(user_message)

     if (chat_id == group_chat_id) {
          # dbot$sendMessage(chat_id = chat_id, text = message)
          DT_m_current = data.table(
               user_id = user_id,
               user_message = user_message,
               name = user_name
          )
          
          dt_messages <<- rbind(dt_messages, DT_m_current)
          if (get_random_number() == 1) {
               
               bot_message <- dt_messages[sample(nrow(dt_messages), 1)]
               while (length(bot_message$user_message) > 0 & length(bot_message$name) > 0) {
                    bot_message <- dt_messages[sample(nrow(dt_messages), 1)]
                    }
               
               dbot$sendMessage(chat_id = chat_id, text = paste0("@",bot_message$name, message_text, 'если думаешь, что "', bot_message$user_message,'"'))
               dt_messages <<- data.table(
                    user_id = as.integer(1),
                    user_message = "",
                    name = ""
                    
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

updater$start_polling()

