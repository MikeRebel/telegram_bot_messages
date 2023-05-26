library(telegram.bot)
library(data.table)


# Set up the bot
source("secret.R")
dbot <- Bot(token = dbot_token)
print(dbot$getMe())
updates <- dbot$getUpdates()

if (length(updates) > 0) {
     group_chat_id <- chat_id <- updates[[1]][["message"]][["chat"]][["id"]]
} else {
     print("Напишите что-то в группе бота")
}

# Set up the channel
channel_name <- "тестовая"
# channel_id <- updates[[1]][["message"]][["chat"]][["id"]]

# Set up the message
message_text <- " ты душнила дня"

# Replace YOUR_GROUP_CHAT_ID with the actual ID of the group chat
if (updates[[1]][["message"]][["chat"]][["title"]] == channel_name) {
     group_chat_id <- updates[[1]][["message"]][["chat"]][["id"]]
} else {
     print(paste0("Напишите что-то в группе ",channel_name))
}


dt_messages <- data.table(
     user_id = as.integer(1),
     user_message = ""
     
)
dt_messages <- dt_messages[-1]



handle_message <- function(dbot, update) {
     message <- update$message$text
     chat_id <- update$message$chat_id
     user_id <- update$message$from_user
     # Do something with the message, chat_id, and user_id
     print(message)

     if (chat_id == group_chat_id) {
          # dbot$sendMessage(chat_id = chat_id, text = message)
          DT_m_current = data.table(
               user_id = user_id,
               user_message = message
          )
          dt_messages <<- rbind(dt_messages, DT_m_current)
          
     }
}

rm(message_handler)
message_handler <- MessageHandler(handle_message, MessageFilters$all)

rm(updater)
updater <- Updater(token = dbot_token)
updater <- updater + message_handler

updater$start_polling()

