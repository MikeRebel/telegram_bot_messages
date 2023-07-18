library(telegram.bot)
library(data.table)
library(maditr)

source("secret.R")
source("helpers.R")

bot <- Bot(token = wbot_token)
print(bot$getMe())
updates <- bot$getUpdates()

user_profiles <- list()

# обрабатываем сообщения от пользователя. Если есть условные фразы, заносим данные в профиль.
handle_message <- function(bot, update) {
     user_message <- as.character(update$message$text)
     chat_id <- update$message$chat_id
     user_id <- update$message$from_user
     user_name <- update$message$from$username
     message_date <- as.Date.POSIXct(update$message$date)
     bot$sendMessage(chat_id = chat_id,  text = paste0(user_message, " | ",
                                                       user_id, " | ",
                                                       user_name, " | ",
                                                       message_date, " ! ")
     )
}

rm(updater)
updater <- Updater(token = wbot_token)

start_handler <- CommandHandler("start", start)

win_handler <- CommandHandler("win", win)
date_handler <- CommandHandler("date", date)
time_handler <- CommandHandler("time", time)
link_handler <- CommandHandler("link", link)

analyse_handler <- CommandHandler("analyse", analyse)
show_data_handler <- CommandHandler("show", show_data)

updater <- updater + MessageHandler(handle_message, MessageFilters$text)
updater <- updater + start_handler
updater <- updater + win_handler
updater <- updater + date_handler
updater <- updater + time_handler
updater <- updater + link_handler
updater <- updater + analyse_handler
updater <- updater + show_data_handler


updater$start_polling()
