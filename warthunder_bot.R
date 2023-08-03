library(telegram.bot)
library(data.table)
library(maditr)

source("secret.R")
source("helpers.R")

bot <- Bot(token = wbot_token)
print(bot$getMe())
updates <- bot$getUpdates()


# Команды
# start - Strat bot
# win  - выигрыш или поражение, 1 цифра, 0 или 1.
# date - день и месяц завершения боя, 4 цифры, например 0711
# timestamp  - час окончания боя, например 9 или 15.
# link - код реплея из ссылки на сайте warthunder.ru/ru/tournament/replay/, например 93438164578768068.
# analyse - показать результаты анализа.
# show - показать данные для анализа.

warthunder_user_profile <- data.table(
     user_id = as.integer(1),
     user_name = "",
     win = as.integer(1),
     win_date = "",
     win_timestamp = as.integer(1),
     win_link = as.integer(1),
     message_date = as.Date.POSIXct(Sys.time())
)
warthunder_user_profile <- warthunder_user_profile[-1]

# for setup execute 
# saveRDS(warthunder_user_profile,paste0(warthunder_save_path, "warthunder_user_profile.rds"))

warthunder_user_profile <- readRDS(paste0(warthunder_save_path, "warthunder_user_profile.rds"))

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
timestamp_handler <- CommandHandler("time", w_bot_timestamp)
link_handler <- CommandHandler("link", link)

analyse_handler <- CommandHandler("analyse", analyse)
show_data_handler <- CommandHandler("show", show_data)

updater <- updater + MessageHandler(handle_message, MessageFilters$text)
updater <- updater + start_handler
updater <- updater + win_handler
updater <- updater + date_handler
updater <- updater + timestamp_handler
updater <- updater + link_handler
updater <- updater + analyse_handler
updater <- updater + show_data_handler


updater$start_polling()

# saveRDS(warthunder_user_profile,paste0(warthunder_save_path, "warthunder_user_profile.rds"))
