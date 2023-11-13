source("secret.R")
source("helpers.R")

bot <- Bot(token = sst_token)
print(bot$getMe())
updates <- bot$getUpdates()

if (length(updates) > 0) {
     group_chat_id <- chat_id <- updates[[1]][["message"]][["chat"]][["id"]]
} else {
     print("Напишите что-то в группе бота")
}
print(group_chat_id)
print(chat_id)

start_handler <- CommandHandler("start", start)
list_handler <- CommandHandler("list", list)
# Команды
# start - Начало работы. Для управления проектом используйте id проекта из команды /list
# list - список открытых проектов из Survey Studio

#### daemon ####
rm(updater)
updater <- Updater(token = sst_token)
updater <- updater + start_handler
updater <- updater + list_handler
updater$start_polling()
