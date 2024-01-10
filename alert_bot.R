source("secret.R")
source("helpers.R")

# Создаем объект бота
bot<- Bot(token = bot_token)
updates <- bot$getUpdates()
print(bot$getMe())

if (length(updates) > 0) {
     group_chat_id <- chat_id <- updates[[1]][["message"]][["chat"]][["id"]]
} else {
     print("Напишите что-то в группе бота")
}
print(group_chat_id)
print(chat_id)

# Отправляем сообщение в группу
dinner<-bot$send_message(chat_id = group_chat_id, text = "Обед начинается!")

# Удаляем сообщение через час
a <- function() {
     Sys.sleep(900)
     bot$delete_message(chat_id = group_chat_id, message_id = dinner$message_id)
}

a()
# Отправляем сообщение в группу
bot$send_message(chat_id = group_chat_id, text = "Мы на кухне!")

# Удаляем сообщение через 15 минут
bot$delete_message(chat_id = group_chat_id, message_id = SECOND_MESSAGE_ID, time = 900)