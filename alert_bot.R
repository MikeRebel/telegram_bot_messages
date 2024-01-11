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

a()

b()

# функции отправки сообщений
a <- function() {
     dinner<-bot$send_message(chat_id = group_chat_id, text = "Обед начинается.")
     Sys.sleep(1200)
     bot$delete_message(chat_id = group_chat_id, message_id = dinner$message_id)
}

b <- function() {
     dinner<-bot$send_message(chat_id = group_chat_id, text = "На кухне.")
     Sys.sleep(900)
     bot$delete_message(chat_id = group_chat_id, message_id = dinner$message_id)
}



