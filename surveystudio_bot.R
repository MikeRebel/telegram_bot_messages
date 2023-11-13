source("secret.R")
source("helpers.R")

bot <- Bot(token = sst_token)
print(bot$getMe())
updates <- bot$getUpdates()


# Команды