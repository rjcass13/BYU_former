library(tokenizers)
library(dplyr)
library(stringr)

cplt <- read.csv('CompanyComplaints.csv')
tokens = sapply(cplt$Complaint,tokenize_words)
tally.word = function(X,target.word){
    sum(stringr::str_count(X,pattern = target.word))
}
#tokens = sapply(which$Complaint,tokenize_words)
distinct_words = function(token) {
    length(unique(token))
}
avg_word_length <- function(X) {
    mean(sapply(X, nchar))
}


x.debt = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'debt'))
x.collect = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'collect|collection|collecting'))
x.credit = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'credit'))
x.account = x.debt = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'account|accounts'))
x.company = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'company|companies'))
x.report = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'report|reporting'))
x.info = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'info|information'))
x.consumer = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'consumer|consumers|customer|customers'))
x.money = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'money|currency'))
x.bank = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'bank|banks'))
x.paypal = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'paypal|venmo|zelle'))
x.sent = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'sent|sending|receive|receiving|recieved'))
x.back = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'back|return'))
x.loan = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'loan|lease|loans'))
x.mortgage = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'mortgage|mortgages'))
x.payment = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'payment|payments|down'))
x.home = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'home|homes|house|apartment|houses'))
x.navient = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'navient'))
x.student = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'student|university|college|school'))
x.check = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'check|checks|checking'))
x.funds = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'funds|fund|funding'))
x.card = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'card|cards'))
x.payday = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'payday|paycheck|pay'))
x.title= as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'title|titles'))
x.personal= as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'personal'))
x.vehicle = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'vehicle|vehicles|car|cars|truck|trucks|van|vans'))


x.length = as.vector(sapply(X = tokens, FUN = length))
x.word_length <- as.vector(sapply(X = tokens, FUN = avg_word_length))
n <- length(tokens)
x.unique_word_count <- vector(mode = "numeric", length = n)
for (i in 1:n) {
    x.unique_word_count[i] <- distinct_words(tokens[[i]])
}
x.repeat_words <- x.length - x.unique_word_count
x.capital_count <- str_count(cplt$Complaint, "[A-Z]")
x.char_count <- nchar(cplt$Complaint)
x.capital_ratio <- x.capital_count/x.char_count
x.sentence_count <- str_count(cplt$Complaint, "[.!?]\\s*[^.!?]") + 1


x.debtcollect <- x.debt + x.credit + x.account + x.collect + x.company + x.report
x.creditreport <- x.credit + x.report + x.account + x.info + x.consumer
x.moneyComplaint <- x.account + x.money + x.bank + x.paypal + x.sent + x.back
x.mortgageComplaint <- x.loan + x.mortgage + x.payment + x.account + x.home
x.studentloans <- x.loan + x.payment + x.navient + x.student + x.account + x.credit
x.accountComplaint <- x.account + x.bank + x.check + x.money + x.funds
x.creditcard <- x.card + x.credit + x.account + x.bank + x.payment + x.sent
x.loanComplaint <- x.loan + x.payday + x.title + x.personal
x.vehicleComplaint <- x.vehicle + x.loan 

fullXData <- cbind(cplt$Department,
               x.debt, x.collect,x.credit, x.account, x.company,x.report,x.info,x.consumer,x.money,x.bank,x.paypal,
               x.sent,x.back,x.loan,x.mortgage,x.payment,x.home,x.navient,x.student,x.check,x.funds,x.card,
               x.payday, x.title, x.personal, x.vehicle,
               x.length, x.word_length, x.unique_word_count, x.repeat_words, x.capital_ratio, x.sentence_count,
               x.debtcollect,x.creditreport,x.moneyComplaint,x.mortgageComplaint,
               x.studentloans,x.accountComplaint,x.creditcard, x.loanComplaint, x.vehicleComplaint)

fullXData <- as.data.frame(fullXData)


colnames(fullXData) <- c('department', 'debt', 'collect', 'credit', 'account', 'company', 'report', 'info', 'consumer', 
    'money', 'bank', 'paypal', 'sent', 'back', 'loan', 'mortgage', 'payment', 'home', 'navient', 'student', 'check', 
    'funds', 'card', 'payday', 'title', 'personal', 'vehicle', 'length', 'word_length', 'unique_word_count', 'repeat_words', 'capital_count', 'sentence_count',
    'debtcollect', 'creditreport', 'moneycomplaint', 'mortgagaecomplaint', 'studentloans', 'accountcomplaint', 'creditcard', 'loancomplaint', 'vehiclecomplaint')
fullXData$department <- as.factor(fullXData$department)
fullXData <- fullXData %>%
    mutate(across(-all_of('department'), as.numeric))


saveRDS(fullXData,"fullXData.rds")




###############################
### Top Words by Department ###
###############################
# Find top words for each department

# dep_names <- c('debt', 'cred_rep', 'mon', 'mort', 'st_loan', 'acct', 'card')
# words_to_exclude <- c('xxxx', 'that', 'this', 'have', 'they', 'with', 'from')
# set.seed(1337)
# for (i in 1:7) {
#     dep_tokens <- tokens[which(comp$Department == departments[i])]
#     ind <- sample(1:length(dep_tokens), .2 * length(dep_tokens))
#     dep_tokens <- dep_tokens[ind]
#     words <- unlist(dep_tokens)

#     word_counts <- as.data.frame(table(words))
#     colnames(word_counts) <- c("word", "frequency")
#     word_counts$word <- as.character(word_counts$word)
#     word_counts <- word_counts[order(word_counts$frequency, decreasing = TRUE), ]
#     word_counts <- word_counts[which(nchar(word_counts$word) >= 4), ]
#     word_counts <- word_counts[-which(word_counts$word %in% words_to_exclude), ]
  
#     var_name <- paste0(dep_names[i], '_word_counts')
#     assign(var_name, word_counts)
#     cat('Done', dep_names[i], '\n')
# }

# dep_tokens <- tokens[which(comp$Department == departments[2])]
# nchar(as.character(acct_word_counts$word))


# Top words per department:
# Debt Collection: debt, credit, account, collection, company, report
# Credit Report: credit, report, account, information, reporting, consumer, accounts
# Money: account, money, bank, paypal, sent, back
# Mortgage: loan, mortgage, payment, account, payments, home
# Student Loans: loan, loans, payments, payment, navient, student, account, credit
# Account: account, bank, check, money, funds, checking
# Credit Card: card, credit, account, bank, payment, received