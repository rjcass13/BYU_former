library(tokenizers)
library(dplyr)
library(stringr)

cplt <- read.csv('Unit_6/Homework/CompanyComplaints.csv')
tokens = sapply(cplt$Complaint,tokenize_words)
tally.word = function(X,target.word){
    sum(stringr::str_count(X,pattern = target.word))
}
#tokens = sapply(which$Complaint,tokenize_words)

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
               x.length,x.debtcollect,x.creditreport,x.moneyComplaint,x.mortgageComplaint,
               x.studentloans,x.accountComplaint,x.creditcard, x.loanComplaint, x.vehicleComplaint)
# whichData <-cbind(x.debt, x.collect,x.credit, x.account, x.company,x.report,x.info,x.consumer,x.money,x.bank,x.paypal,
#                   x.sent,x.back,x.loan,x.mortgage,x.payment,x.home,x.navient,x.student,x.check,x.funds,x.card,
#                   x.payday, x.title, x.personal, x.vehicle,
#                   x.length,x.debtcollect,x.creditreport,x.moneyComplaint,x.mortgageComplaint,
#                   x.studentloans,x.accountComplaint,x.creditcard, x.loanComplaint, x.vehicleComplaint)
# whichData <- whichData |>
#   as.data.frame()


#Words to Add
# Debt Collection: debt, credit, account, collection, company, report
# Credit Report: credit, report, account, information, reporting, consumer
# Money: account, money, bank, paypal, sent, back
# Mortgage: loan, mortgage, payment, account, payments, home
# Student Loans: loan, loans, payments, payment, navient, student, account, credit
# Account: account, bank, check, money, funds, checking
# Credit Card: card, credit, account, bank, payment, received
saveRDS(fullXData,"Unit_6/Homework/fullXData.rds")
# 
# temp <- readRDS("Unit_6/Homework/fullXData.rds")
# library(rpart)
# library(rpart.plot)
# mod = rpart(cplt$Department ~ x.length + x.collect + x.debt  + x.credit + x.loan,control = rpart.control(cp = 0.0))
# rpart.plot(mod)
# 
# table(cplt$Department)
