library(torch)
library(tokenizers)

# Load data
comp <- readRDS("fullXData.rds")
###########################
## Actual Model Building ##
###########################
set.seed(1337)
ind <- sample(1:nrow(comp), .1 * nrow(comp), replace = FALSE)

# Define Test and Train splits
y_train <- comp$department[-ind]
y_test <- comp$department[ind]
x_train <- comp[-ind, -1]
x_test <- comp[ind, -1]

# Standardize and convert into tensors
x_train_mean <- apply(x_train, 2, mean)
x_train_sd <- apply(x_train, 2, sd)
x_train_stand <- sweep(sweep(x_train, 2, x_train_mean, "-"), 2, x_train_sd, "/")
x_test_stand <- sweep(sweep(x_test, 2, x_train_mean,"-"), 2, x_train_sd, "/")

x_train <- torch_tensor(as.matrix(x_train_stand))
y_train <- torch_tensor(y_train)

x_test <- torch_tensor(as.matrix(x_test_stand))
y_test <- torch_tensor(y_test)


net = nn_module(
  "class_net",
  initialize = function(){
    self$linear1 = nn_linear(41,128) # Start with columns in the X matrix
    self$linear2 = nn_linear(128,64)
    #self$linear3 = nn_linear(64,64)
    self$linear4 = nn_linear(64,9) # End with classification categories
    
  },
  forward = function(x) {
    x %>%
      self$linear1() %>%
      nnf_hardtanh() %>%
      self$linear2() %>%
      nnf_hardtanh() %>%
      #self$linear3() %>%
      #nnf_hardtanh() %>%
      self$linear4() %>%
      nnf_softmax(2)
  }
)

#### INITIALIZE THE MODEL
torch_manual_seed(2)
model = net()
# Create the cost function and the optimizer
criterion = nn_cross_entropy_loss()
optimizer = optim_adam(model$parameters, lr = 0.005)
### TRAIN THE MODEL
#specify the number of epochs
epochs = 1000
loss_values <- numeric()
accuracy_values <- numeric()
test_loss_values <- numeric()

for(i in 1: epochs){
    optimizer$zero_grad()
    y_pred = model$forward(x_train)
    loss = criterion(y_pred, y_train)
    loss$backward()
    optimizer$step()
    
    ### administration:
    # Append loss and accuracy values to vectors
    loss_values <- c(loss_values, as.numeric(loss))
  
  
    # Calculate validation loss
    test_outputs = model(x_test)
    test_loss = criterion(test_outputs, y_test)
  
    # Append the current validation loss to the vector
    test_loss_values <- c(test_loss_values, test_loss$item())
  
    #check training
    if(i %% 100 == 0){
        winners = y_pred$argmax(dim=2)
        corrects = (winners == y_train)
        accuracy = corrects$sum()$item() / y_train$size()
        
        cat("Epoch:", i, "Loss:", loss$item(),"Accuracy:", accuracy, "\n")
        accuracy_values <- c(accuracy_values, accuracy)
    }
}

# # Plot the loss values
# plot(1:epochs, loss_values, type="l", col="blue", xlab="Epochs", 
#      ylab="Loss", main="Loss vs Epochs")
# lines(1:epochs, test_loss_values, col="red")
# legend("topright", legend=c("Training Loss", "Validation Loss"),
#        col=c("blue", "red"), lty=1)


### Test Set Evaluation
y_test_pred = model$forward(x_test)     #classify the new data
loss_test = criterion(y_test_pred, y_test) #get the loss for this classification
winners_test = y_test_pred$argmax(dim=2)   #get the category with highest probability
corrects_test = (winners_test == y_test)
accuracy_test = corrects_test$sum()$item() / y_test$size()
cat("Test", "Loss:", loss_test$item(),"Accuracy:", accuracy_test, "\n")









library(rpart)
library(rpart.plot)
tree.model <- rpart(department~., data=comp, subset=ind, method="class", control=rpart.control(cp=.025))
final.rpart <- prune(tree.model, cp=tree.model$cptable[which.min(tree.model$cptable[,"xerror"]),"CP"])
rpart.plot(final.rpart)
