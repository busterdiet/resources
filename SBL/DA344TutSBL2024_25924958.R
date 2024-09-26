library(FNN)

set.seed(5)

my_csv_reader <- function(file_path){
  data <- read.csv(file_path, header=TRUE, sep=",")
  return(data)
}

my_separate_X_y <- function(data){
  X <- data[,1:(ncol(data)-1)]
  y <- data[,ncol(data)]
  return(list(X=X, y=y))
}

my_KNN_classifications <- function(X, q, y, k){
  
  knn_res <- knn(X, q, y, k)
  print(table(knn_res))
  return(knn_res)
}

data <- my_csv_reader("/Users/justin/Library/Mobile Documents/com~apple~CloudDocs/University/3rd Year/2nd Semester/Data Analytics 344/Tutorials/iris.csv")
separated_data <- my_separate_X_y(data)

train_indices <- sample(1:nrow(separated_data$X), 0.8 * nrow(separated_data$X))
X_train <- separated_data$X[train_indices, ]
y_train <- separated_data$y[train_indices]
X_test <- separated_data$X[-train_indices, ]
y_test <- separated_data$y[-train_indices]


my_KNN_classifications(X_train, X_test, y_train, k = 3)

knn_pred <- knn(train = X_train, test = X_test, cl = y_train, k = 3)
accuracy <- sum(knn_pred == y_test) / length(y_test)
print(paste("Accuracy:", accuracy))


