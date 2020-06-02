library(ggplot2) # Data visualization
library(readr)
library(dplyr)# CSV file I/O, e.g. the read_csv function

#Reading the dataset
train=read.csv("/home/grigor/Downloads/dataset.csv", header = TRUE)
head(train)

dim(train)

str(train)

summary(train)

nrow(train)

ncol(train)

colSums(sapply(train,is.na))

tail(train)



var_names = names(train)
var_names

numeric_var = select_if(train, is.numeric)
colSums(sapply(numeric_var, is.na))

categorical_var = select_if(train, is.factor)
colSums(sapply(categorical_var, is.na))
  
summary(categorical_var)

summary(numeric_var)

#Exploring the target feature
table(train$Type)

cat_var_names = names(categorical_var)
cat_var_names

with(train, table(Type, CHARSET))

with(train, table(Type, WHOIS_COUNTRY))

#PLOTTING THE TARGET FEATURE
#install.packages('repr')
library(repr)
options(repr.plot.width=5, repr.plot.height=5)
ggplot(train, aes(x = Type)) + geom_bar(color='blue',fill = "#FF6666")

ggplot(train, aes(x = CHARSET, fill = Type)) + geom_bar(position = "fill") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_fill_gradient(low="green", high="yellow")

options(repr.plot.width=7, repr.plot.height=4)
ggplot(train, aes(x = WHOIS_COUNTRY, fill = Type)) + geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_gradient(low="blue", high="red")

#PLOTTING THE TARGET FEATURE WITH THE NUMERIC FEATURES.
ggplot(train, aes(x = URL_LENGTH)) + geom_histogram(binwidth = 5, fill = "#DD1111")+ scale_fill_hue(l=40, c=35)

ggplot(train, aes(x = NUMBER_SPECIAL_CHARACTERS)) + geom_histogram(binwidth = 0.5, fill = "gold")+ scale_fill_hue(l=40, c=35)

library(purrr)
library(tidyr)

train %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

train %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density()                         # as density

#PLOTTING THE NUMERIC AND CAGEORICAL FEATURES
ggplot(train, aes(x=Type,y=URL_LENGTH)) + geom_point()

ggplot(train, aes(x=REMOTE_IPS,y=URL_LENGTH)) + geom_point(fill='red')

ggplot(train, aes(x=REMOTE_IPS,y=DNS_QUERY_TIMES)) + geom_point(fill='red')

ggplot(train, aes(x=Type, y=DNS_QUERY_TIMES, fill=Type)) + 
  geom_boxplot(alpha=0.3) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  theme(legend.position="none")+
  ggtitle("Boxplot of Type by DNS_QUERY_TIMES")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(train, aes(x=Type, y=APP_PACKETS, fill=Type)) + 
  geom_boxplot(alpha=0.3) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  theme(legend.position="none")+
  ggtitle("Boxplot of Type by APP_PACKETS")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(train, aes(x=log(REMOTE_APP_BYTES),y=REMOTE_APP_PACKETS)) + geom_point(color='blue')+ facet_wrap(~Type)

ggplot(train, aes(x=log(SOURCE_APP_BYTES),y=SOURCE_APP_PACKETS)) + geom_point(color='gold')+ facet_wrap(~Type)

ggplot(train, aes(x=log(DIST_REMOTE_TCP_PORT),y=SOURCE_APP_PACKETS)) + geom_point(color='green')+facet_wrap(~Type)

ggplot(train, aes(x=log(DIST_REMOTE_TCP_PORT),y=REMOTE_IPS)) + geom_point(color='red')+ facet_wrap(~Type)

ggplot(train, aes(x=log(CONTENT_LENGTH),y=URL_LENGTH)) + geom_point(color='orange')+ facet_wrap(~Type)

#CORRELOGRAM PLOT FOR NUMERIC FEATURES.
options(repr.plot.width=7, repr.plot.height=7)
numeric_var1 <- subset(numeric_var, select = -c(CONTENT_LENGTH, DNS_QUERY_TIMES))
library(corrplot)
M <- cor(numeric_var1)
corrplot(M, method = "circle")

options(repr.plot.width=8, repr.plot.height=8)
corrplot(M, method = "number")

options(repr.plot.width=5, repr.plot.height=5)


#DATA PREPARATION

missing_row = train[!complete.cases(train),]
head(missing_row)

nrow(missing_row)
sum(is.na(train))/(nrow(train)*ncol(train))
pMiss = function(x){sum(is.na(x))/length(x)*100}
apply(train,2,pMiss)

options(repr.plot.width=8, repr.plot.height=5)
#install.packages('VIM')
#library(VIM)
#aggr_plot = aggr(train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
 #                labels=names(train), cex.axis=.7, gap=1,
  #               ylab=c("Histogram of missing data","Pattern"))

#install.packages('Hmisc')
library(Hmisc)
train$DNS_QUERY_TIMES=impute(train$DNS_QUERY_TIMES, 0)
train$SERVER=impute(train$SERVER, 'nginx')
train$CONTENT_LENGTH=impute(train$CONTENT_LENGTH, mean)

head(train$CONTENT_LENGTH, 100)

ggplot(train, aes(REMOTE_APP_PACKETS, y=SOURCE_APP_PACKETS)) + 
  geom_boxplot(color = 'red')
#boxplot(train$URL_LENGTH, data=train, color = 'red')

boxplot(train$URL_LENGTH, data=train, color = 'red', xlab = 'URL LENGTH',col = 'green')

boxplot(train$SOURCE_APP_PACKETS, data=train, xlab = 'Source App Packets', col = 'gold')

boxplot(TCP_CONVERSATION_EXCHANGE~Type,data=train, main="Tcp Conversation exchange vs Type",
        xlab="Website Type", ylab="TCP conversation", col=(c("gold","darkgreen")))

boxplot(DIST_REMOTE_TCP_PORT~CHARSET,data=train, main="Dist Remote Tcp Port vs Type",
        xlab="Charset", ylab="Remote TCP Port", col=(c("gold","darkgreen")))
boxplot(REMOTE_APP_BYTES~Type,data=train, main="Remote App Bytes vs Type",
        xlab="Type", ylab="Remote App Bytes", col=(c("gold","darkgreen")))
boxplot(SOURCE_APP_PACKETS~Type,data=train, main="Source App Packets vs Type",
        xlab="Type", ylab="Source App Bytes", col=(c("gold","darkgreen")))


#############################   DEALING WITH OUTLIERS    ###################################
train_cap = train[,c("REMOTE_APP_PACKETS", "URL_LENGTH", "SOURCE_APP_PACKETS","REMOTE_APP_BYTES","SOURCE_APP_BYTES",
                     "APP_BYTES", "REMOTE_IPS", "DIST_REMOTE_TCP_PORT","TCP_CONVERSATION_EXCHANGE")]
train_other = train[,-c(2,11,12,13,14,15,16,17,18)]
head(train_other)

#check 
str(train_cap)
str(train_other)


#outlier capping fucntion
pcap <- function(x){
  for (i in which(sapply(x, is.numeric))) {
    quantiles <- quantile( x[,i], c(.05, .95 ), na.rm =TRUE)
    x[,i] = ifelse(x[,i] < quantiles[1] , quantiles[1], x[,i])
    x[,i] = ifelse(x[,i] > quantiles[2] , quantiles[2], x[,i])}
  x}

# Replacing extreme values with percentiles
train_capped = pcap(train_cap)
summary(train$URL_LENGTH)
summary(train_capped)

#join back with rest of data;
train_combine = cbind(train_other, train_capped)
summary(train_combine)

#############################   TRANSFORMATIONS    ###################################
#log transfomartions
train_combine$REMOTE_APP_PACKETS=log(train_combine$REMOTE_APP_PACKETS+1)
train_combine$URL_LENGTH=log(train_combine$URL_LENGTH+1)
train_combine$SOURCE_APP_PACKETS=log(train_combine$SOURCE_APP_PACKETS+1)
train_combine$TCP_CONVERSATION_EXCHANGE=log(train_combine$TCP_CONVERSATION_EXCHANGE+1)
train_combine$DIST_REMOTE_TCP_PORT=log(train_combine$DIST_REMOTE_TCP_PORT+1)
train_combine$REMOTE_APP_BYTES=log(train_combine$REMOTE_APP_BYTES+1)
train_combine$SOURCE_APP_BYTES=log(train_combine$SOURCE_APP_BYTES+1)
train_combine$APP_BYTES=log(train_combine$APP_BYTES+1)
train_combine$REMOTE_IPS=log(train_combine$REMOTE_IPS+1)

train_cap$REMOTE_APP_PACKETS=log(train_cap$REMOTE_APP_PACKETS+1)
train_cap$URL_LENGTH=log(train_cap$URL_LENGTH+1)
train_cap$SOURCE_APP_PACKETS=log(train_cap$SOURCE_APP_PACKETS+1)
train_cap$TCP_CONVERSATION_EXCHANGE=log(train_cap$TCP_CONVERSATION_EXCHANGE+1)
train_cap$DIST_REMOTE_TCP_PORT=log(train_cap$DIST_REMOTE_TCP_PORT+1)
train_cap$REMOTE_APP_BYTES=log(train_cap$REMOTE_APP_BYTES+1)
train_cap$SOURCE_APP_BYTES=log(train_cap$SOURCE_APP_BYTES+1)
train_cap$APP_BYTES=log(train_cap$APP_BYTES+1)
train_cap$REMOTE_IPS=log(train_cap$REMOTE_IPS+1)

str(train_combine)

####MOdeling
indexes = sample(1:nrow(train_combine), size=0.2*nrow(train_combine))

# Split data
x_test = train_combine[indexes,]
dim(x_test) 
x_train = train_combine[-indexes,]
dim(x_train)

#Load Train and Test datasets
#Identify feature and response variable(s) and values must be numeric and numpy arrays
#x_train <- train_combine[,train_combine.isnumeric]
y_train <- train_combine$Type
#x_test <- 
#x <- cbind(x_train,y_train)
# Train the model using the training sets and check score
linear <- lm(y_train ~ ., data = train_cap)


summary(linear)
#Predict Output
predicted= predict(linear,x_test)
accuracy(predicted, x_test$Ty)

# Train the model using the training sets and check score
logistic <- glm(y_train ~ ., data = train_cap,family='binomial')
summary(logistic)
#Predict Output
predicted= predict(logistic,x_test)

###################Decision Tree########################
library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)
# grow tree 
fit <- rpart(y_train ~ ., data = train_cap ,method="class",control = rpart.control(cp = 0.01)) #try with train_other, train_combine
summary(fit)
#Predict Output 
predicted= predict(fit,x_test)
plotcp(fit)
printcp(fit)
rpart.plot(fit, 
           box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)

#######################Support Vector Machine###########################
library(e1071)
#x <- cbind(x_train,y_train)
# Fitting model
fit <-svm(y_train ~ ., data = train_cap)# try using the train_cap and train_other
summary(fit)
#Predict Output 
predicted= predict(fit,x_test)


library(e1071)
# Fitting model
fit <-naiveBayes(y_train ~ ., data = train_combine)#read and try to find parameters
summary(fit)
#Predict Output 
predicted= predict(fit,x_test)


library(randomForest)
#install.packages('forecast')
library(forecast)
# Fitting model
fit <- randomForest(y_train ~ ., train_cap,importance =TRUE,ntree=500,nodesize=7, na.action=na.roughfix)
summary(fit)
varImpPlot(fit, type=1) # plotting the graph
#Predict Output 
predicted= predict(fit,x_test)
accuracy(predicted, x_test$Ty)

length(predicted)
length(x_test$Ty)
