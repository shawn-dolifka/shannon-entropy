#Shawn Dolifka
#CSCE 489-700

library(tidyverse)
library(tidytext)

#-------------------------------------------------#
#Helper Functions
#-------------------------------------------------#

#Shannon entropy function
shannon.entropy = function(p, base = 2)
{
  # remove zeros and normalize, just in case
  p = p[p > 0] / sum(p)
  
  H = sum(-p*log(p,base))
  
  # return the value of H
  return(H)
}

#Normalize the probabilities
normalize = function(x)
{
  x = x/sum(x)
  return(x)
}

#-------------------------------------------------#
#Import the data and clean it
#-------------------------------------------------#

#Import tweets into tibble
tweets = read_csv("sentiment.self.drive.csv")

#Remove Twitter handles
tweets$text = gsub("(^|[^@\\w])@(\\w{1,15})\\b","",tweets$text)

#Remove URLs
tweets$text = gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)","",tweets$text)

#Remove numbers
tweets$text = gsub('[[:digit:]]+', '', tweets$text)

#Remove random punctuation
tweets$text = gsub("(?!')[[:punct:]]", "", tweets$text, perl=TRUE)

#-------------------------------------------------#
#Tokenize Words
#-------------------------------------------------#

#Get Tweet words
tweetWords = tweets %>%
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  ungroup()

#-------------------------------------------------#
#Calculate word frequencies
#-------------------------------------------------#

#Word frequency for all Tweets combined
tweet_freq = tweetWords %>% 
  group_by(tweet_id, sentiment) %>%
  count(word, sort = T) %>%
  ungroup() %>%
  mutate(probability = n / sum(n)) %>% 
  select(-n)

#Fill in any N/A
tweet_freq$probability = tweet_freq$probability %>% replace_na(0)

#Split up Tweets by tweet_id
new_freq = split(tweet_freq,tweet_freq$tweet_id)

#Normalize the probability of all individual Tweets
for (i in 1:2385)
{
  new_freq[[i]]$probability = normalize(new_freq[[i]]$probability)
  
  #Append the entropy to the Tweets
  new_freq[[i]] = new_freq[[i]] %>%
    mutate(entropy = shannon.entropy(new_freq[[i]]$probability))
}

#Bind all Tweets back together into original Tibble
new_freq = do.call("rbind", new_freq)

#Separate Tweets out by sentiment
positiveTweets = subset(new_freq,sentiment == "positive")
neutralTweets = subset(new_freq,sentiment == "neutral")
negativeTweets = subset(new_freq,sentiment == "negative")

#-------------------------------------------------#
#Calculate entropy
#-------------------------------------------------#

#Create tweet_id and entropy, Positive Tweets
pos_entropy = positiveTweets %>% 
  select(-sentiment,-word,-probability) %>% 
  group_by(tweet_id) %>% 
  summarise_all(funs(max))

#Create tweet_id and entropy, Neutral Tweets
neut_entropy = neutralTweets %>% 
  select(-sentiment,-word,-probability) %>% 
  group_by(tweet_id) %>% 
  summarise_all(funs(max)) 

#Create tweet_id and entropy, Negative Tweets
neg_entropy = negativeTweets %>% 
  select(-sentiment,-word,-probability) %>% 
  group_by(tweet_id) %>% 
  summarise_all(funs(max))

#-------------------------------------------------#
#Write entropy to file
#-------------------------------------------------#

#Join all entropies into single table
data = bind_rows(pos_entropy,neut_entropy,neg_entropy)

#Export entropy data to CSV
write.csv(data,file = "entropy.csv",row.names = F)

#-------------------------------------------------#
#Create a table of the entropies
#-------------------------------------------------#

#Get the Mean entropy of Tweets
pos_mean = mean(pos_entropy$entropy)
neut_mean = mean(neut_entropy$entropy)
neg_mean = mean(neg_entropy$entropy)

#Get the Median entropy of Tweets
pos_median = median(pos_entropy$entropy)
neut_median = median(neut_entropy$entropy)
neg_median = median(neg_entropy$entropy)

#Get the Variance entropy of Tweets
pos_var = var(pos_entropy$entropy)
neut_var = var(neut_entropy$entropy)
neg_var = var(neg_entropy$entropy)

#Put all values into a table/tibble
entropy_table = data_frame(
  sentiment = c("positive","neutral","negative"),
  mean = c(pos_mean,neut_mean,neg_mean),
  median = c(pos_median,neut_median,neg_median),
  variance = c(pos_var,neut_var,neg_var)
)

#View the created table
str(entropy_table)