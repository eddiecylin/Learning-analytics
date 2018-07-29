# NLP/Text Mining: Analyzing Students' Note-Taking in A Graduate Class

- **Project description**:    
Using students notes from a graduate-level class that teaches machine learning/data analytics, this project aims to apply NLP/text mining analysis to exploring students' motivation to take notes and the nature of notes per se. This graduate class required students to complete weekly readings and write notes. Students could decide their own pace of writing notes but were encouraged to do so along with weekly class schedule. Students were encouraged to focus on the highlights in the reading materials but not their own reflection or comments. Specific questions probed in this project are:

**1. How weekly taught topics affect student's note-taking behavior?**    
**2. What is the relation between type of class reading & note-taking?**    
**3. How does time of submitting notes affect note sentiment & note length?**    

#### laod packages
```{r}
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(rvest)
library(purrr)
library(plotly)
library(stringr)
library(lubridate)
# for Mac computer
Sys.setlocale("LC_ALL", "C")
```

#### import documents
```{r}
# create a list of all the files
file.list <- list.files(path="/Users/Eddie/Desktop/DATA/NEW-R/RProject-github/7. Class-Notes-Text-Analysis/class-notes", pattern="2017.csv", full.names = T)
# import data & rbind them
D1 <- do.call("rbind", lapply(file.list, read.csv, header = TRUE, stringsAsFactors = FALSE))
# create another df for reference of weekly class 
D2 <- read.csv("week-list.csv", header = TRUE) 
D2 <- D2[order(D2$week), ]
```

#### select columns and labeling
```{r}
D1 <- select(D1, "X...Key","Item.Type","Pages", "Title", "Notes", "Date.Added")
paste('number of rows of D1 before removing duplicates: ', nrow(D1))
D1 <- unique(D1)
paste('number of rows of D1 after removing duplicates: ', nrow(D1)) # 1007
colnames(D1)[1] <- "doc_id"
colnames(D1)[5] <- "text"
```

#### clean the `text` column
```{r}
D1_c <- filter(D1, text != "") # removing empty data in `text` column results in 615 rows
D1_c$text <- gsub("<.*?>", "", D1_c$text)
D1_c$text <- gsub("nbsp", "", D1_c$text)
D1_c$text <- gsub("nbspnbspnbsp", "", D1_c$text)
D1_c$text <- gsub("[\x01-\x1f\x7f-\xff]", "", D1_c$text)
```

#### build corpus for the text data
```{r}
corpus <- VCorpus(VectorSource(D1_c$text))
# remove spaces
corpus <- tm_map(corpus, stripWhitespace)
# convert to lower case
corpus <- tm_map(corpus, tolower)
# remove pre-defined stop words ('the', 'a', etc)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
# convert words to stems
corpus <- tm_map(corpus, stemDocument)
# I decided not to remove numbers since they could be part of the notes
# corpus <- tm_map(corpus, removeNumbers)
# remove punctuation
corpus <- tm_map(corpus, removePunctuation)
# convert to plain text for mapping by wordcloud package
corpus <- tm_map(corpus, PlainTextDocument, lazy = TRUE)
# convert corpus to a term document matrix - so each word can be analyzed individuallly
tdm.corpus <- TermDocumentMatrix(corpus)
tdm.corpus$dimnames$Docs <- paste(D1_c$doc_id)
saveRDS(tdm.corpus, file = "tdm.corpus.rds")
```

#### find common words
```{r}
findFreqTerms(tdm.corpus, lowfreq=50, highfreq=Inf)
word.count <- sort(rowSums(as.matrix(tdm.corpus)), decreasing=TRUE)
word.count <- data.frame(word.count)
word.count
# no surprise, some most common words reflect the nature of this class. Examples include: data, learn, student, model, use, can, network, predict, edu, etc
# some common words such as 'can' and 'one' show that the corpus can be better cleaned. But since the result is not giving much insights, I let it pass for now.
```

#### show word cloud
```{r}
col=brewer.pal(6,"Dark2")
#generate cloud
wordcloud(corpus, min.freq=100, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)

```
![text-cloud](https://github.com/eddiecylin/data-analytics/blob/master/7.%20NLP:Text-Mining-Class-Notes/text-cloud.png)

- like lots of text mining analyses, this is fun to look at but not really telling us much. The words in the text cloud simply replect what is taught in this class.

#### merge `D1_clean` with D2 week-topic 
```{r}
colnames(D1_c) <- tolower(colnames(D1_c))
D_merged <- inner_join(D1_c, D2, bY = "title") # 296 rows
# there is a significant loss when the weekly topic labels
# reason: there are many incidents of mismatch using the primary key `title` to join the 2 df,
# primarily because there are many false titles in D1 that can be found in D2 as reference. For 
# future reference, this problem should be addressed with database manager and investigate how the 
# raw data were imported intitially.
```

### Does (median) average length of note change throughout the semester? If so, how?

#### prepare dataframe 
```{r}
D_merged <- D_merged[order(D_merged$week), ] 
# note that the label of weeks represent the relative order of the classese. Yet, after excluding false data and skip the weeks of exams, holidays, guest talk, etc., we have 8 weeks of class topic (week4: prediction data is lost)
D_merged$text.length <- sapply(strsplit(D_merged$text, " "), length)
DF_topic_week_text_length <- select(D_merged, topic, week, text.length)
# table(DF_topic_week_text_length$week) # due to data attrition, week has smaller sample size of 6 documents only. Median average is applied to reduce the potential issue of unbalanced sample sizes
DF_topic_week_text_length$topic <- as.character(DF_topic_week_text_length$topic)
DF_topic_week_text_length$week <- as.character(DF_topic_week_text_length$week)

DF_topic_week_text_length$week_topic <- paste(DF_topic_week_text_length$week, DF_topic_week_text_length$topic, sep = '.')
# remove leading and trailing whitespace in topic names(I returned tothis step after discovered it a few steps later)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
DF_topic_week_text_length$week_topic <- trim(DF_topic_week_text_length$week_topic)
# median average of word count in text
DF_topic_week_text_length$week_topic <- as.factor(DF_topic_week_text_length$week_topic)

DF_avg_wd_length <- DF_topic_week_text_length %>%
        group_by(week_topic) %>%
        summarise(median.avg = median(text.length))
```

#### create a plot for average word length
```{r}
m <- list(
  l = 50,
  r = 65,
  b = 135,
  t = 80,
  pad = 3
)

p_wd_len <- plot_ly(DF_avg_wd_length, x = ~week_topic, y = ~median.avg, type = 'bar',
        marker = list(color = c('rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                                'rgba(222,45,38,0.8)', 'rgba(0,0,255,1)',
                                'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                                 'rgba(204,204,204,1)', 'rgba(204,204,204,1)'))) %>%
  layout(title = "Average Word Count of Student Notes by Weekly Teaching Topic\n ",
         annotations = list(x = 1, y = 1, text = "*week4 is not included due to lack of data integrity", showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=8, color="red")), xaxis = list(title = ""), yaxis = list(title = "word count (median)"), margin = m) 

p_wd_len
```
![bar-chart-week](https://github.com/eddiecylin/data-analytics/blob/master/7.%20NLP:Text-Mining-Class-Notes/bar-chart-week.png)
see interactive plot: (https://plot.ly/~cyl2138/1/)

- Students tended to write more about the topic of Networks Analysis, and less about Principal Component Analysis. Since the instructor assigned the same number of readings to each taught topic, the difference in note length could result from: (1) difficulty to comprehend the topic, (2) events in the semester schedule(e.g. mid-term week)

## Does type of class readings affect the motivation of taking notes(measured by note length)?

#### create another dataframe
```{r}
DF_reading_type <- select(D_merged, item.type, text.length) %>%
        group_by(item.type) %>%
        summarise(avg.wd.length = median(text.length)) %>%
        arrange(-avg.wd.length)
```

#### plot it 
```{r}
m <- list(
  l = 150,
  r = 55,
  b = 80,
  t = 50,
  pad = 1
)
p_reading_tp <- plot_ly(DF_reading_type, x = ~avg.wd.length, y = ~reorder(item.type, avg.wd.length), name = 'Average Word Count of Student Notes (median)',
              type = 'bar', orientation = 'h',
              marker = list(color = c('rgba(50, 171, 96,1)', 'rgba(50, 171, 96,0.9)',
                                'rgba(50, 171, 96,0.9)', 'rgba(50, 171, 96,0.8)',
                                'rgba(50, 171, 96,0.6)', 'rgba(50, 171, 96, 0.5)',
                                 'rgba(50, 171, 96,0.4)'), line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
  layout(annotations = list(x = 0.95, y = 1, text = "Average Length of Sudent Note by Reading Type", showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=20, color='rgba(0,0,0,0.5)')),
         yaxis = list(title = "", showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
         xaxis = list(title = "average word count (median)", zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE), margin = m)%>%
  add_annotations(xref = 'x1', yref = 'y',
                  x = DF_reading_type$avg.wd.length * 1 + 3.5,  y = DF_reading_type$item.type,
                  text = round(DF_reading_type$avg.wd.length,1),
                  font = list(family = 'Arial', size = 12, color = 'rgb(50, 171, 96)'),
                  showarrow = FALSE)

p_reading_tp

```
![bar-chart-reading](https://github.com/eddiecylin/data-analytics/blob/master/7.%20NLP:Text-Mining-Class-Notes/bar-chart-readings.png)
see interactive plot:(https://plot.ly/~cyl2138/3/)

1. Interestingly, students tended to write more when they read journal articles or blog posts
2. As students may increase the length of their notes in the fasion of 'copy-&-paste', it will be worth looking into the nature of notes between different type of readings
3. For instance, do students tend to copy-paste when it is a journal article or a book? And do students add more of their personal views in their notes when it is a blog post since there is not much to copy-paste? Especially when factoring in that these 2 types of readings yielded similar note length.

## What's the relation between note sentiment, note length, time of submitting the note in a day?

#### import sentiment dictionaries & create sentiment dataframe(only positive-negative sentiment is used for the sake of simplicity)
```{r}
positive <- readLines("positive-words.txt")
negative <- readLines("negative-words.txt")
# search for matches between each word and the two lexicons
D1_c$positive <- tm_term_score(tdm.corpus, positive)
D1_c$negative <- tm_term_score(tdm.corpus, negative)
# generate an overall pos-neg score for document
D1_c$score <- D1_c$positive - D1_c$negative
```
#### re-create a dataframe for note length
```{r}
# note length prep
D1_c$text.length <- sapply(strsplit(D1_c$text, " "), length)
```

#### create dataframe for submit time
```{r}
#  submit time prep
df_date_time <- str_split_fixed(D1_c$date.added," ", 2)
colnames(df_date_time) <- c('date', 'time')
D1_c <- cbind(D1_c, df_date_time)
D1_c$hour <- substr(D1_c$time, 0, 2)
```

#### merge dataframes
```{r}
DF_sen_time <- select(D1_c, score, hour) %>%
        group_by(hour) %>%
        summarise(mean.sen.score = mean(score))

DF_len_time <- select(D1_c, text.length, hour) %>%
        group_by(hour) %>%
        summarise(median.text.length = median(text.length))

DF_sen_len_time <- full_join(DF_sen_time, DF_len_time, by = 'hour')
DF_sen_len_time$hour <- as.numeric(DF_sen_len_time$hour)
colnames(DF_sen_len_time) <- c('time_of_the_day', 'sentiment', 'word_count')
# here I use mean average sentiment as denominator to normalize the original sentiment scores
DF_sen_len_time$relative_sentiment <- (DF_sen_len_time$sentiment/mean(DF_sen_len_time$sentiment))*100

```

#### plot it
```{r}
p_sen_len_time <- plot_ly(DF_sen_len_time, x = ~time_of_the_day, y = ~word_count, name = 'average word count', type = 'scatter', mode = 'lines+markers') %>% 
        add_trace(y = ~relative_sentiment, name = 'average note sentiment(%)', mode = 'lines+markers') %>% 
        layout(title = "Avg Note Length vs. Note Sentiment by Time",
         annotations = list(x = 1.45, y = 0.3, text = "**the relative note sentiment score(%)\n is computed by deviding by\n group mean & multiplying 100\n (e.g. original score of 0.13\n will be 16% after divided\n by 0.8(mean) and  *100)", showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=8, color="black")), xaxis = list(title = "Time of the Day"), yaxis = list(title = "Word Count / Sentiment")) 
        
p_sen_len_time  

```

![line-chart-sen](https://github.com/eddiecylin/data-analytics/blob/master/7.%20NLP:Text-Mining-Class-Notes/line-chart-sen.png)
see interactive plot:(https://plot.ly/~cyl2138/5/)

1. Assuming that students submitted notes right after they wrote it, then students tended to feel positve and write more between 14:00 and 16:00 despite the drop in between. 
2. At other times during the day (e.g.13:00 or 19:00), students may feel negative but still write quite a bit. 
3. Yet, these results are exploratory rather than decisive, as the note sentiment may not reflect writers' own sentiment and students could also hold out their notes and submit later when finishing them.
4. Yet, with a more rigorous research design, this analytical approach may bear potential to explore the best time for users' engagement. Such as deployment of a test for students in education or maybe ask for cutomer reviews in marketing.

##  LDA topic modeling across student notes(document)
```{r}
#Term Frequency Inverse Document Frequency
dtm.tfi <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))
D1_c$doc_id <- c(1:nrow(D1_c))
dtm.tfi$dimnames$Docs <- paste(D1_c$doc_id)
#Remove very uncommon terms (term freq inverse document freq < 0.5)
dtm.tfi <- dtm.tfi[,dtm.tfi$v >= 0.5]
#Remove non-zero entries
rowTotals <- apply(dtm.tfi, 1, sum) #Find the sum of words in each Document
dtm.tfi <- dtm.tfi[rowTotals > 0, ] #Divide by sum across rows
lda.model = LDA(dtm.tfi, k = 10, seed = 150)
# build a df 
gammaDF <- as.data.frame(lda.model@gamma)
rownames(gammaDF) <- dtm.tfi$dimnames$Docs
names(gammaDF) <- c(1:10)
gammaDF # posterior topic distribution for each document
```

```{r}
saveRDS(gammaDF, file = "gammaDF.rds")
```

#### link topic with document(student note)
```{r}
toptopics <- as.data.frame(cbind(document = row.names(gammaDF), 
  topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
#toptopics$document <- sort(as.integer(toptopics$document))
week_pos <- as.character(toptopics$document)
week_pos_int <- as.integer(week_pos)
week_sel <- D1$week[week_pos_int]
toptopics$week <- week_sel
toptopics <- toptopics[complete.cases(toptopics), ]
```

#### find common words and topics
```{r}
#  most common terms in each topic
terms(lda.model) 
# association between documents and topics
topics(lda.model)
```
1. LDA didn't give much insight into students' notes in this case
2. Similar to the text cloud, these topics reflect mostly the nature of the class
3. If the students were asked to focus on their reflection/ application based on weekly readings, then LDA could give some more interesting results.
4. In a way, this reflects one of the principles of using LDA, which is, having a wide range of and distinct topic varieties is important (which is not the case in this project)

## Project summary: 

1. Students tended to write more in their notes regarding topics such as Networks or Clustering Analysis but less regarding topic like Principal Component Analyis. The difference could be addressed by examing the difficulty to comprehend the topic via a interview or questionnaire.

2. Students tended to write more in their notes when readings are journal articles or blog posts. Considering the different nature in these readings but their notes length is similar, it is worth looking into if there is a difference between 'copy-&-paste' and 'personal comments/reflection' when students were writing based on these readings.

3. The submission time does affect note length and note sentiment. Note sentiment and length also correlate differently at different times during the day. However, the relation between 'feeling positve' and 'write more notes' should be further examined with a more rigorous study design though there emerged a tendency as such in this project.
