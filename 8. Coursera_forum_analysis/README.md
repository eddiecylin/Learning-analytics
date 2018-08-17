# Coursera Forum Post Analysis: Investigating User Interaction across 28 MOOCs

**Project description**:    
This project uses data from discussion threads from of 28 Coursera MOOCs, as well as, focuses on the user interaction revealed by their online activities of forum posts and replies. All of these 28 Coursera MOOCs focus on quantitative knowledge and programing skills including data analytics, machine learning, finance, statistics, etc. This project aims to use exploratory data analysis and data visualization to answer the questions in the following:

**1. What is the relation between the number of course users, number of forum posts, and views of forum posts?**
    
**2. Using frequency counts and time lapse for post replies as index for user interaction, which course has the highest/lowest user interaction?**    
    
**3. What is the ratio of forum posts by different types of participants across 28 courses?**    

**4. What is the general pattern of post replies? Does a forum post elicit many replies mostly from a few users (concentrated) or fewer replies but from several users (spread out)?**    


#### data source: (https://github.com/elleros/courseraforums):
Language independent analysis and classification of discussion threads in coursera MOOC forums, by Lorenzo A. Rossi and Omprakash Gnawali, IEEE International Conference on Information Reuse and Integration (IRI), August 2014.

```{r eval=FALSE, include=FALSE}
# BiBTeX entry for data source
@inproceedings{coursera-iri2014,
   author = {Lorenzo A. Rossi and Omprakash Gnawali},
   title = {{Language Independent Analysis and Classification of Discussion Threads in Coursera MOOC Forums}},
   booktitle = {Proceedings of the IEEE International Conference on Information Reuse and Integration (IRI 2014)},
   month = aug,
   year = {2014}
}
```

#### load data
```{r}
df_course_info <- as.data.frame(read.csv("course_information.csv"))
df_threads <- as.data.frame(read.csv("course_threads.csv"))
df_posts <- as.data.frame(read.csv("course_posts.csv"))
```

#### filter & merge dataframes
```{r}
# filter courses that are quantitative and taught in English 
df_course <- filter(df_course_info, type == 'Q' & language == 'E') # course
# limit uers to be students only
df_user <- filter(df_posts, user_type == 'Student')
# merge df_course and df_threads to filter df_thread 
df_course_thread <- left_join(df_course, df_threads, by = 'course_id')
# filter thread to be relate to assignment, lectures, discusion, quiz, and exercise
df_course_thread$og_forum <- tolower(df_course_thread$og_forum)
target_forum <- c('assignment', 'assignments', 'lecture', 'lectures','discussion', 'discussions','quiz', 'quizzes','exercise', 'exercises')
df_course_thread <- filter(df_course_thread, grepl(paste(target_forum, collapse="|"), og_forum)) # 28,413 rows 
```

## some exploratory data visualization 

#### 1. compare number of course users and number of forum threads  
```{r}
# course, # of users, # of threads
df_usr_thrd <- unique(select(df_course_thread, name, num_users, num_threads))
df_usr_thrd$name <- as.character(df_usr_thrd$name)
df_usr_thrd <- df_usr_thrd[order(df_usr_thrd$num_users), ]
x_order <- df_usr_thrd$name 
df_usr_thrd$name <- factor(df_usr_thrd$name, levels = x_order)

p_usr_thrd <- plot_ly(df_usr_thrd, x = ~name, y = ~num_users, type = 'bar', name = '# of users', marker = list(color = 'rgb(49,130,189)')) %>%
  add_trace(y = ~num_threads, name = '# of threads', marker = list(color = 'rgb(204,204,204)')) %>%
  layout(title = "Number of Users v.s Number of Threads", xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = "count"),
         margin = list(b = 100),
         barmode = 'group')
p_usr_thrd

#link_p_usr_thrd = api_create(p_usr_thrd, filename="p_usr_thrd")
```

![p_usr_thrd](https://github.com/eddiecylin/data-analytics/blob/master/8.%20Coursera_forum_analysis/p_usr_thrd.png)
see interactive plot: (https://plot.ly/~cyl2138/9/)

-  In general, courses with more users also have more forum posts about assignments, class exercises, lectures, though there are some exceptions among these courses.


#### 2. compare number of threads and number of thread views
```{r}
df_thrd_view <- unique(select(df_course_thread, name, num_threads, num_views))%>%
        group_by(name, num_threads) %>%
        summarise(average_thread_views = mean(num_views))

df_thrd_view$average_thread_views <- trunc(df_thrd_view$average_thread_views)
df_thrd_view$name <- factor(df_thrd_view$name, levels = x_order)

p_thrd_view <- plot_ly(df_thrd_view, x = ~name, y = ~num_threads, type = 'bar', name = '# of threads', marker = list(color = 'rgb(204,204,204)')) %>%
  add_trace(y = ~average_thread_views, name = '# of average thread views', marker = list(color = 'rgb(255,153,51)')) %>%
  layout(title = "Number of Threads v.s Average Thread Views" , xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = "count"),
         margin = list(b = 100),
         barmode = 'group')

p_thrd_view

#link_p_thrd_view = api_create(p_thrd_view, filename="p_thrd_view")
```
![p_thrd_view](https://github.com/eddiecylin/data-analytics/blob/master/8.%20Coursera_forum_analysis/p_thrd_view.png)
see interactive plot: (https://plot.ly/~cyl2138/7/)

- As previously mentioned, though courses with more users tend to have more forum posts, the views of other people’s forum posts seem to be consistent across all courses with a ceiling of 500 ~ 600 views.

#### 3. investigate user interaction with an ad hoc interaction index: no-to-self forum replies
```{r}
# prepare dataframe
df_usr_inter <- select(df_posts, post_id, course_id, parent_id, order, user_id, user_type, post_time) %>%
        filter(user_type == "Student")
df_usr_inter <- unique(df_usr_inter)
# select the English-taught courses 
df_usr_inter <- df_usr_inter[df_usr_inter$course_id %in% df_course$course_id, ]
# feature engineer evidence for user interaction: (1) who replied to whose post, (2) time lapse between post & reply
df_usr_inter <- select(df_usr_inter, user_id, parent_id, post_id, post_time, course_id)
# split into 2 df for original post & reply to original post
df_inter_ori_post <- filter(df_usr_inter, parent_id == 0)
df_inter_reply_ori <- filter(df_usr_inter, parent_id != 0)
colnames(df_inter_ori_post)[1] <- "post_creator"
colnames(df_inter_ori_post)[4] <- 'original_post_time'
df_inter_ori_post$post_ref <- paste(df_inter_ori_post$course_id, df_inter_ori_post$post_id, sep = "-" )
# create the same reference in the df_inter_reply_ori dataframe
df_inter_reply_ori$post_ref <- paste(df_inter_reply_ori$course_id, df_inter_reply_ori$post_id, sep = "-" )
colnames(df_inter_reply_ori)[1] <- "post_replier"
colnames(df_inter_reply_ori)[4] <- 'reply_post_time'
df_inter_ori_post <- select(df_inter_ori_post, post_creator, original_post_time, post_ref)
df_inter_reply_ori <- select(df_inter_reply_ori, post_replier, reply_post_time, post_ref, course_id)
# join 2 df 
df_inter_merged <- left_join(df_inter_reply_ori, df_inter_ori_post, by = "post_ref") 
# remove NAs (replies are sent not to any original posts)
df_inter_merged <- df_inter_merged[complete.cases(df_inter_merged), ]
# analyze average frequency for user reply to other (not to self)
df_inter_freqency <- select(df_inter_merged, post_replier, post_creator, course_id) %>%
        filter(post_replier != post_creator) %>%
        group_by(post_replier, post_creator, course_id) %>%
        summarise(count = n()) 
df_inter_freqency <- df_inter_freqency[order(df_inter_freqency$course_id), ]
# compute average non-to-self reply post counts
df_inter_fre_avg <- aggregate(. ~ course_id, df_inter_freqency[3:4], mean)
# analyze avaerage response time for user reply
df_inter_lapse <- select(df_inter_merged, post_replier, post_creator, original_post_time, reply_post_time, course_id) %>%
        filter(post_replier != post_creator) %>%
        filter(original_post_time < reply_post_time) 
# note: there are 272 data points whose original post time are larger than post reply time. This suggest there may 
# exist a problem in the backend when timestamps were recorded before data importation in this project
df_inter_lapse$original_post_time <- anytime(df_inter_lapse$original_post_time)
df_inter_lapse$reply_post_time <- anytime(df_inter_lapse$reply_post_time)
df_inter_lapse$time_lapse <- difftime(df_inter_lapse$reply_post_time,df_inter_lapse$original_post_time, units="hours")
df_inter_lapse$time_lapse <- round(df_inter_lapse$time_lapse, 1)
df_inter_lapse_avg <- aggregate(. ~ course_id, df_inter_lapse[5:6], median)
# create an peer interaction index by combining average (1)reponse count to others' posts & (2)time lapse before reply to original posts
# (1)reponse count to others' posts: higher -> more interactive
# (2)time lapse: lower -> more interactive
inter_index <- data.frame(course_id = df_inter_fre_avg$course_id, interaction_index = scale(df_inter_fre_avg$count/df_inter_lapse_avg$time_lapse))
inter_index <- inner_join(inter_index, df_course[1:2], by = 'course_id')
df_inter_index <- inter_index[2:3] 
df_inter_index$dir <- ifelse(df_inter_index$interaction_index > 0, "postive", "negative")
df_inter_index$dir <- factor(df_inter_index$dir)
df_inter_index$name <- as.character(df_inter_index$name)
```

```{r}
# plot it across 28 unique courses
m_peer_inter <- list(
  l = 250,
  r = 2,
  b = 50,
  t = 50,
  pad = 1
)
p_peer_inter <- plot_ly(df_inter_index, type = "bar", x = ~interaction_index, y = ~name, split = ~dir, orientation="h") %>%
        layout(title = "Peer Interaction Index by Course", 
         xaxis = list(title = "interaction index (normalized)"),
         yaxis = list(autorange = "reversed",tickfont = list(size = 8), title =""), margin = m_peer_inter)

p_peer_inter

#link_p_peer_inter = api_create(p_peer_inter, filename="p_peer_inter")
```
![p_peer_inter](https://github.com/eddiecylin/data-analytics/blob/master/8.%20Coursera_forum_analysis/p_peer_inter.png)
see interactive plot: (https://plot.ly/~cyl2138/11/)

- By combining two factors: (1) number of non-to-self post replies, and (2) time lapse for post replies, some courses have shown higher user interaction than others ans the difference could be large.

#### 4.post by different types of course participants 
```{r}
df_role <- select(df_posts, course_id, user_type) %>%
        filter(user_type != "Anonymous")
# use previous df_course dataframe to filter out target courses
df_role2 <- inner_join(df_role, df_course) %>%
        select(name, course_id, user_type) %>%
        group_by(name, user_type) %>%
        summarise(total = n()) %>%
        group_by(name) %>%
        mutate(percent = total/sum(total))
        
df_role2$percent <- round(df_role2$percent, 5)      
df_role2_wide <- spread(df_role2, user_type, percent)
colnames(df_role2_wide) <- gsub(" ", "_", colnames(df_role2_wide))
```

```{r}

m <- list(
  l = 250,
  r = 2,
  b = 100,
  t = 100,
  pad = 1
)
df_role2_wide$name <- as.character(df_role2_wide$name)

p_post_by_role <- plot_ly(df_role2_wide, x = ~Community_TA*100, y = ~name , type = 'bar', orientation = 'h', name = 'Community_TA',
        marker = list(color = 'rgba(255, 153, 153, 1.0)',
                      line = list(color = 'rgba(128, 128, 128, 1.0)',
                                  width = 0.1))) %>%
  add_trace(x = ~Coursera_Staff*100, name = 'Coursera_Staff',
            marker = list(color = 'rgba(153, 255, 153, 1.0)',
                          line = list(color = 'rgba(128, 128, 128, 1.0)',
                                      width = 0.1))) %>%
        add_trace(x = ~Coursera_Tech_Support*100, name = 'Coursera_Tech_Support',
            marker = list(color = 'rgba(255, 204, 153, 1.0)',
                          line = list(color = 'rgba(128, 128, 128, 1.0)',
                                      width = 0.1))) %>%
        add_trace(x = ~Instructor*100, name = 'Instructor',
            marker = list(color = 'rgba(204, 153, 255, 1.0)',
                          line = list(color = 'rgba(128, 128, 128, 1.0)',
                                      width = 0.1))) %>%
        add_trace(x = ~Staff*100, name = 'Staff',
            marker = list(color = 'rgba(255, 255, 153, 1.0)',
                          line = list(color = 'rgba(128, 128, 128, 1.0)',
                                      width = 0.1))) %>%
        add_trace(x = ~Student*100, name = 'Student',
            marker = list(color = 'rgba(224, 224, 224, 1.0)',
                          line = list(color = 'rgba(128, 128, 128, 1.0)',
                                      width = 0.1))) %>%
        layout(barmode = 'stack', title = "Forum Post by Participant's Role", 
         xaxis = list(title = "aggregate percentage(%)"),
         yaxis = list(autorange = "reversed",tickfont = list(size = 8), title =""), margin = m)

p_post_by_role

#link_p_post_by_role = api_create(p_post_by_role, filename="p_post_by_role")
```
![p_post_by_role](https://github.com/eddiecylin/data-analytics/blob/master/8.%20Coursera_forum_analysis/p_post_by_role.png)
see interactive plot: (https://plot.ly/~cyl2138/13/)

- Not surprisingly, student users account for the majority of forum posts while community TA, instructors, staff’s participation is also significant in some courses. However, there are courses whose forum posts are made predominantly by student users only.

#### 5. what is the general pattern of post replies to the same original forum post creator(i.e. how and how many replies are given to a perosn who asks a question)?
```{r}
# figure out which class has the highest total number of forum replies
df_most <- select(df_inter_merged, post_creator, post_replier, course_id) %>% 
        group_by(course_id) %>% 
        dplyr::summarise(count = n())
most <- filter(df_most, df_most$count == max(df_most$count)) #startup-001
```

```{r}
# use startup-001 as an example to see the user interaction through their post and replies to posts
library(plyr)
df_inter_grid <- select(df_inter_merged, post_creator, post_replier, course_id) %>%
        filter(course_id == "startup-001")
# as it is hard to visualize thousands of users in a heatmap in the following, we target users whose post reply is larger than three (>=3) which is above mean. 
df_select_usr <- df_inter_grid[c(1,3)]
df_select_usr$post_creator <- as.character(df_select_usr$post_creator)

df_select_usr <- df_select_usr %>%
        group_by(post_creator) %>%
        dplyr::summarise(total = n())

df_select_usr <- df_select_usr[df_select_usr$total >= 3, ]
select_usr <- df_select_usr$post_creator
# get the selected users
df_inter_grid <- df_inter_grid[c(1,2)]
df_inter_grid$post_creator <- as.character(df_inter_grid$post_creator)
df_inter_grid <- filter(df_inter_grid, post_creator %in% select_usr)
# just a view of the fist 100 data rows among these selected users (post creators)
df_inter_grid <- df_inter_grid[1:100, ]
df_inter_grid <- df_inter_grid %>%
        group_by(post_creator, post_replier) %>%
        dplyr::summarize(count = n())

# plot the heatmap 
df_inter_grid$post_creator <- factor(df_inter_grid$post_creator)
df_inter_grid$post_replier <- factor(df_inter_grid$post_replier)

df_inter_grid_hmap <- plyr::ddply(df_inter_grid, .(post_creator), transform, rescale = rescale(count))

p_heatmap <- ggplot(df_inter_grid_hmap, aes(post_creator, post_replier)) + geom_tile(aes(fill = rescale), colour = "white") + scale_fill_gradient2(low = "white", high = "red") + scale_y_discrete(limits = rev(levels(df_inter_grid_hmap$post_replier))) + labs(title="Forum Posts & Replies by Users", x="post creator", y="post replier") + theme_bw() + theme(panel.grid=element_blank(), panel.border=element_blank(), axis.text=element_text(size=1, face ="bold"),
        axis.title=element_text(size=12), plot.title = element_text(hjust = 0.5))

p_heatmap_ptly <- ggplotly(p_heatmap)
p_heatmap_ptly

# link_p_heatmap_ptly = api_create(p_heatmap_ptly, filename="p_heatmap_ptly")
```
![p_heatmap_ptly](https://github.com/eddiecylin/data-analytics/blob/master/8.%20Coursera_forum_analysis/p_heatmap_ptly.png)

- Initial findings suggest the post replies follow the  “spread out” fashion. This means for users who tend to receive many replies for their original posts (and those receive fewer replies a like), the replies are given by multiple users with low frequency counts rather few single users with high frequency counts.

## Project summary: 

1. In general, the number of users in a course correlates positively with the number of forum posts. However, a few exceptions do exist where a course with more users does not necessarily have more posts compared to one with relatively fewer users

2. Using an ad hoc user interaction index which considers both the frequency counts of forum posts and time lapse before post replies, the course Statistics: Making Sense of Data has the highest interaction score while the course Probabilistic Graphical Models has the lowest user interaction. 

3. Students account for the majority of forum posts across the 28 courses. However, in courses such as Game Theory and Functional Programing(Scala), we can see significant contribution of forum posts from Community TA. On the other hand, in courses such as Linear and Integer Programing and Nanotechnology the instructor also plays a role regarding forum posts.

4. Initial findings suggest that for student users that receive relatively more post replies, those replies come from other various users rather than a few users. In a sense, this means the user interaction through forum posts is more “spread out”. For example, a person who posts a question on the course forum is likely to receive several replies from different users rather than the same or few users for many times.
