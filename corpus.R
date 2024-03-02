library(openxlsx)
library(tidyverse)
library(readr)
library(stringr)
library(textclean)
library(tidytext)
library(RcppMeCab)
library(tidyr)
library(jsonlite)
setwd("C:/Users/hwwe1/Desktop/ytb_tm_files/project_mj/train")


# 긍정 사전 호출
pos_words_not_dict <- read_json("pos_words(not_dict).json")
pos_words_not_dict <- paste0(pos_words_not_dict)


# 부정 사전 호출
neg_words <- read_json("neg_words.json")
neg_words <- paste0(neg_words)


# 불용어 단어 사전 호출 (영상과 무관한 댓글 최대한 필터링)
filter_words <- read_json("filter_words.json")
filter_words <- paste0(filter_words)


# 전체
df_big <- read.xlsx("new_trainset_comments.xlsx")
df_big <- tibble(df_big)
df_big <- distinct(df_big)
write.xlsx(df_big, "new_trainset_comments.xlsx")
#-----------------------------------
df <- read.xlsx("df_169732_complete.xlsx")



# 댓글 공감(좋아요)와 감성 점수수
df_num_likes <- df %>%
  filter(num_likes > 2000)
cor(df_num_likes$num_likes, df_num_likes$sentiments) 
#----------------------------------
hist(df$sentiments) # 매우 부적 편포인디.... 0~0.2 부분이 유난히 많음, 데이터 불균형 문제

under02 <- df %>% 
  filter(0.2 > sentiments)

hist(under02$sentiments)


under02 %>% 
  filter(0.1 > sentiments) %>% nrow()

under005 <- under02 %>% 
  filter(0.05 > sentiments)

hist(under005$sentiments)
sample(x = under005$sentiments,
       size = 20000,
       replace = FALSE) %>% hist()


df_divide10 <- df %>% 
  mutate(category = ifelse(sentiments>= 0.9, "9",
                           ifelse(sentiments>= 0.8, "8",
                                  ifelse(sentiments>= 0.7, "7",
                                         ifelse(sentiments>= 0.6, "6", 
                                                ifelse(sentiments>= 0.5, "5",
                                                       ifelse(sentiments>= 0.4, "4",
                                                              ifelse(sentiments>= 0.3, "3",
                                                                     ifelse(sentiments>= 0.2, "2",
                                                                            ifelse(sentiments>= 0.1, "1", "0"))))))))))
df_divide10 %>%
  group_by(category) %>%
  count(category)

df_divide10 %>%
  group_by(category) %>%
  summarise(mean = mean(sentiments)) %>%
  mutate(mean5 = mean * 10)


df_divide5 <- df %>% 
  mutate(category = ifelse(sentiments>= 0.8, "5",
                           ifelse(sentiments>= 0.6, "4",
                                  ifelse(sentiments>= 0.4, "3",
                                         ifelse(sentiments>= 0.2, "2","1")))))
                           
df_divide5 %>%
  group_by(category) %>%
  count(category)

df_divide5 %>%
  group_by(category) %>%
  summarise(mean = mean(sentiments)) %>%
  mutate(mean5 = mean * 5)

prop.table(table(df_divide10$category))


# oversample minority class
library(ROSE) 
df_smote <- SMOTE(category ~ ., df_divide5, perc.over = 15000, k = 5)


library(scutr)
df_balanced <- SCUT(data = df_divide10, 
     "category",
     oversample = oversample_smote,
     undersample = undersample_mclust)



hist(df_divide5$sentiments)

df_divide5 %>%
  select(comment) %>%
  filter(length(comment) > 3) %>%
  nrow



# 댓글길이 파악 및 필터링
# 너무 짧은 것은 분석의 의미가 없음
library(stringr)
df_divide5$length <- str_length(df_divide5$comment)

df_divide5 %>%
  filter(length < 4) %>%
  nrow()

View(df_divide5 %>%
       filter(length < 4))

View(df_divide5 %>%
       filter(length == 4))

df_divide5 <- df_divide5 %>%
  filter(length > 4)

df_divide5 %>%
  group_by(category) %>%
  count(category)

hist(df_divide5$sentiments)

df_divide5 %>%
  group_by(category) %>%
  count(category) %>%
  ggplot() +
  geom_col(aes(x = category,
               y = n))

df_divide10 %>%
  group_by(category) %>%
  count(category) %>%
  ggplot() +
  geom_col(aes(x = category,
               y = n))



# 불용어 처리 (영상 주제 무관 댓글 필터링)-----------------------------------------
filter_words <- "예수|하나님|석열|슬롯|문재인|문재앙|이재명|윤석열|민주당|한동훈|장관|법무부장관|더불어민주당|허경영|슬@롯@머@신|홀??덤|전라도|대깨문|좌파|우파|극우|극좌|이준석|정치|국회의원|선전|선동|사상|검찰|대통령|천공|건희|국민의힘|국힘|운동권|탄핵|정권교체|좌좀|이념|일베|클리앙|남팍|홍어|노무현|달빛창|김대중|땅끄|전두환|보빨|물소|버팔로|이기야|한남|냄져|김치녀|국회|국개|의원|오세훈|검새|완박|새누리|박근혜|우병우|최순실|대남|국정|청문회|박정희|이승만|야당|여당|민노총|최강욱|추미애|윤버버|황교안|간첩|안철수|윤통|자유한국당|미래통합당|보수|진보"

df_divide5 %>% # 필터링 후 댓글 수
  filter(!str_detect(comment, filter_words)) %>%
  nrow()

View(df %>% # 필터링 대상 댓글 파악,
       filter(str_detect(comment, filter_words)))ㄹㄹ

df %>% # 필터링 대상 단일 댓글 파악(데이터 원본)
  filter(str_detect(comment, "대깨문")) %>%
  select(comment)

df_divide5 %>% # 필터링 대상 단일 댓글 파악,
  filter(str_detect(comment, "진보")) %>%
  select(comment)

df_divide5 <- df_divide5 %>% # 필터링 반영
  filter(!str_detect(comment, filter_words))
#------------------------------------------------------------------

# 부적절한 감성 점수가 부여된 댓글 필터링
# 이건 어케 일일히 찾음;; 좀 있어도 그렇게 영향을 줄까 글쎄.
View(df_divide5 %>%
       filter(category == 5))

View(df_divide5 %>%
       filter(category == 4))

View(df_divide5 %>%
       filter(category == 3))

View(df_divide5 %>%
       filter(category == 2))

View(df_divide5 %>%
       filter(category == 1))


# 일단 여기서 저장하고, pretrained model에 넣어볼까...
write.xlsx(df_divide5, "df_divide5_filtered.xlsx")


View(df %>%
       filter(sent == 3))


df <- read.xlsx("df_divide5_filtered.xlsx")
df <- df %>%
  mutate(bi_sent = ifelse(sentiment_rating == 4 | sentiment_rating == 5, "1", 
                          ifelse(sentiment_rating == 1 | sentiment_rating == 2, "0", "else")))

bi_df <- df %>%
  filter(bi_sent == 0 | bi_sent == 1)

bi_df <- bi_df[c(1,2,6,5,3)]
write.xlsx(bi_df, "bi_df.xlsx")


bi_df %>%
  group_by(bi_sent) %>%
  summarise(mean = mean(sentiments))

bi_df %>%
  group_by(bi_sent) %>%
  count(bi_sent)
#------------


df <- read.xlsx("df_divide5_filtered.xlsx")
df <- df %>%
  mutate(bi_sent = ifelse(sentiment_rating == 4 | sentiment_rating == 5, "1", 
                          ifelse(sentiment_rating == 1, "0", "else")))

table(df$bi_sent)

bi_df2 <- df %>%
  filter(bi_sent == 0 | bi_sent == 1)

bi_df2 <- bi_df2[c(1,2,6,5,3)]


bi_df2 <- bi_df2 %>%
  filter(bi_sent != "else")
  

write.xlsx(bi_df2, "bi_df2.xlsx")


bi_df %>%
  group_by(bi_sent) %>%
  summarise(mean = mean(sentiments))

bi_df %>%
  group_by(bi_sent) %>%
  count(bi_sent)




# bi_df3---------------------------
df <- read.xlsx("df_divide5_filtered.xlsx")
df <- df %>%
  mutate(bi_sent = ifelse(sentiment_rating == 4 | sentiment_rating == 5, "1", 
                          ifelse(sentiment_rating == 2, "0", "else")))

table(df$bi_sent)

bi_df3 <- df %>%
  filter(bi_sent == 0 | bi_sent == 1)

bi_df3 <- bi_df3[c(1,2,6,5,3)]


bi_df3 <- bi_df3 %>%
  filter(bi_sent != "else")

table(bi_df3$bi_sent)


write.xlsx(bi_df3, "bi_df3.xlsx")
#---------------------



# bi_df4---------------------------
df <- read.xlsx("df_divide5_filtered.xlsx")
df <- df %>%
  mutate(bi_sent = ifelse(sentiments > 0.5, "1", "0"))


bi_df4 <- df %>%
  filter(bi_sent == 0 | bi_sent == 1)

bi_df4 <- bi_df4[c(1,2,6,5,3)]

table(bi_df4$bi_sent)

write.xlsx(bi_df4, "bi_df4.xlsx")
#---------------------
df <- read.xlsx("df_divide5_filtered.xlsx")

hist(df$sentiments)

df <- df %>%
  mutate(binary_sentiment = ifelse(sentiments >= 0.5, 
                                   1, 0))


write.xlsx(df, "df_divide5_filtered.xlsx")
#-----------------

df <- read.xlsx("preprocessed_train_data.xlsx")
df <- tibble(df)
table(df$binary_sentiment)

# 긍정적인 댓글로 예상되는 댓글들을 더 가져오기
# 부정적인 댓글 수 줄이기 -> 긍정, 부정 댓글 수 비율 1:1 맞춰야겠당.


#-----
add_comments_from_pos_videos <- read.xlsx("add_comments_from_pos_videos.xlsx")
add_comments_from_pos_videos <- tibble(add_comments_from_pos_videos[1])


add_comments_from_pos_videos <- add_comments_from_pos_videos %>%
  mutate(comment = str_squish(replace_html(comment)),
         comment = str_squish(comment),
         comment = str_replace_all(comment, "[^가-힣]", " "),
         comment = str_squish(comment))

filter_words <- "예수|하나님|석열|슬롯|문재인|문재앙|이재명|윤석열|민주당|한동훈|장관|법무부장관|더불어민주당|허경영|슬@롯@머@신|홀??덤|전라도|대깨문|좌파|우파|극우|극좌|이준석|정치|국회의원|선전|선동|사상|검찰|대통령|천공|건희|국민의힘|국힘|운동권|탄핵|정권교체|좌좀|이념|일베|클리앙|남팍|홍어|노무현|달빛창|김대중|땅끄|전두환|보빨|물소|버팔로|이기야|한남|냄져|김치녀|국회|국개|의원|오세훈|검새|완박|새누리|박근혜|우병우|최순실|대남|국정|청문회|박정희|이승만|야당|여당|민노총|최강욱|추미애|윤버버|황교안|간첩|안철수|윤통|자유한국당|미래통합당|보수|진보"

cdd_comments_from_pos_videos %>% # 필터링 후 댓글 수
  filter(!str_detect(comment, filter_words)) %>%
  nrow()

add_comments_from_pos_videos <- add_comments_from_pos_videos %>% # 필터링 후 댓글 수
  filter(!str_detect(comment, filter_words)) %>%
  filter(comment != "")

add_comments_from_pos_videos$length <- str_length(add_comments_from_pos_videos$comment)

View(add_comments_from_pos_videos %>%
       filter(length > 4))

add_comments_from_pos_videos <- add_comments_from_pos_videos %>%
  filter(length > 4)

write.xlsx(add_comments_from_pos_videos, "addi_comments_from_pos_videos(filtered).xlsx")

#--------------------------------------------
df <- read.xlsx("add_comments_from_pos_videos2.xlsx")
df <- tibble(df)

df <- tibble(df[1])

df <- df %>%
  mutate(comment = str_squish(replace_html(comment)),
         comment = str_squish(comment),
         comment = str_replace_all(comment, "[^가-힣]", " "),
         comment = str_squish(comment))

df <- df %>% # 필터링 후 댓글 수
  filter(!str_detect(comment, filter_words)) %>%
  filter(comment != "")

df$length <- str_length(df$comment)

df <- df %>%
  filter(length > 4)

write.xlsx(add_comments_from_pos_videos, "add_comments_from_pos_videos2.xlsx(filtered).xlsx")


df2 <- read.xlsx("addi_comments_from_pos_videos(filtered).xlsx")


comments_from_postive_videos <- tibble(rbind(df2, df))
write.xlsx(comments_from_postive_videos,
           "comments_from_postive_videos.xlsx") # 이게 이번에 새로 긍정 영상에서 뽑은 댓글 전체임 8만개 넘음 ㄷㄷ

#-------------------------------
df <- read.xlsx("preprocessed_train_data.xlsx")
df_30 <- read.xlsx("comments_from_postive_videos_30000.xlsx")
df_30_60 <- read.xlsx("comments_from_postive_videos_30000_60000.xlsx")
df_60_others <- read.xlsx("comments_from_postive_videos_60000_others.xlsx")

df_0_all <- rbind(df_30, df_30_60, df_60_others)
df_0_all <- tibble(df_0_all)

df_0_all <- df_0_all %>%
  mutate(binary_sentiment = ifelse(sentiments >= 0.5, 1, 0)) %>%
  rename(sentiments_gru = sentiments)


df_0_all <- df_0_all %>% 
  mutate(sentiment_rating5 = ifelse(sentiments_gru>= 0.8, "5",
                           ifelse(sentiments_gru>= 0.6, "4",
                                  ifelse(sentiments_gru>= 0.4, "3",
                                         ifelse(sentiments_gru>= 0.2, "2","1")))))
df_0_all
df[c(1,5,3,6,4)]

#-----------------------------------------------------
# 긍정적인 영상에서 댓글을 뽑았음에도, 부정 댓글 59%, 긍정 댓글 41%;;
# 수율 41% (완전 정확하게는 알기 어려움, 가짜 긍정도 있으니까.)
df_0_all %>%
  count(binary_sentiment) %>%
  mutate(ratio = n/sum(n) * 100) 



df_0_all_pos <- df_0_all %>% filter(binary_sentiment == 1)

View(df_0_all %>% filter(binary_sentiment == 0))
df_0_all %>% filter(binary_sentiment == 0) %>% nrow()


#------
new_trainset_comments <- rbind(df_0_all, df[c(1,5,3,6,4)])
new_trainset_add_only_pos <-  rbind(df_0_all_pos, df[c(1,5,3,6,4)])

write.xlsx(new_trainset_comments, "new_trainset_comments.xlsx")

# 기존 trainset: 부정 72.4%, 긍정 27.6%, 154,374개
df %>%
  count(binary_sentiment) %>%
  mutate(ratio = n/sum(n) * 100) 


# new_trainset_comments(새로 뽑은 영상 댓글 다 넣음): 부정 67.6%, 긍긍정 32.4%, 239,997개
new_trainset_comments %>%
  count(binary_sentiment) %>%
  mutate(ratio = n/sum(n) * 100) 

# new_trainset_add_only_pos(새로 뽑은 영상에서 긍정 댓글만만 넣음): 부정 댓글 59%, 긍정 댓글 41%, 189,491개
new_trainset_add_only_pos %>%
  count(binary_sentiment) %>%
  mutate(ratio = n/sum(n) * 100) 

#write.xlsx

#----------------긍정단어 사전 만들기--------------------------
# < 긍정 단어를 뽑아서 부정 댓글에서 가짜 부정 댓글을 뽑아보자!>
# 실제 긍정, 부정 분류된 댓글들 교정: 0(부정)으로 분류된 댓글들 단어 빈도 추출 -> 긍정 단어 들어간 댓글들을 긍정(1)로 재분류(교정)
# mecab 설치해서 형태소 제대로 분리
# 연구자의 주관과 형태소 사전을 이용해서 긍정단어, 부정단어 탐지하자
# posParallel(enc2utf8(df_try_revise_neg$comment[1000]))
# c(posParallel(enc2utf8(df_try_revise_neg$comment[1000])))
# pos(df_try_revise_neg$comment[1000])
# nng count에 각 nng에 군산대 감성사전로 점수 부여
# 긍정이니까, 플러스 단어만 추출
# 

df_try_revise_neg <- df_0_all %>% filter(binary_sentiment == 0)


tokenized_df_try_revise_neg <- df_try_revise_neg %>% 
  unnest_tokens(input = comment, 
                output = words,
                token = posParallel,
                drop = F)

tokenized_df_try_revise_neg %>% print(n= 10)


tokenized_count_df_try_revise_neg <- df_try_revise_neg %>% 
  unnest_tokens(input = comment, 
                output = words,
                token = posParallel,
                drop = F) %>%
  count(words) %>%
  arrange(desc(n))
write.xlsx(tokenized_df_try_revise_neg, "tokenized_df_try_revise_neg.xlsx")

tokenized_count_df_try_revise_neg <- read.xlsx("tokenized_count_df_try_revise_neg.xlsx")



tokenized_count_df_try_revise_neg %>% print(n= 100)


# 명사 단어별 카운트
nng_counts <- tokenized_count_df_try_revise_neg %>%
  filter(str_detect(words, "nng"))

# word에 품사 태그 제거
nng_counts <- nng_counts %>%
  mutate(words = str_replace_all(words, "[^가-힣]", " "),
         words = str_squish(words))

nng_counts %>% print(n=50)

# 군산대 감성사전 불러오기
#dict <- read.csv("SentiWord_Dict.txt", sep = "\t")
#dict <- dict %>%
  #rename(words=X...) %>%
  #rename(polarity = X1)
#write.xlsx(dict, "KNU_sentiment_words.xlsx")
dict <- tibble(read.xlsx("KNU_sentiment_words.xlsx"))

nng_counts <- nng_counts %>%
  left_join(dict, by = "words") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

nng_counts %>%
  arrange(-polarity) %>%
  print(n = 50)

View(nng_counts %>%
       arrange(-polarity) %>%
       filter(polarity == 0))

nng_counts_pos <- nng_counts %>%
  arrange(-polarity) %>%
  filter(polarity > 0)




pos_words <- paste0(nng_counts_pos$words,  "|") # 긍정 댓글 뽑고 | 넣음 (str_detect() 함수 쓰려면 |가 단어 사이에 있어야 함함
pos_words <- paste(pos_words, collapse = "") # 단어 사이사이 쌍따옴표 제거
pos_words <- gsub("\\|$", "", pos_words) # 맨 마지막에 | 지우기


df_try_revise_neg %>% # 필터링 후 댓글 수
  filter(str_detect(comment, pos_words))
# 군산 감성 사전에 없는 긍정 단어도 있을터이니... 가짜 부정(실제로는 긍정) 댓글을 더 찾을 수 있을 것으로 기대됨

# 군산 사전에 없는 긍정적인 단어들 추가
add_pos_words <- "|눈썰미|연륜|행운|의인|인재|쾌차|힐링|마음씨|나이팅게일|애국심|애국자|은인|히어로|슈퍼맨|특진|화이팅|국뽕|소중|훌륭|듬직|든든|좋|대단|멋"
pos_words <- paste(pos_words, add_pos_words)
pos_words <- str_replace_all(pos_words, " ", "")


pos_from_dict <- paste0(dict %>% filter(polarity > 0) %>% select(words))
pos_from_dict

pos_from_dict <- str_replace_all(pos_from_dict, "[^가-힣]", " ")
pos_from_dict <- str_squish(pos_from_dict)
pos_from_dict <- str_replace_all(pos_from_dict, "[^가-힣]", "|")


pos_words <- paste(pos_words, pos_from_dict)
pos_words <- str_replace_all(pos_words, " ", "")



"대단" %in% dict$words
#----------------
# 부정으로 잘못 분류된 긍정 댓글을 교정하기 
#pos_words <- read_json("pos_words.json")
#pos_words <- paste0(pos_words)

pos_words_not_dict <- read_json("pos_words(not_dict).json")
pos_words_not_dict <- paste0(pos_words_not_dict)

#add_pos_not_dict <- "사랑|훌륭|깨끗|시원|만족|따뜻|상냥|은혜|편안|감동|조화|칭찬|기쁨|탐|경치|긍정|말끔|멀끔|성공|세련|싱싱|씩|온전|용감|유쾌|칭송|튼튼|공덕|공손|긍지|매력|믿음직|가뿐|감명|호감|명랑|황홀|효도|훤칠|유쾌|최상|호감|헌신|멋|미소|생기"

#pos_words_not_dict <- paste(pos_words_not_dict, add_pos_not_dict)
#pos_words_not_dict <- str_replace_all(pos_words_not_dict, " ", "")



df_try_revise_neg %>% # 필터링 후 댓글 수
  filter(str_detect(comment, pos_words_not_dict)) %>% nrow()

df_try_revise_neg %>%
  mutate(binary_sentiment = ifelse(str_detect(comment, pos_words_not_dict), 1, "binary_sentiment")) %>%
  filter(binary_sentiment==1) %>% 
  nrow()
  
# 엥, 모든게 다 긍정으로 교정될리가 없는데...

df_try_revise_neg %>%
  filter(str_detect(comment, "아름"))
# 그러네 뭔가 잘못됨;

#pos_dict_freq <-


# 태그 0 -> 162,212개
df_big %>% 
  filter(binary_sentiment == 0) %>%
  nrow()

# 태그 0인 162,212개 댓글 중에서 가짜 부정(긍정)의 수는?
#부정에서 진짜 부정과 긍정 분리

df_big_0 <- df_big %>% 
  filter(binary_sentiment == 0) %>%
  mutate(binary_sentiment = ifelse(str_detect(comment, pos_words_not_dict), 1, 0))

df_big_0 %>%
  group_by(binary_sentiment) %>%
  count(binary_sentiment)

df_big_0 %>%
  filter(binary_sentiment == 1) %>% print(n=100)

table(df)

# 긍정에서 진짜 긍정과 가짜 부정 분리

#
#-------------- 부정 사전 만들기: 긍정댓글들 속에서 부정 댓글 찾기----------------------
df_big_0_neg <- df_big_0 %>% filter(binary_sentiment == 0) #부정에서 긍정 빼고 남은 부정댓글들을 기반으로 사전만들어보기

df_big_0_neg <- df_big_0_neg %>% 
  unnest_tokens(input = comment, 
                output = words,
                token = posParallel,
                drop = F) %>%
  count(words) %>%
  arrange(desc(n))


df_big_0_neg <- df_big_0_neg %>%
  filter(str_detect(words, "nng|xr")) %>% print(n=25)

df_big_0_neg <- df_big_0_neg %>%
  mutate(words = str_replace_all(words, "[^가-힣]", " "),
         words = str_squish(words))

neg_dict <- dict %>% filter(polarity < 0)

df_big_0_neg <- df_big_0_neg %>%
  left_join(dict, by = "words") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))


View(df_big_0_neg %>% filter(polarity < 0))
neg_words <- df_big_0_neg %>% filter(polarity < 0) %>% select(words)
neg_words <- paste0(neg_words)
neg_words <- str_replace_all(neg_words, "[^가-힣]", " ")
neg_words <- str_squish(neg_words)
neg_words <- str_replace_all(neg_words, "[^가-힣]", "|")

neg_words <-  "범죄|범죄자|욕|눈물|피해|살인|죄|잘못|마약|폭행|범인|불법|실수|벌|환자|감옥|병|죄수|소름|화|짜증|도둑|살인죄|의심|바보|고통|악마|충격|손해|망신|불편|개새끼|부족|분노|포기|시비|화재|상처|거짓말|폭력|노예|장애|협박|겁|한숨|창피|불안|스트레스|욕심|혐오|낭비|지옥|오해|해|죽음|실망|공포|후회|주작|횡령|위기|곰팡이|싸움|최악|살인자|울음|빚|비난|탈|가짜|납치|부담|오류|헛소리|찐따|부작용|흉악범|형벌|살해|죄책감|불만|불량|괴물|머저리|재앙|지지|마비|소음|먼지|미친개|위법|중상|가난|개자식|창피|위조|거짓|안달|편견|부상|중독|재난|화풀이|모욕|바이러스|불행|비판|결함|멍청이|일진|천박|  아픔|허무|부심|왕따|욕설|부끄러움|상습범|엉터리|파괴|피눈물|저주|유죄|남용|비극|억지|질병|화가|고함|독|변태|악의|오염|두려움|부조리|자책|찌질이|훼손|망상|비참|소홀|아쉬움|어려움|저급|허접|황천길|겁쟁이|고집|깜놀|분통|슬픔|공포증|봉변|절망|불치병|좀도둑|찌꺼기|모순|분란|허접|환상|흉터|간질|공갈|마이너스|망연자실|불신|실망감|우울증|의혹|잉여|강간죄|공포감|노름|불면증|비만|쌍놈|우울|원한|잡놈|질투|창녀|통증|트롤|폐렴|한탄|감기|구토|꼴불견|불충분|악순환|안습|열폭|잔소리|종말|죄의식|증오|히스테리|가래|강탈|꼬락서니|낭패|몸살|문맹|병맛|분에|불평|비방|비탄|사치|설레발|심장병|악당|악연|역경|자괴감|고충|놀림|망실|무기력|분탕질|분풀이|불안감|소외|소홀|속임수|악몽|앞잡이|야속|울분|울화|윽박|지루|추위|치욕|편애|혐오감|흉악|흉악|강박|격노|경련|경멸|곤욕|귀머거리|기침|돌머리|듣보잡|멸시|못난이|무서움|배탈|버럭|비참|빈대|상심|서러움|속상|속상|수두|신경질|아파|악성|악취|안물|약점|염증|오도|욕|욕심쟁이|위축|유감|음모|전염병|졸도|좌절|진절머리|출혈|치통|탈진|태업|훼방|흉물|겉치레|결석|고난|고난|괴멸|굴욕|근심|꺼림|꾸중|노숙자|도적질|독감|독사|망연|모멸감|무단결석|반란|반역|변비|부스럼|불경기|불화|불황|불효|비련|살기|상한|설사|설움|섬뜩|시달림|시무룩|시샘|신물|실점|심술|심통|심한|쌍년|어지럼증|열증|자해|전전긍긍|죄악감|죽임|진물|질색|질투심|탈영|폭군|피로|현기증|현피"

write_json(neg_words, "neg_words.json")
re_js <- read_json("pos_words.json")
#---------------------------

# 부정 태그 댓글 전체
df_big %>%
  filter(binary_sentiment == 0) %>% nrow() # 162212개

# 부정 태그 댓글 중 긍정 댓글
df_big %>%
  filter(binary_sentiment == 0) %>%
  filter(str_detect(comment, pos_words_not_dict)) %>% nrow() # 54505개


# 긍정 태그 댓글 전체
df_big %>%
  filter(binary_sentiment == 1) %>% nrow() # 77785개

df_big %>%
  filter(binary_sentiment == 1) %>%
  filter(str_detect(comment, neg_words)) %>% nrow() # 23320개




df_big_0 <- df_big %>% 
  filter(binary_sentiment == 0) %>%
  mutate(binary_sentiment = ifelse(str_detect(comment, pos_words_not_dict), 1, 0))

table(df_big_0$binary_sentiment)


df_big_1 <- df_big %>% 
  filter(binary_sentiment == 1) %>%
  mutate(binary_sentiment = ifelse(str_detect(comment, neg_words), 0, 1))

table(df_big_1$binary_sentiment)


#-------------------------수정 완료-------------------
#revised_comments_dataset <- rbind(df_big_0, df_big_1)
write.xlsx(revised_comments_dataset, "revised_comments_dataset.xlsx")
revised_comments_dataset <- revised_comments_dataset %>% 
  filter(!str_detect(comment, filter_words))


rcd0 <- revised_comments_dataset %>% filter(binary_sentiment == 0)
rcd1 <- revised_comments_dataset %>% filter(binary_sentiment == 1)

rcd0 <- sample_n(rcd0, size = 100000)
rcd1 <- sample_n(rcd1, size = 100000)

revised_comments_dataset1.5 <- rbind(rcd0, rcd1)
write.xlsx(revised_comments_dataset1.5, "revised_comments_dataset1.5.xlsx")

revised_comments_dataset1.5 %>% count(binary_sentiment)




new_trainset_comments <- read.xlsx("new_trainset_comments.xlsx")
new_trainset_comments <- tibble(new_trainset_comments)


table(revised_comments_dataset$binary_sentiment)

revised_comments_dataset %>% 
  count(binary_sentiment) %>%  # 부정 54.6 %, 긍정 45.4%
  mutate(ratio = n/sum(n) * 100)


# 아님 긍정에서 가짜 긍정을 빼고, 부정에서 가짜 부정을 빼볼까?
df_big_0 <- df_big %>% 
  filter(binary_sentiment == 0) %>%
  mutate(binary_sentiment = ifelse(str_detect(comment, pos_words_not_dict), 1, 0))

table(df_big_0$binary_sentiment)


df_big_1 <- df_big %>% 
  filter(binary_sentiment == 1) %>%
  mutate(binary_sentiment = ifelse(str_detect(comment, neg_words), 0, 1))

table(df_big_1$binary_sentiment)


df_big_0 <- df_big_0 %>% filter(binary_sentiment == 0)
df_big_1 <- df_big_1 %>% filter(binary_sentiment == 1) 

revised_comments_dataset2 <- rbind(df_big_0, df_big_1) # 162,149
revised_comments_dataset2 <- revised_comments_dataset2 %>% 
  filter(!str_detect(comment, filter_words))

write.xlsx(revised_comments_dataset2, "revised_comments_dataset2.2.xlsx")

#-----------------------------------------------
# 위 학습 데이터셋에서 긍정부정 댓글 5만개로 통일 시킨 버전
df_big_0 <- df_big_0 %>%
  filter(!str_detect(comment, filter_words))

df_big_1 <- df_big_1 %>%
  filter(!str_detect(comment, filter_words))

df_big0_sample <- sample_n(df_big_0, size = 50000)
df_big1_sample <- sample_n(df_big_1, size = 50000)
revised_comments_dataset3 <- rbind(df_big0_sample, df_big1_sample)
revised_comments_dataset3 %>% 
  filter(str_detect(comment, filter_words))
write.xlsx(revised_comments_dataset3, "revised_comments_dataset3.2.xlsx")

revised_comments_dataset3 %>% count(binary_sentiment)
#----------------------------------\\ 사전 재구축 -> 가짜 긍/부정 필터링 다시--------------
new_trainset_comments <- read.xlsx("new_trainset_comments.xlsx")
new_trainset_comments <- tibble(new_trainset_comments)

ntc0_tokenized <- read.xlsx("ntc0_tokenized.xlsx")
ntc0 <- new_trainset_comments %>% filter(binary_sentiment == 0)
ntc0_tokenized <- ntc0 %>% 
  unnest_tokens(input = comment, 
                output = words,
                token = posParallel,
                drop = F) %>%
  count(words) %>%
  arrange(desc(n))
write.xlsx(ntc0_tokenized, "ntc0_tokenized.xlsx")


ntc1_tokenized <- read.xlsx("ntc1_tokenized.xlsx")
ntc1 <- new_trainset_comments %>% filter(binary_sentiment == 1)
ntc1_tokenized <- ntc1 %>% 
  unnest_tokens(input = comment, 
                output = words,
                token = posParallel,
                drop = F) %>%
  count(words) %>%
  arrange(desc(n))
write.xlsx(ntc1_tokenized, "ntc1_tokenized.xlsx")


ntc0_tokenized <- ntc0_tokenized %>%
  filter(str_detect(words, "nng|xr|va|vv")) %>%
  mutate(words_1 = str_replace_all(words, "[^가-힣]", " "),
         words_1 = str_squish(words_1),
         length = str_length(words_1)) %>%
  filter(length > 1)

ntc1_tokenized <- ntc1_tokenized %>%
  filter(str_detect(words, "nng|xr|va|vv")) %>%
  mutate(words_1 = str_replace_all(words, "[^가-힣]", " "),
         words_1 = str_squish(words_1),
         length = str_length(words_1)) %>%
  filter(length > 1)


ntc0_words <- paste0(ntc0_tokenized$words_1,  "|") # 긍정 댓글 뽑고 | 넣음 (str_detect() 함수 쓰려면 |가 단어 사이에 있어야 함함
ntc0_words <- paste(ntc0_words, collapse = "") # 단어 사이사이 쌍따옴표 제거
ntc0_words <- gsub("\\|$", "", ntc0_words) # 맨 마지막에 | 지우기


ntc1_words <- paste0(ntc1_tokenized$words_1,  "|") # 긍정 댓글 뽑고 | 넣음 (str_detect() 함수 쓰려면 |가 단어 사이에 있어야 함함
ntc1_words <- paste(ntc1_words, collapse = "") # 단어 사이사이 쌍따옴표 제거
ntc1_words <- gsub("\\|$", "", ntc1_words) # 맨 마지막에 | 지우기

ntc1_tokenized_not0 <-ntc1_tokenized %>%
  filter(!str_detect(words, ntc0_words))

ntc1_tokenized_not0 <- paste0(ntc1_tokenized_not0$words_1,  "|") 
ntc1_tokenized_not0 <- paste(ntc1_tokenized_not0, collapse = "") 
ntc1_tokenized_not0 <- gsub("\\|$", "", ntc1_tokenized_not0)
write_json(ntc1_tokenized_not0, "ntc1_tokenized_not0.json")


ntc0_tokenized_not1 <- ntc0_tokenized %>%
  filter(!str_detect(words, ntc1_words))

ntc0_tokenized_not1 <- paste0(ntc0_tokenized_not1$words_1,  "|") 
ntc0_tokenized_not1 <- paste(ntc0_tokenized_not1, collapse = "") 
ntc0_tokenized_not1 <- gsub("\\|$", "", ntc0_tokenized_not1)
write_json(ntc0_tokenized_not1, "ntc0_tokenized_not1.json")



ntc0 <- new_trainset_comments %>%
  filter(binary_sentiment == 0) %>%
  mutate(binary_sentiment = ifelse(str_detect(comment, ntc1_tokenized_not0),
                                   "1", "0"))

ntc0 %>% count(binary_sentiment)

ntc0 %>%
  mutate(binary_sentiment = ifelse(str_detect(comment, ntc0_tokenized_not1),
                                   "0", "1")) %>% count(binary_sentiment)



ntc1 <- new_trainset_comments %>%
  filter(binary_sentiment == 1) %>%
  mutate(binary_sentiment = ifelse(str_detect(comment, ntc0_tokenized_not1),
                                   "0", "1"))




#---------------
df <- read.xlsx("revised_comments_dataset3.2.xlsx")
df <- tibble(df)
df


df0_tokenized <- df %>% 
  filter(binary_sentiment==0) %>% 
  unnest_tokens(input = comment, 
                output = words,
                token = posParallel,
                drop = F) %>%
  count(words) %>%
  arrange(desc(n))

df1_tokenized <- df %>% 
  filter(binary_sentiment==1) %>% 
  unnest_tokens(input = comment, 
                output = words,
                token = posParallel,
                drop = F) %>%
  count(words) %>%
  arrange(desc(n))


df0_tokenized <- df0_tokenized %>%  
  filter(str_detect(words, "nng|xr|va|vv")) %>%
  mutate(words_1 = str_replace_all(words, "[^가-힣]", " "),
         words_1 = str_squish(words_1),
         length = str_length(words_1)) %>%
  filter(length > 1)

df1_tokenized <- df1_tokenized %>%  
  filter(str_detect(words, "nng|xr|va|vv")) %>%
  mutate(words_1 = str_replace_all(words, "[^가-힣]", " "),
         words_1 = str_squish(words_1),
         length = str_length(words_1)) %>%
  filter(length > 1)


df %>% 
  filter(binary_sentiment==0) %>% 
  filter(str_detect(comment, "나쁜")) %>% 
  print(n=Inf)
 
df %>% 
  filter(binary_sentiment==1) %>% 
  filter(str_detect(comment, "인권")) %>% 
  print(n=Inf)


