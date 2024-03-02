library(future)
availableCores()
plan(multisession, workers = 16)

library(jsonlite)
library(openxlsx)
library(tidyverse)
setwd("C:/Users/hwwe1/Desktop/ytb_tm_files/project_mj/train")


df <- read.xlsx("revised_com_60000.xlsx")
df <- tibble(df)
df <- unique(df)

df <- df %>%
  mutate(binary_nsr_past = ifelse(gru_nsr > 0.5, 1, 0))

df <- df[c(1,2,3,7,4,5,6)]


df <- df %>% distinct(comment, .keep_all = TRUE)

write.xlsx(df, "revised_com_60000_2.xlsx")


df <- df %>%
  rename(binary_nsr = binary_sentiment) %>%
  mutate(correct = ifelse(binary_nsr == binary_nsmc, "o", "x"))

df %>%
  filter(binary_nsr_past == binary_nsmc) %>% nrow()

df %>%
  filter(binary_nsr == binary_nsmc) %>% nrow()

df %>%
  filter(binary_nsr == binary_nsr_past) %>% nrow()

df_nsmc <- df %>%
  filter(correct == "o") %>%
  select(comment, gru_nsr, gru_nsr_past, gru_nsmc, binary_nsmc)

View(df %>%
       filter(correct == "o") %>%
       select(comment, gru_nsr, gru_nsmc, binary_nsmc))


setwd("C:/Users/hwwe1/Desktop/ytb_tm_files/project_mj/train")

# 긍정 사전 호출
pos_words_not_dict <- read_json("pos_words(not_dict).json")
pos_words_not_dict <- paste0(pos_words_not_dict)


# 부정 사전 호출
neg_words <- read_json("neg_words.json")
neg_words <- paste0(neg_words)


View(df %>%
       filter(binary_nsmc == 1) %>%
       filter(str_detect(comment, neg_words)))


df %>%
  filter(str_detect(comment, pos_words_not_dict)) %>%
  group_by(binary_nsr) %>%
  count(binary_nsmc)

df %>%
  filter(str_detect(comment, neg_words)) %>%
  group_by(binary_nsr) %>%
  count(binary_nsmc)


df %>%
  filter(str_detect(comment, neg_words) & str_detect(comment, pos_words_not_dict))

options(digits=3)

df %>%
  mutate()

df_o <- df %>% filter(correct == "o")
df_x <- df %>% filter(correct == "x")

cor(df_o$gru_nsr, df_o$gru_nsmc)

df_big <- read.xlsx("new_trainset_comments.xlsx")
df_big <- tibble(df_big)
  
df_big %>% distinct(comment, .keep_all = TRUE)

df_4 <- read.xlsx("revised_comments_dataset.xlsx")
df_4 <- tibble(df_4)
df_4 %>% distinct(comment, .keep_all = TRUE)



df_62<- read.xlsx("revised_com_60000_2.xlsx")
df_30 <- read.xlsx("revised_com_rest_30000.xlsx")

df_30 <- df_30 %>%
  mutate(binary_nsmc = ifelse(gru_nsmc > 0.5, 1, 0))
df_89 <- tibble(rbind(df_62, df_30))
df_89

write.xlsx(df_89, "revised_com_rest_89000.xlsx")



df <- tibble(read.xlsx("revised_com_rest_89000.xlsx"))
df1 <- tibble(read.xlsx("revised_com_rest_60_90.xlsx"))
df2 <- tibble(read.xlsx("revised_com_rest_90_120.xlsx"))
df3 <- tibble(read.xlsx("revised_com_rest_30_60.xlsx"))

df_123 <- rbind(df1, df2, df3)
df_123 <- df_123 %>%
  mutate(binary_nsmc = ifelse(sentiments > 0.5, 1, 0))

write.xlsx(df_123, "df_123.xlsx")

df_123 <- tibble(read.xlsx("df_123.xlsx"))

df_179 <- rbind(df, df_123)
write.xlsx(df_179, 
           "revised_com_rest_179.xlsx")


df_179 <- tibble(read.xlsx("revised_com_rest_179.xlsx"))
df_179 <- df_179[-c(5)]
df_179 <- df_179 %>%
  rename(binary_nsr = binary_nsr_past) %>%
  mutate(binary_nsr_past = ifelse(gru_nsr > 0.5, 1, 0))

df_179 <- df_179[c(1,2,3,7,4,5,6)]
write.xlsx(df_179, 
           "revised_com_rest_179.xlsx")



df_179 %>%
  count(binary_nsmc)

df_179 %>%
  count(binary_nsr)

df_179 %>% filter(binary_nsmc == 1)

qqnorm(df_179$gru_nsmc)
qqline(df_179$gru_nsmc)


df_179_re <- df_179 %>% 
  mutate(plus = gru_nsr + gru_nsmc,
         minus = gru_nsmc - gru_nsr,
         minus_ab = abs(gru_nsmc - gru_nsr),
         mean = (gru_nsr + gru_nsmc)/2)

df_179_re <- df_179_re %>% 
  mutate(correct = ifelse(binary_nsmc == binary_nsr,
                          "o", "x"))


df_179 %>% 
  filter(binary_nsmc == binary_nsr) %>%
  count(binary_nsmc)

View(df_179 %>% 
       filter(binary_nsmc == binary_nsr))

df_179_re <- df_179 %>% 
  mutate(correct = ifelse(binary_nsmc == binary_nsr,
                          "o", "x"))

df_179_re_x <- df_179_re %>% 
  filter(correct == "x")

df_179_re_x %>%
  count(binary_nsmc, binary_nsr)

write.xlsx(df_179_re, "df_179_re.xlsx")

df_179 <- tibble(read.xlsx("df_179_re.xlsx"))
rest120_129 <- tibble(read.xlsx("revised_com_rest_120_129.xlsx"))
rest129_159 <- tibble(read.xlsx("revised_com_rest_129_159.xlsx"))
rest_159_end <- tibble(read.xlsx("revised_com_rest_159_end.xlsx"))

rest_159_end %>%
  filter(str_detect(comment, "#"))



rest120_end <- rbind(rest120_129, rest129_159, rest_159_end)
write.xlsx(rest120_end, "rest120_end.xlsx")

rest120_end <- tibble(read.xlsx("rest120_end.xlsx"))

rest120_end <- rest120_end %>%
  mutate(binary_nsr_past = ifelse(gru_nsr > 0.5, 1, 0),
         binary_nsmc = ifelse(gru_nsmc > 0.5, 1, 0))

rest120_end %>%
  filter(str_detect(comment, "#"))


rest120_end <- rest120_end[c(1,2,3,6,4,5,7)]

df_nsr_nsmc <- rbind(df_179[1:7], rest120_end)
write.xlsx(df_nsr_nsmc, "df_nsr_nsmc.xlsx")
df_nsr_nsmc <- tibble(read.xlsx("df_nsr_nsmc.xlsx"))

df_nsr_nsmc %>%
  filter(str_detect(comment, "#"))


df_nsr_nsmc_re <- df_nsr_nsmc %>%
  mutate(correct = ifelse(binary_nsmc == binary_nsr,
                          "o", "x"))

df_nsr_nsmc_re <- df_nsr_nsmc_re %>% 
  mutate(plus = gru_nsr + gru_nsmc,
         minus = gru_nsmc - gru_nsr,
         minus_ab = abs(gru_nsmc - gru_nsr),
         mean = (gru_nsr + gru_nsmc)/2)

df_nsr_nsmc_re <- df_nsr_nsmc_re %>% distinct(comment, .keep_all = TRUE)
write.xlsx(df_nsr_nsmc_re, "df_nsr_nsmc_re.xlsx")

df_nsr_nsmc_re %>%
  count(correct)

setwd("C:/Users/hwwe1/Desktop/ytb_tm_files/project_mj/train/trains")
df_nsr_nsmc_re <- tibble(read.xlsx("df_nsr_nsmc_re.xlsx"))

df_nsr_nsmc_re %>%
  filter(str_detect(comment, "#"))

df_nsr_nsmc_re_o_1 <- df_nsr_nsmc_re %>%
  filter(correct == "o")  %>%
  filter(binary_nsmc == 1)

df_nsr_nsmc_re_o_0 <- df_nsr_nsmc_re %>%
  filter(correct == "o")  %>%
  filter(binary_nsmc == 0)

df_nsr_nsmc_re_o_1 <- sample_n(df_nsr_nsmc_re_o_1, size = 50000)
df_nsr_nsmc_re_o_0 <- sample_n(df_nsr_nsmc_re_o_0, size = 50000)

df_nsr_nsmc %>% filter(duplicated(comment))
View(df_nsr_nsmc %>% filter(duplicated(comment)))

df_nsr_nsmc_re_o_100 <- rbind(df_nsr_nsmc_re_o_1, df_nsr_nsmc_re_o_0)

#-----------------------------------
# 긍정 사전 호출
pos_words_not_dict <- read_json("pos_words(not_dict).json")
pos_words_not_dict <- paste0(pos_words_not_dict)


# 부정 사전 호출
neg_words <- read_json("neg_words.json")
neg_words <- paste0(neg_words)


df_nsr_nsmc_re_o_1
df_nsr_nsmc_re_o_0


View(df_nsr_nsmc_re_o_1 %>% # 필터링 후 댓글 수
       filter(str_detect(comment, neg_words)))

df_nsr_nsmc_re_o_0 %>% # 필터링 후 댓글 수
  filter(str_detect(comment, pos_words_not_dict))

df_nsr_nsmc_re_o_100 %>%
  filter(binary_nsr_past != binary_nsr)





comments_for_tr <- df_nsr_nsmc_re[c(1,2)]


comments_for_tr <- comments_for_tr %>%
  mutate(id = seq(1,length(comments_for_tr$comment),1))

comments_for_tr <- comments_for_tr[c(3,1,2)]
write.xlsx(comments_for_tr, "comments_for_tr.xlsx")

sum(comments_for_tr$length)


comments_for_tr %>%
  filter(id <= 20000) %>%
  summarise(length_sum = sum(length))

# deepl은 백만자 이상 문서를 번역할 수 없음...


comments_for_tr %>%
  filter(id <= 2000) %>%
  summarise(length_sum = sum(length))


comments_for_tr <- comments_for_tr %>%
  filter(id <= 20000)


df_nsr_nsmc_re <- tibble(read.xlsx("df_nsr_nsmc_re.xlsx"))

setwd('C:\\Users\\hwwe1\\Desktop\\ytb_tm_files\\project_mj\\train\\vader & blob')
df_vabl <- tibble(read.xlsx("vader_blob_eng_all_comments.xlsx"))
df_vabl <- df_vabl[c(3,4,5)]
df_vabl <- df_vabl %>%
  rename(eng_comment = comment)


intergrated <- tibble(cbind(df_nsr_nsmc_re, df_vabl))
intergrated <- intergrated[c(1,13,2,14,15,3,6,4,5,7:12)]


intergrated_scores <- intergrated[c(1,4,5,6,7)]

write.xlsx(intergrated, "intergrated_all.xlsx")
write.xlsx(intergrated_scores, "intergrated_scores.xlsx")


cor(intergrated_scores[-1])


intergrated_scores2 <- intergrated[c(1,4,5,6,7:10)]

intergrated_scores2 <- intergrated_scores2 %>%
  mutate(category_vader = ifelse(vader_poloarity > 0, 1, 
                                 ifelse(vader_poloarity == 0, 0, -1)),
         category_blob = ifelse(blob_polarity > 0, 1, 
                                 ifelse(blob_polarity == 0, 0, -1)))

intergrated_scores2 <- intergrated_scores2 %>%
  rename(vader_polarity = vader_poloarity)

write.xlsx(intergrated_scores2, "intergrated_scores2.xlsx")

setwd('C:\\Users\\hwwe1\\Desktop\\ytb_tm_files\\project_mj\\train\\vader & blob')
intergrated_scores2 <- tibble(read.xlsx("intergrated_scores2.xlsx"))



intergrated_scores2 %>% 
  mutate(vader_polarity2 = (vader_polarity+1)/2) %>% 
  select(vader_polarity, vader_polarity2) %>%
  print(n=100)

 
intergrated_scores3 <- intergrated_scores2 %>% 
  mutate(vader_polarity = (vader_polarity+1)/2,
         blob_polarity = (blob_polarity+1)/2,
         category_vader = ifelse(vader_polarity > 0.5, 1, 0),
         category_blob  = ifelse(blob_polarity > 0.5, 1, 0))
  
write.xlsx(intergrated_scores3, "intergrated_scores3.xlsx")
  
cor(intergrated_scores3[2:5])

setwd('C:\\Users\\hwwe1\\Desktop\\ytb_tm_files\\project_mj\\train\\vader & blob')
intergrated_scores3 <- tibble(read.xlsx("intergrated_scores3.xlsx"))
View(intergrated_scores3[c(1,6:10)])


intergrated_scores3 %>% count(binary_nsr_past)
intergrated_scores3 %>% count(binary_nsr)
intergrated_scores3 %>% count(binary_nsmc)
intergrated_scores3 %>% count(category_vader)
intergrated_scores3 %>% count(category_blob)

intergrated_scores3 %>% group_by(binary_nsr_past) %>% count(category_blob)
intergrated_scores3 %>% group_by(binary_nsr) %>% count(category_blob)
intergrated_scores3 %>% group_by(binary_nsmc) %>% count(category_blob)

intergrated_scores3 %>% group_by(binary_nsr_past) %>% count(category_vader)
intergrated_scores3 %>% group_by(binary_nsr) %>% count(category_vader)


intergrated_scores3 %>% group_by(binary_nsmc) %>% count(category_vader) ##
intergrated_scores3 %>%
  filter((binary_nsmc == 1)&(category_vader == 1))

intergrated_scores3 %>%
  filter((binary_nsmc == 1)&(category_vader == 1)) %>%
  select(comment, category_vader)



intergrated_scores3_00 <- intergrated_scores3 %>%
  filter((binary_nsmc == 0)&(category_vader == 0))

intergrated_scores3_11 <- intergrated_scores3 %>%
  filter((binary_nsmc == 1)&(category_vader == 1))

sample_n(intergrated_scores3_00, size = 50000)
sample_n(intergrated_scores3_11, size = 50000)

intrg0011 <- rbind(sample_n(intergrated_scores3_00, size = 50000),
                   sample_n(intergrated_scores3_11, size = 50000))
#--------------
intergrated_scores4 <- intergrated_scores3 %>%
  filter((vader_polarity != 0.5)&(blob_polarity != 0.5))

intergrated_scores4 %>% count(category_vader)
intergrated_scores4 %>% count(binary_nsmc)
  
intergrated_scores4 %>% 
  filter(category_vader == 0) %>% 
  select(comment, category_vader) %>%
  sample_n(size=30) %>%
  print(n=Inf)
  
intergrated_scores4 %>% 
  filter(category_vader == 1) %>% 
  select(comment, category_vader) %>%
  sample_n(size=30) %>%
  print(n=Inf)


intergrated_scores4 %>%
  filter((binary_nsmc == 0)&(category_vader == 0))

intergrated_scores4 %>%
  filter((binary_nsmc == 1)&(category_vader == 1))

#-------------------------------------
intergrated_scores5 <- intergrated_scores3[c(1:5)]
intergrated_scores5 <- intergrated_scores5 %>%
  mutate(mean_score = (vader_polarity + blob_polarity + gru_nsr + gru_nsmc)/4,
         binary_label = ifelse(mean_score > 0.5, 1, 0))

intergrated_scores5 <- intergrated_scores5 %>%
  mutate(mean_score = (vader_polarity + gru_nsmc)/2,
         binary_label = ifelse(mean_score > 0.5, 1, 0))

#---------------
intergrated_scores6 <- intergrated_scores4[c(1:5)]
intergrated_scores6 <- intergrated_scores6 %>%
  mutate(mean_score = (vader_polarity + blob_polarity + gru_nsr + gru_nsmc)/4,
         binary_label = ifelse(mean_score > 0.5, 1, 0))

intergrated_scores6 <- intergrated_scores6 %>%
  mutate(mean_score = (vader_polarity + gru_nsmc)/2,
         binary_label = ifelse(mean_score > 0.5, 1, 0))

intergrated_scores6 %>%
  filter((mean_score > 0.55) | (mean_score < 0.45)) %>% 
  select(comment, mean_score, binary_label) %>% 
  filter(binary_label == 1) %>%
  sample_n(size=30) %>%
  print(n=Inf)

intergrated_scores6 %>%
  filter((mean_score > 0.55) | (mean_score < 0.45)) %>% 
  select(comment, mean_score, binary_label) %>% 
  count(binary_label)

intergrated_scores7 <- intergrated_scores6 %>%
  filter((mean_score > 0.55) | (mean_score < 0.45)) %>% 
  select(comment, mean_score, binary_label)

intergrated_scores7_neg <- intergrated_scores7 %>% 
  filter(binary_label == 0) %>%
  sample_n(size=50000) 
  
intergrated_scores7_pos <- intergrated_scores7 %>% 
  filter(binary_label == 1) %>%
  sample_n(size=50000) 


intergrated_scores7_100 <- rbind(intergrated_scores7_neg, intergrated_scores7_pos)

intergrated_scores7_100 %>% sample_n(size = 100) %>% print(n = 20)
  
write.xlsx(intergrated_scores7_100, "intergrated_scores7_100.xlsx")
  
  
  
  

intergrated_scores6 %>% 
  filter(binary_label == 1) %>% 
  select(comment, binary_label) %>%
  sample_n(size=30) %>%
  print(n=Inf)

intergrated_scores5 %>% 
  filter(binary_label == 0) %>% 
  select(comment, binary_label) %>%
  sample_n(size=30) %>%
  print(n=Inf)


intergrated_scores3 %>%
  filter((binary_nsmc == 0)&(category_vader == 0))

intergrated_scores3 %>%
  filter((binary_nsmc == 1)&(category_vader == 0))

intergrated_scores3 %>%
  filter((binary_nsmc == 0)&(category_vader == 1))




intergrated_scores2 %>%
  filter((binary_nsmc == 0)&(category_vader == -1))


View(intergrated_scores2 %>%
       filter((binary_nsmc == 0)&(category_vader == -1)))



intergrated_scores2 %>%
  filter(category_vader != 0)

intergrated_scores2 %>%
  filter((category_blob != 0)&
           (category_vader != 0)) %>%
  count(binary_nsmc)


intergrated_scores2 %>%
  filter((category_blob != 0)&
           (category_vader != 0)) %>%
  count(binary_nsr)

View(intergrated_scores2 %>%
       filter((category_blob != 0)&
                (category_vader != 0)) )



write.xlsx(intrg0011, "intrg0011.xlsx")



intergrated_scores3 %>%
  filter(vader_polarity != 0.5) %>%
  select(comment, vader_polarity, category_vader) %>%
  count(category_vader)

comment_vader <- intergrated_scores3 %>%
  filter(vader_polarity != 0.5) %>%
  select(comment, vader_polarity, category_vader)


comment_vader1 <- comment_vader %>% filter(category_vader == 1) %>% sample_n(size = 75000)
comment_vader0 <- comment_vader %>% filter(category_vader == 0) %>% sample_n(size = 75000)

vader_15 <- rbind(comment_vader1, comment_vader0)
write.xlsx(vader_15, "vader_15.xlsx")







