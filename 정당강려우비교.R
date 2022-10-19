#2.1
c("tidyverse","tidytable","rvest","janitor","tidytext","RcppMeCab","tidylo","gt")->pkg
sapply(pkg,function(x){
  if(!require(x,ch=T))install.packages(x,dependencies = T)
})
sapply(pkg,require,ch=T)
url<-"https://www.peoplepowerparty.kr/renewal/about/preamble.do"
text_css<-"#wrap>div.content-area.about.preamble>div>div.page-content>div.content-box>div>div.line-txt-box>div"
url%>% read_html()%>%
  html_node(css=text_css)%>%
  html_text()->ppp_v
saveRDS(ppp_v,"ppp_v.rds")

url<-"https://theminjoo.kr/introduce/rule/doct"
text_css<-"#content>div.minjoo_rule.tap_wrap>div.rule_cnt.tap_cnt_box.container_t>div>dl"

url%>%read_html()%>%
  html_node(css=text_css)%>%
  html_text()->tmj_v
saveRDS(tmj_v,"tmj_v.rds")

readRDS("ppp_v.rds")%>%
  tibble(text=.)%>%
  unnest_tokens(output = word1,input = text, token=pos)%>%
  separate(col=word1,
           into = c("word","pos"),
           sep = "/") -> ppp_df

names(ppp_df)

readRDS("tmj_v.rds")%>%
  tibble(text=.)%>%
  unnest_tokens(word1,text,token=pos)%>%
  separate(col=word1,
           into = c("word","pos"),
           sep="/")->tmj_df
names(tmj_df)
  
#2.3
nrow(ppp_df)->n_ppp
nrow(tmj_df)->n_tmj     
data.table(
  국민의당=n_ppp,
  더불어민주당=n_tmj
)%>%gt()%>%
  tab_header("강령 어휘 水")
#2.3.2
ppp_df->df
n_ppp->n_total     

df%>%
  filter(str_length(word)>1)%>%
  filter(pos=='nng')%>%
  count(word,sort = T)%>%
  mutate(n_bytotal10000=round(n/n_total*10000,0))%>%
  head(15)->top_ppp

tmj_df->df
n_tmj->n_total     
df%>%
  filter(str_length(word)>1)%>%
  filter(pos=='nng')%>%
  count(word,sort = T)%>%
  mutate(n_bytotal10000=round(n/n_total*10000,0))%>%
  head(15)->top_tmj

bind_cols(top_ppp, top_tmj) %>% 
  gt() %>% 
  tab_header("상위 빈도 명사") %>%
  tab_spanner(label="국민의당",columns=1:3)%>%
  tab_spanner(label="더불어민주당",columns=4:6) %>% 
  cols_label(word...1 = "명사",
             n...2 = "빈도",
             n_bytotal10000...3 = "만분율",
             word...4 = "명사",
             n...5 = "빈도",
             n_bytotal10000...6 = "만분율")

#2.3.2.1
inner_join(ppp_df%>%count(word,sort=T),
           tmj_df%>%count(word,sort = T),
           by=c("word"))%>%filter(str_length(word)>1)%>%
  mutate(ppp_by10000=round(n.x/n_ppp,5)*10000,
         tmj_by10000=round(n.y/n_tmj,5)*10000)%>%
  arrange(desc(ppp_by10000))%>%head(15)%>%
  gt()%>%tab_header("양당이 함께 사용한 단어")%>%tab_spanner(
    label = "빈도",columns = 2:3)%>%tab_spanner(label = "만분률",columns = 4:5)%>%cols_label(word="단어",
                                                                                        n.x="국민의당",
                                                                                        ppp_by10000="국민의당",
                                                                                        n.y="더불어민주당",
                                                                                        tmj_by10000="더불어민주당")


#2.3.3
inner_join(
  ppp_df %>% count(word, sort = T),
  tmj_df %>% count(word, sort = T),
  by = c("word")
  ) %>% filter(str_length(word) >1) %>%
  mutate(ppp_by10000 = round(n.x/n_ppp, 5) * 10000,
         tmj_by10000 = round(n.y/n_tmj, 5) * 10000,
         diff = ppp_by10000 - tmj_by10000) %>%
  arrange(desc(diff)) %>%
  head(15) -> com_ppp
inner_join(
  ppp_df %>% count(word, sort = T),
  tmj_df %>% count(word, sort = T),
  by = c("word")
) %>% filter(str_length(word) >1) %>%
  mutate(ppp_by10000 = round(n.x/n_ppp, 5) * 10000,
         tmj_by10000 = round(n.y/n_tmj, 5) * 10000,
         diff = ppp_by10000 - tmj_by10000) %>%
  arrange(diff) %>%
  head(15) -> com_tmj

bind_cols(
  com_ppp %>% select.(-c(n.x, n.y)), 
  com_tmj %>% select.(-c(n.x, n.y)) ) %>%
  gt() %>% tab_header( "공동어 중 상대적으로 더 많이 쓴 단어") %>%
  tab_spanner(
  label = "국민의당 기준",
  columns = 1:4
) %>% tab_spanner(
  label = "더불어민주당 기준",
  columns = 5:8
) %>% cols_label(
  word...1 = "명사",
  ppp_by10000...2 = "만분율ppp",
  tmj_by10000...3  = "만분율tmj",
  diff...4 = "차이",
  word...5 = "명사",
  ppp_by10000...6 = "만분율ppp",
  tmj_by10000...7  = "만분율tmj",
  diff...8 = "차이",
)

##2.3.3.2 문서 전반의 상대빈도

bind_rows(ppp_df, tmj_df, .id = "party")  %>% 
  filter(str_length(word) > 1) %>% 
  count(word, party) %>% 
  bind_log_odds(set = party,
                feature = word, 
                n = n) %>% 
  arrange(-log_odds_weighted) -> weighted_log_odds_df

####ERROR
bind_cols(weighted_log_odds_df %>%   
    group_by(party = ifelse(party == 1, "ppp", "tmj")) %>% 
    arrange(party) %>% 
    select.(-party) %>%   
    head(15),weighted_log_odds_df %>%   
    group_by(party = ifelse(party == 1, "ppp", "tmj")) %>% 
    arrange(desc(party)) %>% 
    select.(-party) %>%     
    head(15) 
) %>% gt() %>% tab_header(
  "상대적으로 많이 사용한 단어"
) %>% tab_spanner(
  label = "국민의당 기준",
  columns = 1:3
) %>% tab_spanner(
  label = "더불어민주당 기준",
  columns = 4:6
) %>% cols_label(
  word...1 = "명사",
  n...2 = "빈도",
  log_odds_weighted...3 = "가중상대빈도",
  word...4 = "명사",
  n...5 = "빈도",
  log_odds_weighted...6 = "가중상대빈도"
) %>% fmt_number(
  columns = starts_with("log"), 
  decimals = 2
)


#2.3.4 감정어 빈도
url_v <- "https://github.com/park1200656/KnuSentiLex/archive/refs/heads/master.zip"
dest_v <- "knusenti.zip"
download.file(url = url_v, 
              destfile = dest_v,
              mode = "wb")
unzip("knusenti.zip")
senti_name_v <- list.files("KnuSentiLex-master/.")[9]
senti_dic_df <- read_tsv(str_c("data/KnuSentiLex-master/", senti_name_v), col_names = F)
senti_dic_df <- senti_dic_df %>% rename(word = X1, sScore = X2)
senti_dic_df %>% 
  filter(!is.na(sScore)) %>% 
  add_row(word = "갈등", sScore = -1) -> senti_dic_df 
senti_dic_df %>% 
  filter(!is.na(sScore)) %>% count(sScore)
saveRDS(senti_dic_df, "knu_dic.rds")
list.files(pattern = "^knu")
readRDS("knu_dic.rds") -> knu_dic_df
ppp_df %>% inner_join(knu_dic_df) -> emo_ppp
tmj_df %>% inner_join(knu_dic_df) -> emo_tmj
bind_cols(
  emo_ppp %>% 
    count(word, sScore, sort = T) %>% 
    filter(str_length(word) > 1) %>% 
    mutate(word = reorder(word, n)) %>% 
    head(15),
  emo_tmj %>% 
    count(word, sScore, sort = T) %>% 
    filter(str_length(word) > 1) %>% 
    mutate(word = reorder(word, n)) %>% 
    head(15) 
) %>% gt() %>% tab_header(
  "많이 사용한 감정어"
) %>% tab_spanner(
  label = "국민의당",
  columns = 1:3
) %>% tab_spanner(
  label = "더불어민주당",
  columns = 4:6
) %>% cols_label(
  word...1 = "감정어",
  sScore...2 = "감정점수",
  n...3 = "빈도",
  word...4 = "감정어",
  sScore...5 = "감정점수",
  n...6 = "빈도"
) 
emo_ppp %>% 
  mutate(감정 = case_when(
    sScore > 0 ~ "긍정",
    sScore < 0 ~ "부정",
    TRUE ~ "중립"
  )) -> emo2_ppp
emo_tmj %>% 
  mutate(감정 = case_when(
    sScore > 0 ~ "긍정",
    sScore < 0 ~ "부정",
    TRUE ~ "중립"
  )) -> emo2_tmj
inner_join(by = "감정",
           emo2_ppp %>% tabyl(감정) %>% 
             adorn_totals() %>% 
             adorn_pct_formatting(),
           emo2_tmj %>% tabyl(감정) %>% 
             adorn_totals() %>% 
             adorn_pct_formatting()
) %>% gt() %>% tab_header(
  "감정어 비율"
) %>% tab_spanner(
  columns = 2:3,
  label = "국민의당"
) %>% tab_spanner(
  columns = 4:5,
  label = "더불어민주당"
) %>% cols_label(
  n.x = "빈도",
  percent.x = "백분율",
  n.y = "빈도",
  percent.y = "백분율"
)  


inner_join(
  emo2_ppp %>% count(word, 감정, sort = T),
  emo2_tmj %>% count(word, 감정, sort = T),
  by = c("word", "감정")
) %>% 
  filter(str_length(word) > 1) %>% 
  mutate(ppp_by10000 = round(n.x/n_ppp, 5) * 10000,
         tmj_by10000 = round(n.y/n_tmj, 5) * 10000) %>% 
  arrange(감정) %>% 
  select.(-감정) %>% 
  head(15) %>% 
  gt() %>% tab_header(
    "양당이 함께 사용한 긍정어"
  ) %>% tab_spanner(
    columns = starts_with("n"),
    label = "빈도"
  ) %>% tab_spanner(
    columns = ends_with("10000"),
    label = "만분률"
  ) %>% cols_label(
    n.x = "ppp",
    n.y = "tmj",
    ppp_by10000 = "ppp",
    tmj_by10000 = "tmj"
  )


bind_rows(emo_ppp, emo_tmj, .id = "party") %>% 
  filter(str_length(word) > 1) %>% 
  count(word, party) %>% 
  bind_log_odds(set = party,
                feature = word, 
                n = n) %>% 
  arrange(-log_odds_weighted) -> weighted_log_odds_df


##ERROR
bind_cols(
  weighted_log_odds_df %>%   
    group_by(par = case_when(
      party == 1L ~ "ppp"
      TRUE ~ "tmj")) %>% 
    arrange(party) %>% 
    select.(-party) %>%   
    head(15),
  
  weighted_log_odds_df %>%   
    group_by(par = ifelse(party == 1L, "ppp", "tmj")) %>% 
    arrange(desc(party)) %>% 
    select.(-party) %>%   
    head(15)
  
  ) %>% gt() %>% tab_header(
  "상대적으로 더 많이 사용한 감정어"
) %>% tab_spanner(
  label = "국민의당 기준",
  columns = 1:3
) %>% tab_spanner(
  label = "더불어민주당 기준",
  columns = 4:6
) %>% cols_label(
  word...1 = "감정어",
  n...2 = "빈도",
  log_odds_weighted...3 = "가중상대빈도",
  word...4 = "감정어",
  n...5 = "빈도",
  log_odds_weighted...6 = "가중상대빈도"
) %>% fmt_number(
  columns = starts_with("log"), 
  decimals = 2
)



#2.4
old.hooks <- fansi::set_knit_hooks(knitr::knit_hooks)
library(glue)
library(crayon)
library(fansi)
options(crayon.enabled = TRUE)
crayon_words <- function(input_text, word = " "){
  replaced_text <- str_replace_all(input_text, word, "{red{word}}")
  for (i in 1:length(replaced_text)) {
    crayon_text <- glue::glue_col(deparse(replaced_text[[i]]))
    print(crayon_text)
    
  }
}
"국가적 위기 해결에 앞장서야" %>%
  crayon_words(input_text = ., "해결")
readRDS("ppp_v.rds") %>% 
  tibble(text = .) %>%
  unnest_tokens(output = sentences,input = text,
                tooken=:"regex",pattern = "\\.") -> ppp_st
