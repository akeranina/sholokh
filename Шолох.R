library(XML)
library(tidyverse)
library(tidytext)
library(tokenizers)
library(stopwords)
library(udpipe)
library(ggplot2)
library(ggraph)
library(igraph)
library(rulexicon)
library(forcats)
library(dplyr)
library(plotly)
library(wordcloud2)

`%notin%` <- Negate(`%in%`)

chars_names <- c("гординиус", "горди", "сай", "марцела", "ринда", "мелисса", "ноа", "пиония",
                 "бундруми", "шаграух", "петр", "хинхо", "цфат", "аверик", "рили", "улиус",
                 "варрок", "айрин", "бетти", "хасилиус", "габриэль", "дайен", "очо", "орлин",
                 "джи", "ол`эн", "рейнич", "велот", "драби", "шлэйл", "ол’эн", "карлиннан", "авен",
                 "эш", "селия", "о́рлин", "льюь", "митрас", "бастерс", "бурундук", "иладриль")

additional_names <- c("лис", "сайнор", "аутурни", "дану", "селеста",
                      "авена", "рэнди", "пиония", "давьер", "кес", "андрис", "йоукли", "мчаться")

not_for_plot <- c("гординиус", "горди", "сай", "ринда", "мелисса", "ноа",
                  "бундруми", "шаграух", "петр", "цфат", "аверик", "рили", 
                  "варрок", "айрин", "бетти", "хасилиус", "габриэль", "дайен", "очо", "орлин",
                  "джи", "ол`эн", "рейнич", "велот", "драби", "шлэйл", "ол’эн", "авен",
                  "эш", "о́рлин", "льюь", "митрас", "бастерс", "иладриль","лис", "давьер", "кес",
                  "йоукли", "мчаться")


main_chars_names <- c("тинави", "полынь", "кадия", "анте", "дахху", "теннет", 
                      "карл", "лиссай", "рэндом", "мелисандр")

body_parts <- c("глаз", "рука", "голова", "палец", "лицо", "нога", "язык", "губа", "запястье",
                "плечо", "нос", "волос", "бровь", "взгляд", "голос", "грудь", "ухо")

other <- c("самый", "другой", "сам", "шолох", "больший", "долгий", "раз")

### Приведение текста в нужный вид

# Теневые блики

filename = "Теневые блики.fb2"
doc <- xmlTreeParse(filename, useInternalNodes = T)
rootnode <- xmlRoot(doc)
ns <- xmlNamespace(rootnode)

chapter_nodes <- getNodeSet(rootnode, "//a:body//a:section", 
                            namespaces = c("a"=ns)) 

get_chapter <- function(node) {
  chapter <- xmlValue(node[["title"]])
  return(chapter)
}

get_text <- function(node) {
  text <- xmlElementsByTagName(node, "p") %>%
    map_chr(xmlValue) %>%
    str_c(collapse = " ")
  return(text)
}

doc_id <- rep_along(1:48, "Теневые блики")
chapter_id <- map_chr(chapter_nodes, get_chapter)
texts <- map_chr(chapter_nodes, get_text)

# тиббл

sholokh_tbl <- tibble(doc_id = doc_id, 
                        chapter_id = chapter_id, 
                        text = texts) 

sholokh_tbl <- sholokh_tbl %>%
  separate(chapter_id, into = c("chapter_id", "chapter_name"), sep = "\\d+", remove = F) %>%
  filter(row_number() %in% c(2:42))

new_col <- str_replace_na(sholokh_tbl$chapter_name, replacement = "Эпилог. Море")

sholokh_tbl <- sholokh_tbl %>%
  mutate(chapter_id = paste(as.character(row_number()), new_col[row_number()], sep = ". ")) %>%
  select(-chapter_name)

# Тень разрастается

filename = "Тень разрастается.fb2"
doc <- xmlTreeParse(filename, useInternalNodes = T)
rootnode <- xmlRoot(doc)
ns <- xmlNamespace(rootnode)

chapter_nodes <- getNodeSet(rootnode, "//a:body//a:section//a:section", 
                            namespaces = c("a"=ns)) 

doc_id <- rep_along(1:42, "Тень разрастается")
chapter_id <- map_chr(chapter_nodes, get_chapter)
texts <- map_chr(chapter_nodes, get_text)

sholokh_2_tbl <- tibble(doc_id = doc_id, 
                      chapter_id = chapter_id, 
                      text = texts)

sholokh_2_tbl <- sholokh_2_tbl %>%
  filter(row_number() %in% 1:36) %>%
  mutate(chapter_id = paste(as.character(row_number()), chapter_id[row_number()], sep = ". "))

# Призрачные рощи

filename = "Призрачные рощи.fb2"
doc <- xmlTreeParse(filename, useInternalNodes = T)
rootnode <- xmlRoot(doc)
ns <- xmlNamespace(rootnode)

chapter_nodes <- getNodeSet(rootnode, "//a:body//a:section", 
                            namespaces = c("a"=ns)) 

doc_id <- rep_along(1:58, "Призрачные рощи")
chapter_id <- map_chr(chapter_nodes, get_chapter)
texts <- map_chr(chapter_nodes, get_text)

sholokh_3_tbl <- tibble(doc_id = doc_id, 
                        chapter_id = chapter_id, 
                        text = texts)

sholokh_3_tbl <- sholokh_3_tbl %>%
  filter(row_number() %in% 1:39)

# полный тиббл
full_sholokh <- rbind.data.frame(sholokh_tbl, sholokh_2_tbl, sholokh_3_tbl) %>%
  rename(chapter = chapter_id)

### Лемматизирую и токенизирую, чищу

russian_syntagrus <- udpipe_load_model(file = "russian-syntagrus-ud-2.5-191206.udpipe")
sholokh_ann <- udpipe_annotate(russian_syntagrus, full_sholokh$text, 
                               doc_id = full_sholokh$doc_id, tagger = "default", parser = "none")
sholokh_ann <- as_tibble(sholokh_ann)

tidy_sholokh <- sholokh_ann %>%
  rename(chapter_id = paragraph_id) %>%
  select(c(doc_id, chapter_id, sentence_id, sentence, token, lemma, upos)) %>%
  mutate(lemma = tolower(lemma)) %>% 
  mutate_at(vars(lemma), ~
              case_when(lemma == "кадий" ~  "кадия",
                        str_detect(lemma, "тинавь") ~ "тинави",
                        str_detect(lemma, "дахх[ой]*") ~ "дахху",
                        str_detect(lemma, "мелисандра") ~ "мелисандр",
                        str_detect(lemma, "лиссый") ~ "лиссай",
                        str_detect(lemma, "лиссать") ~ "лиссай",
                        str_detect(lemma, "унн[ить]*") ~ "унни",
                        str_detect(lemma, "шолохо") ~ "шолох",
                        str_detect(lemma, "теннета") ~ "теннет",
                        str_detect(lemma, "шэрхенмист[ос]*") ~ "шэрхенмиста",
                        str_detect(lemma, "сайнора") ~ "сайнор",
                        str_detect(lemma, "бокка") ~ "бокки",
                        str_detect(lemma, "бром") ~ "брома",
                        str_detect(lemma, "рэнд[ий]*") ~ "рэндом",
                        str_detect(lemma, "селест") ~ "селеста",
                        str_detect(lemma, "дан") ~ "дану",
                        str_detect(lemma, "селесты") ~ "селеста",
                        str_detect(lemma, "мудр[е]?") ~ "мудра",
                        TRUE ~ .)) %>%
  filter(!upos %in% c("PUNCT", "SYM", "X"))

#### Самые характерные слова каждой книги

tidy_sholokh <- tidy_sholokh %>%
  mutate_at(vars(lemma), ~
              case_when(lemma == "мумие" ~  "мумия",
                        str_detect(lemma, "ринд[ыу]?") ~ "ринда",
                        str_detect(lemma, "горгуль[е]?") ~ "горгулья",
                        str_detect(lemma, "мелисс") ~ "мелисса",
                        str_detect(lemma, "марцел[ыь]?") ~ "марцела",
                        str_detect(lemma, "кат-ш[а]?") ~ "кат-ши",
                        str_detect(lemma, "вир[аов]+") ~ "вир",
                        str_detect(lemma, "ушлепка") ~ "ушлепок",
                        str_detect(lemma, "малекий") ~ "малек",
                        str_detect(lemma, "йоукль") ~ "йоукли",
                        str_detect(lemma, "кромена") ~ "кромен",
                        str_detect(lemma, "аутурень") ~ "аутурни",
                        str_detect(lemma, "трясух") ~ "трясуха",
                        str_detect(lemma, "мурав[оье]*") ~ "муравей",
                        str_detect(lemma, "колдуна") ~ "колдун",
                        str_detect(lemma, "короля") ~ "король",
                        str_detect(lemma, "шалаша") ~ "шалаш",
                        str_detect(lemma, "щалаш") ~ "шалаш",
                        str_detect(lemma, "шев") ~ "шева",
                        str_detect(lemma, "пламень") ~ "пламя",
                        str_detect(lemma, "сабль") ~ "сабля",
                        str_detect(lemma, "мумье") ~ "мумия",
                        str_detect(lemma, "пустоша") ~ "пустошь",
                        str_detect(lemma, "королев") ~ "королева",
                        str_detect(lemma, "сели[еию]?") ~ "селия",
                        str_detect(lemma, "рощ") ~ "роща",
                        str_detect(lemma, "секвойь") ~ "секвойя",
                        str_detect(lemma, "дежурств»") ~ "дежурство",
                        str_detect(lemma, "«график[a»]*") ~ "график",
                        str_detect(lemma, "«график") ~ "график",
                        str_detect(lemma, "портать") ~ "портал",
                        str_detect(lemma, "квест[а-я]?") ~ "квест",
                        str_detect(lemma, "^ант[а-я]?") ~ "анте",
                        str_detect(lemma, "башн[ь]?") ~ "башня",
                        str_detect(lemma, "маньяка") ~ "маньяк",
                        str_detect(lemma, "единорог[уой]*") ~ "единорог",
                        str_detect(lemma, "волн") ~ "волна",
                        str_detect(lemma, "ловчие") ~ "ловчий",
                        str_detect(lemma, "круста") ~ "круст",
                        str_detect(lemma, "[«]?ушлеп[а-я»]*") ~ "ушлепок",
                        TRUE ~ .))

tidy_sholokh <- tidy_sholokh %>%
  mutate_at(vars(book), ~
              case_when(book == "Теневые блики" ~  "1. Теневые блики",
                        str_detect(book, "Тень разрастается") ~ "2. Тень разрастается",
                        str_detect(book, "Призрачные рощи") ~ "3. Призрачные рощи",
                        TRUE ~ .))

tidy_sholokh <- tidy_sholokh %>%
  mutate(upos = case_when(
    lemma %in% c("сам", "самый", "другой") ~ "PRON",
    lemma %in% c("первый", "второй", "третий") ~ "NUM",
    lemma %in% c("лиссай", "дахху") ~ "PROPN",
    lemma %in% c("ловчий", "маньяк", "ушлепок") ~ "NOUN",
    lemma == "бы" ~ "PART",
    TRUE ~ upos  # оставляем остальные значения без изменений
  ))

save(tidy_sholokh, file = "tidy_sholokh.RData")

book_word_counts <- tidy_sholokh %>%
  count(book, lemma, sort = T) %>% 
  ungroup()

book_upos_counts <- tidy_sholokh %>%
  count(book, upos, sort = T) %>%
  ungroup()

total_counts <- book_word_counts %>% 
  group_by(book) %>% 
  summarise(total_words = sum(n))

book_word_counts <- book_word_counts %>% 
  left_join(total_counts)

book_upos_counts <- book_upos_counts %>%
  left_join(total_counts)

book_word_counts <- book_word_counts %>% 
  mutate(rf = round((n / total_words), 5) * 100)

book_word_tfidf <- book_word_counts %>% 
  bind_tf_idf(lemma, book, n) %>%
  arrange(-tf_idf)

tfidf_with_names <- book_word_tfidf %>% 
  arrange(-tf_idf) %>% 
  group_by(book) %>% 
  top_n(20) %>% 
  ungroup() %>% 
  ggplot(aes(reorder_within(lemma, tf_idf, book), tf_idf, fill = book)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), hjust = 1.5) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))
tfidf_with_names

garbage <- c('круто', "гномий", "драконий", "чичко", "дорхес", "такт", 
             "эрик", "шэрхен", "пх", "по-моему", "«нет»")

beasts <- c("баргест", "коблинау", "дракончик", "кат-ши", "хейлонд", 
            "вампир", "единорог", "горгулья", "бэльбог", "буххшо", "шувгей")

tfidf_without_names <- book_word_tfidf %>%
  arrange(-tf_idf) %>%
  filter(lemma %notin% chars_names & lemma %notin% garbage) %>%
  group_by(book) %>% 
  top_n(20) %>% 
  ungroup() %>% 
  ggplot(aes(reorder_within(lemma, tf_idf, book), tf_idf, fill = book)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), hjust = 1.5) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )
tfidf_without_names

ggsave("tf_idf.png", width = 16, height = 8, limitsize = F)

tfidf_without_beasts <- book_word_tfidf %>%
  arrange(-tf_idf) %>%
  filter(lemma %notin% chars_names & lemma %notin% garbage) %>%
  filter(lemma %notin% beasts) %>%
  group_by(book) %>% 
  top_n(20) %>% 
  ungroup() %>% 
  ggplot(aes(reorder_within(lemma, tf_idf, book), tf_idf, fill = book)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), hjust = 1.5) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )
tfidf_without_beasts

### Самые частые части речи
tidy_book_upos_counts <- tidy_sholokh %>%
  #filter(upos %notin% c("DET", "AUX")) %>%
  count(book, upos, sort = T) %>%
  ungroup() %>%
  left_join(total_counts) %>%
  mutate(rf = round((n / total_words), 5) * 100) %>%
  select(-total_words) %>%
  arrange(-rf)


# Создаем вектор сопоставления между английскими тэгами и русскими сокращениями
upos_to_russian <- c(NOUN = "Сущ.", VERB = "Гл.", AUX = "Гл.", ADJ = "Прил.", ADV = "Нар.", 
                     PRON = "Мест.", DET = "Мест.", ADP = "Предл.", NUM = "Числ.", 
                     SCONJ = "Подч. союз", CCONJ = "Соч. союз", PART = "Част.", 
                     PROPN = "Имя собст.", INTJ = "Межд.")

# Функция для замены тэгов на русские сокращения
replace_upos_with_russian <- function(upos_vector) {
  sapply(upos_vector, function(upos) upos_to_russian[[upos]], USE.NAMES = FALSE)
}

# Пример использования функции
tidy_book_upos_counts$upos_rus <- replace_upos_with_russian(tidy_book_upos_counts$upos)

corr_order = c("Сущ.", "Гл.", "Мест.", "Предл.", "Нар.", "Прил.", 
               "Част.", "Соч. союз", "Имя собст.", "Подч. союз", "Числ.", "Межд.")

tidy_book_upos_counts <- tidy_book_upos_counts %>%
  mutate(upos_rus = factor(upos_rus, levels = corr_order))

upos_plot <- tidy_book_upos_counts %>%
  ggplot(aes(x = upos_rus, y = n, fill = book)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Часть речи", y = "Частотность, слов", 
       fill = "Книга", title = "Самые популярные части речи") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  scale_x_reordered()
upos_plot

ggsave("Части_речи.png", width = 16, height = 8, limitsize = F)

### Просто самые частые слова

another_tidy_book_word_counts <- tidy_sholokh %>%
  filter(upos %notin% c("PRON", "DET", "PART", "VERB", "ADV", "NUM")) %>%
  count(book, lemma, sort = T) %>% 
  ungroup() %>%
  left_join(total_counts) %>%
  filter(lemma %notin% stopwords(language = "ru")) %>%
  filter(lemma %notin% stopwords(language = "ru", source = "marimo")) %>%
  filter(lemma %notin% chars_names) %>%
  mutate(rf = round((n / total_words), 5) * 100) %>%
  select(-total_words) %>%
  arrange(-rf) %>%
  mutate_at(vars(book), ~
              case_when(book == "Теневые блики" ~  "1. Теневые блики",
                        str_detect(book, "Тень разрастается") ~ "2. Тень разрастается",
                        str_detect(book, "Призрачные рощи") ~ "3. Призрачные рощи",
                        TRUE ~ .))

another_tidy_book_word_counts <- tidy_sholokh %>%
  filter(upos == "PROPN") %>%
  count(book, lemma, sort = T) %>% 
  ungroup() %>%
  left_join(total_counts) %>%
  #filter(lemma %notin% stopwords(language = "ru")) %>%
  #filter(lemma %notin% stopwords(language = "ru", source = "marimo")) %>%
  filter(lemma %notin% chars_names) %>%
  mutate(rf = round((n / total_words), 5) * 100) %>%
  select(-total_words) %>%
  arrange(-rf) %>%
  mutate_at(vars(book), ~
              case_when(book == "Теневые блики" ~  "1. Теневые блики",
                        str_detect(book, "Тень разрастается") ~ "2. Тень разрастается",
                        str_detect(book, "Призрачные рощи") ~ "3. Призрачные рощи",
                        TRUE ~ .))

another_tidy_book_word_counts %>%
  #filter(lemma %notin% main_chars_names 
        # & lemma %notin% chars_names 
        # & lemma %notin% additional_names) %>%
  filter(lemma %notin% c("дом", "ловча", "лазарет", "генерал", "башня",
                         "давьер", "кад", "кес", "лис", "карла", "мары")) %>%
  group_by(book) %>% 
  slice_max(order_by = rf, n = 15) %>%
  ungroup() %>%
  mutate(lemma = fct_reorder(lemma, rf)) %>% # Используем fct_reorder для упорядочивания lemma
  ggplot(aes(x = reorder(lemma, rf), y = rf, fill = rf)) +
  geom_bar(stat = "identity", show.legend = F) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), hjust = 1.5) +
  labs(x = NULL, y = "Частотность", title = "Самые частые имена собственные") +
  facet_wrap(~book, scales = "free") +
  coord_flip() +
  scale_fill_gradient2(high = "red", low = "blue",
                           mid = "green", midpoint = 0.1) +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12))

ggsave("Прил.png", width = 16, height = 8, limitsize = F)

tidy_book_word_counts %>%
  group_by(book) %>% 
  top_n(25, wt = rf) %>% 
  ungroup() %>%
  ggplot(aes(lemma, n, col = book)) +
  geom_point(size = 1.2, alpha = 0.7, show.legend = F) +
  theme_bw() +
  coord_flip()


##### Эволюция Теннета и Полыни

polyn <- c("куратор", "ловчий", "напарник")

tennet <- c("маньяк", "теннет", "предприниматель", "кавалер", "ушлепок")

polyn_plot <- another_tidy_book_word_counts %>%
  filter(lemma %in% polyn) %>%
  ggplot(mapping = aes(book, n, fill = lemma)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Книга", y = "Число вхождений", fill = "Слово", title = "Эволюция Полыни") +
  theme_bw(base_family = "Arial Narrow", base_size =  14) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_fill_brewer(palette = "Dark2")
polyn_plot

tennet_plot <- book_word_counts %>%
  filter(lemma %in% tennet) %>%
  select(book, lemma, n) %>%
  group_by(book) %>% 
  summarise(total_words = sum(n))

tennet_words <- book_word_counts %>%
  filter(lemma %in% tennet) %>%
  select(book, lemma, n) %>%
  left_join(tennet_plot) %>%
  mutate(rf = round((n / total_words), 5) * 100) %>%
  mutate(lemma = factor(lemma, levels = tennet))
#mutate(n = case_when(
#n > 200 ~ 200,
#TRUE ~ n))

ggplot(tennet_words, mapping = aes(book, n, fill = lemma)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9)) +
  labs(x = "Книга", y = "Частотность", fill = NULL, title = "Эволюция Анте") +
  theme_bw(base_family = "Arial Narrow", base_size =  14) +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, hjust = 0.5, vjust = 1),
    plot.subtitle = element_text(size = 16, hjust = 0.5, vjust = 1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)) +
  scale_fill_brewer(palette = "Dark2")

# Фильтруем данные для нужных слов
filtered_data <- tidy_sholokh %>%
  #filter(lemma %in% c("куратор", "ловчий", "напарник"))
  filter(lemma %in% tennet)

# Создаем диаграмму рассеяния
scatter_plot <- filtered_data %>%
  ggplot(aes(x = chapter_id, y = sentence_id, color = lemma, shape = lemma)) +
  geom_point(size = 4, alpha = 0.6) +
  labs(x = "Глава", y = "Предложение", color = NULL, shape = NULL, title = "Эволюция Полыни") +
  facet_wrap(~book, scales = "free") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, hjust = 0.5, vjust = 1),
    plot.subtitle = element_text(size = 16, hjust = 0.5, vjust = 1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  )
print(scatter_plot)

scatter_plot_2 <- filtered_data %>%
  ggplot(aes(x = chapter_id, y = sentence_id, color = lemma, shape = lemma)) +
  geom_point(size = 4, alpha = 0.6) +
  scale_shape_manual(values = c(15:19)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Глава", y = "Предложение", color = NULL, shape = NULL, title = "Эволюция Анте") +
  facet_wrap(~book, scales = "free") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, hjust = 0.5, vjust = 1),
    plot.subtitle = element_text(size = 16, hjust = 0.5, vjust = 1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  )
print(scatter_plot_2)

#### Совместная встречаемость

furniture <- c("стол", "коридор", "дверь")

x <-  tidy_sholokh %>% 
  filter(lemma %notin% body_parts & lemma %notin% other) %>%
  filter(lemma %notin% not_for_plot) %>%
  subset(upos %in% c("PROPN", "NOUN")) %>%
  subset(book == "1. Теневые блики")

y <-  tidy_sholokh %>% 
  filter(lemma %notin% body_parts & lemma %notin% other) %>%
  filter(lemma %notin% not_for_plot) %>%
  filter(lemma != 'улиус' & lemma != 'дружка') %>%
  subset(upos %in% c("PROPN", "NOUN")) %>%
  subset(book == "2. Тень разрастается")

z <-  tidy_sholokh %>% 
  filter(lemma %notin% body_parts & lemma %notin% other) %>%
  filter(lemma %notin% not_for_plot & lemma %notin% furniture) %>%
  filter(lemma != 'улиус' & lemma != 'осома') %>%
  subset(upos %in% c("PROPN", "NOUN")) %>%
  subset(book == "3. Призрачные рощи")

meme <- tidy_sholokh %>% 
  filter(lemma %in% body_parts | lemma == "полынь" | lemma == 'браслет' | lemma == 'амулет')

meme <- tidy_sholokh %>% 
  filter(lemma %in% body_parts | lemma == "ловчий" | lemma == 'браслет' | lemma == 'амулет')

cooc <- cooccurrence(meme, term = "lemma", group = c("chapter_id", "sentence_id"))

polyn <- as_tibble(cooc) %>%
  filter(term1 == "полынь" | term2 == "полынь")

lovchy <-  as_tibble(cooc) %>%
  filter(term1 == "ловчий" | term2 == "ловчий") %>%
  mutate_at(vars(term1), ~
            case_when(term1 == "ловчий" ~ "полынь",
                      TRUE ~ .)) %>%
  mutate_at(vars(term2), ~
              case_when(term2 == "ловчий" ~ "полынь",
                        TRUE ~ .)) %>%
  rename(cooc_1 = cooc)

all_polyn <- polyn %>%
  cross_join(lovchy) %>%
  filter(term1.x == term1.y | term1.x == term2.y | term2.x == term1.y | term2.x == term2.y) %>%
  filter(row_number() %in% c(1,22,40,64,81,105,118,142,160,177,204,221,243,258,284,298,320,340,361))

all_polyn <- all_polyn %>%
  mutate(n = cooc+cooc_1) %>%
  select(term1.x, term2.x, n) %>%
  cross_join(body) %>%
  filter(lemma == term1.x | lemma == term2.x)

polyn2 <- all_polyn %>%
  rename(term1 = term1.x) %>%
  rename(term2 = term2.x) %>%
  rename(cooc = n) %>%
  select(term1, term2, cooc)

wordnetwork <- head(polyn2, 19)
wordnetwork <- graph_from_data_frame(wordnetwork)

ggplot_graph <- ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "lightblue") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Части тела Полыни :)")
ggplot_graph
ggsave("Встречаемость_Рощи_без_кабинета.png", width = 16, height = 8, limitsize = F)


interactive_graph <- ggplotly(ggplot_graph)
interactive_graph
        

#### анализ тональности

сhen_skiena <- hash_sentiment_chen_skiena
rusenti2017 <- hash_rusentilex_2017
afinn <- hash_sentiment_afinn_ru
nrc <- hash_sentiment_nrc_emolex_ru

afinn <- afinn %>% 
  mutate_at(vars(score), ~
              case_when(token == "гоблин" ~  0,
                                     TRUE ~ .))

sholokh_sent <- tidy_sholokh %>%
  select(book, chapter_id, lemma) %>%
  rename(token = lemma) %>%
  inner_join(afinn) %>%
  group_by(book) %>%
  add_count(chapter_id, name = 'tokens_per_chapter')

first_tokens <- sholokh_sent %>%
  filter(book == '1. Теневые блики') %>%
  group_by(chapter_id) %>%
  select(tokens_per_chapter) %>%
  distinct()

second_tokens <- sholokh_sent %>%
  filter(book == '2. Тень разрастается') %>%
  group_by(chapter_id) %>%
  select(tokens_per_chapter) %>%
  distinct()

third_tokens <- sholokh_sent %>%
  filter(book == '3. Призрачные рощи') %>%
  group_by(chapter_id) %>%
  select(tokens_per_chapter) %>%
  distinct()

first_chapters <- as_tibble_col(full_sholokh$chapter_id[1:41], column_name = "chapter")
second_chapters <- as_tibble_col(full_sholokh$chapter_id[42:77], column_name = "chapter")
third_chapters <- as_tibble_col(full_sholokh$chapter_id[78:116], column_name = "chapter")

first_book_sent <- sholokh_sent %>%
  filter(book == "1. Теневые блики") %>%
  group_by(chapter_id) %>%
  summarise(sum = sum(score)) %>%
  mutate(book = rep_along(1:41, "1. Теневые блики")) %>%
  arrange(chapter_id) %>%
  add_column(first_chapters) %>%
  inner_join(first_tokens) %>%
  mutate(mean = sum/tokens_per_chapter) %>%
  select(book, chapter, tokens_per_chapter, mean, sum) %>%
  mutate(tone = case_when(mean > 0 ~ "positive",
                          mean >= -0.25 & mean <= 0 ~ "neutral",
                          mean < -0.25 ~ "negative"))
chapter_order_1 <- unique(first_book_sent$chapter) # Преобразование переменной `chapter` в фактор с уровнями в правильном порядке
first_book_sent <- first_book_sent %>%
  mutate(chapter = factor(chapter, levels = chapter_order_1))

second_book_sent <- sholokh_sent %>%
  filter(book == "2. Тень разрастается") %>%
  group_by(chapter_id) %>%
  summarise(sum = sum(score)) %>%
  mutate(book = rep_along(1:36, "2. Тень разрастается")) %>%
  arrange(chapter_id) %>%
  add_column(second_chapters) %>%
  inner_join(second_tokens) %>%
  mutate(mean = sum/tokens_per_chapter) %>%
  select(book, chapter, tokens_per_chapter, mean, sum) %>%
  mutate(tone = case_when(mean > 0 ~ "positive",
                          mean >= -0.25 & mean <= 0 ~ "neutral",
                          mean < -0.25 ~ "negative"))
chapter_order_2 <- unique(second_book_sent$chapter)
second_book_sent <- second_book_sent %>%
  mutate(chapter = factor(chapter, levels = chapter_order_2))

third_book_sent <- sholokh_sent %>%
  filter(book == "3. Призрачные рощи") %>%
  group_by(chapter_id) %>% 
  summarise(sum = sum(score)) %>%
  mutate(book = rep_along(1:39, "3. Призрачные рощи")) %>%
  arrange(chapter_id) %>%
  add_column(third_chapters) %>%
  inner_join(third_tokens) %>%
  mutate(mean = sum/tokens_per_chapter) %>%
  select(book, chapter, tokens_per_chapter, mean, sum) %>%
  mutate(tone = case_when(mean > 0 ~ "positive",
                          mean >= -0.25 & mean <= 0 ~ "neutral",
                          mean < -0.25 ~ "negative"))
chapter_order_3 <- unique(third_book_sent$chapter)
third_book_sent <- third_book_sent %>%
  mutate(chapter = factor(chapter, levels = chapter_order_3))

sholokh_chapter_sent <- rbind.data.frame(first_book_sent, second_book_sent, third_book_sent)

sholokh_book_sent <- sholokh_chapter_sent %>%
  select(book, mean) %>%
  group_by(book) %>%
  summarise(book_mean = mean(mean))

top_words <- sholokh_sent %>%
  filter(book == "3. Призрачные рощи") %>%
  filter(chapter_id == 36) %>%
  add_count(token) %>%
  arrange(desc(n)) %>%
  unique.data.frame(.)

pos_words <- sholokh_sent %>%
  mutate(tone = case_when(mean > 0 ~ "positive",
                          mean >= -0.25 & mean <= 0 ~ "neutral",
                          mean < -0.25 ~ "negative")) %>%
  filter(tone == "positive") %>%
  count(token, sort = T) %>%
  ungroup()

total_pos_words <- pos_words %>% 
  summarise(total_pos = sum(n))

pos_words <- pos_words %>%
  mutate(total_pwords = total_pos_words)

neg_words <- sholokh_sent %>%
  mutate(tone = case_when(mean > 0 ~ "positive",
                          mean >= -0.25 & mean <= 0 ~ "neutral",
                          mean < -0.25 ~ "negative")) %>%
  filter(tone == "negative") %>%
  count(book, token, sort = T) %>% 
  ungroup()

total_neg_words <- neg_words %>% 
  group_by(book) %>% 
  summarise(total_neg = sum(n))

neg_words <- neg_words %>%
  left_join(total_neg_words)

pos_plot <- pos_words %>% 
  group_by(book) %>%
  top_n(20, wt = n) %>%
  ggplot(aes(reorder_within(token, n, book), n, fill = book)) +
  geom_bar(stat="identity", show.legend = F) +
  theme_bw() +
  labs(x="Главные позитивные слова", y="Частота") +
  facet_wrap(~book, scales = "free") +
  scale_x_reordered() +
  coord_flip()
pos_plot

neg_plot <- neg_words %>% 
  group_by(book) %>%
  top_n(20, wt = n) %>%
  ggplot(aes(reorder_within(token, n, book), n, fill = book)) +
  geom_bar(stat="identity", show.legend = F) +
  theme_bw() +
  labs(x="Главные негативные слова", y="Частота") +
  facet_wrap(~book, scales = "free") +
  scale_x_reordered() +
  coord_flip()
neg_plot

first_sent_plot <-
  ggplot(first_book_sent, aes(chapter, sum,  fill = tone)) +
  geom_col(show.legend = F) +
  coord_flip()
first_sent_plot

first_map <- first_book_sent %>%
  ggplot(aes(x=chapter, y=1, fill=mean)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "#56B4E9", mid = "#FFFFFF", high = "#E69F00", midpoint = -0.25) +
  labs(x = NULL, y = NULL, fill = "Тональность", title = 'Тональность "Теневых бликов"') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"))
first_map

first_map <- first_book_sent %>%
  ggplot(aes(x=chapter, y=1, fill=mean)) + 
  geom_tile() + 
  scale_fill_gradient2(high = "#4DAF4A", mid = "#FFFFB3", low = "#555555", midpoint = -0.25) +
  labs(x = NULL, y = NULL, fill = "Тональность", title = 'Тональность "Теневых бликов"') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"))
first_map

second_sent_plot <-
  ggplot(second_book_sent, aes(chapter, sum, fill = tone)) +
  geom_col(show.legend = F)
second_sent_plot

second_map <- second_book_sent %>%
  ggplot(aes(x=chapter, y=1, fill=mean)) + 
  geom_tile() + 
  scale_fill_gradient2(high = "#4DAF4A", mid = "#FFFFB3", low = "#555555", midpoint = -0.25) +
  labs(x = NULL, y = NULL, fill = "Тональность", title = 'Тональность "Тень разрастается"') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"))
second_map

third_sent_plot <-
  ggplot(third_book_sent, aes(chapter, sum, fill = tone)) +
  geom_col(show.legend = F)
third_sent_plot

third_map <- third_book_sent %>%
  ggplot(aes(x=chapter, y=1, fill=mean)) + 
  geom_tile() + 
  scale_fill_gradient2(high = "#4DAF4A", mid = "#FFFFB3", low = "#555555", midpoint = -0.25) +
  labs(x = NULL, y = NULL, fill = "Тональность", title = 'Тональность "Призрачных Рощ"') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"))
third_map

ggsave("Тональность_Рощи_mean.png", width = 16, height = 8, limitsize = F)

# Ящик с усами
ggplot(sholokh_chapter_sent, aes(x = book, y = mean, fill = book)) +
  geom_boxplot(show.legend = F, varwidth = T) +
  geom_dotplot(show.legend = F, binaxis = 'y', stackdir = 'center', dotsize = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(colour = 'black')) +
  labs(title = "Boxplot тональности по книгам", x = NULL, y = "Тональность")

ggsave("Ящик_с_усами.png", width = 16, height = 8, limitsize = F)

# Линейный график
ggplot(sholokh_chapter_sent, aes(x = chapter, y = mean, group = book, color = book)) +
  geom_line(size = 1.5) + # Увеличение толщины линии
  theme_bw() +
  theme(
    axis.text.x = element_blank(), # Удаление подписей названий глав
    axis.ticks.x = element_blank() # Удаление делений на оси X
  ) +
  labs(title = "Изменение тональности по главам", x = "", y = "Тональность", color = 'Книга')

ggsave("Тональность_по_книгам.png", width = 16, height = 8, limitsize = F)

pos_words <- sholokh_sent %>%
  filter(book == '3. Призрачные рощи' & chapter_id == 39) %>%
  filter(score > 0) %>%
  count(token, sort = T) %>%
  ungroup() %>%
  select(-book) %>%
  filter(row_number() %in% sample(c(1:94), 50))

all_pos_words <- sholokh_sent %>%
  filter(book == '3. Призрачные рощи' & chapter_id == 39) %>%
  filter(score > 0) %>%
  count(token, sort = T) %>%
  ungroup() %>%
  select(-book)

top_50_words <- head(pos_words, 50)

pal <- c('#522E75', '#107050', '#D50B53', '#05328E', '#F05837', "#4DAF4A", "#E69F00", "#56B4E9")
shuffled_pal <- sample(pal)

pol <- all_polyn %>%
  select(lemma, n) %>%
  mutate_at(vars(lemma), ~
  case_when(lemma == "волос" ~  "волосы",
            TRUE ~ .))

wordcloud2(top_50_words, size = 1, minSize = 1,
           fontFamily = 'Monotype Corsiva',
           color = rep(shuffled_pal, length.out = nrow(top_50_words)), 
           backgroundColor = '#E6EFF3')