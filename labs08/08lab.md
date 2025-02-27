Lab 08 - Text Mining/NLP
================
Richard Yin (<https://github.com/tirangol/JSC370-labs>)

# Learning goals

- Use `unnest_tokens()` and `unnest_ngrams()` to extract tokens and
  ngrams from text
- Use dplyr and ggplot2 to analyze and visualize text data
- Try a theme model using `topicmodels`

# Lab description

For this lab we will be working with the medical record transcriptions
from <https://www.mtsamples.com/> available at
<https://github.com/JSC370/JSC370-2025/tree/main/data/medical_transcriptions>.

# Deliverables

1.  Questions 1-7 answered, knit to pdf or html output uploaded to
    Quercus.

2.  Render the Rmarkdown document using `github_document` and add it to
    your github site. Add link to github site in your html.

### Setup packages

You should load in `tidyverse`, (or `data.table`), `tidytext`,
`wordcloud2`, `tm`, and `topicmodels`.

## Read in the Medical Transcriptions

Loading in reference transcription samples from
<https://www.mtsamples.com/>

``` r
library(tidytext)
library(tidyverse)
library(wordcloud2)
library(tm)
library(topicmodels)

mt_samples <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/main/data/medical_transcriptions/mtsamples.csv")
mt_samples <- mt_samples |>
  select(description, medical_specialty, transcription)

head(mt_samples)
```

------------------------------------------------------------------------

## Question 1: What specialties do we have?

We can use `count()` from `dplyr` to figure out how many different
medical specialties are in the data. Are these categories related?
overlapping? evenly distributed? Make a bar plot.

``` r
mt_samples |>
  count(medical_specialty, sort = TRUE) |>
  arrange(n) |>
  mutate(medical_specialty = factor(medical_specialty, levels=medical_specialty)) |>
  ggplot(aes(x=medical_specialty, y=n)) +
    geom_bar(stat='identity') +
    coord_flip() +
    theme_light() +
    labs(x='Medical Specialty', y='Count', title='Counts of Medical Specialties')

length(unique(mt_samples$medical_specialty))
```

There are 30 unique medical specialties in the data spread very
unevenly, where surgery is by far the most common, followed by
orthopedic, radiology, general medicine, and neurology. The categories
do not appear to be related.

------------------------------------------------------------------------

## Question 2: Tokenize

- Tokenize the the words in the `transcription` column
- Count the number of times each token appears
- Visualize the top 20 most frequent words with a bar plot
- Create a word cloud of the top 20 most frequent words

### Explain what we see from this result. Does it makes sense? What insights (if any) do we get?

``` r
tokens <- mt_samples |>
  select(transcription) |>
  unnest_tokens(token, transcription) |>
  group_by(token) |>
  summarize(count = n()) |>
  slice_max(count, n=20)

tokens |>
  arrange(count) |>
  mutate(token = factor(token, levels=token)) |>
  ggplot(aes(x=token, y=count)) +
    geom_bar(stat='identity') +
    theme_light() +
    labs(x='Token', y='Count') +
    coord_flip()

wordcloud2(tokens)
```

The most common words in the transcriptions column mostly happen to be
some of the most common words in the English language, including
conjunctions like “the” and “and” and “to”, making this visualization
not informative. However, the prevalence of “patient” is unique for this
dataset and makes sense, given the dataset is about descriptions of
patient symptoms.

------------------------------------------------------------------------

## Question 3: Stopwords

- Redo Question 2 but remove stopwords
- Check `stopwords()` library and `stop_words` in `tidytext`
- Use regex to remove numbers as well
- Try customizing your stopwords list to include 3-4 additional words
  that do not appear informative

### What do we see when you remove stopwords and then when you filter further? Does it give us a better idea of what the text is about?

``` r
head(stopwords("english"))
length(stopwords("english"))
head(stop_words)

tokens <- mt_samples |>
  select(transcription) |>
  unnest_tokens(token, transcription) |>
  group_by(token) |>
  summarize(count = n()) |>
  rename(word=token) |>
  filter(!grepl('[0-9_.]', word)) |>  # Removed underscores & periods
  anti_join(stop_words, by='word') |>
  filter(!word %in% stopwords('english')) |>
  rename(token=word) |>
  slice_max(count, n=20)

tokens |>
  arrange(count) |>
  mutate(token = factor(token, levels=token)) |>
  ggplot(aes(x=token, y=count)) +
    geom_bar(stat='identity') +
    theme_light() +
    labs(x='Token', y='Count', title='Token Occurrences') +
    coord_flip()

wordcloud2(tokens)
```

I decided to remove some symbols (\_, .) but not additional words
because the top 20 words all made sense and if I picked any other random
words to remove, this wouldn’t affect the visualization so there would
be no point.

After removing stop words, we see more insightful words that have a
heavy medical connotation as expected from the dataset, for example
actions like “performed”, “removed”, “procedure”, “incision” and
“anesthesia”, diagnostic terms like “pain”, “patient”, “history”,
“noted”, and “diagnosis”, parts of the body like “skin”, “blood”, and
“anterior”, as well as measurements like “mg”, “mm”, and “time”.

------------------------------------------------------------------------

## Question 4: ngrams

Repeat question 2, but this time tokenize into bi-grams. How does the
result change if you look at tri-grams? Note we need to remove stopwords
a little differently. You don’t need to recreate the wordclouds.

``` r
sw_start <- paste0("^", paste(stopwords('en'), collapse=" |^"), "$")
sw_end <- paste0("", paste(stopwords('en'), collapse="$| "), "$")

tokens_bigram <- mt_samples |>
  select(transcription) |>
  unnest_tokens(ngram, transcription, token = "ngrams", n = 2) |>
  filter(!str_detect(ngram, sw_start)) |>
  filter(!str_detect(ngram, sw_end)) |>
  filter(!grepl('[0-9_.]', ngram))

tokens_trigram <- mt_samples |>
  select(transcription) |>
  unnest_tokens(ngram, transcription, token = "ngrams", n = 3) |>
  filter(!str_detect(ngram, sw_start)) |>
  filter(!str_detect(ngram, sw_end)) |>
  filter(!grepl('[0-9_.]', ngram))

tokens_bigram |>
  group_by(ngram) |>
  summarize(count = n()) |>
  slice_max(count, n=10)

tokens_trigram |>
  group_by(ngram) |>
  summarize(count = n()) |>
  slice_max(count, n=10)
```

The most common bigrams seem are compound nouns like “operating room”,
“preoperative diagnosis”, “postoperative diagnosis”, “blood loss”, and
“stable condition”, plus a describer of age (“year old”).

The most common trigrams seem to describe actions, such as “prepped and
draped”, “incision was made”, “tolerated the procedure”, “patient was
taken”, “patient was brought”. There are also some descriptive nouns
(“estimated blood loss”, “past medical history”, “review of systms”).

------------------------------------------------------------------------

## Question 5: Examining words

Using the results from the bigram, pick a word and count the words that
appear before and after it, and create a plot of the top 20.

``` r
library(stringr)
# e.g. patient, blood, preoperative...

tokens_bigram |>
  filter(str_detect(ngram, 'patient .*|.* patient')) |>
  mutate(word = str_remove(ngram, "patient"),
         word = str_remove_all(word, " ")) |>
  group_by(word) |>
  summarize(count = n()) |>
  slice_max(count, n=20) |>
  arrange(count) |>
  mutate(word = factor(word, levels=word)) |>
  ggplot(aes(x=word, y=count)) +
    geom_bar(stat='identity') +
    theme_light() +
    labs(x='Word', y='Count', title='Word Occurrences near "Patient"') +
    coord_flip()
```

------------------------------------------------------------------------

## Question 6: Words by Specialties

Which words are most used in each of the specialties? You can use
`group_by()` and `top_n()` from `dplyr` to have the calculations be done
within each specialty. Remember to remove stopwords. How about the 5
most used words?

``` r
most_common <- mt_samples |>
  unnest_tokens(word, transcription) |>
  group_by(medical_specialty, word) |>
  count(word, name = "count") |>
  ungroup() |>
  filter(!grepl('[0-9_.]', word)) |>
  anti_join(stop_words, by='word') |>
  filter(!word %in% stopwords('english')) |>
  group_by(medical_specialty)

most_common |>
  slice_max(count, n=1, with_ties=FALSE)

most_common |>
  slice_max(count, n=5, with_ties=FALSE)
```

“Patient” is the most common word for the majority of specialties, which
makes sense as this is the most common word overall, although this word
is less likely for specialties with less records
(e.g. allergy/immunology where the most common word appears 4 times,
cardiovascular/pulmonary with 25, dentistry with 10, lab medicine -
pathology with 35, rheumatology with 50). Some specialties also have
most common words corresponding to what they study (“sleep” for sleep
medicine, “foot” for podiatry, “eye” for ophthalmology).

## Question 7: Topic Models

See if there are any themes in the data by using a topic model (LDA).

- you first need to create a document term matrix
- then you can try the LDA function in `topicmodels`. Try different k
  values.
- create a facet plot of the results from the LDA (see code from
  lecture)

``` r
transcripts_dtm <- mt_samples |>
  select(transcription) |>
  unnest_tokens(word, transcription) |>
  filter(!grepl('[0-9_.]', word)) |>
  anti_join(stop_words, by='word') |>
  filter(!word %in% stopwords('english')) |>
  DocumentTermMatrix()


transcripts_dtm <- as.matrix(transcripts_dtm)   

transcripts_lda <- LDA(transcripts_dtm, k=6, control=list(seed=1234))

top_terms <- 
  tidy(transcripts_lda, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 12) |> 
  ungroup() |>
  arrange(topic, -beta)

top_terms |>
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  theme_light()
```

It is difficult to make out any themes from the LDA model because every
topic has a heavy overlap in words and especially for “patient”, “left”,
“procedure”, and “history”. This conclusion generally did not change for
many values of $k$ (3, 4, 6, 8, 10)
