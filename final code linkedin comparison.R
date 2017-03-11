library(LDAvis)
library(tm)
library(lda)


LinkedIn.raw<-readRDS("sampleProfile.rds")

#LinkedIn.raw <- read.csv("C:\\Users\\Abhishek\\Dropbox\\MIT Data Science\\LinkedIn.csv")

txt <- as.list(as.character(LinkedIn.raw$Content))
nms <- as.list(as.character(LinkedIn.raw$ProfileName))


nms <- gsub("[^[:graph:]]", " ",nms)
LinkedInContent <- setNames(txt, nms)
LinkedInContent <- sapply(LinkedInContent, function(x) paste(x, collapse = " "))

stop_words <- c(stopwords("SMART"), " ","")

# LinkedInContent prepartaion
LinkedInContent <- gsub("'", "", LinkedInContent)  
LinkedInContent <- gsub("[[:punct:]]", " ", LinkedInContent)  
LinkedInContent <- gsub("[[:cntrl:]]", " ", LinkedInContent) 
LinkedInContent <- gsub("[^[:graph:]]", " ",LinkedInContent)
LinkedInContent <- tolower(LinkedInContent) 

# tokenize
doc.list <- strsplit(LinkedInContent, "[[:space:]]+")

#table of terms
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

#remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

#format to lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)
#tf i df
#the data set stats
D <- length(documents) 
W <- length(vocab)  
doc.length <- sapply(documents, function(x) sum(x[2, ]))  
N <- sum(doc.length) 
term.frequency <- as.integer(term.table)

# MCMC and model tuning parameters:
K <- 20

#G <- 5000
G <- 100
alpha <- 0.02
eta <- 0.02

# Fit the model:
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)

t2 <- Sys.time()
t2 - t1  

theta <- t(apply(fit$document_sums + alpha, 2, function(x) {x/sum(x)}))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

LinkedInContent.list <- list(phi = phi,
                    theta = theta,
                    doc.length = doc.length,
                    vocab = vocab,
                    term.frequency = term.frequency)

#create the JSON object to feed the visualization:
json <- createJSON(phi = LinkedInContent.list$phi, 
                   theta = LinkedInContent.list$theta, 
                   doc.length = LinkedInContent.list$doc.length, 
                   vocab = LinkedInContent.list$vocab, 
                   term.frequency = LinkedInContent.list$term.frequency)

serVis(json, out.dir = 'visa35', open.browser = TRUE)


