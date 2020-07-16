library("quanteda")

data_dictionary_HuLiu <-
    dictionary(list(positive = scan(file = "sources/Hu-Liu/positive-words.txt",
                                    what = "character", comment.char = ";"),
                    negative = scan(file = "sources/Hu-Liu/negative-words-UTF8.txt",
                                    what = "character", comment.char = ";")))

meta(data_dictionary_HuLiu) <- 
  list(
    title = "Positive and negative words from Hu and Liu (2004)",
    url = "http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html",
    description = "A quanteda dictionary object containing 2,006 positive and 4,783 negative words from Hu and Liu (2004, 2005).",
    reference = "Hu, M. & Liu, B. (2004). Mining and Summarizing Customer Reviews. In Proceedings of the ACM SIGKDD International Conference on Knowledge Discovery and Data Mining (KDD-2004), Aug 22--25, 2004, Seattle, Washington, USA. https://www.cs.uic.edu/~liub/publications/kdd04-revSummary.pdf
    
    Liu, M., Hu, M., & Cheng, J. (2005). Opinion Observer: Analyzing and Comparing Opinions on the Web. In Proceedings of the 14th International World Wide Web conference (WWW-2005), May 10--14, 2005, Chiba, Japan.  https://www.cs.uic.edu/~liub/publications/www05-p536.pdf",
    license = "Unknown"
  )
polarity(data_dictionary_HuLiu) <- list(pos = "positive", neg = "negative")

usethis::use_data(data_dictionary_HuLiu, overwrite = TRUE)
