The files are organized as follows: 

* The Scraping folder contains the R script necessary to scrape the 1300+ transcripts of morning conferences.
* The Cleaning folder contains the R scripts necessary to clean such transcripts. First, the cleaning script deals with keeping AMLO's words only, among other steps, and the pre-processing script deals with tokenization, cleaning and turning into a DFM. This folder also contains the resulting objects of each step of cleaning/processing. 
* The Analysis folder contains the R scripts necessary to analyze the transcripts. As the script names suggest, the AnalysisRaw consists of wordcounts and basic textual statistics; Freq and LexDiv computes the most frequent words and Lexical Diversity; KWICK and Readability performs such analyses on the transcripts; Sentiment applies the AFINN and NRC dictionaries to the transcripts, and Populism applies the populist dictionary. 
* The Dictionaries folder contains the full AFINN, NRC and Populist dictionaries that were applied.
* The Latex folder contains all the necessary files for the writing of the paper, including tex and bibtex files. 
