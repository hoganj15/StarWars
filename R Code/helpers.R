library(tidyverse)
library(plotly)
library(wordcloud2)
library(hash)

scripts = read.csv('CSV Files/scripts.csv')
avg_embeds = read.csv('CSV Files/avg_embeds.csv')
embeds = read.csv('CSV Files/embeds.csv')
tsne = read.csv('CSV Files/tsne.csv')
word_counts = read.csv('CSV Files/word_counts_by_movie.csv')

scripts = scripts %>% group_by(film) %>% mutate(line_num = row_number())



### FUNCTIONS
#Wordcloud generator
create_wordcloud = function(films, img, word_counts){
  #get all rows containing desired movies and add them to a temp table
  temp = data.frame()
  for (movie in films){
    temp = rbind(temp, word_counts %>% filter(film == movie) %>% select(word, count))
  }
  #group by word and return word name and freq
  wc_df = temp %>% group_by(word) %>% summarize(freq = sum(count)) %>% arrange(desc(freq))
  wc = wordcloud2(wc_df, color="white", backgroundColor = "black", figPath = paste('Images/', img, '.jpg', sep=""))
  
  return(wc)
}

###Sentiment Time Series
sentiment_time = function(characters, films, scripts, color_palette = color_palette){
  scripts = scripts %>% group_by(film) %>% mutate(line_num = row_number())
  #get unique set of characters
  chars = unique(scripts[c("character", "film")])
  
  #filter out only the characters and films desired
  df = data.frame()
  for (char in characters){
    for (movie in films){
      df = rbind(df, chars %>% filter(character == char & film == movie))
    }
  }
  
  #plot the sentiment over time for each character desired
  palette = color_palette(length(df$character))
  
  fig = plot_ly()
  for (i in 1:length(df$character)){
    plot_df = scripts %>% filter(character == as.character(df[i, 1]) & film == as.character(df[i, 2]) & compound != 0) #use script data for characters in question, remove instances with 0 compound score
    fig = fig %>% add_trace(data=plot_df, x=~line_num, y=~compound, mode="markers", name=paste(df[i, 1], "-", df[i, 2]), marker = list(color = palette[i], opacity = 0.5))
    fig = fig %>% add_lines(data = plot_df, x = ~line_num, y = ~fitted(loess(compound ~ line_num)), name = paste(df[i, 1], "-", df[i, 2], "LOESS"), line = list(color = palette[i], width = 2.4), showlegend = F)
  }
  fig = fig %>% layout(hovermode = 'x unified', title="Sentiment Over Time", xaxis=list(title="Line Number"), yaxis=list(title="Sentiment Score"))
  return(fig)
}

###sentiment Bar Chart
sentiment_bar = function(characters, films, scripts, met, color_palette = color_palette){
  
  h = hash()
  h[["Sentiment"]] = "compound"
  h[["Positive"]] = "pos"
  h[["Neutral"]] = "neu"
  h[["Negative"]] == "neg"
  
  avg_sentiments = spread(scripts %>% group_by(character, film) %>% summarize(sentiment = mean(get(h[[met]]))), key=film, value=sentiment)
  df = data.frame()
  for (char in characters){
    df = rbind(df, avg_sentiments %>% filter(character == char) %>% select(films))
  }
  
  palette = color_palette(3)
  
  fig = plot_ly(df, x=~get(films[1]), y=~character, type="bar", name=films[1], text = films[1], marker = list(color = palette[1]), orientation = "h", hoverinfo = "x+text")
  if (length(films) >= 2){fig = fig %>% add_trace(x=~get(films[2]), name=films[2], text=films[2], marker = list(color = palette[2]))}
  if (length(films) == 3){fig = fig %>% add_trace(x=~get(films[3]), name=films[3], text=films[3], marker = list(color = palette[3]))}
  fig = fig %>% layout(title=paste(met, "Score of Selected Characters and Films"), xaxis = list(title="Character"), yaxis=list(title=paste(met, "Sentiment Score")))
  
  return(fig)
}

###Sentiment over number of lines
sentiment_over_lines = function(characters, films, scripts, met, color_palette = color_palette){
  data = scripts %>% group_by(character, film) %>% summarize(lines = n(), Sentiment = mean(compound), Positive = mean(pos), Neutral = mean(neu), Negative = mean(neg))
  df = data.frame()
  for (char in characters){
    for (movie in films){
      df = rbind(df, data %>% filter(character == char & film == movie))
    }
  }
  
  t = ~paste("Name:", character, "<br>Film:", film, paste("<br>", met, ":", sep = ""), round(get(met), 3))
  fig = plot_ly(data = df, x = ~lines, y = ~get(met), name = ~paste(character, "-", film), marker = list(color = color_palette(length(df$character)), line = list(color = "black", width = 1), size = 20), text = t, hoverinfo = "text")
  fig = fig %>% layout(title = paste(met, "over Total Lines"), showlegend = F, xaxis = list(title = "Line Count"), yaxis = list(title = met))
  return(fig)
}


###TSNE Plot
tsne_plot = function(characters, films, tsne, color_palette = color_palette){
  #filter for characters and movies desired
  df = data.frame()
  for (char in characters){
    for (movie in films){
      df = rbind(df, tsne %>% filter(character == char & film == movie))
    }
  }
  
  palette = color_palette(100)
  
  m = list(size=~0.2*line_count+8, line=list(color='black', width=1), text=~character) #marker characteristics
  t = ~paste("Character:", character, "<br>Film:", film, "<br>Lines:", line_count, "<br>Sentiment:", round(Sentiment, 2)) #hover text
  ax = list(title="", showline=F, zeroline=F, showticklabels=T, tickfont=list(size=10)) #axis
  fig = plot_ly(data=df %>% rename(Sentiment = compound), x=~x, y=~y, type="scatter", mode='markers', colors = palette, name=~paste(character, "-", film), marker = m, color = ~Sentiment, text = t, hoverinfo = "text")
  fig = fig %>% layout(showlegend = F, xaxis = ax, yaxis = ax, title = "TSNE Plot, Sized by Line Count, Colored by Avg. Sentiment")
  #fig = fig %>% hide_colorbar()
  
  return(fig)
}