#library(XML)
#library(rjson)

#' Get Artists
#'
#' Creates a data frame containing details of the top result for the searched for artists
#'
#' @param artists A value or vector of artists to search for
#' @author James Thomson
#' @examples result<-getArtists("David Bowie")
#' results<-getArtists(c("David Bowie", "Bob Dylan"))
#' 

getArtists<-function(artists) {
 
if (Sys.info()[1]=="Windows"){
              out<-NULL
              for (i in c(1:length(artists))) {
                  eval(parse(text=paste0("search<-readLines('http://ws.spotify.com/search/1/artist?q=", artists[i], "')")))
                  searchxml<-xmlToList(search)
                  temp<-data.frame(
                  artist=as.character(searchxml$artist$name),
                  artist_id=substring(searchxml$artist$.attrs, as.integer(gregexpr(":", searchxml$artist$.attrs)[[1]])[2]+1, nchar(searchxml$artist$.attrs)),
                  artist_pop=as.numeric(searchxml$artist$popularity)*100
                                      )
                  rownames(temp)<-NULL
                  out<-rbind(out, temp)
                                              }
                            }
else {
            out<-NULL
            for (i in c(1:length(artists))) {
            eval(parse(text=paste0("url<-'http://ws.spotify.com/search/1/artist?q=", artists[i], "'")))
            save_file<-'temp.txt'
            download.file(url, save_file, method = "wget", quiet=TRUE)
            search <- readLines(save_file)
            file.remove(save_file)
            searchxml<-xmlToList(search)
            temp<-data.frame(
            artist=as.character(searchxml$artist$name),
            artist_id=substring(searchxml$artist$.attrs, as.integer(gregexpr(":", searchxml$artist$.attrs)[[1]])[2]+1, nchar(searchxml$artist$.attrs)),
            artist_pop=as.numeric(searchxml$artist$popularity)*100
            )
            rownames(temp)<-NULL
            out<-rbind(out, temp)
                                        }             
}


return(out)
}





#' Get Related Artists
#'
#' Creates a data frame containing details of the related artists taken from the output from getArtists
#'
#' @param GetArtistsOutput A data.frame object produced from the getArtists function
#' @author James Thomson
#' @examples artist<-getArtists(c("David Bowie", "Bob Dylan"))
#' related_artist<-getRelatedArtists(artist)
#' 

getRelatedArtists<-function(getArtistsOutput) {
  
  if (Sys.info()[1]=="Windows"){  
    
    artists_relate<-data.frame(from_artist=NULL,from_artist_id=NULL,position=NULL, to_artist=NULL, to_artist_id=NULL, to_artist_pop=NULL)
    for (i in c(1:nrow(getArtistsOutput))) {
      eval(parse(text=paste0("related<-fromJSON(file=\"https://api.spotify.com/v1/artists/", getArtistsOutput[i,2], "/related-artists\")")))
      
      for (j in c(1:20)){
        temp<-data.frame(
          from_artist=getArtistsOutput[i,1],
          from_artist_id=getArtistsOutput[i,2], 
          position=i,
          to_artist=related$artists[[j]]$name,
          to_artist_id=substring(related$artists[[j]]$uri, as.integer(gregexpr(":", related$artists[[j]]$uri)[[1]])[2]+1, nchar(related$artists[[j]]$uri)),
          to_artist_pop=related$artists[[j]]$popularity
        )
        artists_relate<-rbind(artists_relate, temp)
      }
    }  
  }
  
  
  else {  
    artists_relate<-data.frame(from_artist=NULL,from_artist_id=NULL,position=NULL, to_artist=NULL, to_artist_id=NULL, to_artist_pop=NULL)
    for (i in c(1:nrow(getArtistsOutput))) {
      
      eval(parse(text=paste0("url<-'https://api.spotify.com/v1/artists/", getArtistsOutput[i,2], "/related-artists'")))
      save_file<-'temp.txt'
      download.file(url, save_file, method = "wget", quiet=TRUE)
      fp <- file.path(save_file)
      related <- fromJSON(file = fp)
      file.remove(save_file)  
      
      for (j in c(1:20)){
        temp<-data.frame(
          from_artist=getArtistsOutput[i,1],
          from_artist_id=getArtistsOutput[i,2], 
          position=i,
          to_artist=related$artists[[j]]$name,
          to_artist_id=substring(related$artists[[j]]$uri, as.integer(gregexpr(":", related$artists[[j]]$uri)[[1]])[2]+1, nchar(related$artists[[j]]$uri)),
          to_artist_pop=related$artists[[j]]$popularity
        )
        artists_relate<-rbind(artists_relate, temp)
      }
    }
  }
  return(artists_relate)
}



#' Visualize Related Artists
#'
#' Creates a object containing a list of artists and a list of artists relationships then outputs 
#' a force direct layout to the file location specified
#'
#' @param artist an artist or list of artists
#' @param steps the number of steps you want to explore related artists for
#' @param output_file an output location and filename
#' @author James Thomson
#' @examples out<-visRelatedArtists(artist="David Bowie", steps=2, output_file="vis.html")
#' out<-visRelatedArtists(artist=c("David Bowie", "Bob Dylan"), steps=1, output_file="vis.html")
#' 


visRelatedArtists<-function(artist, steps=2, output_file) {
  
  artists<-getArtists(artist)
  artists$step<-0
  
  if (steps==1) {
    
    #Step 1
    
    #get related artists
    artists_relate<-getRelatedArtists(artists)
    artists_relate$step<-1
    
    #update the artist lists
    artist_list<-artists_relate[,c(4:6)]
    artist_list_new<-artist_list[!(artist_list$to_artist %in% artists$artist),]  
    colnames(artist_list_new)<-c("artist", "artist_id", "artist_pop")
    artist_list_new$step<-1
    artists<-rbind(artists, artist_list_new) 
    
  } else {  
    
    #Step 1
    #get related artists
    artists_relate<-getRelatedArtists(artists)
    artists_relate$step<-1
    
    #update the artist lists
    artist_list<-artists_relate[,c(4:6)]
    artist_list_new<-artist_list[!(artist_list$to_artist %in% artists$artist),]  
    colnames(artist_list_new)<-c("artist", "artist_id", "artist_pop")
    artist_list_new$step<-1
    artists<-rbind(artists, artist_list_new) 
    
    
    #Steps 2 onwards
    k<-3
    for (k in c(2:steps)) {
      
      artist_list<-artists_relate[artists_relate$step==(k-1),c(4:6)]
      #remove artists already checked
      artist_list_new<-artist_list[!(artist_list[,1] %in% artists_relate[,2]),]  
      
      #get related artists and add to list      
      temp<-getRelatedArtists(artist_list_new)
      temp$step<-k
      artists_relate<-rbind(artists_relate, temp)
      
      
      #update the artist list
      artist_list<-artists_relate[,c(4:6)]
      #need to create a unique list
      unique_artist_list<-aggregate(artist_list$to_artist_pop, by=list(artist_list$to_artist, artist_list$to_artist_id), FUN=max)
      #find which ones are new
      artist_list_new<-unique_artist_list[!(unique_artist_list$Group.1 %in% artists$artist),]  
      #add them
      colnames(artist_list_new)<-c("artist", "artist_id", "artist_pop")
      artist_list_new$step<-k
      artists<-rbind(artists, artist_list_new) 
      
    }
    
  }
  
  
  return(list(artists=artists, relationships=artists_relate))
  
}


