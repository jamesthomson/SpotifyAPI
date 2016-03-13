

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
  
    out<-NULL
    for (i in c(1:length(artists))) {
 
      url<-paste0("https://api.spotify.com/v1/search?q=", gsub(" ", "%20", artists[i]), "&type=artist")
      search<-content(GET(url))
      temp<-data.frame(
        artist=search$artists$items[[1]]$name,
        artist_id=search$artists$items[[1]]$id,
        artist_pop=search$artists$items[[1]]$popularity
      )
      rownames(temp)<-NULL
      out<-rbind(out, temp)
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
     
    artists_relate<-data.frame(from_artist=NULL,from_artist_id=NULL,position=NULL, to_artist=NULL, to_artist_id=NULL, to_artist_pop=NULL)
    for (i in c(1:nrow(getArtistsOutput))) {
      url<-paste0("https://api.spotify.com/v1/artists/", getArtistsOutput[i,2], "/related-artists")
      related<-content(GET(url))
      for (j in c(1:20)){
        temp<-data.frame(
          from_artist=getArtistsOutput[i,1],
          from_artist_id=getArtistsOutput[i,2], 
          position=j,
          to_artist=related$artists[[j]]$name,
          to_artist_id=substring(related$artists[[j]]$uri, as.integer(gregexpr(":", related$artists[[j]]$uri)[[1]])[2]+1, nchar(related$artists[[j]]$uri)),
          to_artist_pop=related$artists[[j]]$popularity
        )
        artists_relate<-rbind(artists_relate, temp)
      }
    }  
     

  return(artists_relate)
}







#' Get Artists Albums
#'
#' Creates a data frame containing details of an artists albums taken from the output from getArtists. This is limited by Spotify to a maximum of 50 albums.
#'
#' @param getArtistsOutput A data.frame object produced from the getArtists function
#' @param country the country the albums are availble in by default GB
#' @param albumType the album types to select. By defult album. Options: album, single, compilation, appears_on 
#' @param cleanDups some results contain duplicate albums due to reissues. TRUE by default will clean this up
#' @author James Thomson
#' @examples getArtistsAlbums(getArtists("The Veils"))


getArtistsAlbums<-function(getArtistsOutput, country="GB", albumType="album", cleanDups=TRUE) {
  
  
    
    albums<-data.frame(artist=NULL,artist_id=NULL,album=NULL, album_id=NULL)
    for (i in c(1:nrow(getArtistsOutput))) {
      url<-paste0("https://api.spotify.com/v1/artists/", getArtistsOutput[i,2], "/albums?album_type=", albumType, "&limit=50&country=", country)
      list<-content(GET(url))
      for (j in c(1:length(list$items))){
        temp<-data.frame(
          artist=getArtistsOutput[i,1],
          artist_id=getArtistsOutput[i,2], 
          album=list$items[[j]]$name,
          album_id=list$items[[j]]$id
        )
        albums<-rbind(albums, temp)
      }
    }  

  if (cleanDups==TRUE){albums<-albums[!duplicated(albums$album), ]}
  return(albums)
}




#' Get Album Tracks
#'
#' Creates a data frame containing details of an artists albums taken from the output from getArtists. This is limited by Spotify to a maximum of 50 albums.
#'
#' @param getArtistsOutput A data.frame object produced from the getArtists function
#' @param country the country the albums are availble in by default GB
#' @param albumType the album types to select. By defult album. Options: album, single, compilation, appears_on 
#' @param cleanDups some results contain duplicate albums due to reissues. TRUE by default will clean this up
#' @author James Thomson
#' @examples tracks<-getAlbumsTracks(getArtistsAlbums(getArtists("The Veils")))


getAlbumsTracks<-function(getArtistsAlbumsOutput) {
  
#   if (Sys.info()[1]=="Windows"){  
    
    albumtracks<-data.frame(artist=NULL,artist_id=NULL,album=NULL, album_id=NULL, track=NULL, track_id=NULL, track_number=NULL, track_length=NULL, preview_url=NULL)
    
    for (i in c(1:nrow(getArtistsAlbumsOutput))) {
      url<-paste0("https://api.spotify.com/v1/albums/", getArtistsAlbumsOutput[i,4], "/tracks?limit=50")
      list<-content(GET(url))
      
      for (j in c(1:length(list$items))){
        temp<-data.frame(
          artist=getArtistsAlbumsOutput[i,1],
          artist_id=getArtistsAlbumsOutput[i,2], 
          album=getArtistsAlbumsOutput[i,3], 
          album_id=getArtistsAlbumsOutput[i,4],          
          track=list$items[[j]]$name,
          track_id=list$items[[j]]$id,
          track_number=list$items[[j]]$track_number,
          track_length=format(.POSIXct(list$items[[j]]$duration_ms/1000,tz="GMT"), "%M:%S"),
          preview_url=ifelse(is.null(list$items[[j]]$preview_url), "NO PREVIEW", list$items[[j]]$preview_url)
        )
        albumtracks<-rbind(albumtracks, temp)
      }
    }  
  
  return(albumtracks)
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
    
    #update the artist list
    artist_list<-artists_relate[,c(4:6)]
    #need to create a unique list
    unique_artist_list<-aggregate(artist_list$to_artist_pop, by=list(artist_list$to_artist, artist_list$to_artist_id), FUN=max)
    #find which ones are new
    artist_list_new<-unique_artist_list[!(unique_artist_list$Group.1 %in% artists$artist),]  
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
  
  nodes<-data.frame(name=artists$artist, group=artists$step, popularity=artists$artist_pop)
  links<-data.frame(source=artists_relate$from_artist, target=artists_relate$to_artist, value=1, distance=40+artists_relate$position*5)
  
  
  
  dfToJSON<-function(df, mode='vector'){
    
    colToList<-function(x, y){
      
      col.json<-list()
      
      #Build up a list of coordinates
      
      for (i in 1:length(x)){
        
        ni<-list(x=x[i], y=y[i])
        col.json[[length(col.json)+1]]<-ni
      }
      
      return(col.json)
      
    }
    
    
    if (mode=='vector'){
      
      for.json<-list()
      
      for (j in 1:ncol(df)){
        for.json[[length(for.json)+1]]<-list(name=colnames(df)[j] , data=df[,j])
      }
      
    }
    
    if (mode=='coords') {
      
      for.json<-list()
      
      for (j in 2:ncol(df)){
        for.json[[length(for.json)+1]]<-list(name=colnames(df)[j] , data=colToList(x=df[,1], y=df[,j]))
      }
      
    }
    
    if (mode=='rowToObject') {
      
      for.json<-list()
      
      for (j in 1:nrow(df)){
        for.json[[length(for.json)+1]]<-df[j,]
      }
      
    }
    
    jj<-toJSON(for.json)
    
    return(jj)
    
  }
  
  
  
  
  
  
  jsonConvert<-function(nodes, links){
    
    nodes<-data.frame(lapply(nodes, as.character), stringsAsFactors=FALSE)
    #links<-data.frame(lapply(links, as.character), stringsAsFactors=FALSE)
    links.lu<-NULL
    
    i<-2
    for (i in 1:nrow(links)){
    
    temp<-data.frame(  
      s=which(nodes$name==links$source[i])-1,
      t=which(nodes$name==links$target[i])-1,
      v=links$value[i],
      d=links$distance[i])
      
      links.lu<-rbind(links.lu, temp)
      
    }
    links.df<-data.frame(links.lu)
    names(links.df)<-c("source", "target", "value", "distance")
    
    n<-dfToJSON(nodes,  'rowToObject')
    e<-dfToJSON(links.df,  'rowToObject')
    
    json<-paste0("{ \"nodes\":", n, ", \"links\": ", e, "}")
    return(list(Type="json:nodes_links", json=json))
    
  }
  
  Force<-function(JSON, file_out){
    
    if (JSON$Type!="json:nodes_links"){stop("Incorrect json type for this D3")}
    
    header<-"<!DOCTYPE html>
    
    <style>
    
    .link {
    stroke: #ccc;
    }
    
    .node text {
    pointer-events: none;
    font: 10px sans-serif;
    }
    
    
    </style>
    <body>
    <script src=\"http://d3js.org/d3.v3.min.js\"></script>
    
    <script type='text/javascript' src=\"http://labratrevenge.com/d3-tip/javascripts/d3.tip.v0.6.3.js\"> </script>
    
    <script type=\"application/json\" id=\"mis\">"
    
    footer<-"</script>
    
    <script>
    //Constants for the SVG
    var width = 1000,
    height = 1000;
    
    //Set up the colour scale
    var color = d3.scale.category20();
    
    //Set up the force layout
    var force = d3.layout.force()
    .charge(-120)
    .linkDistance(function(d) { return  d.distance; }) 
    .size([width, height]);
    
    //Append a SVG to the body of the html page. Assign this SVG as an object to svg
    var svg = d3.select(\"body\").append(\"svg\")
    .attr(\"width\", width)
    .attr(\"height\", height);
    
    //Read the data from the mis element 
    var mis = document.getElementById('mis').innerHTML;
    graph = JSON.parse(mis);
    
    //Creates the graph data structure out of the json data
    force.nodes(graph.nodes)
    .links(graph.links)
    .start();
    
    //Create all the line svgs but without locations yet
    var link = svg.selectAll(\".link\")
    .data(graph.links)
    .enter().append(\"line\")
    .attr(\"class\", \"link\")
    .style(\"stroke-width\", function (d) {return (d.value*0.5); });
    
    //Do the same with the circles for the nodes - no 
    //Changed
    var node = svg.selectAll(\".node\")
    .data(graph.nodes)
    .enter().append(\"g\")
    .attr(\"class\", \"node\")
    .call(force.drag);
    
    node.append(\"circle\")
    .attr(\"r\", 8)
    .attr(\"r\", function(d) { return d.popularity/5; })
    .style(\"fill\", function (d) { return color(d.group)

;
})
    
    node.append(\"text\")
    .attr(\"dx\", 10)
    .attr(\"dy\", \".35em\")
    .text(function(d) { return d.name });
    //End changed
    
    
   //Now we are giving the SVGs co-ordinates - the force layout is generating the co-ordinates which this code is using to update the attributes of the SVG elements
    force.on(\"tick\", function () {
    link.attr(\"x1\", function (d) {
    return d.source.x;
  })
    .attr(\"y1\", function (d) {
    return d.source.y;
    })
    .attr(\"x2\", function (d) {
    return d.target.x;
    })
    .attr(\"y2\", function (d) {
    return d.target.y;
    });
    
    //Changed
    
    d3.selectAll(\"circle\").attr(\"cx\", function (d) {
    return d.x;
    })
    .attr(\"cy\", function (d) {
    return d.y;
    });
    
    d3.selectAll(\"text\").attr(\"x\", function (d) {
    return d.x;
    })
    .attr(\"y\", function (d) {
    return d.y;
    });
    
    //End Changed
    
});
 </script>
 </body>
 </html>"
    
    fileConn<-file(file_out)
    writeLines(paste0(header, JSON$json, footer), fileConn)
    close(fileConn)
    
}     
    
  json<-jsonConvert(nodes,links)
  Force(json, file_out=output_file)
    
  return(list(artists=artists, relationships=artists_relate, json=json))
  
}







#' Visualize Artists Album History
#'
#' Creates a object containing an artists back catalogue and a visualisation of their back catalogue at the file location specified
#'
#' @param artist an artist
#' @param output_file an output location and filename
#' @author James Thomson
#' @examples out<-visDiscography(artist="David Bowie", output_file="Disco.html")
#' 
#' 



visDiscography<-function(artist,  output_file) {
  
  jsonNestedData<-function(structure, values=NULL, top_label="Top") {
    
    if (is.null(values)) {
      
      #bottom level   
      labels<-data.frame(table(structure[,ncol(structure)-1]))
      for (i in c(1:nrow(labels))) {
        items<-structure[structure[,ncol(structure)-1]==labels[i,1],ncol(structure)]
        eval(parse(text=paste0(gsub(" ", "_",gsub("[[:punct:]]","",labels[i,1])),"<-list(name=\"", labels[i,1], "\", children=list(", paste0("list(name=as.character(items[", c(1:length(items)), "]))", collapse=","),  "))")))
      }
      
      #iterate through other levels
      for (c in c((ncol(structure)-2):1)) {
        labels<-data.frame(table(structure[,c]))        
        lookup<-data.frame(table(structure[,c], structure[,c+1]))
        lookup2<-lookup[lookup$Freq!=0,]
        for (i in c(1:nrow(labels))) {
          eval(parse(text=paste0(gsub(" ", "_",gsub("[[:punct:]]","",labels[i,1])),
                                 "<-list(name=\"", 
                                 labels[i,1], 
                                 paste0("\", children=list(", 
                                        paste0(gsub(" ", "_", gsub("[[:punct:]]","",lookup2[lookup2$Var1==labels[i,1],2])), collapse=","), ")"),
                                 ")")
          ))
        }
      }
      
      #final top level
      labels<-data.frame(table(structure[,1]))
      eval(parse(text=paste0("Top<-list(name=\"", top_label,"\" , children=list(", paste(gsub(" ", "_",gsub("[[:punct:]]","",labels[i,1])), collapse=","), ")",")")))           
      
    } else {
      
      
      
      #bottom level   
      labels<-data.frame(table(structure[,ncol(structure)-1]))
      for (i in c(1:nrow(labels))) {
        items<-structure[structure[,ncol(structure)-1]==labels[i,1],ncol(structure)]
        vals<-values[structure[,ncol(structure)-1]==labels[i,1]]
        eval(parse(text=paste0(gsub(" ", "_",gsub("[[:punct:]]","",labels[i,1])),"<-list(name=\"", labels[i,1], "\", children=list(", paste0("list(name=as.character(items[", c(1:length(items)), "]), value=vals[",c(1:length(items)),"])", collapse=","),  "))")))
      }
      
      #iterate through other levels
      for (c in c((ncol(structure)-2):1)) {
        labels<-data.frame(table(structure[,c]))        
        lookup<-data.frame(table(structure[,c], structure[,c+1]))
        lookup2<-lookup[lookup$Freq!=0,]
        for (i in c(1:nrow(labels))) {
          eval(parse(text=paste0(gsub(" ", "_",gsub("[[:punct:]]","",labels[i,1])),
                                 "<-list(name=\"", 
                                 labels[i,1], 
                                 paste0("\", children=list(", 
                                        paste0(gsub(" ", "_",gsub("[[:punct:]]","", lookup2[lookup2$Var1==labels[i,1],2])), collapse=","), ")"),
                                 ")")
          ))
        }
      }
      
      #final top level
      labels<-data.frame(table(structure[,1]))
      eval(parse(text=paste0("Top<-list(name=\"", top_label,"\" , children=list(", paste(gsub(" ", "_", labels[,1]), collapse=","), ")",")")))           
      
    }  
    
    json<-toJSON(Top)
    return(list(Type="json:nested", json=json))
  }
  
  
  
  
  D3Dendro<-function(JSON, text=15, height=3000, width=1000, file_out){
    
    if (JSON$Type!="json:nested"){stop("Incorrect json type for this D3")}
    
    header<-paste0("<!DOCTYPE html>
                   <meta charset=\"utf-8\">
                   <style>
                   
                  .node {
                   font: 15px sans-serif;
                   }
                   
                   .link {
                   fill: none;
                   stroke: #ccc;
                   stroke-width: 1.5px;
                   }
  			   
                   .node circle {
                   fill: #fff;
				            transition:0s 0.05s;
                   }
				   
				          .node circle:hover {
                   fill: #FF0000;
				            transition: 0s;
                   } 
                   
                   </style>
                   <body>
                   <script src=\"http://d3js.org/d3.v3.min.js\"></script>
                   
                   <script type=\"application/json\" id=\"data\">")
    
    
    footer<-paste0("</script>
                   
                   
                   
                   
                   <script>
                   
                   var data = document.getElementById('data').innerHTML;
                   root = JSON.parse(data);
                   
                   
                   var width = ", width, ",
                   height = ", height, ";
                   
                   var cluster = d3.layout.cluster()
                   .size([height-100, width - 500]);
                   
                   var diagonal = d3.svg.diagonal()
                   .projection(function(d) { return [d.y, d.x]; });
                   
                   var svg = d3.select(\"body\").append(\"svg\")
                   .attr(\"width\", width)
                   .attr(\"height\", height)
                   .append(\"g\")
                   .attr(\"transform\", \"translate(40,0)\");
                   
                   
                   var nodes = cluster.nodes(root),
                   links = cluster.links(nodes);
                   
                   var link = svg.selectAll(\".link\")
                   .data(links)
                   .enter().append(\"path\")
                   .attr(\"class\", \"link\")
                   .attr(\"d\", diagonal);
                   
                   var node = svg.selectAll(\".node\")
                   .data(nodes)
                   .enter().append(\"g\")
                   .attr(\"class\", \"node\")
                   .attr(\"transform\", function(d) { return \"translate(\" + d.y + \",\" + d.x + \")\"; })
                   
  				 node.append(\"circle\")
					 .attr(\"r\", 10)
					 .style(\"opacity\", function(d) {if(d.children){return 0;} else {return 1;}})
					 .on(\"click\", function(d) {if(!d.children) {window.open(d.value);} })
					 ;
				 
				 
					node.append(\"image\")
					.attr(\"xlink:href\", function(d) {if(d.children){return \"http://www.clker.com/cliparts/W/i/K/w/1/D/glossy-orange-circle-icon-md.png\"} 
														else {return \"http://www2.psd100.com/icon/2013/09/1101/Orange-play-button-icon-0911053546.png\"}})
			
					.attr(\"x\", -7)
					.attr(\"y\", -7)
					.attr(\"width\", 14)
					.attr(\"height\", 14)
					.on(\"click\", function(d) {if(!d.children) {window.open(d.value);} })
					;
                   
                   node.append(\"text\")
                   .attr(\"dx\", function(d) { return d.children ? 50 : 8; })
                   .attr(\"dy\", function(d) { return d.children ? 20 : 4; })
                   .style(\"text-anchor\", function(d) { return d.children ? \"end\" : \"start\"; })
                   .text(function(d) { return d.name; });
                   
                   
                   d3.select(self.frameElement).style(\"height\", height + \"px\");
                   
                   </script>") 
    
    fileConn<-file(file_out)
    writeLines(paste0(header, JSON$json, footer), fileConn)
    close(fileConn)
    
  }
  
  
  
  
  tracks<-getAlbumsTracks(getArtistsAlbums(getArtists(artist)))
  json<-jsonNestedData(tracks[,c(1,3,5)], values=tracks[,9], top_label="Discography")
  D3Dendro(json, file_out=output_file)
    
  return(list(discography=tracks,json=json))
  
  }







