# function kshortest path
# Inspired by the work of Yen et al:
# Yen, Jin Y. (1970). "An algorithm for finding shortest routes from all source nodes to a given destination in general networks". 
# Quarterly of Applied Mathematics 27: 526–530. MR 0102435.
# Yen, Jin Y. (Jul 1971). "Finding the k Shortest Loopless Paths in a Network". Management Science 17 (11): 712–716. doi:10.1287/mnsc.17.11.712.
#
# Arguments:
# graph: the graph
# from:  the source node name in the graph
# to :   the destination node name in the graph
# k  :   number of path to return (default 100)
# len:   maximum length of the path
k_shortestpath <- function(graph, from, to, k=100, len=10000){  
  i <- 1 
  possible_path <- list()
  first_path=0;
  first_path <- get.shortest.paths(graph,from,to, output='both')
  # shortest possible_path 
 shortest.possible_path <- list(list(g=graph, path=first_path$epath, vert=first_path$vpath, dist=shortest.paths(graph,from,to)))
  # until k shortest paths are found
  current_len<-shortest.possible_path[[length(shortest.possible_path)]]$dist; 
  while(i<k&&current_len<=len){ 
    last.path <- shortest.possible_path[[length(shortest.possible_path)]]              
    possible_path <- calculate.possible_path(possible_path, last.path, from, to)   
    sp <- select.shortest.path(possible_path)
    if (length(possible_path)>0) {
		shortest.possible_path[[length(shortest.possible_path)+1]] <- list(g=possible_path[[sp]]$g, path=possible_path[[sp]]$possible_path$path, vert=possible_path[[sp]]$possible_path$vert, dist=possible_path[[sp]]$possible_path$dist)
		current_len=possible_path[[sp]]$possible_path$dist;		
		i=i+1	
		possible_path <- possible_path[-sp]			
	} else {	
		 return(shortest.possible_path)
	}
  }
  return(shortest.possible_path)
}

#k-shortest_path runction but requiring an augmented node in the path
#Example with Sample_1:
#k_shortestpath_with_k(g2, c("x12","x14","x15"),"x1","x2")
#for (e in k_shortestpath_with_k(g2,c("x12","x14","x15"), "x1","x4")$path[[1]$
#
# Arguments:
# graph:            the graph
# additional_node: the name of the additional node that at least one must be in the returned path
# from:             the source node name in the graph
# to :              the destination node name in the graph
# k*  :              number of path to search (default 100)
# len:              maximum length of the path to consider
k_shortestpath_with_k <- function(graph, additional_node,from, to, k=100, len=10000){  
  additional_node=V(graph)[additional_node]; #Ensure that we take vertex number
  i <- 1 
  not_found <- TRUE;
  possible_path <- list()
  first_path <- get.shortest.paths(graph,from,to, output='both')
  if (length(length(first_path$vpath[[1]]))==0) return(list(path=c(""), vert=c(), dist=Inf, k=i)); 
  # shortest possible_path
  shortest.possible_path <- list(list(g=graph, path=first_path$epath, vert=first_path$vpath, dist=shortest.paths(graph,from,to)))
  # until k shortest paths are found  
 
  current_len<-shortest.possible_path[[length(shortest.possible_path)]]$dist; 
  
  while(i<k&&current_len<=len&&not_found){ 
    last.path <- shortest.possible_path[[length(shortest.possible_path)]]
    #print(last.path);
	if (good_path(last.path$vert[[1]], additional_node)) {
    	not_found=FALSE;
    	last.path$k=i;
		return (last.path);
    }            
    possible_path <- calculate.possible_path(possible_path, last.path, from, to)   
    sp <- select.shortest.path(possible_path)
    if (length(possible_path)>0) {
		shortest.possible_path[[length(shortest.possible_path)+1]] <- list(g=possible_path[[sp]]$g, path=possible_path[[sp]]$possible_path$path, vert=possible_path[[sp]]$possible_path$vert, dist=possible_path[[sp]]$possible_path$dist)
		current_len=possible_path[[sp]]$possible_path$dist;		
		i=i+1	
		possible_path <- possible_path[-sp]			
	} else {
	     #no path
		 return(list(path=c(""), vert=c(), dist=Inf, k=i));
	}
  }
  return(list(path=c(""), vert=c(), dist=Inf, k=i));
}

#Helper function
calculate.possible_path <- function(possible_path, path, from, to){
  g <- path$g
  
  for (j in unlist(path$path)){
    newgraph <- delete.edges(g, j) # remove adge
    sp <- get.shortest.paths(newgraph,from,to, output='both') # calculate shortest path
    spd=-1;
	spd <- shortest.paths(newgraph,from,to) # calculate length
    if (spd != Inf&&spd!=-1){ # the the path is found
      if (!contains.path(possible_path, sp$vpath)) # add to list, unless it already contains the same path
      {	  
        possible_path[[length(possible_path)+1]] <- list(g=newgraph, possible_path=list(path=sp$epath, vert=sp$vpath, dist=spd))
      }
    }
  }

  return(possible_path)
}

#Helper function
# does a list contain this path?
contains.path <- function(possible_path, path){
  return( any( unlist( lapply( possible_path, function(x){ identical(x$path$vert,path) } ) ) ) )
}

#Helper function
# which path from the list is the shortest?
select.shortest.path <- function(possible_path){
  return( which.min( unlist( lapply( possible_path, function(x){x$possible_path$dist} ) ) ) )
}
 
# Heuristic function return a list of (path, dist)
#Note that from and to must be the vertex names
#While the additional_node are the vertex number
heuristic2<-function(graph,additional_node, from, to, k=100) {
   if (from==to) return (list(path=c(""), dist=0));
   additional_node=as.numeric(V(graph)[additional_node]);
# find k=100 shortest path by length and take the best if good
	paths=k_shortestpath_with_k(graph, additional_node, from, to, k=k, len=10000);
	#paths=k_shortestpath(graph, from, to, k=k, len=10000);
	#for (p in paths) {	
	if (length(paths$vert[[1]])>0)
		if (is.finite(paths$dist)&&good_path(paths$vert[[1]], additional_node)) {
			return (list(path=V(paths$g)[paths$vert[[1]]]$name, dist=as.numeric(paths$dist)));
		} 
	return (list(path=c(""), dist=Inf));
}


#Note that from and to must be the vertex names in graph
#While the additional_node are the vertex number in graph
#data(Sample_1)
#heuristic3(g2, c(8,9,10), "x1","x3")
heuristic3<-function(graph,additional_node, from, to, len=20) {
 if (from==to) return (list(path=c(""), dist=0));	
 additional_node=as.numeric(V(graph)[additional_node]);
# find the take of distance k to x and y if k is not x nor y 

	if (from%in%V(graph)[additional_node]$name||to%in%V(graph)[additional_node]$name) {	
		return (list(path=c(""), dist=Inf));	
	}
  
	iso_g3short_i=shortest.paths(graph, from,  algorithm = "dijkstra")
	iso_g3short_j=shortest.paths(graph, to,  algorithm = "dijkstra")
   current_path=c();
   current_vert=c();
   current_path_dist=Inf;
    #print(iso_g3short_i) 
    #print(iso_g3short_j)
	for (k in additional_node) {
		m1=iso_g3short_i[from,V(graph)[k]$name];
		m2=iso_g3short_j[to,V(graph)[k]$name];
		#print(m1)
		#print(m2)
		  if (is.finite(m1)&&is.finite(m2)&&(m1+m2<=len)&&(m1+m2<=current_path_dist)) {
			# # #check 5 shortest path for this k
			 paths1=k_shortestpath(graph, from, V(graph)[k]$name, k=2, len=10000);
			 paths2=k_shortestpath(graph, to,V(graph)[k]$name, k=2, len=10000);
			# #print(paths1)
			# #print(paths2)
			 if (length(paths1[[1]]$path)>0&&length(paths2[[1]]$path)>0) 
				for (p1 in paths1) {
					for (p2 in paths2) {
						#dd=length(p1$vert[[1]])+length(p2$vert[[1]])-1;
						if (length(p1$vert[[1]])>0&&length(p2$vert[[1]])>0)  {
							p=join_path(V(p1$g)[p1$vert[[1]]]$name,V(p2$g)[p2$vert[[1]]]$name,TRUE);
							#possible weigh
							#d=length(p)-1;
							d=0;
							#d=weight_path_from_node(graph,p);
							d=length(p)-1;
							#d<-as.numeric(p1$dist)+as.numeric(p2$dist);
							if (d>0&&good_path_h3(p, additional_node,graph)&&d<current_path_dist) {
								#Real weight
								current_path_dist=d;
								#weight_path_from_node(graph,p);;
								current_path=p;
							}
						}
					}
				}
		  }
	}	
	if (is.finite(current_path_dist)) return (list(path=current_path, dist=current_path_dist));
	# return inf if not found
	#cat("not found\n");
	return (list(path=c(""), dist=Inf));	
}
#heuristic3(g2, c(8,9,10), "x1","x2")

#path contain a k and do not contain duplicate
#additional_node is an of the additional_node vertex numbering
good_path<-function(path, additional_node) {
	if (length(path)<3) return (FALSE);
	if (length(unique(path))!=length(path)) return (FALSE);	
	if (!any((path[2:(length(path)-1)]) %in% additional_node)) return (FALSE);  
	return (TRUE);
}

good_path_h3<-function(path, additional_node,graph) {
	if (length(path)<3) return (FALSE);
	if (length(unique(path))!=length(path)) return (FALSE);	
	if (!any((path[2:(length(path)-1)]) %in% V(graph)[additional_node]$name)) return (FALSE);  
	return (TRUE);
}

#join two path, we expect path1 to be from left->right as path2 then, we must inverse path2 and remove the middle vertex
join_path<-function(path1, path2, second_is_reverse=FALSE) {	
	if (second_is_reverse) path2=rev(path2);
	if (path1[length(path1)]==path2[length(path2)]) path2=rev(path2);		
	if (path1[length(path1)]==path2[1]) {
		return (c(path1[1:length(path1)-1], path2));
	} else {
		return (c(path1, path2));
	}
}

#get path length
weight_path_from_edge<-function(graph, path) {
	if (length(path)<2||length(table(path))<2) return(0);
	w<-0;
	for (e in path) {
		wt=E(graph)[e]$weight;
		if (!is.null(wt)) {
			w=w+wt;
		} else {
			w=w+1;
		}
	}
	return (w);
}

# Node: the path length between to node is set to 1
weight_path_from_node<-function(graph, path) {
	if (length(path)<2||length(table(path))<2) return(0);		
	w<-0;
	for (i in 1:(length(path)-1)) {
		w=w+1
	}
	return (w);	
}

#Return the original and augmented node of a graph
get_node_type<-function(g1,g2,taxnames='') {	
	g1names<-V(g1)$name;    #list of nodes taxnames in g1	
	if (taxnames=='') {
		#we take all the node node in graph1			
		g2_unique_names<-V(g2)[!(V(g2)$name %in% g1names)]$name;			
	} else {
		g2_unique_names<-V(g2)[V(g2)$tax==as.factor(taxnames)]$name;		
	}
	g1_unique_names<-V(g1)[!(V(g1)$name %in% g2_unique_names)]$name;
return (list("original"=g1_unique_names, "augmented"=g2_unique_names, "original_number_in_g2"=as.numeric(V(g2)[g1_unique_names]), "augmented_number_in_g2"=as.numeric(V(g2)[g2_unique_names])));
}



