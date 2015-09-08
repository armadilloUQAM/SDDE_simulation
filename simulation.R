#Simulation for the article:
#
#
library(SDDE)
source("k_shortestpath.R")
source("complete_network_trace.R")
options(error = function() traceback())
# Repeat 100 times (with 10 path in 10 diff. graph)
nreplicate=10;
npath=10;
networks=list();
result_h1=list();
result_h2=list();
result_h3=list();
result_h4=list();

time_h1=list();
time_h2=list();
time_h3=list();
time_h4=list();


for (i in 1:nreplicate) {
	r=random_network(100,5,type="erdos");
	set.seed(sample(1:10000000,1));
	networks[[i]]=r;
	j=1;
	
	while (j<=npath) {
		node1=sample(V(r$g1)$name,1)
		node2=sample(V(r$g1)$name,1)
		if (node1!=node2) {
			tp0 <- proc.time();
			l1=complete_trace(r$g1,r$g2, node1=node1, node2=node2, heuristic=1);
			temps1<-proc.time()-tp0
			tp0 <- proc.time();
			l2=complete_trace(r$g1,r$g2, node1=node1, node2=node2, heuristic=2);
			temps2<-proc.time()-tp0
			tp0 <- proc.time();
			l3=complete_trace(r$g1,r$g2, node1=node1, node2=node2, heuristic=3);
			temps3<-proc.time()-tp0
			tp0 <- proc.time();
			l4=complete_trace(r$g1,r$g2, node1=node1, node2=node2, heuristic=4);
			temps4<-proc.time()-tp0
			result_h1[[paste(i,j,sep="_")]]=l1;
			result_h2[[paste(i,j,sep="_")]]=l2;
			result_h3[[paste(i,j,sep="_")]]=l3;	
			result_h4[[paste(i,j,sep="_")]]=l4;			
			time_h1[[paste(i,j,sep="_")]]=temps1;
			time_h2[[paste(i,j,sep="_")]]=temps2;
			time_h3[[paste(i,j,sep="_")]]=temps3;
			time_h4[[paste(i,j,sep="_")]]=temps4;
			
			j=j+1;
		}
	}
	

}
print(time_h1);
# Compare result here (Table 1)
Table1=array("",c(nreplicate*npath,4));
Table2=array("",c(nreplicate*npath,4)); #Path
Table3=array("",c(nreplicate*npath,4)); #Taxa
Table4=array(0,c(nreplicate*npath,4)); #length
Table5=array(0,c(nreplicate*npath,4)); #time
Table6=array(FALSE,c(nreplicate*npath,4)); #valid

for (i in 1:nreplicate) {
		for (j in 1:npath) {
		id=paste(i,j,sep="_");
		#print(id)
		#print(result_h1[[id]]$path_type);
		index=(i-1)*npath+j;
		Table1[index,1]=result_h1[[id]]$path_type;
		Table1[index,2]=result_h2[[id]]$path_type;
		Table1[index,3]=result_h3[[id]]$path_type;
		Table1[index,4]=result_h4[[id]]$path_type;
		
		p1=paste(result_h1[[id]]$path,collapse=",");
		p2=paste(result_h2[[id]]$path,collapse=",");
		p3=paste(result_h3[[id]]$path,collapse=",");
		p4=paste(result_h4[[id]]$path,collapse=",");
		
		Table2[index,1]=p1;
		if (p1!=p2) Table2[index,2]=p2;
		if (p1!=p3) Table2[index,3]=p3;
		if (p1!=p4) Table2[index,4]=p4;
		
		Table3[index,1]=paste(result_h1[[id]]$path_visited_taxa,collapse=",");
		Table3[index,2]=paste(result_h2[[id]]$path_visited_taxa,collapse=",");
		Table3[index,3]=paste(result_h3[[id]]$path_visited_taxa,collapse=",");
		Table3[index,4]=paste(result_h4[[id]]$path_visited_taxa,collapse=",");
		
		
		Table4[index,1]=result_h1[[id]]$augmented_path_length;
		Table4[index,2]=result_h2[[id]]$augmented_path_length;
		Table4[index,3]=result_h3[[id]]$augmented_path_length;
		Table4[index,4]=result_h4[[id]]$augmented_path_length;
		
		
		Table5[index,1]=time_h1[[id]][1];
		Table5[index,2]=time_h2[[id]][1];
		Table5[index,3]=time_h3[[id]][1];
		Table5[index,4]=time_h4[[id]][1];
		
		 Table6[index,1]=length(table(result_h1[[id]]$path_visited_taxa))>1;
		 Table6[index,2]=length(table(result_h2[[id]]$path_visited_taxa))>1
		 Table6[index,3]=length(table(result_h3[[id]]$path_visited_taxa))>1;
		 Table6[index,4]=length(table(result_h4[[id]]$path_visited_taxa))>1;
		 
	}
}
#Save the results in different Tables
#divided for one column for each heuristic
##########################
# Columns:
##########################
# 1		heuristic1 (SDDE)
# 2 	heuristic2
# 3 	heuristic3
# 4 	heuristic4
##########################
# Tables:
##########################
#Table1		Type of path found
#Table2		The path found
#Table 3	The node taxa information (group)
#Table 4	The path length
#Table 5	The time required for the search
#Table 6	If we found a solution in this iteration?
print(Table1);
write.table(Table1,"Table1.txt",sep=",");
print(Table2);
write.table(Table2,"Table2.txt",sep=",");
print(Table3);
write.table(Table3,"Table3.txt",sep=",");
print(Table4);
write.table(Table4,"Table5.txt",sep=",");
print(Table5);
write.table(Table5,"Table5.txt",sep=",");
print(Table6);
write.table(Table6,"Table6.txt",sep=",");