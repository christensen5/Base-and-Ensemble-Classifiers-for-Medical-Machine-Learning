PartitionData <- function(data, targetValues, filepath_to_folder, name) {

	stopifnot(is.data.frame(data) && is.vector(targetValues))
	stopifnot(nrow(data)==length(targetValues))
	
	nrow=nrow(data)
	
	path.data.Btrain = paste0(filepath_to_folder,"/",name,"-data-Btrain.data")
	path.data.Mtrain = paste0(filepath_to_folder,"/",name,"-data-Mtrain.data")
	path.data.Mtest = paste0(filepath_to_folder,"/",name,"-data-Mtest.data")
	
	path.tv.Btrain = paste0(filepath_to_folder,"/",name,"-tv-Btrain.data")
	path.tv.Mtrain = paste0(filepath_to_folder,"/",name,"-tv-Mtrain.data")
	path.tv.Mtest = paste0(filepath_to_folder,"/",name,"-tv-Mtest.data")
	
	
	if(nrow%%4==0) {
		
		write.table(        data[ seq(  	      1  	      ,      nrow/2       , 1) , ], path.data.Btrain, quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
		write.table(        data[ seq(   	 (nrow/2)+1 	  ,    (3/4)*nrow     , 1) , ], path.data.Mtrain, quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
		write.table(        data[ seq( 	  ((3/4)*nrow)+1	  ,       nrow        , 1) , ], path.data.Mtest,  quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
		
		write.table(targetValues[ seq(   	     1   	      ,      nrow/2       , 1)   ], path.tv.Btrain, quote=FALSE, row.names=FALSE, col.names=FALSE)
		write.table(targetValues[ seq(   	 (nrow/2)+1  	  ,    (3/4)*nrow     , 1)   ], path.tv.Mtrain, quote=FALSE, row.names=FALSE, col.names=FALSE)
		write.table(targetValues[ seq(	  ((3/4)*nrow)+1 	  ,       nrow        , 1)   ], path.tv.Mtest,  quote=FALSE, row.names=FALSE, col.names=FALSE)
	}
	
	if(nrow%%2==0 && nrow%%4==2) {
		
		write.table(        data[ seq(           1            ,      nrow/2       , 1) , ], path.data.Btrain, quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
		write.table(        data[ seq(       (nrow/2)+1       , floor((3/4)*nrow) , 1) , ], path.data.Mtrain, quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
		write.table(        data[ seq( ceiling(((3/4)*nrow))  ,       nrow        , 1) , ], path.data.Mtest,  quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
		
		write.table(targetValues[ seq(            1           ,      nrow/2       , 1)   ], path.tv.Btrain, quote=FALSE, row.names=FALSE, col.names=FALSE)
		write.table(targetValues[ seq(       (nrow/2)+1       , floor((3/4)*nrow) , 1)   ], path.tv.Mtrain, quote=FALSE, row.names=FALSE, col.names=FALSE)
		write.table(targetValues[ seq( ceiling(((3/4)*nrow))  ,       nrow        , 1)   ], path.tv.Mtest,  quote=FALSE, row.names=FALSE, col.names=FALSE)
	}
	
	if(nrow%%2==1) {
		
		write.table(        data[ seq(          1             ,    floor(nrow/2)   , 1) , ], path.data.Btrain, quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
		write.table(        data[ seq(   ceiling(nrow/2)      ,  floor((3/4)*nrow) , 1) , ], path.data.Mtrain, quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
		write.table(        data[ seq( ceiling(((3/4)*nrow))  ,        nrow        , 1) , ], path.data.Mtest,  quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
		
		write.table(targetValues[ seq(            1           ,    floor(nrow/2)   , 1)   ], path.tv.Btrain, quote=FALSE, row.names=FALSE, col.names=FALSE)
		write.table(targetValues[ seq(    ceiling(nrow/2)     ,  floor((3/4)*nrow) , 1)   ], path.tv.Mtrain, quote=FALSE, row.names=FALSE, col.names=FALSE)
		write.table(targetValues[ seq( ceiling(((3/4)*nrow))  ,        nrow        , 1)   ], path.tv.Mtest,  quote=FALSE, row.names=FALSE, col.names=FALSE)

	}
	
}
