# NOTE: beeswarm moves the y-values slightly for optimal positioning but bar is at originally calculated mean.
# The input is a table of all genera abundances for a certain domain.
tb <- read.table("Healthy_IBD_taxa_genus_mapped_reads_norm.txt",header=T)
# Enter the domain below.
PFAM <- "DUF4111"
library(beeswarm)
library(ggplot2)
library(ggtree)
# Prints a basic R graph but also deposits a data frame of the point positions.
beeswarmp1 <- beeswarm(Abun ~ IBD, data = tb, method = "center",pwcol=c(1:nrow(tb)),spacing=1.5,corral="wrap")
# Remove the rows that have 0 abundance.
beeswarmp <- beeswarmp1[beeswarmp1$y.orig!=0,]
# Add a fake column to beeswarmp to generate a legend in the base plot.
beeswarmp$fake <- unlist(list(newdf$taxa,sample(newdf$taxa,size=nrow(beeswarmp)-length(newdf$taxa),replace=TRUE)))
# Set a data frame that will be updated in the loop.
newdf=data.frame(comp=c(1:(ncol(tb)-4)),taxa=colnames(tb)[-1:-4])
# Set a ggplot base with the 0 abundance value points and mean bars added in.
p <- ggplot(beeswarmp,aes(x,y,colour=fake))+geom_point(alpha=0)+guides(colour= guide_legend(title=NULL,override.aes=list(size=5,alpha=1,fill=NA)))+scale_x_continuous(labels=c("Healthy","IBD"),breaks=c(1,2))+labs(title=PFAM,x="",y="Abundance")+geom_point(data=beeswarmp1[beeswarmp1$y.orig==0,],aes(x,y.orig),size=4.5,alpha=0.9,colour="grey48")+ geom_segment(data=beeswarmp1[beeswarmp1$x.orig==0,],aes(x=0.75,y=mean(y.orig),xend=1.25,yend=mean(y.orig),colour = "segment"),colour="grey48",lineend = "round")+ geom_segment(data=beeswarmp1[beeswarmp1$x.orig==1,],aes(x=1.75,y=mean(y.orig),xend=2.25,yend=mean(y.orig),colour = "segment"),colour="grey48",lineend = "round")
for (i in seq_along(beeswarmp$x)) {
    # Update the taxonomic compositions for each person/sample.
    newdf[,1] <- unlist(tb[beeswarmp[i,4],][-1:-4])
    # Set the pie function (has to be inside the loop or can’t access the updates to the newdf).
    pie <- ggplot(newdf,aes(x=factor(1),y=comp,fill=taxa))+geom_bar(stat="identity",width=1)+coord_polar(theta="y")+theme_tree()+xlab(NULL)+ylab(NULL)+theme_transparent()
    # Print the piechart on the graph where beeswarm intended.
    p <- subview(p,pie,beeswarmp$x[i],beeswarmp$y[i],width=0.063,height=0.063)
}
pdf(paste(PFAM,"_tax_comp.pdf",sep=""),width=9)
print(p)
dev.off()
