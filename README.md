# Phyloseq_Comparing-groups

# Comparing two groups:
Dist2 = distance(bacteria2, "unifrac", weighted = TRUE)

Ordinate2 = ordinate(bacteria2, "CCA", Dist2)

library("ggplot2")

o1 = plot_ordination(bacteria2, Ordinate2, "samples", color = "Mouse.Type", title = "plot_ordination, CCA, wUF")

o1

o1+facet_wrap(~taxonmy3, 3)

o1+geom_polygon(aes(fill=Grp))+geom_point(size=2)+ggtitle("samples")

o3=plot_ordination(bacteria2, Ordinate2, type="split", color = "Mouse.Type")

o3

plot_bar(bacteria2, "Grp", fill="Mouse.Type", facet_grid=~taxonomy3)

'?'(merge_phyloseq_pair)

merge12 <- merge_phyloseq(bacteria1, bacteria2)

m1<-merge_samples(b10new, "Grp")

sample_names(b10new)

m1

OTU12 <-merge_phyloseq_pair(OTU, OTU1)

c1<-phyloseq(OTU,TAX)

c11<- merge_phyloseq(c1, SD)

sample_names()

TAX12 <- merge_phyloseq_pair(TAX, TAX1)

TAX12

SD12 <- merge_phyloseq_pair(SD, SD1)

SD12

tree12<-merge_phyloseq_pair(tree, tree1)

bacteria12 <- phyloseq(OTU12, TAX12, SD12)

bacteria12

bacteria12 <- subset_taxa(bacteria12, taxonomy2=="Bacteria")

myTaxa12 = names(sort(taxa_sums(bacteria12), decreasing = TRUE)[1:10])

myTaxa

b12 = prune_taxa(taxa_sums(bacteria12) > 0, bacteria12)

mergedb12=merge_samples(b12, "Grp")

mergedb12

SD12 = merge_samples(sample_data(b12), "Mouse.Type")

SD12

identical(SD12, sample_data(mergedb12))

b12OTU10 = names(sort(taxa_sums(b12), TRUE)[1:30])

any(taxa_sums(b12OTU10)== 0)

b10_12=prune_taxa(b12OTU10, b12)

b10new = prune_taxa(taxa_sums(b10_12)>0, b10_12)

any(sample_sums(b10new) == 0)

b10new = prune_samples(sample_sums(b10_12)>0, b10_12)

trial12 <- transform_sample_counts(b10new, function(x) 100* x/sum(x))

trial12

'?'(transform_sample_counts)

// b12mb10 = prune_taxa(b12OTU10, mergedb12)

rowSums(otu_table(b10_12))

'?'(rowSums)

//p1111<-plot_bar(b10_12, "taxonomy3", fill = "taxonomy3", facet_grid=~Grp)

p1111

p12<-plot_bar(trial12, "Grp", fill = "Grp", facet_grid=~taxonomy3)

p12

library("ggplot2")

p12 + geom_bar(aes(color=Grp, fill= Grp), stat="identity", position="stack")

ss <- subset_taxa(trial12, taxonomy3 %in% c('Actinobacteria','Bacteroidetes','Chloroflexi','Firmicutes','Proteobacteria','Tenericutes'))

glom <- tax_glom(ss, taxrank = 'taxonomy3')

dat <- psmelt(glom)

library("ggplot2")

ggplot(dat, aes(x=Grp, y=Abundance, fill=taxonomy3)) + geom_boxplot()
plot_richness(b10new, Mouse.Type)

'?'(geom_boxplot)

p2 <- plot_richness(b10new, "Grp1", "Grp")

p2 <- p2+geom_boxplot(data=p2$data, aes(x=Grp1, y=value, color=NULL))

p2

plot_net(b10new, maxdist=0.3, point_label="Mouse.Type")

plot_heatmap(b10new)

dist = distance(m1, "unifrac", weighted = TRUE)

ord = ordinate(bacteria2, "CCA",dist)

library("ggplot2")

b.ord <- ordinate(m1, "CCA", "unifrac")

p0 = plot_ordination(m1, b.ord, "samples", color = "Mouse.Type")

p0+geom_jitter(aes(fill=Mouse.Type)) + geom_point(size=10)


