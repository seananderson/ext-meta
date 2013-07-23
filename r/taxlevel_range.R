bivalves$Tax.level<-factor(bivalves$Tax.level)
qplot(Tax.level, lnor, data=bivalves, facets=~Trait.category) + theme_bw(base_size=17) + geom_boxplot()

