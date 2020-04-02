
metricas <- data.frame(dataset = c("bladder", "breast", "colon", "kidney",
                                   "lung", "prostate", "thyroid", "oral",
                                   "uterine", "pancreatic"),
                       nnet = c(0.7541667, 0.8700000, 0.7150416, 0.8791667,
                                0.8361364, 0.6570000, 0.7133200, 0.8742437,
                                0.6834450, 0.6556061),
                       lda = c(0.7729167, 0.8566667, 0.7452914, 0.8697980,
                               0.8207008, 0.6660000, 0.7559288, 0.8714067,
                               0.6678606, 0.7420779))

metricas

# Test Wilcoxon
wilcox.test(metricas$nnet, metricas$lda, paired = TRUE)

# el test no proporciona evidencias significativas con un 95% de confianza de una precisión distinta de los modelos aplicados