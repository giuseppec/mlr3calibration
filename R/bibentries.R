bibentries = list(
  weissman2024 = bibentry(
    bibtype = "Manual",
    key = "weissman2024",
    author = person("Gary", "Weissman"),
    title = "gmish: Miscellaneous Functions for Predictive Modeling",
    year = "2024",
    note = "R package version 0.1.0, commit b1ef2af",
    url = "https://github.com/gweissman/gmish/tree/b1ef2afef76ec2f232f5ad1d3697f31b39377204"
  ),
  austin2019 = bibentry(
    bibtype = "Article",
    key = "austin2019",
    author = c(
      person(c("Peter", "C."), "Austin"),
      person(c("Ewout", "W."), "Steyerberg")
    ),
    title = paste(
      "The Integrated Calibration Index (ICI) and Related Metrics for Quantifying",
      "the Calibration of Logistic Regression Models"
    ),
    journal = "Statistics in Medicine",
    year = "2019",
    volume = "38",
    number = "21",
    pages = "4051-4065",
    doi = "10.1002/sim.8281"
  ),
  hosmer2000 = bibentry(
    bibtype = "Book",
    key = "hosmer2000",
    author = c(
      person(c("David", "W."), "Hosmer"),
      person("Stanley", "Lemeshow")
    ),
    title = "Applied Logistic Regression",
    edition = "Second",
    publisher = "John Wiley & Sons",
    address = "New York",
    year = "2000",
    doi = "10.1002/0471722146"
  ),
  cox1958 = bibentry(
    bibtype = "Article",
    key = "cox1958",
    author = person(c("D.", "R."), "Cox"),
    title = "Two Further Applications of a Model for Binary Regression",
    journal = "Biometrika",
    year = "1958",
    volume = "45",
    number = "3/4",
    pages = "562-565",
    doi = "10.1093/biomet/45.3-4.562"
  ),
  spiegelhalter1986 = bibentry(
    bibtype = "Article",
    key = "spiegelhalter1986",
    author = person(c("D.", "J."), "Spiegelhalter"),
    title = "Probabilistic Prediction in Patient Management and Clinical Trials",
    journal = "Statistics in Medicine",
    year = "1986",
    volume = "5",
    number = "5",
    pages = "421-433",
    doi = "10.1002/sim.4780050506"
  ),
  niculescu2005 = bibentry(
    bibtype = "InProceedings",
    key = "niculescu2005",
    author = c(
      person("Alexandru", "Niculescu-Mizil"),
      person("Rich", "Caruana")
    ),
    title = "Predicting Good Probabilities with Supervised Learning",
    booktitle = "Proceedings of the 22nd International Conference on Machine Learning",
    year = "2005",
    pages = "625-632",
    doi = "10.1145/1102351.1102430"
  ),
  platt2000 = bibentry(
    bibtype = "InCollection",
    key = "platt2000",
    author = person(c("John", "C."), "Platt"),
    title = "Probabilities for SV Machines",
    editor = c(
      person(c("Alexander", "J."), "Smola"),
      person("Peter", "Bartlett"),
      person("Bernhard", "Schoelkopf"),
      person("Dale", "Schuurmans")
    ),
    booktitle = "Advances in Large-Margin Classifiers",
    publisher = "The MIT Press",
    year = "2000",
    pages = "61-74",
    doi = "10.7551/mitpress/1113.003.0008"
  ),
  zadrozny2002 = bibentry(
    bibtype = "InProceedings",
    key = "zadrozny2002",
    author = c(
      person("Bianca", "Zadrozny"),
      person("Charles", "Elkan")
    ),
    title = "Transforming Classifier Scores into Accurate Multiclass Probability Estimates",
    booktitle = "Proceedings of the 8th ACM SIGKDD International Conference on Knowledge Discovery and Data Mining",
    year = "2002",
    pages = "694-699",
    doi = "10.1145/775047.775151"
  ),
  kull2017 = bibentry(
    bibtype = "InProceedings",
    key = "kull2017",
    author = c(
      person("Meelis", "Kull"),
      person("Telmo", "Silva Filho"),
      person("Peter", "Flach")
    ),
    title = "Beta Calibration: A Well-Founded and Easily Implemented Improvement on Logistic Calibration for Binary Classifiers",
    booktitle = "Proceedings of the 20th International Conference on Artificial Intelligence and Statistics",
    year = "2017",
    volume = "54",
    pages = "623-631",
    url = "https://proceedings.mlr.press/v54/kull17a.html"
  ),
  silvafilho2017 = bibentry(
    bibtype = "Manual",
    key = "silvafilho2017",
    author = c(
      person(c("Telmo", "M."), "Silva Filho"),
      person("Meelis", "Kull")
    ),
    title = "betacal: Beta Calibration",
    year = "2017",
    note = "R package version 0.1.0",
    url = "https://CRAN.R-project.org/package=betacal"
  ),
  pedregosa2011 = bibentry(
    bibtype = "Article",
    key = "pedregosa2011",
    author = c(
      person("Fabian", "Pedregosa"),
      person("Ga\u00ebl", "Varoquaux"),
      person("Alexandre", "Gramfort"),
      person("Vincent", "Michel"),
      person("Bertrand", "Thirion"),
      person("Olivier", "Grisel"),
      person("Mathieu", "Blondel"),
      person("Peter", "Prettenhofer"),
      person("Ron", "Weiss"),
      person("Vincent", "Dubourg"),
      person("Jake", "Vanderplas"),
      person("Alexandre", "Passos"),
      person("David", "Cournapeau"),
      person("Matthieu", "Brucher"),
      person("Matthieu", "Perrot"),
      person("\u00c9douard", "Duchesnay")
    ),
    title = "Scikit-learn: Machine Learning in Python",
    journal = "Journal of Machine Learning Research",
    year = "2011",
    volume = "12",
    number = "85",
    pages = "2825-2830",
    note = "See also the probability calibration user guide: https://scikit-learn.org/stable/modules/calibration.html",
    url = "https://jmlr.org/papers/v12/pedregosa11a.html"
  ),
  guo2017 = bibentry(
    bibtype = "InProceedings",
    key = "guo2017",
    author = c(
      person("Chuan", "Guo"),
      person("Geoff", "Pleiss"),
      person("Yu", "Sun"),
      person(c("Kilian", "Q."), "Weinberger")
    ),
    title = "On Calibration of Modern Neural Networks",
    booktitle = "Proceedings of the 34th International Conference on Machine Learning",
    year = "2017",
    volume = "70",
    pages = "1321-1330",
    url = "https://proceedings.mlr.press/v70/guo17a.html"
  ),
  nixon2019 = bibentry(
    bibtype = "InProceedings",
    key = "nixon2019",
    author = c(
      person("Jeremy", "Nixon"),
      person(c("Michael", "W."), "Dusenberry"),
      person("Linchuan", "Zhang"),
      person("Ghassen", "Jerfel"),
      person("Dustin", "Tran")
    ),
    title = "Measuring Calibration in Deep Learning",
    booktitle = "Proceedings of the IEEE/CVF Conference on Computer Vision and Pattern Recognition Workshops",
    year = "2019",
    pages = "38-41",
    url = paste0(
      "https://openaccess.thecvf.com/content_CVPRW_2019/html/",
      "Uncertainty_and_Robustness_in_Deep_Visual_Learning/",
      "Nixon_Measuring_Calibration_in_Deep_Learning_CVPRW_2019_paper.html"
    )
  ),
  kull2019 = bibentry(
    bibtype = "InProceedings",
    key = "kull2019",
    author = c(
      person("Meelis", "Kull"),
      person("Miquel", "Perello Nieto"),
      person("Markus", "K\u00e4ngsepp"),
      person("Telmo", "Silva Filho"),
      person("Hao", "Song"),
      person("Peter", "Flach")
    ),
    title = "Beyond Temperature Scaling: Obtaining Well-Calibrated Multi-Class Probabilities with Dirichlet Calibration",
    booktitle = "Advances in Neural Information Processing Systems",
    year = "2019",
    volume = "32",
    pages = "12295-12305",
    url = "https://proceedings.neurips.cc/paper_files/paper/2019/hash/8ca01ea920679a0fe3728441494041b9-Abstract.html"
  ),
  gupta2022 = bibentry(
    bibtype = "InProceedings",
    key = "gupta2022",
    author = c(
      person("Chirag", "Gupta"),
      person("Aaditya", "Ramdas")
    ),
    title = "Top-Label Calibration and Multiclass-to-Binary Reductions",
    booktitle = "International Conference on Learning Representations",
    year = "2022",
    url = "https://openreview.net/forum?id=WqoBaaPHS-"
  ),
  arad2025 = bibentry(
    bibtype = "InProceedings",
    key = "arad2025",
    author = c(
      person("Alon", "Arad"),
      person("Saharon", "Rosset")
    ),
    title = "Improving Multi-Class Calibration through Normalization-Aware Isotonic Techniques",
    booktitle = "Proceedings of the 42nd International Conference on Machine Learning",
    year = "2025",
    volume = "267",
    pages = "1574-1603",
    url = "https://proceedings.mlr.press/v267/arad25a.html"
  ),
  silvafilho2023 = bibentry(
    bibtype = "Article",
    key = "silvafilho2023",
    author = c(
      person("Telmo", "Silva Filho"),
      person("Hao", "Song"),
      person("Miquel", "Perello-Nieto"),
      person("Raul", "Santos-Rodriguez"),
      person("Meelis", "Kull"),
      person("Peter", "Flach")
    ),
    title = paste(
      "Classifier Calibration: A Survey on How to Assess and Improve",
      "Predicted Class Probabilities"
    ),
    journal = "Machine Learning",
    year = "2023",
    volume = "112",
    number = "9",
    pages = "3211-3260",
    doi = "10.1007/s10994-023-06336-7"
  ),
  fan2025 = bibentry(
    bibtype = "Article",
    key = "fan2025",
    author = c(
      person(c("Kwok", "Lung"), "Fan"),
      person("Gene", "Pennello"),
      person("Qi", "Liu"),
      person("Nicholas", "Petrick"),
      person(c("Ravi", "K."), "Samala"),
      person(c("Frank", "W."), "Samuelson"),
      person(c("Yee", "Lam", "Elim"), "Thompson"),
      person("Qian", "Cao")
    ),
    title = paste(
      "Calzone: A Python Package for Measuring Calibration of",
      "Probabilistic Models for Classification"
    ),
    journal = "Journal of Open Source Software",
    year = "2025",
    volume = "10",
    number = "114",
    pages = "8026",
    doi = "10.21105/joss.08026"
  ),
  hosmer2013 = bibentry(
    bibtype = "Book",
    key = "hosmer2013",
    author = c(
      person(c("David", "W."), "Hosmer, Jr."),
      person("Stanley", "Lemeshow"),
      person(c("Rodney", "X."), "Sturdivant")
    ),
    title = "Applied Logistic Regression",
    edition = "Third",
    publisher = "John Wiley & Sons",
    address = "Hoboken",
    year = "2013",
    doi = "10.1002/9781118548387"
  )
)
