#web scrape
library(rvest)
#Search ((("2017/07/01"[Date - Publication] : "3000"[Date - Publication]) AND Clinical Trial[ptyp])) AND (((("PloS one"[Journal] OR "PLoS medicine"[Journal]) AND randomized[Title/Abstract]) NOT (systematic review[Title/Abstract] OR meta-analysis[Title/Abstract])) AND Clinical Trial[ptyp]) Filters: Clinical Trial
pmid <- read.csv("/Users/zhang/Documents/2019/AuthorContribution/pmid.csv",
                 header = F)
pmid <- unlist(pmid)
length(pmid)#number of included component trials
PubMed_url <- str_c("https://www.ncbi.nlm.nih.gov/pubmed/",pmid)
#PubMed_url <- PubMed_url[1:3]
#link to plosone
PlosOne_url <- NULL
for (web in PubMed_url) {
  PlosOne_url1 <- web%>%
  read_html()%>%
    html_node(".linkoutlist ul:nth-child(2) li:nth-child(1) a")%>%
    html_attrs()
  PlosOne_url2 <- PlosOne_url1[2]
  PlosOne_url <- c(PlosOne_url,PlosOne_url2)
  cat(".")
}
sum(grepl("http://dx.plos.org/",PlosOne_url))#to make sure all elements were correct.
PlosAuthor_url <- NULL
for (web in PlosOne_url) {
  PlosAuthor_url1 <- web%>%
    read_html()%>%
    html_node(".article-tab-2")%>%
    html_attrs()
  PlosAuthor_url2 <- str_c("https://journals.plos.org",PlosAuthor_url1[1])  
  PlosAuthor_url <- c(PlosAuthor_url,PlosAuthor_url2)
  cat(".")
}
sum(grepl("https://journals.plos.org/",PlosAuthor_url))
#a simple example
AllRole <- NULL;
for (PoneAuthor in PlosAuthor_url) {
  allSourceCode <- PoneAuthor %>%	
    read_html()
  Title <- allSourceCode %>%
    html_nodes("h1")%>%
    html_text()
  TitleText <- Title[2]
  DOI <- allSourceCode %>%
    html_nodes("li")%>%
    html_text(trim = T)
  DOI <- grep("https://doi.org/",DOI,value = T)
  allSourceCode <- allSourceCode %>% 
    html_node("dl") 
  AuthorsText <- allSourceCode %>%
    html_nodes("dt") %>%
    html_text()
  AuthorsText <- gsub("\n",replacement = "",AuthorsText)
  RolesText <- allSourceCode %>%
    html_nodes("dd") %>%
    html_text()
  Varlist <- c("Conceptualization","Data curation","Formal analysis",
               "Funding acquisition","Investigation",
               "Methodology","Project administration","Resources",
               "Software","Supervision","Validation",
               "Visualization","Writing – original draft",
               "Writing – review & editing",
               "E-mail:")
  dtRole <- NULL
  for (var in Varlist) {
    Rolel <- unlist(lapply(RolesText, function(xx){
      grepl(var,xx)
    }))
    dtRole <- cbind(dtRole,Rolel)
  }
  colnames(dtRole) <- Varlist
  dtRole <- as.data.frame(dtRole)
  dtRole <- cbind(AuthorsText,dtRole)
  dtRole <- cbind(DOI,dtRole)
  dtRole <- cbind(TitleText,dtRole)
  dtRole$AuthorOrder <- 1: length(AuthorsText)
  AllRole <- rbind(AllRole,dtRole)
  cat(".")
}
# Analysis
sum(AllRole$`E-mail:`)#number of corresponding authors
length(unique(AllRole$DOI))
dt <- AllRole
#name should be formatted to facilitate further analysis
names(dt) <- sub(' ',"_",names(dt))
names(dt) <- gsub(':|–|-| |&',"",names(dt))
names(dt)
#dataCleazing
library(plyr)
dtGroup <- ddply(dt,.(DOI),function(xx){
  xx$StudyGroup <- sum(grepl("group|team|Group|Team|research|investigators|project|Investigators|Collaboration|behalf|Network",xx$AuthorsText))>0
  return(xx)
})
dim(dtGroup)
dtGroup <- dtGroup[!grepl("group|team|Group|Team|research|investigators|project|Investigators|Collaboration|behalf|Network",
                          dtGroup$AuthorsText),]
dim(dtGroup)
#new variables
dtGroup$FirstAuTag <- dtGroup$AuthorOrder==1
dtGroup$NoRoles <- rowSums(dtGroup[,4:17])
summary(dtGroup$NoRoles)
dtFinal <- dtGroup[dtGroup$NoRoles!=0,]
length(unique(dtFinal$DOI))
dim(dtFinal)
#corresponding author
#univariate analysis
library(DataExplorer)
plot_correlation(sapply(dtFinal[,4:17], #convert logic to numeric type
                        as.numeric))
library(tableone)
vars <- names(dtFinal)[c(4:17,19:22)]
tabCor <- CreateTableOne(vars = vars, strata = 'Email',
                         data = dtFinal)
print(tabCor,quote = T,nonnormal = c("AuthorOrder","NoRoles"))
#impact of number of roles on the corresponding author
library(ggplot2)
ggplot(dtFinal, aes(x=Email, y=NoRoles,fill=Email)) + 
  geom_boxplot()+
  labs(x="Correspondence", y = "Number of Roles")+
  scale_fill_brewer(palette="Dark2")
library(ggpubr)
ggboxplot(dtFinal, x = "Email", y = "NoRoles",
         color = "Email", line.color = "gray", line.size = 0.4,
         xlab ="Correspondence", ylab = "Number of Roles",
         palette = "jco",notch=T)+
  stat_compare_means(aes(label = paste0(..method..,
                                        "\np < 0.001", ..p.signif..)),
                     label.x = 1.4)
#multivariable regression model
mod <- glm( Email ~ Conceptualization + Data_curation +Formal_analysis+
              Funding_acquisition + Investigation + Methodology +
              Project_administration + Resources + Software + 
              Supervision + Validation + Visualization + Writing_originaldraft +
              Writing_reviewediting + FirstAuTag,
           data = dtFinal,family = "binomial")
ShowRegTable(mod,quote = T)
#author order
varsFirst <- names(dtFinal)[c(4:18,20,22)]
tabFirst <- CreateTableOne(vars = varsFirst,strata = "FirstAuTag",
                           data = dtFinal)
print(tabFirst,quote = T,nonnormal = c("AuthorOrder","NoRoles"))
modFirst <- glm( FirstAuTag ~ Conceptualization + Data_curation +Formal_analysis+
                   Funding_acquisition + Investigation + Methodology +
                   Project_administration + Resources + Software + 
                   Supervision + Validation + Visualization + Writing_originaldraft +
                   Writing_reviewediting,
                 data = dtFinal,family = "binomial")
ShowRegTable(modFirst,quote = T)
#author order and number of roles 
ggscatter(dtFinal, x = "NoRoles", y = "AuthorOrder",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          xlab = "Number of Roles",ylab = "Author Order")+
  stat_cor(method = "pearson", label.x = 10, label.y = 30,
           aes(label = paste(..rr.label.., "p < 0.001",sep = "~`,`~")))
mod1 <- lm(AuthorOrder~Conceptualization + Data_curation +Formal_analysis+
             Funding_acquisition + Investigation + Methodology +
             Project_administration + Resources + Software + 
             Supervision + Validation + Visualization + Writing_originaldraft +
             Writing_reviewediting,
           data = dtFinal)
ShowRegTable(mod1,quote = T,exp = F)
