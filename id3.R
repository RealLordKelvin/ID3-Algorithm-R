dataset <- read.csv("file:///C:/Users/pc/python-virtual-environments/lernende_Systeme_Assignments/Assignment_1/mushrooms_new.txt", sep = ",")

## zun�chst target_attribut festlegen, was wollen wir vorhersagen?

head(dataset)

# giftig oder nicht giftig --> "class" e oder p

targetAttribute <- dataset[,ncol(dataset)]
head(targetAttribute)

# wir m�chten nun wissen was ist unser anfang node bzw. parentNode
# Daf�r ben�tigen wir zun�chst die InformationGain f�r jede Variable
# Danach vergleichen wir diese. Die h�here "gewinnt".

## erster Schritt: Entropy
# f�r n-ary classifikation gilt: H(S) = Summe -pi * log2 * pi

ntropy <- function(S) {
  pi <- table(S)/length(S) #  mein pi -> relative h�ufigkeit der Beobachtung
  HS <- sum(-pi*log2(pi)) 
  return(HS)
}

# entropy der jeweiligen feature
sapply(dataset, function(x) ntropy(x))

## zweiter Schritt: Informationsgain
# wir ben�tigen zwei variablen in der function: 

info_gain <- function(S, A){
  originalEntropy <- ntropy(S) # original entropy von S (seite 8)
  rel.FreqI <- table(A)/length(A) # p*
  relativeEntropy <- numeric(length(rel.FreqI))
  for (i in 1:length(rel.FreqI)) {
    Proportion <- table(S[which(A==levels(A)[i])])/length(S[which(A==levels(A)[i])]) # p*
    relativeEntropy[i] <- rel.FreqI[i]*sum(-Proportion*log2(Proportion),na.rm = TRUE) # Information gain formel
  }
  information_gain <- round(originalEntropy-sum(relativeEntropy),4) 
  return(information_gain)
}

# S : vorlesungsunterlagen H(S)

# setting targetattribute to last column

targetAttribute <- dataset[,ncol(dataset)]
head(targetAttribute)

dataset$targetClass <- dataset[,1]
dataset <- dataset[,-1]
head(dataset)

Gain <- sapply(dataset[-ncol(dataset)], info_gain, S = targetAttribute) # WIr z�hlen nicht unser targetattribute dazu
which.max(Gain) 


y1 <- which.max(Gain) ## w�hle odor als mein parentNode "Create Root for the tree"

### ID3 Algorithmus ###



# checken f�r purity

purity <- function(A){
  nlevels(A)==1 # kontrollieren ob eine variable nur ein feature haben
}
purity(dataset[,5]);purity(dataset)

# function for empty attribute

check_empty <- function(E){
  nlevels(E) == 0
}
check_empty(dataset[,2])

# ID3 algorithm

# features attributes

library(dplyr)
attributes_features <- dataset %>% 
  sapply(levels)

class(attributes_features)
attributes_features[-Att_gain]
Att_gain <- sapply(dataset, info_gain, S = dataset[,ncol(dataset)])
str(Att_gain)
length(attributes_features)

node <- as.data.frame(matrix())
node$obsCount <- nrow(dataset[root])
# Divide and conquer strategie anwenden --> recursion --> splite datensatz in kleineren simpleren subsets

myId3 <- function(mynode, mydata){
  # Step 1: Create a root node
  root <- y1
  node$obsCount <- as.data.frame(nrow(dataset[root]))
  if (purity(dataset)){ 
    child <- as.data.frame(unique(dataset[,ncol(dataset)]))
    node$feature <- tail(names(dataset), 1)
    child$obsCount <- nrow(dataset)
    child$feature <- ''              
    #} else if (check_empty(feature_attributes)) { # check if empty
    #return (root)
    } else {
      
      #Att_gain <- sapply(dataset_ohne, info_gain, S = dataset[,ncol(dataset)]) # WIr z�hlen nicht unser targetattribute dazu
      ig <- sapply(colnames(dataset)[-ncol(dataset)], 
                   function(x) info_gain(dataset[,x], dataset[,ncol(dataset)]))
      
      feature <- names(ig)[ig == max(ig)][1]
      
      #feature <- names(which.max(ig))
      node$feature <- feature
      #take the subset of the data-set having that feature value
      feature_subs <- split(dataset[,!(names(dataset) %in% feature)], dataset[,feature], drop = TRUE)
      
      #feature_subset <- split(dataset[ ,names(dataset[,-root]), drop = FALSE], 
      #                  dataset[ ,root], 
      #                  drop = TRUE)
      
      for(i in 1:length(feature_subs)) {
        #construct a node having the name of that feature value
        child <- node$AddChild(names(feature_subs)[i])
        #call the algorithm recursively       
        myId3(child, feature_subs)
  }
 }
}

myId3(mynode = node,mydata = dataset)

dataset_ohne <- dataset[-("targetClass")]
Att_gain <- sapply(dataset_ohne, info_gain, S = dataset[,ncol(dataset)])
Att_gain

ig <- sapply(colnames(dataset)[-ncol(dataset)], 
             function(x) info_gain(dataset[,x], dataset[,ncol(dataset)]))

child <- node$AddChild(names(dataset)[i])
