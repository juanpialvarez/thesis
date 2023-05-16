R.Version() #"R version 3.6.3 (2020-02-29)"
RStudio.Version() #‘1.3.1093’

#Clear
rm(list = ls())

#Library
library(tidycensus)
library(network)
library(sna)
library(tidyverse)
library(igraph)
library(ggplot2)
library(tidygraph)
library(readxl)
library(stargazer)
library(rvest)
library(stringr)
library(pdftools)
library(NLP)
library(tm)
library(RSelenium)
#Set workind direcotry, read data into R and name the file FRG

setwd("/Users/juanpabloalvarezvega/Dropbox/Desktop2021/Masters/Thesis/FRC")
frg<-read.csv("Data/splc-hate-groups-2019.csv")

#Check data fro cleaning 

str(frg)

#Extract levels from state

State_levels<-levels(frg$State)

# Convert factors to characters

frg$Title     <- as.character(factor(frg$Title))
frg$State     <- as.character(factor(frg$State))  
frg$Group     <- as.character(factor(frg$Group))
frg$City      <- as.character(factor(frg$City))
frg$Ideology  <- as.character(factor(frg$Ideology))
# Remove black nationalism????

frg<-subset(frg, Ideology!="Black Separatist")

# Check that it changed

class(frg$State)

# Create a data frame with integers for Title and State representing each state. I'm also adding
# the character vectors for state and title so as to see the particular group or state related to that
# integer

edge_list<-data.frame(
  as.integer(factor(frg$State)), # Nest factor() in as.integer() since otherwise it will mess up your factor.
  as.integer(factor(frg$Title)),
  frg$State, #for some reason these get turned to factors when I create the data frame
  frg$Title
  )

#Change edge_list names and check them

names(edge_list)<- c("State_id", "Title_id", "State", "Title")
str(edge_list)

#Change State and Title to character vectors

edge_list$State<-as.character(factor(frg$State))
edge_list$Title<-as.character(factor(frg$Title))


#Created a list composed of data frames which have all the states in which that group operates.
#OJO: repeated observations are not a mistake. Repeated observations happen when a state has
# various instances of that group.

edges<-list()
for (i in edge_list$Title_id){
  edges[[i]]<-subset(edge_list, Title_id==i)
}

length(edges)#Lists of data sets retain the column names of the original data set.

#Generate a vector with a list of each State. 
edges_2<-list()
#Loop will go through the list of edges that I've started.

for (x in 1:length(edges)){

  #Part 1: for each state make a list of other states that have the same organizations. 
  
  #For each state name
  
  for (i in 1:length(edge_list$State)){
    
    #If the state name is in the data frame column state then do the operations below, else skip. 
    
    if(edges[[x]]$State[i] %in% edges[[x]]$State){
      
      #Create a vector named to that contains all the states in the state column
      #but the state being looped.
    
      to<-edges[[x]]$State_id[-i]
      St<-edges[[x]]$State[-i]
      
      #Then create a vector named from that is of the length of to which contains the name 
      #of the state beeing looped. 
      
      from<-rep(edges[[x]]$State_id[i], length(to))
      TT<-rep(edges[[x]]$State[i], length(to))
      
    } else { 
      
      next
      
    }
    
    #create a data frame named con which contains from and to.
    
    con<-data.frame(from, to, St, TT)
    
    #Part two: Make sure there are no repeated observations if a state has other states to connect to.
    #if a state is alone, we still should maintain the node. 
    
    #if the number of columns is higher than 1 then do what's bellow, otherwise skip.
    
    if(nrow(con)>1){
      
      #Replace con with a data frame where the i_th observation of to is not equal to the i_th observation of from. 
      
      con<-subset(con, !to==from)
      
    } else{
      
      next
      
    }

  #Part 3:  Place the results in edges_2 by each data set starting with the first one   
    
    if (x==1){
      
      edges_2[[1]]<-con
      
    } else
      
      edges_2[[length(edges_2)+1]]<-con
    
  }
  
}

# Check edges_2 to see if factors agree with state ids
edges[[1]]
State_levels[44]

# Next step is to put together all the data bases into one. 

edges<-list()

# Going through all the objects in edges.

for (x in 1:length(edges_2)){
  
  #Create three vectors.
  
  l<-c()
  s<-c()
  h<-c()

    # Going through all the objects in edges again.
  
  for (i in 1:length(edges_2)){
    
    # If the state is not in the to column to, then subset the particular state into L   
    
    if(match(edges_2[[x]]$from[1], edges_2[[i]]$to)%in%l){
      
      next
      
    } else {
      
      l[i]<-match(edges_2[[x]]$from[1], edges_2[[i]]$to)
    
    }
    
    # If the specific observation in L is missing then skip otherwise subset the ith observation of 
    # s and h from the from column in the ith and xth data frame of edges_2. 
    
    if(is.na(l[i])){
      
      next
      
    } else {
      
      s[i]<-unique(edges_2[[i]]$from)
      h[i]<-unique(edges_2[[x]]$from)
      
    }
    
  }

  # Create a data frame h and s 
  
  edges[[x]]<-data.frame(h, s)
  
}

# Subset the first observation and then bind the rest of the data frames in pr under that one

for (i in 1:length(edges)){
  
  if(i==1){
    
    pr<-edges[[i]]
    
  }else{
    
    pr<-rbind(pr, edges[[i]])
    
  }
  
}

#Subset those that are not missing observations and give a factor in the integer. 

pr<-subset(pr, !is.na(s))
pr$s<-as.integer(pr$s)
pr$h<-as.integer(pr$h)
pr$s<-factor(pr$s, 1:51)
pr$h<-factor(pr$h, 1:51)
levels(pr$s)<-State_levels
levels(pr$h)<-State_levels

# Change name of h to from and s to to. 

names(pr)<-c("from", "to")

# Group by both from and to and then summarize them based on the number of observations in the group.
# Don't knog how to use group_by and summarize without the pipe operator. 

edge_list <- pr %>%  
  group_by(from, to) %>%
  dplyr::summarise(extremists = n()) %>% 
  ungroup()




#Create graph object.

g <- graph_from_data_frame(edge_list, directed = F)

#Set names attribute to graph. 

V(g)$name <- State_levels

strength(g, vids = V(g), mode ="all", weights = NULL)

#Simplify g so that there are no repeated edges. 

g<-simplify(g)

#Check max degree and table to see if the largest degree has less than 50 edges

max(degree(g))
table(degree(g))

#Check which States have the highest amount of connections

V(g)[degree(g)>37]
degree(g)
# It says it is weighted but don't know how to visualize it. Also have to construct Vertex weights dividing amount of 
# groups by population

is.weighted(g)
# layout_on_sphere

print(E(g)$weight)


plot(g,
     layout=layout_with_graphopt,
     vertex.color="gold",
     vertex.size=10,
     vertex.frame.color="gray", 
     vertex.label.color="black",
     vertex.label.cex=0.8, 
     vertex.label.dist=2,
     edge.curved=0.2)



# Read data

prius<- read_excel("Data/PIRUS_May2020/PIRUS_Public_May2020.xlsx")

# Convert all -99 and -88 to NA

prius[prius == -99 | prius == -88]<- NA

#Clean data

  for (i in 1:145){
    
    #if is factor and the length of the levels is higher than 2 then replace it with characters
    
    if(is.factor(prius[,i]) & length(levels(prius[,i]>2))){
      
      prius[,i]<-as.character(factor(prius[,i]))
      
    }
    
    #if is numeric and the length of the levels is higher than 2 then replace it with factor
    
    if(is.numeric(prius[,i]) & length(unique(!is.na(prius[,i])))<=2){
      
      prius[,i]<-as.factor(prius[,i])
      
    }
    
  }

# Convert state to factor and subset non-far-right individuals

prius<-subset(prius, Radicalization_Far_Right == 1)
prius$N_individuals<-as.factor(factor(prius$Loc_Habitation_State1, State_levels))

# Number of radicalized individuals by state

N_individuals <- prius %>%  
  group_by(N_individuals) %>%
  dplyr::summarise(extremists = n()) %>% 
  ungroup()


#create operator %notin% to find which observations are missing from the list of states 

`%notin%` <- Negate(`%in%`)

#Obtain the observation numbers 

No_obs        <- which(State_levels%notin%unique(prius$N_individuals))

#Character vector for conversion to factor (necessary for loop)

No_obs        <- State_levels[No_obs]

#Convert to factor

No_obs        <- as.factor(factor(No_obs, State_levels))

#Extract weight and number of radicalized individuals (otherwise loop doesn't work)
#weight is number of people. 

extremists        <- N_individuals$extremists
N_individuals <- N_individuals$N_individuals

N_individuals<-N_individuals[-49]
extremists<-extremists[-49]
#Create a loop to add the missing states to the vector as last observations with  value
#of 0 in the weight variable which stands for radicalized individuals

for (i in 1:3){
  N_individuals[length(N_individuals)+1]<- No_obs[i]
  extremists[length(extremists)+1]<- 0
}

#Create a data frame with both variables and order it so that we can retain which state is which

d_frame <- cbind(N_individuals, extremists)
d_frame <- as.data.frame(d_frame)
d_frame <- d_frame[order(d_frame$N_individuals),]

#Add column for vector degree and replace numeric states with the actual names of the states

d_frame$degree<-degree(g)
d_frame$N_individuals<-State_levels
d_frame$betness<-betweenness(g)
d_frame$cls<-closeness(g)
d_frame$e_vector<-eigen_centrality(g)$vector

#Read in population data for 2019

population<-get_acs(geography = "state", variables = "B01003_001",  year = 2019)
population<-subset(population, NAME!="Puerto Rico")

#HOW TO ORDER
#population <- population[order(population$State),]

#Add population into data and divide. 

d_frame$population<-population$estimate

#Vertex_weight <- frg %>%  
#  group_by(State) %>%
#  dplyr::summarise(extremists = n()) %>% 
#  ungroup() 
  
# Vertex_weight$extremists<-extremists/log(d_frame$pop)


#When adjusting for population size the marginal effect is not significant but the 
#intercept is highly significant (maybe try by city to have a wider range of observations)

# https://stats.stackexchange.com/questions/18844/when-and-why-should-you-take-the-log-of-a-distribution-of-numbers
#Veriance 0 means log transform 

#Standardize population per 100k 
d_frame$population<-d_frame$population/100000

#### Regression attempt 01

regression_degree<-lm(extremists ~ degree + log(population) + degree:log(population), d_frame)
summary(regression_degree)

regression_betweeness<-lm(extremists ~ betness + log(population) + betness:log(population), d_frame)
summary(regression_betweeness)

regression_closseness<-lm(extremists ~ cls + log(population) + cls:log(population), d_frame)
summary(regression_closseness)

regression_eigenvector<-lm(extremists ~ e_vector + log(population) + e_vector:log(population), d_frame)
summary(regression_eigenvector)

#Put into stargazer
stargazer(regression_degree,
          regression_betweeness,
          regression_closseness,
          regression_eigenvector, title="Results", align=TRUE, type="latex", out = "Regs_1.txt")
#Run regression with dep var trump voting in a state
#read data
President<- read_csv("Data/President/1976-2020-president.csv")

#REPUBLIICAN
str(President)

#add data for year 2016 (Trump)
President_r<-subset(President, party_simplified=="REPUBLICAN" & year==2016)

#remove repeated observations
repeated<-which(duplicated(President_r$state))
President_r<-President_r[-22,]

#add total votes into new column 

d_frame$NE_Trump<-President_r$totalvotes

#### Regression attempt 02

Trump_degree<-lm(NE_Trump ~ degree + log(population) + degree:population, d_frame)
summary(regression_degree)

Trump_betweeness<-lm(NE_Trump ~ betness + log(population) + betness:population, d_frame)
summary(regression_betweeness)

Trump_closseness<-lm(NE_Trump ~ cls + log(population) + cls:population, d_frame)
summary(regression_closseness)

Trump_eigenvector<-lm(NE_Trump ~ e_vector + log(population) + e_vector:population, d_frame)
summary(regression_eigenvector)

#stargazer
stargazer(Trump_degree,
          Trump_betweeness,
          Trump_closseness,
          Trump_eigenvector, title="Results", align=TRUE, type="latex", out = "Regs_2.txt")

#Read detainer data source in CSV
#https://trac.syr.edu/phptools/immigration/detain/

detainers<-read.csv("Data/Detainer_data.csv")

#Proportion of detainers that were sent to ICE over those that werent
#yes is refused and no is accepted 

detainers$ratio<-detainers$Yes/detainers$No

#add ratio to regression data frame 

d_frame$ratio<-detainers$ratio

#unemployment rate data set bureau of labor statistics

unemp_rate<-read_xlsx("Data/u_rate.xlsx")

#add unemployment rate to rergression data frame 

d_frame$u_rate<-unemp_rate$u_rate

#### Regression attempt 3

regression_degree<-lm(ratio ~ degree + log(population) +  NE_Trump  + degree:log(population), d_frame)
summary(regression_degree)

regression_betweeness<-lm(ratio ~ betness +  NE_Trump + log(population) + betness:log(population), d_frame)
summary(regression_betweeness)

regression_closseness<-lm(ratio ~ cls +  NE_Trump  + log(population) + cls:log(population), d_frame)
summary(regression_closseness)

regression_eigenvector<-lm(ratio ~ e_vector + NE_Trump + log(population) + e_vector:log(population), d_frame)
summary(regression_eigenvector)

stargazer(regression_degree,
          regression_betweeness,
          regression_closseness,
          regression_eigenvector, title="Results", align=TRUE, type="latex", out = "Regs_3.txt")


regression_degree<-lm(ratio ~ degree + population +  NE_Trump + u_rate + degree:population, d_frame)
summary(regression_degree)

regression_betweeness<-lm(ratio ~ betness +  NE_Trump + u_rate + population + betness:population, d_frame)
summary(regression_betweeness)

regression_closseness<-lm(ratio ~ cls +  NE_Trump + u_rate + population + cls:population, d_frame)
summary(regression_closseness)

regression_eigenvector<-lm(ratio ~ e_vector + NE_Trump + u_rate + population + e_vector:population, d_frame)
summary(regression_eigenvector)

stargazer(regression_degree,
          regression_betweeness,
          regression_closseness,
          regression_eigenvector, title="Results", align=TRUE, type="latex", out = "Regs_3.txt")

#State abbreviations 

st_abb<-read_xlsx("Data/St_abb.xlsx")

#enter us census API key and install it 
#census_api_key("f4f4ee920c5287f7ffbeb5e84922e287c1acab05", install = T)

#create data frame with all variables from the cesus I will use

v17 <- load_variables(2019, "acs5", cache = TRUE)

#Take migration variables 

two_par_ch_fb_six   <- get_acs(geography = "state", variables = "B05009_005",  year = 2019)
two_par_fb_six      <- get_acs(geography = "state", variables = "B05009_007",  year = 2019)
one_par_ch_fb_six   <- get_acs(geography = "state", variables = "B05009_015",  year = 2019)
one_par_fb_six      <- get_acs(geography = "state", variables = "B05009_017",  year = 2019)
two_par_ch_fb_steen <- get_acs(geography = "state", variables = "B05009_023",  year = 2019)
two_par_fb_steen    <- get_acs(geography = "state", variables = "B05009_025",  year = 2019)
one_par_ch_fb_steen <- get_acs(geography = "state", variables = "B05009_033",  year = 2019)
one_par_fb_steen    <- get_acs(geography = "state", variables = "B05009_035",  year = 2019)

#Make a list of data sets to automate the process of subseting variables

mig<- list(two_par_ch_fb_six=two_par_ch_fb_six, two_par_fb_six=two_par_fb_six, one_par_ch_fb_six=one_par_ch_fb_six, one_par_fb_six=one_par_fb_six,
           two_par_ch_fb_steen=two_par_ch_fb_steen, two_par_fb_steen=two_par_fb_steen, one_par_ch_fb_steen=one_par_ch_fb_steen, one_par_fb_steen=one_par_fb_steen)

#Subset Puerto Rico and assign the vectors to regression data set

for (i in 1:length(mig)){
  mig[[i]]<-subset(mig[[i]], NAME!="Puerto Rico")
  d_frame[,length(d_frame)+1]<-mig[[i]][[4]]
  
  #use the names I gave them as variable names by adding it to the  end of the list of names  and then changing the 
  #attribute in d_frame
  
  name<-names(d_frame)
  name[length(name)]<-names(mig[i])
  names(d_frame)<-name
}

#### Regression attempt 04

regression_degree<-lm(ratio ~ degree + population +  NE_Trump + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                        two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + u_rate +
                        degree:two_par_fb_six + degree:two_par_fb_steen + degree:one_par_fb_steen,
                      d_frame)
summary(regression_degree)

regression_betweeness<-lm(ratio ~ betness +  NE_Trump + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + u_rate +
                            betness:two_par_fb_six + betness:two_par_fb_steen + betness:one_par_fb_steen,
                          d_frame)
summary(regression_betweeness)

regression_closseness<-lm(ratio ~ cls +  NE_Trump + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + u_rate +
                            cls:two_par_fb_six + cls:two_par_fb_steen + cls:one_par_fb_steen,
                          d_frame)
summary(regression_closseness)

regression_eigenvector<-lm(ratio ~ e_vector + NE_Trump + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                             two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + u_rate +
                             e_vector:two_par_fb_six + e_vector:two_par_fb_steen + e_vector:one_par_fb_steen,
                           d_frame)
summary(regression_eigenvector)

stargazer(regression_degree,
          regression_betweeness,
          regression_closseness,
          regression_eigenvector, title="Results", align=TRUE, type="latex", out = "Regs_4.txt")

#### Regression attempt 05

regression_degree<-lm(extremists ~ ratio + degree + population  + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                        two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + u_rate + degree:one_par_ch_fb_steen + degree:ratio,
                      d_frame)
summary(regression_degree)

regression_betweeness<-lm(extremists ~ ratio + betness + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + u_rate + population + betness:one_par_ch_fb_steen + betness:ratio,
                          d_frame)
summary(regression_betweeness)

regression_closseness<-lm(extremists ~ ratio + cls + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + u_rate + population + cls:one_par_ch_fb_steen + cls:ratio,
                          d_frame)
summary(regression_closseness)

regression_eigenvector<-lm(extremists ~ ratio + e_vector + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                             two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + u_rate + population + e_vector:one_par_ch_fb_steen + e_vector:ratio,
                           d_frame)
summary(regression_eigenvector)

stargazer(regression_degree,
          regression_betweeness,
          regression_closseness,
          regression_eigenvector, title="Results", align=TRUE, type="latex", out = "Regs_5.txt")

#create a pattern that will be used to subset each indicator 
male          <- seq(3, 66, by = 7)
male_ue       <- seq(8, 71, by = 7)
male_nlf      <- seq(9, 72, by = 7)
female        <- seq(89, 152, by = 7)
female_ue     <- seq(94, 157, by = 7)
female_nlf    <- seq(95, 158, by = 7)
youth_m       <- seq(3, 17, by = 7)
youth_m_ue    <- seq(8, 22, by = 7)
youth_m_nlf   <- seq(9, 23, by = 7)
youth_f       <- seq(89, 103, by = 7)
youth_f_ue    <- seq(94, 108, by = 7)
youth_f_nlf   <- seq(95, 109, by = 7)
elderly_m     <- seq(73, 83, by = 5)
elderly_m_ue  <- seq(76, 86, by = 5)
elderly_m_nlf <- seq(77, 87, by = 5)
elderly_f     <- seq(159, 169, by = 5)
elderly_f_ue  <- seq(162, 172, by = 5)
elderly_f_nlf <- seq(163, 173, by = 5)

#Create a list of all the sequences

UE_vars<-list(male = male, male_ue = male_ue, male_nlf= male_nlf,
              female = female, female_nlf = female_nlf, female_ue = female_ue,
              youth_m = youth_m, youth_m_nlf = youth_m_nlf, youth_m_ue = youth_m_ue,
              youth_f = youth_f, youth_f_nlf = youth_f_nlf, youth_f_ue = youth_f_ue,
              elderly_f = elderly_f, elderly_f_nlf = elderly_f_nlf, elderly_f_ue = elderly_f_ue,
              elderly_m = elderly_m, elderly_m_nlf = elderly_m_nlf, elderly_m_ue = elderly_m_ue)

#create a function to obtain name of vectors
obt_name<-function(x){
  deparse(substitute(x))
  }

#create a function that gets the specific unemployment variable from tidy census 
UE_dfs<-function(x){
  assign (paste(obt_name(x), "list", sep = "_"), list())
  for (i in 1:length(x)){
    a<-nchar(x)
    if(a[i]<2){
      assign( paste(obt_name(x), i, sep="_"), get_acs(geography = "state", variables = paste("B23001_00",x[i], sep =""),  year = 2019))
    } else{
      if(a[i]==2){
        assign( paste(obt_name(x), i, sep="_"), get_acs(geography = "state", variables = paste("B23001_0",x[i], sep =""),  year = 2019))
      } else {
        assign( paste(obt_name(x), i, sep="_"), get_acs(geography = "state", variables = paste("B23001_",x[i], sep =""),  year = 2019))
      }
    }
    assign (paste(obt_name(x), i, sep="_") ,subset(get(paste(obt_name(x), i, sep="_")), NAME!="Puerto Rico"))
    text_vars<-paste(paste(obt_name(x), "list", sep = "_"), paste("[[", i ,"]]", sep = ""), "<-", paste(obt_name(x), i, sep="_"), sep = "") 
    eval(parse(text=text_vars))
  }
  return(get(paste(obt_name(x), "list", sep = "_")))
}

#apply the function accross  the list (couldn't use lapply)
for (i in 1:length(UE_vars)){
  assign(paste(names(UE_vars[i]), "list", sep = "_"), list())
  assign(paste(names(UE_vars[i]), i, sep = "_"), UE_dfs(UE_vars[[i]]))
  text_vars<-paste(paste(names(UE_vars[i]), "list", sep = "_"), paste("[[", i ,"]]", sep = ""), "<-", paste(names(UE_vars[i]), i, sep="_"), sep = "") 
  eval(parse(text=text_vars))
}

# Enter the lists into a new list called UE_vars_list (could have just done this in the prrevious loop >:|)
UE_vars_list<-list(male_list=male_list, male_ue_list=male_ue_list, male_nlf_list=male_nlf_list,
              female_list=female_list, female_nlf_list=female_nlf_list, female_ue_list=female_ue_list,
              youth_m_list=youth_m_list, youth_m_nlf_list=youth_m_nlf_list, youth_m_ue_list=youth_m_ue_list,
              youth_f_list=youth_f_list, youth_f_nlf_list=youth_f_nlf_list, youth_f_ue_list=youth_f_ue_list,
              elderly_f_list=elderly_f_list, elderly_f_nlf_list=elderly_f_nlf_list, elderly_f_ue_list=elderly_f_ue_list,
              elderly_m_list=elderly_m_list, elderly_m_nlf_list=elderly_m_nlf_list, elderly_m_ue_list=elderly_m_ue_list)

#Subset the values and name of the variable and place it into d_frame
df_sum<-c()
for (x in 1:length(UE_vars_list)) {
  for (i in 1:length(UE_vars_list[[x]][[x]])){
    if (i==1){
      df_sum<-UE_vars_list[[x]][[x]][[i]][[4]]
    }
    else {
      df_sum<-df_sum + UE_vars_list[[x]][[x]][[i]][[4]]
    }
  }
  d_frame[,length(d_frame)+1]<-df_sum
  name<-names(d_frame)
  name[length(name)]<-names(UE_vars[x])
  names(d_frame)<-name
}

#create new variables from the  variables  obtained and normalize the populationulation by dividing by 100000

d_frame$unemployed<-(d_frame$male_ue+d_frame$female_ue+d_frame$elderly_f+d_frame$elderly_m)/100000
d_frame$not_labourforce<-(d_frame$female_nlf+d_frame$male_nlf+d_frame$elderly_f_nlf+d_frame$elderly_m_nlf)/100000
d_frame$youth_unemp<-(d_frame$youth_m_ue+d_frame$youth_f_ue)/100000
d_frame$youth_notlabf<-(d_frame$youth_m_nlf+d_frame$youth_f_nlf)/100000
d_frame$elderly<-(d_frame$elderly_f+d_frame$elderly_m)/100000
d_frame$youth<-(d_frame$youth_f+d_frame$youth_m)/100000
d_frame$adults<-(d_frame$male+d_frame$female-d_frame$youth)/100000
d_frame$y_dependecy<-(d_frame$adults/d_frame$youth)/100000
d_frame$e_dependecy<-(d_frame$adults/d_frame$elderly)/100000
d_frame$e_unemployment<-(d_frame$elderly_m_ue+d_frame$elderly_f_ue)/100000
d_frame$a_unemployment<-(d_frame$unemployed-d_frame$youth_unemp-d_frame$e_unemployment)/100000
d_frame$elderly_nlf<-(d_frame$elderly_f_nlf+d_frame$elderly_m_nlf)/100000
d_frame$adult_nlf<-(d_frame$not_labourforce-d_frame$elderly_nlf-d_frame$youth_notlabf)/100000

#### Regression attempt 06

regression_degree<-lm(extremists ~ ratio + degree + population  + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                        two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + u_rate +
                        y_dependecy + e_dependecy + not_labourforce + youth_unemp + adult_nlf + youth_notlabf +
                        degree:one_par_ch_fb_steen + degree:ratio + degree:youth_unemp + degree:e_dependecy + degree:y_dependecy + degree:y_dependecy,
                      d_frame)
summary(regression_degree)

regression_betweeness<-lm(extremists ~ ratio + betness + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +
                            u_rate + population + y_dependecy + e_dependecy + not_labourforce + youth_unemp + adult_nlf + youth_notlabf +
                            betness:one_par_ch_fb_steen + betness:ratio + betness:youth_unemp + betness:e_dependecy + betness:y_dependecy,
                          d_frame)
summary(regression_betweeness)

regression_closseness<-lm(extremists ~ ratio + cls + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +
                            u_rate + population + y_dependecy + e_dependecy + not_labourforce + youth_unemp + adult_nlf + youth_notlabf +
                            cls:one_par_ch_fb_steen + cls:ratio + cls:youth_unemp + cls:e_dependecy + cls:y_dependecy,
                          d_frame)
summary(regression_closseness)

regression_eigenvector<-lm(extremists ~ ratio + e_vector + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                             two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +
                             u_rate + population + y_dependecy + e_dependecy + not_labourforce + youth_unemp + adult_nlf + youth_notlabf +
                             e_vector:one_par_ch_fb_steen + e_vector:ratio + e_vector:youth_unemp + e_vector:e_dependecy + e_vector:y_dependecy,
                           d_frame)
summary(regression_eigenvector)

stargazer(regression_degree,
          regression_betweeness,
          regression_closseness,
          regression_eigenvector, title="Results", align=TRUE, type="latex", out = "Regs_6.txt")

#poverty variable 

pov<-get_acs(geography = "state", variables = "B17001_002",  year = 2019)
pov<-subset(pov, NAME!="Puerto Rico")

#var is listed for a reason gotta unlist

pov<-unlist(pov[4])
d_frame$poverty<-pov/100000

#Load GINI data from Statista
gini<-read_excel("Data/GINI.xlsx", sheet = 2)
gini<-subset(gini, State!="United States")

#Data is listed again and it also is given as a character
gini<-as.numeric(unlist(gini[2]))
d_frame$inequality<-unlist(gini)

#### Regression attempt 07

regression_degree<-lm(extremists ~ ratio + degree + population  + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                        two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + u_rate +
                        y_dependecy + e_dependecy + not_labourforce + poverty + inequality + youth_unemp + adult_nlf + youth_notlabf +
                        degree:one_par_ch_fb_steen + degree:ratio + degree:youth_unemp + degree:e_dependecy + degree:y_dependecy + degree:y_dependecy,
                      d_frame)
summary(regression_degree)

regression_betweeness<-lm(extremists ~ ratio + betness + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +
                            u_rate + population + y_dependecy + e_dependecy + poverty + inequality + not_labourforce + youth_unemp + adult_nlf + youth_notlabf +
                            betness:one_par_ch_fb_steen + betness:ratio + betness:youth_unemp + betness:e_dependecy + betness:y_dependecy,
                          d_frame)
summary(regression_betweeness)

regression_closseness<-lm(extremists ~ ratio + cls + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +
                            u_rate + population + y_dependecy + e_dependecy + poverty + inequality + not_labourforce + youth_unemp + adult_nlf + youth_notlabf +
                            cls:one_par_ch_fb_steen + cls:ratio + cls:youth_unemp + cls:e_dependecy + cls:y_dependecy,
                          d_frame)
summary(regression_closseness)

regression_eigenvector<-lm(extremists ~ ratio + e_vector + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                             two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +
                             u_rate + population + y_dependecy + e_dependecy + poverty + inequality + not_labourforce + youth_unemp + adult_nlf + youth_notlabf +
                             e_vector:one_par_ch_fb_steen + e_vector:ratio + e_vector:youth_unemp + e_vector:e_dependecy + e_vector:y_dependecy,
                           d_frame)
summary(regression_eigenvector)

stargazer(regression_degree,
          regression_betweeness,
          regression_closseness,
          regression_eigenvector, title="Results", align=TRUE, type="latex", out = "Regs_7.txt")

#Education variables. Don't know exactly what  I did here, come back later and figure it  out. 
Edu_vars<-list()
nam<-c()
for (i in 1:6){
  #Assign to variables named Education_i with the sepecific census variable.
  assign( paste("Education", i, sep = "_"), get_acs(geography = "state", variables = paste("B06009_00", i, sep = ""),  year = 2019))
  #Subset Puerto Rico observation. 
  var_text<-paste(paste("Education", i, sep = "_"), "<-", "subset", "(" , paste("Education", i, sep = "_") , "," ,"NAME!='Puerto Rico')", sep = "")
  eval(parse(text=var_text))
  #Enter de specific data frames into a list.
  Edu_vars[[i]]<-get(paste("Education", i, sep = "_"))
  #Make a vector of names
  nam[i]<-paste("Education", i, sep = "_")
}

#Enter the vector of names as the name attributes of the list.
names(Edu_vars)<-nam

#Enter the specific value vectors into the data frame.
for (i in 1:length(Edu_vars)){
  #Enter the vector into the regression data frame dividing by 100,000 to normalize the population 
  d_frame[length(d_frame)+1]<-Edu_vars[[i]][[4]]/100000
  #Make a vector with names of the regression data frame.
  name_vars<-names(d_frame)
  #Substitute the last name of the vector with the  ith observation of the name vector made in the past for loop.
  name_vars[length(name_vars)]<-nam[i]
  #Enter the new vector as the names attribute of the data frame. 
  names(d_frame)<-name_vars
}

#### Regression attempt 09

regression_degree<-lm(extremists ~ ratio + degree + population + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                        two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + u_rate +
                        y_dependecy + e_dependecy + not_labourforce + poverty + inequality + youth_unemp + adult_nlf + youth_notlabf +
                        degree:one_par_ch_fb_steen + degree:ratio + degree:youth_unemp + degree:e_dependecy + degree:y_dependecy + degree:y_dependecy,
                      d_frame)
summary(regression_degree)

regression_betweeness<-lm(extremists ~ ratio + betness + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +
                            u_rate + population + y_dependecy + e_dependecy + poverty + inequality + not_labourforce + youth_unemp + adult_nlf + youth_notlabf +
                            betness:one_par_ch_fb_steen + betness:ratio + betness:youth_unemp + betness:e_dependecy + betness:y_dependecy,
                          d_frame)
summary(regression_betweeness)

regression_closseness<-lm(extremists ~ ratio + cls + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +
                            u_rate + population + y_dependecy + e_dependecy + poverty + inequality + not_labourforce + youth_unemp + adult_nlf + youth_notlabf +
                            cls:one_par_ch_fb_steen + cls:ratio + cls:youth_unemp + cls:e_dependecy + cls:y_dependecy,
                          d_frame)
summary(regression_closseness)

regression_eigenvector<-lm(extremists ~ ratio + e_vector + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                             two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +
                             u_rate + population + y_dependecy + e_dependecy + poverty + inequality + not_labourforce + youth_unemp + adult_nlf + youth_notlabf +
                             e_vector:one_par_ch_fb_steen + e_vector:ratio + e_vector:youth_unemp + e_vector:e_dependecy + e_vector:y_dependecy,
                           d_frame)
summary(regression_eigenvector)

stargazer(regression_degree,
          regression_betweeness,
          regression_closseness,
          regression_eigenvector, title="Results", align=TRUE, type="latex", out = "Regs_9.txt")



#### Regression attempt 10
str(d_frame)
regression_degree<-lm(extremists ~ ratio + degree + population + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                        two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + u_rate +
                        y_dependecy + e_dependecy + not_labourforce + poverty + inequality + youth_unemp + adult_nlf + youth_notlabf +
                        degree:ratio + Education_1 + Education_2 + Education_3 + Education_4 + Education_5 + Education_6 + NE_Trump, d_frame)
summary(regression_degree)

regression_betweeness<-lm(extremists ~ ratio + betness + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +
                            u_rate + population + y_dependecy + e_dependecy + poverty + inequality + not_labourforce + youth_unemp + adult_nlf + youth_notlabf +
                            betness:ratio + Education_1 + Education_2 + Education_3 + Education_4 + Education_5 + Education_6 + NE_Trump, d_frame)
summary(regression_betweeness)

regression_closseness<-lm(extremists ~ ratio + cls + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +
                            u_rate + population + y_dependecy + e_dependecy + poverty + inequality + not_labourforce + youth_unemp + adult_nlf + youth_notlabf +
                            cls:ratio + Education_1 + Education_2 + Education_3 + Education_4 + Education_5 + Education_6 + NE_Trump, d_frame)
summary(regression_closseness)

regression_eigenvector<-lm(extremists ~ ratio + e_vector + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                             two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +
                             u_rate + population + y_dependecy + e_dependecy + poverty + inequality + not_labourforce + youth_unemp + adult_nlf + youth_notlabf +
                             e_vector:ratio + Education_1 + Education_2 + Education_3 + Education_4 + Education_5 + Education_6 + NE_Trump, d_frame)
summary(regression_eigenvector)

stargazer(regression_degree,
          regression_betweeness,
          regression_closseness,
          regression_eigenvector, title="Results", align=TRUE, type="latex", out = "Regs_10.txt")

#read drug use but number of rows before variable names vary in sheet positions 2, 6, and 7
#must subset rows 1 to 5 because these are not variables
#Load data from National Survey on Drug Use and Health 
#There are 35 sheets, data starts on thte second sheet and in sheet positions 6, 7, 13, 15, 17, 18, 23, 25, 26, 27, 28, 29, 30, 32, 33, and 34,
#data starts on the 6th row. In sheet possitions 21, 22, 24, and 31 data starts on the 7th row.
#The rest start on the 5th row
for (i in 2:34){
  #read each data set with vector names drug_use_#
  if (i==2 | i==6 | i==7 | i==13 | i==15 | i==17 | i==18 | i==23 | i==25 | i==26 |i==27 | i==28 | i==29 | i==30 | i==32 | i==33 | i==34){
    assign( paste("drug", "use", i, sep = "_"), read_excel("Data/2019NSDUHsaeTotals.xlsx", skip = 6 , sheet = i))
    }else{
      if (i==21 | i==22 | i==24 | i==31){
        assign( paste("drug", "use", i, sep = "_"), read_excel("Data/2019NSDUHsaeTotals.xlsx", skip = 7 , sheet = i)) 
        }else{
          assign( paste("drug", "use", i, sep = "_"), read_excel("Data/2019NSDUHsaeTotals.xlsx", skip = 5 , sheet = i))
        }
      }
  for (x in 1:5){
    #Subset observations that are not actually states (first five observations)
    text_vars<-paste(paste("drug", "use", i, sep = "_"), "<-","subset(", paste("drug", "use", i, sep = "_"),",Order!=", x, ")", sep = "")
    eval(parse(text = text_vars))
  }
  }

# Create a vector of names for each data set 
drug_names <-c("duse_past_m","marijuana_ly", "marijuana_pm", "perception_mj", "first_use_mj", "use_notmj_pm", "cocaine_py", "perception_cocaine",
      "heroin_py", "perception_heroin", "metamphetamine_py", "pain_reliver_mu_py", "alcohol_pm", "binge_alcohol_pm", "perception_alcohol",
      "alcohol_bing_alcohol_pm", "tobacco_pm", "cigarette_pm", "perception_smoking", "illicit_drug_disorder", "pain_reliever_disorder",
      "alcohol_disorder", "substance_disorder", "n_not_r_illicit_drugs", "n_not_r_alcohol", "n_not_r_substance", "mental_illness",
      "mental_health_service", "m_depresive_episode", "thoughts_suicide", "plans_suicide", "attempt_suicide")

#Sum all values in rows which give the total value for each age group and make 
#it into a vector of values to ba added into regression data frame.
for(i in 1:length(drug_names)){
  #in data set 16 these values are in columns 3 and 6
  if(i==16){
    drug_text<-paste(drug_names[i], "<-", paste("drug", "use", i+1, sep = "_"), sep = "")
    eval(parse(text=drug_text))
    drop_vars<-paste("rm(",paste("drug", "use", i+1 , sep = "_"), ")")
    eval(parse(text = drop_vars))
    drug_text<-paste(drug_names[i], "<-", "rowSums(", drug_names[i], "[seq(3, 6, by = 3)])", sep = "")
    eval(parse(text = drug_text))
  }else
    if(i==27 | i==28 | i==29 | i==31 | i==32 | i==33){
      #in data set 16 these values are in columns 3 and 9
      drug_text<-paste(drug_names[i], "<-", paste("drug", "use", i+1, sep = "_"), sep = "")
      eval(parse(text=drug_text))
      drop_vars<-paste("rm(",paste("drug", "use", i+1 , sep = "_"), ")")
      eval(parse(text = drop_vars))
      drug_text<-paste(drug_names[i], "<-", "rowSums(", drug_names[i], "[seq(3, 9, by = 3)])", sep = "")
      eval(parse(text = drug_text))
    }else{
      #in data set 16 these values are in columns 3 and 12
      drug_text<-paste(drug_names[i], "<-", paste("drug", "use", i+1, sep = "_"), sep = "")
      eval(parse(text=drug_text))
      drop_vars<-paste("rm(",paste("drug", "use", i+1 , sep = "_"), ")")
      eval(parse(text = drop_vars))
      drug_text<-paste(drug_names[i], "<-", "rowSums(", drug_names[i], "[seq(3, 12, by = 3)])", sep = "")
      eval(parse(text = drug_text))
    }
}

#Add the vectors to the regression data frame and then get the names of the data frame. Substitute the last name 
#(by standard V1) by the ith name in  the list of names of drug use vector, then substitute
#the original names of the data frame with the new names. 

for (i in 1:length(drug_names)){
  d_frame[length(d_frame)+1]<-get(drug_names[i])
  name_drugs<-names(d_frame)
  name_drugs[length(name_drugs)]<-drug_names[i]
  names(d_frame)<-name_drugs
}

write.csv(d_frame, "Data/regression_data.csv")

#### Regression attempt 11

regression_degree<-lm(extremists ~ ratio + degree + population + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                        two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + u_rate +
                        duse_past_m + marijuana_ly + cocaine_py + heroin_py + alcohol_pm + binge_alcohol_pm + tobacco_pm +
                        mental_illness + alcohol_disorder + illicit_drug_disorder + pain_reliever_disorder + substance_disorder +
                        m_depresive_episode + thoughts_suicide + plans_suicide + attempt_suicide + cigarette_pm + not_labourforce +
                        poverty + inequality + youth_unemp + degree:ratio + degree:population + degree:poverty + NE_Trump + 
                        degree:attempt_suicide + degree:one_par_ch_fb_six + degree:tobacco_pm + degree:plans_suicide + degree:inequality +
                        degree:inequality, d_frame)
summary(regression_degree)

regression_betweeness<-lm(extremists ~ ratio + betness + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +
                            duse_past_m + marijuana_ly + cocaine_py + heroin_py + alcohol_pm + binge_alcohol_pm + tobacco_pm +
                            mental_illness + alcohol_disorder + illicit_drug_disorder + pain_reliever_disorder + substance_disorder +
                            m_depresive_episode + thoughts_suicide + plans_suicide + attempt_suicide + cigarette_pm + NE_Trump +
                            u_rate + population + poverty + inequality + not_labourforce + youth_unemp + betness:ratio + betness:population + betness:poverty +
                            betness:attempt_suicide + betness:one_par_ch_fb_six + betness:tobacco_pm + betness:plans_suicide +
                            betness:inequality, d_frame)
summary(regression_betweeness)

regression_closseness<-lm(extremists ~ ratio + cls + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +
                            duse_past_m + marijuana_ly + cocaine_py + heroin_py + alcohol_pm + binge_alcohol_pm + tobacco_pm +
                            mental_illness + alcohol_disorder + illicit_drug_disorder + pain_reliever_disorder + substance_disorder +
                            m_depresive_episode + thoughts_suicide + plans_suicide + attempt_suicide + cigarette_pm + NE_Trump +
                            u_rate + population + poverty + inequality + youth_unemp + cls:ratio  + cls:population + cls:poverty +
                            cls:attempt_suicide + cls:one_par_ch_fb_six + cls:tobacco_pm + cls:plans_suicide +
                            cls:inequality, d_frame)
summary(regression_closseness)

regression_eigenvector<-lm(extremists ~ ratio + e_vector + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                             two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +
                             duse_past_m + marijuana_ly + cocaine_py + heroin_py + alcohol_pm + binge_alcohol_pm + tobacco_pm +
                             mental_illness + alcohol_disorder + illicit_drug_disorder + pain_reliever_disorder + substance_disorder +
                             m_depresive_episode + thoughts_suicide + plans_suicide + attempt_suicide + cigarette_pm + NE_Trump +
                             u_rate + population + poverty + inequality + youth_unemp + e_vector:ratio + e_vector:population + e_vector:poverty +
                             e_vector:attempt_suicide + e_vector:one_par_ch_fb_six + e_vector:tobacco_pm + e_vector:plans_suicide +
                             e_vector:inequality, d_frame)
summary(regression_eigenvector)


  
stargazer(regression_degree,
          regression_betweeness,
          regression_closseness,
          regression_eigenvector, title="Results", align=TRUE, type="latex", out = "Regs_11.txt")


#### Regression attempt 12


regression_degree<-lm(extremists ~ ratio + degree + population + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                        two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + u_rate + 
                        duse_past_m + marijuana_ly + cocaine_py + heroin_py + alcohol_pm + binge_alcohol_pm + tobacco_pm +
                        mental_illness + alcohol_disorder + illicit_drug_disorder + pain_reliever_disorder + substance_disorder +
                        m_depresive_episode + thoughts_suicide + plans_suicide + attempt_suicide + cigarette_pm + NE_Trump +
                        not_labourforce + poverty + inequality + youth_unemp + youth_notlabf + degree:inequality +
                        degree:one_par_ch_fb_steen + degree:ratio + degree:u_rate + degree:poverty + degree:two_par_fb_six, d_frame)
summary(regression_degree)

regression_betweeness<-lm(extremists ~ ratio + betness + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + 
                            duse_past_m + marijuana_ly + cocaine_py + heroin_py + alcohol_pm + binge_alcohol_pm + tobacco_pm +
                            mental_illness + alcohol_disorder + illicit_drug_disorder + pain_reliever_disorder + substance_disorder +
                            m_depresive_episode + thoughts_suicide + plans_suicide + attempt_suicide + cigarette_pm + NE_Trump + betness:inequality +
                            u_rate + population + poverty + inequality + not_labourforce + youth_unemp + youth_notlabf +
                            betness:one_par_ch_fb_steen + betness:ratio + betness:u_rate + betness:poverty + betness:two_par_fb_six, d_frame)
summary(regression_betweeness)

regression_closseness<-lm(extremists ~ ratio + cls + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + 
                            duse_past_m + marijuana_ly + cocaine_py + heroin_py + alcohol_pm + binge_alcohol_pm + tobacco_pm +
                            mental_illness + alcohol_disorder + illicit_drug_disorder + pain_reliever_disorder + substance_disorder +
                            m_depresive_episode + thoughts_suicide + plans_suicide + attempt_suicide + cigarette_pm + NE_Trump + cls:inequality +
                            u_rate + population + poverty + inequality + not_labourforce + youth_unemp + youth_notlabf +
                            cls:one_par_ch_fb_steen + cls:ratio + cls:u_rate + cls:poverty + cls:two_par_fb_six, d_frame)
summary(regression_closseness)

regression_eigenvector<-lm(extremists ~ ratio + e_vector + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                             two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +  
                             duse_past_m + marijuana_ly + cocaine_py + heroin_py + alcohol_pm + binge_alcohol_pm + tobacco_pm +
                             mental_illness + alcohol_disorder + illicit_drug_disorder + pain_reliever_disorder + substance_disorder +
                             m_depresive_episode + thoughts_suicide + plans_suicide + attempt_suicide + cigarette_pm + NE_Trump + e_vector:inequality +
                             u_rate + population + poverty + inequality + not_labourforce + youth_unemp + youth_notlabf +
                             e_vector:one_par_ch_fb_steen + e_vector:ratio + e_vector:u_rate + e_vector:poverty + e_vector:two_par_fb_six, d_frame)
summary(regression_eigenvector)

stargazer(regression_degree,
          regression_betweeness,
          regression_closseness,
          regression_eigenvector, title="Results", align=TRUE, type="latex", out = "Regs_12.txt")

#### Regression attempt 13


regression_degree<-lm(ratio ~ degree + population + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                        two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + u_rate + 
                        duse_past_m + marijuana_ly + cocaine_py + heroin_py + alcohol_pm + binge_alcohol_pm + tobacco_pm +
                        mental_illness + alcohol_disorder + illicit_drug_disorder + pain_reliever_disorder + substance_disorder +
                        m_depresive_episode + thoughts_suicide + plans_suicide + attempt_suicide + cigarette_pm + NE_Trump +
                        not_labourforce + poverty + inequality + youth_unemp + youth_notlabf + degree:inequality +
                        degree:one_par_ch_fb_steen + degree:u_rate + degree:poverty + degree:two_par_fb_six, d_frame)
summary(regression_degree)

regression_betweeness<-lm(ratio ~ betness + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + 
                            duse_past_m + marijuana_ly + cocaine_py + heroin_py + alcohol_pm + binge_alcohol_pm + tobacco_pm +
                            mental_illness + alcohol_disorder + illicit_drug_disorder + pain_reliever_disorder + substance_disorder +
                            m_depresive_episode + thoughts_suicide + plans_suicide + attempt_suicide + cigarette_pm + NE_Trump + betness:inequality +
                            u_rate + population + poverty + inequality + not_labourforce + youth_unemp + youth_notlabf +
                            betness:one_par_ch_fb_steen  + betness:u_rate + betness:poverty + betness:two_par_fb_six, d_frame)
summary(regression_betweeness)

regression_closseness<-lm(ratio ~ cls + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                            two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen + 
                            duse_past_m + marijuana_ly + cocaine_py + heroin_py + alcohol_pm + binge_alcohol_pm + tobacco_pm +
                            mental_illness + alcohol_disorder + illicit_drug_disorder + pain_reliever_disorder + substance_disorder +
                            m_depresive_episode + thoughts_suicide + plans_suicide + attempt_suicide + cigarette_pm + NE_Trump + cls:inequality +
                            u_rate + population + poverty + inequality + not_labourforce + youth_unemp + youth_notlabf +
                            cls:one_par_ch_fb_steen + cls:u_rate + cls:poverty + cls:two_par_fb_six, d_frame)
summary(regression_closseness)

regression_eigenvector<-lm(ratio ~ e_vector + Education_1 + two_par_ch_fb_six + two_par_fb_six + one_par_ch_fb_six + one_par_fb_six +
                             two_par_ch_fb_steen + two_par_fb_steen + one_par_ch_fb_steen + one_par_fb_steen +  
                             duse_past_m + marijuana_ly + cocaine_py + heroin_py + alcohol_pm + binge_alcohol_pm + tobacco_pm +
                             mental_illness + alcohol_disorder + illicit_drug_disorder + pain_reliever_disorder + substance_disorder +
                             m_depresive_episode + thoughts_suicide + plans_suicide + attempt_suicide + cigarette_pm + NE_Trump + e_vector:inequality +
                             u_rate + population + poverty + inequality + not_labourforce + youth_unemp + youth_notlabf +
                             e_vector:one_par_ch_fb_steen + e_vector:u_rate + e_vector:poverty + e_vector:two_par_fb_six, d_frame)
summary(regression_eigenvector)

stargazer(regression_degree,
          regression_betweeness,
          regression_closseness,
          regression_eigenvector, title="Results", align=TRUE, type="latex", out = "Regs_13.txt")




#Oneof"sf1", "sf3", "acs1", "acs3", "acs5", "acs1/profile", "acs3/profile, "acs5/profile", "acs1/subject", "acs3/subject", or "acs5/subject".
#rvest https://www.migrationpolicy.org/data/state-profiles/state/demographics/AL//
# with tidycensus and mpi data I can add covariates to the regression  
# and start justifying them 


# Closeness centrality of a vertex is the number of other vertices divided by the sum
# of all distances between the vertex and all others [8]. In other words, Closeness is
# the sum of all geodesics between the particular node and every other node in the
# network [6]:

# ggraph(g,  layout = 'lgl') +
#   geom_edge_arc(color="gray", curvature=0.3) +            
#   geom_node_point(color="orange", aes(size = extremists)) +     
#   geom_node_text(aes(label = media), size=3, color="gray50", repel=T) +
#   theme_void()

# Nodes should be weighted by groups divided by population and edges should be weighted by the ammount of groups
# that the two nodes share. 

#difference between substitute and quote
#f <- function(argX) {
#list(quote(argX), 
     #substitute(argX), 
     #argX)
#}

#suppliedArgX <- 100
#f(argX = suppliedArgX)
# [[1]]
# argX
# 
# [[2]]
# suppliedArgX
# 
# [[3]]
# [1] 100
