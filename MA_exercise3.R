rm(list=ls())
dev.off()
# ctrl + l to clear the console
install.packages("markovchain", dependencies = TRUE)
install.packages("matlab")
install.packages("Rcpp")
install.packages("wesanderson") # for color pallets
install.packages("reshape2")
install.packages("ChannelAttribution")
library(dplyr)
library(ChannelAttribution)
library(markovchain)
library(reshape2)
library(ggplot2)
library(wesanderson)

#FIRST LETS IMPORT OUR ATTRIBUTION DATA
df_path<-data.frame(read.csv("Exam_Attrib3.csv", header=TRUE,
                             sep = ";", stringsAsFactors = FALSE))
df_path

df_conv<-data.frame(read.csv("Attribution_exercise_conv.csv", header=TRUE,
                             sep = ";", stringsAsFactors = FALSE))
# have a look at the first few rows for df_path
head(df_path) 

#structure of our data frame for df_path
str(df_path)


# have a look at the first few rows for df_conv
head(df_conv)

#structure of our data frame for df_conv
str(df_conv)

###################################################
### LETS PREPARE OUR DATA TO BE ABLE TO ANALYSE IT#
###################################################

df_path=df_path%>%
  arrange(client_id, date) %>%
  group_by(client_id)%>%
  summarise(path=paste(channel, collapse = " > ")) %>%
  ungroup()

str(df_path)
df_path[1,] # a look at the first client


###########################################################################
#lets arrange our path and conversation data
###########################################################################


df_attrib<-merge(df_path, df_conv, by="client_id")
str(df_attrib)


######################################

#lets make our first attribution model

Hmdl = heuristic_models(df_attrib, var_path = "path", var_conv = "conv",
                        var_value="conv_val")

Hmdl

# Lets make our Markov chain model
Mmdl = markov_model(df_attrib, var_path = "path", var_conv = "conv",
                    var_value="conv_val", order=1, out_more = TRUE)

Mmdl

# Lets merge our results and compare them
class(Hmdl)
class(Mmdl) # this is a list and not a table


Results1 = merge(Hmdl,Mmdl$result, by="channel_name")
 

#lets str(Results1_conv)prepare for making a barplot
str(Results1)
vars_to_keep_c=c("channel_name","first_touch_conversions","last_touch_conversions","linear_touch_conversions", "total_conversions")
vars_to_keep_v=c("channel_name","first_touch_value","last_touch_value","linear_touch_value","total_conversion_value")

str(vars_to_keep_c)


Results1_conv=Results1[vars_to_keep_c]
Results1_val=Results1[vars_to_keep_v]

str(Results1_conv)

#lets make the conversion plot

conversion = melt(Results1_conv, id="channel_name")
str(conversion)


po_conv= ggplot(conversion, aes(x=channel_name,y=value, fill=variable))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Attributed conversions")+
  labs(x="channel Names", y="conversion")+
  theme(legend.position="bottom")+
  scale_fill_discrete(name="Attribution model",
                      labels=c("First touch","Last touch",
                               "Linear Touch", "1st Order Markov"))


po_conv
value=melt(Results1_val, id="channel_name")

po_val= ggplot(value, aes(x=channel_name,y=value, fill=variable))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Attributed value")+
  theme(legend.position="bottom")+
  scale_fill_discrete(name="Attribution model",
                      labels=c("First touch","Last touch",
                               "Linear Touch", "1st Order Markov"))

po_val

##################################################################################
##################################################################################
#################### survey data #################################################
##################################################################################
##################################################################################




#lets work with the survey data
survey = data.frame(read.csv("survey_attribution.csv", header=TRUE,
                             sep = ";", stringsAsFactors = FALSE))

str(survey)
#q3_0_GROUP: Need
#q18_0_GROUP: search
#q19_0_GROUP: choice


survey$Need = gsub("," , " > ", survey$Q3_0_GROUP)
survey$search = gsub("," , " > ", survey$Q18_0_GROUP)
survey$choice = gsub("," , " > ", survey$Q19_0_GROUP)


# Full conversion path
survey$path = do.call(paste, c(survey[,c("Need","search","choice")], sep=" > "))

survey$path[1] #Issue with ("> >")

survey$path = gsub(">  >", " > ", survey$path)
survey$path[1]

#let's add value and conversion information
survey$value=survey$Q14_1
survey$convert = rep(1, dim(survey)[1])


#lets only keep the information for attricution modelling
df_attrib=survey[c("Need","search", "choice","path","value", "convert")]
str(df_attrib)



Hmdl = heuristic_models(df_attrib, var_path = "path", var_conv = "convert",
                        var_value="value")

Hmdl

# Lets make our Markov chain model
Mmdl = markov_model(df_attrib, var_path = "path", var_conv = "convert",
                    var_value="value", order=1, out_more = TRUE)

Mmdl


#lets analyze the transition matrix
plot_transition = Mmdl$transition_matrix
str(plot_transition)

levels= as.character(Mmdl$result$channel_name)

plot_transition[1,]


plot_transition$channel_from[plot_transition$channel_from=="1"] = levels[1]
plot_transition

for(i in 1:13){
  plot_transition$channel_from[plot_transition$channel_from==i] = levels[i]
  plot_transition$channel_to[plot_transition$channel_to==i] = levels[i]
}

plot_transition[1,]

#lets make the heatmap of the transition probabilities
cols= wes_palette("Zissou1", 100, type="continuous")
po_heat=ggplot(plot_transition, aes(x=channel_from, y=channel_to,
                                    fill=transition_probability))+
  geom_tile()+
  labs(x="Channel To", y="channel From", fill="Transition Probability")+
  ggtitle("Transiton Heatmap")+
  geom_text(aes(label=round(transition_probability,2)))+
  theme(axis.text.x = element_text(angle=45,vjust = 1, hjust = 1))+
  scale_fill_gradientn(colours = cols)
po_heat







