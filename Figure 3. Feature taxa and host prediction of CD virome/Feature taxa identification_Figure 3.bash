#MaAslin2 analysis to identify feature taxa
setwd('C:/Users/Desktop/Mucosal virus_CD&HC/...')
df_input_data = read.table(file = "virus.txt", header = TRUE, sep = "\t",
                           row.names = 1,
                           stringsAsFactors = FALSE)
df_input_metadata = read.table(file = "metadata.txt", header = TRUE, sep = "\t",
                               stringsAsFactors = FALSE)
#Pre-process
df_meta_GZ = na.omit(df_input_metadata[which(df_input_metadata$Geography=="GZ"),])
row.names(df_meta_GZ) = df_meta_GZ[,1]
df_meta_GZ = df_meta_GZ[,-1]
df_meta_KM = na.omit(df_input_metadata[which(df_input_metadata$Geography=="KM"),])
row.names(df_meta_KM) = df_meta_KM[,1]
df_meta_KM = df_meta_KM[,-1]

#Setting levels of factors_GZ
df_meta_GZ$CD_HC = factor(df_meta_GZ$CD_HC,levels = c("HC","CD"))
df_meta_GZ$Intestinal_inflammation = factor(df_meta_GZ$Intestinal_inflammation,levels = c("HC","Remission","Flare"))
df_meta_GZ$Puffed_food_consumption_frequency = factor(df_meta_GZ$Puffed_food_consumption_frequency,levels = c("N","Y"))
df_meta_GZ$Glucocorticoids_in_past_3_months = factor(df_meta_GZ$Glucocorticoids_in_past_3_months,levels = c("N","Y"))
df_meta_GZ$Immunosuppressant_in_past_3_months = factor(df_meta_GZ$Immunosuppressant_in_past_3_months,levels = c("N","Y"))
df_meta_GZ$Biologics_in_past_3_months = factor(df_meta_GZ$Biologics_in_past_3_months,levels = c("N","Y"))
df_meta_GZ$5_ASA_in_past_3_months = factor(df_meta_GZ$5_ASA_in_past_3_months,levels = c("N","Y"))
df_meta_GZ$Coffee_consumption_frequency=as.numeric(df_meta_GZ$Coffee_consumption_frequency)
df_meta_GZ$Alcoholic_beverages_consumption_frequency=as.numeric(df_meta_GZ$Alcoholic_beverages_consumption_frequency)

#Setting levels of factors_KM
df_meta_KM$CD_HC = factor(df_meta_KM$CD_HC,levels = c("HC","CD"))
df_meta_KM$Intestinal_inflammation = factor(df_meta_KM$Intestinal_inflammation,levels = c("HC","Remission","Flare"))
df_meta_KM$Puffed_food_consumption_frequency = factor(df_meta_KM$Puffed_food_consumption_frequency,levels = c("N","Y"))
df_meta_KM$Glucocorticoids_in_past_3_months = factor(df_meta_KM$Glucocorticoids_in_past_3_months,levels = c("N","Y"))
df_meta_KM$Immunosuppressant_in_past_3_months = factor(df_meta_KM$Immunosuppressant_in_past_3_months,levels = c("N","Y"))
df_meta_KM$Biologics_in_past_3_months = factor(df_meta_KM$Biologics_in_past_3_months,levels = c("N","Y"))
df_meta_KM$5_ASA_in_past_3_months = factor(df_meta_KM$5_ASA_in_past_3_months,levels = c("N","Y"))
df_meta_KM$Coffee_consumption_frequency=as.numeric(df_meta_KM$Coffee_consumption_frequency)
df_meta_KM$Alcoholic_beverages_consumption_frequency=as.numeric(df_meta_KM$Alcoholic_beverages_consumption_frequency)

#Analysis
#identify disease-specific taxa
  fit_data = Maaslin2(
  input_data = df_input_data, 
  input_metadata =df_meta_GZ,
  output = "Intestinal_inflammation_GZ", 
  max_significance = 0.20,
  fixed_effects = c("Intestinal_inflammation"),
  reference = 'Intestinal_inflammation,HC', 
  random_effects = c("Alcoholic_beverages_consumption_frequency","Puffed_food_consumption_frequency","Glucocorticoids_in_past_3_months","Immunosuppressant_in_past_3_months","Biologics_in_past_3_months","5_ASA_in_past_3_months","Coffee_consumption_frequency"))

  fit_data = Maaslin2(
  input_data = df_input_data, 
  input_metadata =df_meta_KM,
  output = "GZ_CD_HC", 
  max_significance = 0.20,
  fixed_effects = c("Intestinal_inflammation"),
  reference = 'Intestinal_inflammation,HC', 
  random_effects = c("Alcoholic_beverages_consumption_frequency","Puffed_food_consumption_frequency","Glucocorticoids_in_past_3_months","Immunosuppressant_in_past_3_months","Biologics_in_past_3_months","5_ASA_in_past_3_months","Coffee_consumption_frequency"))


#Medication&Disease specific taxa
  fit_data = Maaslin2(
  input_data = df_input_data, 
  input_metadata =df_metadata,
  output = "Medication", 
  max_significance = 0.20,
  fixed_effects = c("CD_HC","Glucocorticoids_in_past_3_months","Immunosuppressant_in_past_3_months","Biologics_in_past_3_months","5_ASA_in_past_3_months"),
  random_effects = c("Geography","Alcoholic_beverages_consumption_frequency","Puffed_food_consumption_frequency","Coffee_consumption_frequency"))

#Diet&Disease specific taxa
  fit_data = Maaslin2(
  input_data = df_input_data, 
  input_metadata =df_metadata,
  output = "Diet", 
  max_significance = 0.20,
  fixed_effects = c("CD_HC","Alcoholic_beverages_consumption_frequency","Coffee_consumption_frequency"，"Puffed_food_consumption_frequency",),
  random_effects = c("Geography","Glucocorticoids_in_past_3_months","Immunosuppressant_in_past_3_months","Biologics_in_past_3_months","5_ASA_in_past_3_months"))