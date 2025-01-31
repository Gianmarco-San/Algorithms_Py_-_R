
# Gianmarco Santoro - Master MD2SL - Programming and Algorithms in R


# ----
# A) Si generi un dataframe che simula un campione di n = 400000 studenti appartenenti 
# a N = 200 scuole. Le scuole si distinguono in due gruppi: i Licei Scientifici e 
# gli Istituti Tecnici a indirizzo economico, in rapporto 35 a 65. Sugli studenti 
# si osservano il genere, avente distribuzione di Bernoulli con probabilità di 
# selezionare una femmina pari al 45%, e il voto in matematica, avente una 
# distribuzione normale con media 8 e deviazione standard 1.5 per i maschi del 
# liceo scientifico e media 7 e deviazione standard 0.9 per le femmine del liceo 
# scientifico. Negli istituti tecnici invece il voto in matematica è in media di 1 
# punto inferiore rispetto al liceo scientifico, per entrambi i sessi 
# (la variabilità rimane costante).



# ----
# Set seed for reproducibility
set.seed(23)


# Generate variables
n <- 400000
N <- 200


# School N-long array made of Liceo 35% and Tecnico 65%, with reinserting
school <- sample(c("Liceo", "Tecnico"), N, replace = TRUE, prob = c(0.35, 0.65))


# Gender n-long vector with a Bernoulli distribution 45% Female and 55% Male
sex <- rbinom(n, 1, 0.45)


# if-else assign to 1, if Female, else 0 = Male 
sex <- ifelse(sex == 1, "Female", "Male")


# Create the dataframe, with first 2 columns School and Sex
df <- data.frame(School = school, Sex = sex)


# Function to calculate, from a normal distribution, a MathMark based on School and Sex
calcMathMark <- function(school, sex) {
       if (school == "Liceo"   && sex == "Male")   {return(rnorm(1, mean = 8, sd = 1.5))} 
  else if (school == "Tecnico" && sex == "Male")   {return(rnorm(1, mean = 7, sd = 1.5))} 
  else if (school == "Liceo"   && sex == "Female") {return(rnorm(1, mean = 7, sd = 0.9))} 
  else if (school == "Tecnico" && sex == "Female") {return(rnorm(1, mean = 6, sd = 0.9))}}


# Function to constrain marks between 0 - 10 and consider only one decimal
roundMathMark <- function(school, sex) {round(pmin(pmax(calcMathMark(school, sex), 0), 10), 1)}


# Add MathMark column to df. 
# mapply iterate on rows as input, calculating the Mark based on Sex and School
df$MathMark <- mapply(roundMathMark, df$School, df$Sex)


# Create a new column "SchoolIndex" with unique indices for each school
#  Hypothesis of constant distributed students in schools
df <- transform(df, SchoolIndex = sample(1:200, N))  # transfrom add a new col by the given rule


# Show the header of df
head(df)



# ----
# B) Analizzare il dataframe simulato:
# 1) Contare quanti studenti maschi e femmine ci sono per ogni scuola e per ogni tipo di scuola.


# Number of Female and Male students per kind of school
freq_table_school_sex <- table(df$School, df$Sex)  # creates a two-way contingency table

print(freq_table_school_sex)


# Number of Female and Male students in each school
freq_table_schoolIndex_sex <- table(df$SchoolIndex, df$Sex)

head(freq_table_schoolIndex_sex)
tail(freq_table_schoolIndex_sex)
# print(freq_table_schoolIndex_sex)



# ----
# 2) Calcolare media, mediana, quartili, deviazione standard per il voto in 
#    matematica a livello di studente, distinguendo per genere, e a livello di 
#    scuola, distinguendo per tipo di scuola.


# Calc statistics of MathMark for Male and Female students per kind of school
summary_stats <- aggregate(MathMark ~ School + Sex, data = df, FUN = function(x) {
                          c(Mean    = mean(x), 
                            Median  = median(x),
                            L_Quart = quantile(x, probs = 0.25),
                            U_Quart = quantile(x, probs = 0.75),
                            sd      = sd(x))})

print(summary_stats)



# ----
# 3) Rappresentare tramite un opportuno grafico il voto in matematica in modo da
#    consentire il confronto tra maschi e femmine.


# Colors array
my_cols_sex <- c("#FC4E07", "#00AFBB")


# Boxplot
boxplot(MathMark ~ Sex, 
        df,
        xlab = "Sex", 
        ylab = "Math Marks", 
        main = "Distribution of Math Marks by Sex",
        col = my_cols_sex,      # Colors
        boxwex = 0.3,           # Box width
        cex = 0.5,              # Outliers size
        outpch = 16,            # Outliers shape
        outcol = my_cols_sex)   # Outliers color


# Convert Sex variable to factor with specific levels
df$Sex <- factor(df$Sex, levels = c("Female", "Male"))

# Add legend
legend("bottomright", legend = levels(df$Sex), fill = my_cols_sex, border = NA,
       bg = "white", cex = 0.8, box.lwd = 0, inset = c(0.02, 0.02))



# ----
# 4) Rappresentare tramite un opportuno grafico il voto in matematica in modo da
#    consentire il confronto tra licei scientifici e istituti tecnici.


# My color selection
my_cols <- c("forestgreen", "orange")


# Boxplot
boxplot(MathMark ~ School, 
        df,
        xlab = "School", 
        ylab = "Math Marks", 
        main = "Distribution of Math Marks by School",
        col = my_cols,
        boxwex = 0.3,
        cex = 0.5,
        outpch = 16,
        outcol = my_cols)


# Convert School variable to factor with specific levels
df$School <- factor(df$School, levels = c("Liceo", "Tecnico"))

# Add legend
legend("bottomright", legend = levels(df$School), fill = my_cols, border = NA,
       bg = "white", cex = 0.8, box.lwd = 0, inset = c(0.02, 0.02))



# ----
# 5) Creare una nuova variabile che riporti il voto in matematica in classi di
#    valori, distinguendo tra voto gravemente insufficiente (tra 0 e 5 escluso),
#    insufficiente (tra 5 incluso e 6 escluso), sufficiente (tra 6 incluso e 7
#    escluso), buono (tra 7 incluso e 8.5 escluso), ottimo (almeno 8.5).


# Create a new variable and column with categorical-math-marks
df$CatMark <- cut(df$MathMark,
                  breaks = c(0, 5, 6, 7, 8.5, Inf),
                  labels = c("Severely Insufficient", "Insufficient", "Sufficient", "Good", "Excellent"),
                  right = FALSE)  # to exclude right values

# Display the head of the dataframe
head(df)

# print(df)


# ----
# 6) Distinguendo in base al genere, determinare la distribuzione di frequenza 
#    (frequenze assolute e relative, semplici e cumulate) per la nuova variabile
#    di cui al punto e) e rappresentarla graficamente.

# Absolute frequency table of CatMark by Sex
abs_freq <- table(df$CatMark, df$Sex)

print(abs_freq)


# Relative frequencies distribution of Categorical Marks for each Sex
rel_freq <- prop.table(abs_freq, margin = 2)  # divide freq in each cell by the sum of all frequencies
                                              # margin = 2 to say each column, so analyze freq in each category
print(rel_freq)


# Cumulative absolute frequencies
cumul_freq_abs <- cumsum(abs_freq)   # cumulative sum of the table

names(cumul_freq_abs) <- c("Female-SI", "Female-I", "Female-S", "Female-G", "Female-E",
                         "Male-SI", "Male-I", "Male-S", "Male-G", "Male-E")
print(cumul_freq_abs)


# Cumulative relative frequencies
cumul_freq_rel <- cumsum(rel_freq)

names(cumul_freq_rel) <- c("Female-SI", "Female-I", "Female-S", "Female-G", "Female-E",
                         "Male-SI", "Male-I", "Male-S", "Male-G", "Male-E")
print(cumul_freq_rel)


# Color arrays
hot_col <- c("#FF0000", "#FF4500", "#FF8C00", "#FFA500", "#FFD700")

cold_col <- c("#0000FF", "#0099FF", "#00CCFF", "#66CCFF", "#99CCFF")


# Plotting the absolute frequency distribution
barplot(abs_freq, beside = TRUE, 
        col = c(hot_col, cold_col), 
        main = "Frequency Distribution of Categorical Marks by Sex",
        xlab = "Categorical Marks",
        ylab = "Frequency")


# Add legends for each category
legend("top", 
       legend = c("Severely Insufficient", "Insufficient", "Sufficient", "Good", "Excellent",
                  "Severely Insufficient", "Insufficient", "Sufficient", "Good", "Excellent"),
       fill = c(hot_col, cold_col),
       border = NA, bg = "white", cex = 0.75, box.lwd = 0, inset = c(0, -0.016))



# ----
# Plotting the Cumulative absolute frequency distribution
barplot(cumul_freq_abs, beside = TRUE, 
        col = c(hot_col, cold_col), 
        main = "Cumulative Frequency Distribution of Categorical Marks by Sex",
        xlab = "Categorical Marks",
        ylab = "Frequency")


# Add legends for each category
legend("topleft", 
       legend = c("Severely Insufficient", "Insufficient", "Sufficient", "Good", "Excellent",
                  "Severely Insufficient", "Insufficient", "Sufficient", "Good", "Excellent"),
       fill = c(hot_col, cold_col),
       border = NA, bg = "white", cex = 0.75, box.lwd = 0, inset = c(0.01, 0))



# ----
# Extra plot
# New table to see aggregate Female-Male in each Mark-label
abs_freq_2 <- table(df$Sex, df$CatMark)


# Plotting the frequency distribution
barplot(abs_freq_2, beside = TRUE, 
        col  = my_cols_sex, 
        main = "Frequency Distribution of Categorical Marks by Mark",
        xlab = "Categorical Marks",
        ylab = "Frequency")


# Add legends for each category
legend("topleft", 
       legend = c("Female", "Male"),
       fill   = my_cols_sex,
       border = NA, bg = "white", cex = 0.75, box.lwd = 0, inset = c(0, -0.016))



# ----
# 7) Creare una nuova variabile binaria per un voto in matematica ottimo (almeno
#    pari a 8.5) e costruire la tabella a doppia entrata che riporta le frequenze
#    relative condizionate degli studenti rispetto al voto in matematica
#    (ottimo vs. non ottimo), dato il tipo di scuola.

# Create a binary - var for Marks
df$ExcelMath <- ifelse(df$MathMark >= 8.5, "Excellent", "Not Excellent")

head(df)

conting_table_excel <- table(df$ExcelMath, df$School)

conditional_freq <- prop.table(conting_table_excel, margin = 2)

# print(contingency_table)

print(conditional_freq)
