rm(list=ls())
setwd('/media/arduin/Ubuntu HDD/01. GECE/01. Monetaria')
options(scipen=999)

# Libraries ---------------------------------------------------------------

pacman::p_load(BETS, ggplot2, tidyr, readxl ,dplyr, forcats, fitdistrplus,lubridate,
               tseries, forecast,zoo,wktmo,lmtest, timetk, plotly, stats)


geceDataFrame <- starwars

# Data Structures ---------------------------------------------------------

str(geceDataFrame)
str(geceDataFrame$name)
str(geceDataFrame$height)

## Vector

### Atomic Vector

dbl_var <- c(1, 2.5, 4.5)
int_var <- c(1L, 6L, 10L)
log_var <- c(TRUE, FALSE, T, F)
chr_var <- c("algunos", "tipos","de strings")

## List

x <- list(1:3, "a", c(TRUE, FALSE, TRUE), c(2.3, 5.9))
str(x)

### Recursive
x <- list(list(list(list())))
str(x)

### Linear Models Objects

mod <- lm(mass ~ height, data = geceDataFrame)
is.list(mod)

## Attributes

y <- 1:10
attr(y, "my_attribute") <- "This is a vector"
structure(1:10, my_attribute = "This is a vector")

## Names
x <- c(a = 1, b = 2, c = 3)
x <- 1:3; 
names(x) <- c("a","b", "c")
x <- setNames(1:3, c("a","b", "c"))

## Factors
x <- factor(c("a", "b", "b", "a"))
class(x)
levels(x)

## Matrix
a <- matrix(1:6, ncol = 3, nrow = 2)
b <- array(1:12, c(2, 3, 2))

c <- 1:6
dim(c) <- c(3, 2)
c

dim(c) <- c(2, 3)
c

## Data Frame

df <- data.frame(x = 1:3, y = c("a", "b", "c"))
str(df)

df <- data.frame(
  x = 1:3,
  y = c("a", "b", "c"),
  stringsAsFactors = FALSE)
str(df)

cbind(df, data.frame(z = 3:1))

rbind(df, data.frame(x = 10, y = "z"))


# TIDYR -------------------------------------------------------------------
# Para manipulação de dados no R, o pacote mais utilizado é o "dplyr".
# Mas afinal, qual o diferencial deste pacote em ganho de utilidade?
# O dplyr está para o R assim como o pandas está para o Python.
# Ele fornece uma gramática de manipulação de dados com vários elementos, como verbos, 
#advérbios, funções e o operador pip.
# Podemos, por exemplo, selecionar colunas de dataframes com o comando "select"
select(geceDataFrame, name, mass)
starwars0 = select(geceDataFrame, name, gender, species)
starwars0 = select(geceDataFrame, -skin_color, -eye_color)
select(geceDataFrame, name, starts_with("s"))
select(geceDataFrame, name, ends_with("r"))
select(geceDataFrame, name, contains("color"))
# Já para seleionar linhas temos dois comandos: O "slice" e o "filter"
# Com o slice efetuamos essa operação considerando a numeração das linhas.
# Exemplos:
slice(geceDataFrame, 1)
slice(geceDataFrame, 1:10)
slice(geceDataFrame, -2:-87)
# Já com o filter, podemos selecionar linhas de acordo com alguma característica especifíca.
starwars0 = filter(geceDataFrame, sex=="male")
filter(geceDataFrame, species=="Droid")
filter(geceDataFrame, homeworld=="Tatooine")
filter(geceDataFrame, starships=="X-wing")
# Caso você deseje alterar alguma coluna ou criar outra, o comando "mutate" vai lhe ajudar.
starwars1 = mutate(geceDataFrame,
                   imc = (mass/height^2))
# Perceba que o comando acima nos devolveu um dataframe com várias cédulas vazias na coluna
#do imc.
# Isto se deve ao fato de termos missing values em nosso conjunto de dados.
# Para resolver este problema, podemos criar uma função que substitua os missing values pela
#média da respectiva coluna.
f=function(v) {
  ifelse(is.na(v),
         mean(v,na.rm = TRUE),
         v)
}
f(geceDataFrame$height)
starwars2 = mutate(geceDataFrame,
                   height = f(height))
starwars2 = mutate(starwars2,
                   mass = f(mass))
starwars3 = mutate(starwars2,
                   imc = (mass/height^2))
View(starwars3)
# Uma maneira mais eficiente de efetuar a operação acima é utilizando o operador "pip", que
#serve como um conector de verbos num código. Exemplo:
starwars4 = geceDataFrame %>% #Atalho no teclado para o operador: ctrl+shift+m
  mutate(height = f(height), mass = f(mass)) %>% 
  mutate(imc = (mass/height^2))
# Por fim, o advérbio mais utilizado do dplyr é o "group_by", que serve para agrupar 
#dataframes de acordo com alguma varíavel com a qual estamos trabalhando. Ele é muito
#utilizado junto ao comando summarise, que serve para resumir um conjunto de dados. Exemplo:
starwars5 = starwars4 %>%
  group_by(species) %>%
  summarise(imc_medio = mean(imc))
# GGPLOT2 -----------------------------------------------------------------

## Scater Graph

ggplot(geceDataFrame, aes(x=mass, y=height)) +
  geom_point() + # Show dots
  geom_label(
    label=geceDataFrame$name, 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  )

geceDataFrame2 <- geceDataFrame %>% filter(species == "Human")

ggplot(geceDataFrame2, aes(x=mass, y=height)) +
  geom_point() + # Show dots
  geom_label(
    label=geceDataFrame2$name, 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  )

## Boxplot

logicalFilter <- !(geceDataFrame$species == "Human" | geceDataFrame$species == "Droid")

geceDataFrame$species[logicalFilter] <- "Others"

geceDataFrame %>%
  mutate(class = as.factor(geceDataFrame$species)) %>%
  ggplot(aes(x=class, y=height, fill=class)) + 
  geom_boxplot() +
  xlab("class") +
  theme(legend.position="none") +
  xlab("") +
  xlab("")