# Ano epidemiológico
# epiYear ---------------------------------------------------------------------
#'@description Find to which epidemiological year belongs a given epidemiological week. 
#'@title Define Epidemiological Year
#'@param se numeric vector with epidemiological weeks to be converted
#'@param cut epidemiological week that separates consecutive epidemiological years. Default = 41
#'@return vector of epidemiological years. 
#'@examples
#'epiYear(se = 201012)
#'xx <- epiYear(se = c(201012:201052, 201101:201153))

epiYear <- function(se, cut = 40){
  
  d <- tibble(se = se)
  d <- d %>%
    mutate(year = floor(se / 100),
           eweek = se - year * 100,
           eyear = case_when(
             eweek <= cut ~ year - 1,  # if se <= cut, eYear = previous calendar Year
             TRUE ~ year              # if se > cut, eYear = current calendar Year  
           ))
  
  # Adequando a contagem para os anos que tem SE 53. 
  anoL <- d %>% 
    group_by(year) %>%
    summarise(nsem = max(eweek)) %>%
    filter(nsem == 53) 
  
  d <- d %>% 
    mutate(
      week2 = case_when(eweek > cut ~ eweek - cut,  # inicio do novo ano, week2 comeca em 1 
                        TRUE ~ ifelse((year - 1) %in% anoL$year, 53 - cut, 52 - cut) + eweek) # continuacao do ano epidemiologico 
    )
  
} 


#funcao que mede as sequencia de semanas com pelo menos 5 casos
# USO: x <- c(6,0,6,6,0,6,6,6,0,6,6,6,6,0) ; semanas_consec(x, 5)
# y <- c(1,2,3,4,5,6,7,8,9,10); semanas_consec(y, 5)
# z <- c(1,1,1,1,1,1,1); semanas_consec(z, 5)
# w <- rep(10,10); semanas_consec(w, 5)

semanas_consec <- function(d, v = 5){
  wx <- as.numeric(d > v)
  wwx <- c(0, which(wx == 0),length(wx) + 1)
  wwwx <- diff(wwx) - 1
  wwwx[wwwx > 0]
  }

# funcao que calcula semanas consecutivas com 0 casos
# USO: semanas_consec0(x)
# semanas_consec0(y)
# k <- rep(0,10); semanas_consec0(k)

semanas_consec0<- function(d){
  wx <- as.numeric(d != 0)
  wwx <- c(0, which(wx != 0), length(wx) + 1)
  wwwx <- diff(wwx) - 1
  wwwx[wwwx>0]
}

# semanas consecutivas de crescimento
# USO: semanas_consec_cresc(y)
# semanas_consec_cresc(w)
# b <- c(0,1,2,3,2,1,3,4,5,6,4,3,1,10); semanas_consec_cresc(b)

semanas_consec_cresc<- function(d){
  xx <- diff(d)
  wx <- as.numeric(xx > 0)
  wwx <- c(0, which(wx == 0),length(wx) + 1)
  wwwx <- diff(wwx)
  wwwx[wwwx > 1]
}

