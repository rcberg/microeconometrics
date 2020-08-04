library(rvest)
library(data.table)
library(broom)

## data pull and clean

datalist=list()
for(i in 1:9){
  http_i <- read_html(paste0("https://www.imdb.com/title/tt0206512/episodes?season=",i))
  
  seasons_i <- html_node(http_i , "#episodes_content > div.clear > div.list.detail.eplist")
  
  datalist_i <- list()
  for(j in 1:length(xml_children(seasons_i))){
    datalist_i[[j]] <- data.table( season = i , 
                                   episode = j , 
                                   rating = html_text(
                                     xml_child(
                                       xml_child(
                                         xml_child(
                                           xml_child(
                                             xml_child(seasons_i, j),
                                             2),
                                           4), 
                                         1), 
                                       2)) , 
                                   votes = html_text(
                                     xml_child(
                                       xml_child(
                                         xml_child(
                                           xml_child(
                                             xml_child(
                                               seasons_i, j), 
                                             2), 
                                           4), 
                                         1), 
                                       3))
    )
  }
  
  datalist[[i]] <- rbindlist(datalist_i)
}

data_df <- rbindlist(datalist)

data_df <- data_df[ , `:=`(votes = as.numeric(gsub("[(,)]", "", votes)) , 
                           rating = as.numeric(rating) , 
                           episode_nr = 1:nrow(data_df)
) ]

# rdd data for plot

rdddf <- data_df[, `:=`(treat=ifelse( episode_nr > 60 , 1 , 0 ),
                        group=cut(episode_nr , breaks = seq(0,204,5) ))
]
bin_scatter_df <- cbind(group_nr = 1:length(unique(rdddf$group)) , rdddf[ , .(rating = mean(rating)), by=group ])
#for plot


library(ggplot2)
library(hrbrthemes)

binscatter_plot <- 
  ggplot( data = bin_scatter_df ) + 
  geom_point( aes(x = group_nr , y = rating) ) + 
  geom_vline( aes(xintercept = 12) , linetype = 2 , size = 1) + 
  labs(title = "Average Episode Rating" , 
       subtitle = "Averaged into 5-episode bins" , 
       y = "Avg. Rating" , 
       x = "Episode bin") + 
  theme( panel.background = element_rect(fill = 'white') , 
         panel.grid = element_line(color = 'grey' , linetype = 2))

# regression discontinuity

rdd_function <- function(threshhold, bandwidth, dat){
  input_data <- as.data.table(dat)
  func_dt <- input_data[, `:=`(treat=ifelse( episode_nr > threshhold , 1 , 0 ) ,
                               x = (episode_nr-threshhold))]
  
  regression_discontinuity_df <- func_dt[ episode_nr >= threshhold - bandwidth &
                                            episode_nr <= threshhold + bandwidth ,
  ]
  ols <- lm( data = regression_discontinuity_df , 
             formula = rating ~ treat*poly(x,3)  )
  wls <- lm( data = regression_discontinuity_df , 
             formula = rating ~ treat*poly(x,3) ,
             weights = votes )
  
  ols_output <- cbind( as.data.table(tidy(ols, conf.int = T)) , 
                       band = rep(bandwidth , 
                                  nrow(as.data.table(tidy(ols)))) , 
                       thresh = rep(threshhold , 
                                    nrow(as.data.table(tidy(ols)))) , 
                       model = rep("ols" , 
                                   nrow(as.data.table(tidy(ols)))) 
  )
  wls_output <- cbind( as.data.table(tidy(wls, conf.int = T)) , 
                       band = rep(bandwidth , 
                                  nrow(as.data.table(tidy(wls)))) , 
                       thresh = rep(threshhold , 
                                    nrow(as.data.table(tidy(wls)))), 
                       model = rep("wls" , 
                                   nrow(as.data.table(tidy(wls)))) 
  )
  output <- rbind(ols_output , wls_output)
  
  return(output)
}

# trimmed discontinuity scatter

ggplot( data_df[ episode_nr >= 40 &
                   episode_nr <= 80 , ][,treat:=ifelse(episode_nr>60,1,0)] ,
        aes( x = episode_nr , 
             y = rating , 
             size = votes , 
             color = as.factor(treat))) +
  geom_point( alpha = 0.7 ) + 
  geom_smooth(method = 'lm',
              formula = y~poly(x,3)) +
  geom_vline( xintercept = 60 ,
              linetype = 2) + 
  labs( x = "Episode Number" , 
        y = "IMDB Score" ,
        title = "Spongebob Squarepants Episode Ratings by Season" , 
        subtitle = "The 'Hillenburg Effect'",
        caption = "(Dashed line at Episode 60; the last with S. Hillenburg as writer)") +
  theme( legend.position = 'none' , 
         panel.background = element_rect(fill='white') , 
         panel.grid.major.y = element_line( color = 'grey' ))

ggplot( data_df[ episode_nr >= 20 &
                   episode_nr <= 100 , ][,treat:=ifelse(episode_nr>60,1,0)] ,
        aes( x = episode_nr , 
             y = rating , 
             size = votes , 
             color = as.factor(treat))) +
  geom_point( alpha = 0.7 ) + 
  geom_smooth(method = 'lm' ,
              formula = y~poly(x,3)) +
  geom_vline( xintercept = 60 ,
              linetype = 2) + 
  labs( x = "Episode Number" , 
        y = "IMDB Score" ,
        title = "Spongebob Squarepants Episode Ratings by Season" , 
        subtitle = "The 'Hillenburg Effect'",
        caption = "(Dashed line at Episode 60; the last with S. Hillenburg as writer)") +
  theme( legend.position = 'none' , 
         panel.background = element_rect(fill='white') , 
         panel.grid.major.y = element_line( color = 'grey' ))


bw_vary <-  sapply( seq(6,40,2) , rdd_function, threshhold = 60 , dat = data_df )
bw_outputs <- list()
for(i in 1:ncol(bw_vary)){
  bw_outputs[[i]] <- bw_vary[,i]
}
bw_dt <- rbindlist(bw_outputs)

# ordinary least squares
ols_discontinuity_coefs <- bw_dt[model=="ols"& term=="treat", .(estimate, conf.low, conf.high,band)]

ggplot(ols_discontinuity_coefs) + 
  geom_line(aes(x = band , 
                y = estimate) , 
            size = 1) + 
  geom_line(aes(x = band , 
                y = conf.low),
            linetype = 2 , 
            size = 1 ) + 
  geom_line(aes(x = band , 
                y = conf.high),
            linetype = 2 , 
            size = 1 ) +
  geom_hline( aes(yintercept = 0 ) , color = '#00BFC4' , size = 1) +
  labs( title = "Hillenburg 'Jump' Effect: OLS Estimates" , 
        subtitle = "by Bandwidth Around 60th Episode" , 
        y = "Estimate" , 
        x = "Bandwidth" , 
        caption = "(Bandwidth measured in number of episodes.)") + 
  theme(panel.background = element_rect( fill = 'white') , 
        panel.grid.major.y = element_line( color = 'darkgrey' , linetype = 2))

thresh_vary_bw_func <- function(indx){
  thresh_func_output <- list()
  thresh_vary_band_i <- sapply( indx:(nrow(data_df)-indx) , rdd_function, bandwidth = indx , dat = data_df )
  for(i in 1:ncol(thresh_vary_band_i)){
    thresh_func_output[[i]] <- thresh_vary_band_i[,i]
  }
  return(rbindlist(thresh_func_output))
}

thresh_dt <- rbindlist(lapply(seq(20,40,10), thresh_vary_bw_func))

thresh_vary_ols_treat_coefs <- thresh_dt[model=="ols"&term=="treat", 
                                         .(estimate, conf.low, conf.high,statistic,thresh,band)
][
  , true_disc := ifelse(thresh==60,1,0)
]

ggplot(thresh_vary_ols_treat_coefs) + 
  facet_wrap( ~band , 
              nrow = 3) + 
  geom_histogram( aes(x = estimate) ,
                  fill = "#00BFC4" ,
                  color = 'black' ,
                  binwidth = 1) + 
  geom_vline(xintercept = 0 , color = 'black' , size = 1) + 
  geom_vline( data = thresh_vary_ols_treat_coefs[band==20,] , 
              aes(xintercept = 0.06957) ,
              color = '#F8766D' , 
              size = 1,
              linetype = 2) + 
  geom_vline( data = thresh_vary_ols_treat_coefs[band==30,] , 
              aes(xintercept = 0.70842) ,
              color = '#F8766D' , 
              size = 1,
              linetype = 2) + 
  geom_vline( data = thresh_vary_ols_treat_coefs[band==40,] , 
              aes(xintercept = 0.7745) ,
              color = '#F8766D' , 
              size = 1,
              linetype = 2) + 
  labs( title = "Placebo Quit: OLS 'Jump' Estimates" , 
        subtitle = "by Bandwidth (in number of eps.) around 'quit' episode" , 
        caption = "(Red dashed line representing the estimate of the 60th episode)" , 
        x = "Estimate" , 
        y = "Count") + 
  theme( panel.background = element_rect( fill = 'white' ) , 
         panel.grid.major.x = element_line( color = "grey" , linetype = 2))

thresh_vary_ols_pvalue <- list()

for(i in 1:3){
  indx = (1+i)*10
  band_i_trt_dt <- 
    thresh_vary_ols_treat_coefs[band==indx , ]
  band_pval_trt_dt <- 
    band_i_trt_dt[,`:=`(rank_est = 1+nrow(band_i_trt_dt)-rank(abs(estimate)),
                        p_est = (1/nrow(band_i_trt_dt))*(1+nrow(band_i_trt_dt)-rank(abs(estimate))),
                        rank_t = 1+nrow(band_i_trt_dt)-rank(abs(statistic)) ,
                        p_t = (1/nrow(band_i_trt_dt))*(1+nrow(band_i_trt_dt)-rank(abs(statistic))) 
                        )
    ][
      ,.(rank_est,rank_t,p_est,p_t,band,coef,true_disc)
    ]
 
  thresh_vary_ols_pvalue[[i]] <- band_pval_trt_dt
}

ols_p_val <- rbindlist(thresh_vary_ols_pvalue)
treatment_p_vals <- ols_p_val[true_disc==1,.(coef,band,p_est,p_t,rank_est,rank_t)]

# weighted least squares 
wls_discontinuity_coefs <- bw_dt[model=="wls"&term=="treat", .(estimate, conf.low, conf.high,band)]

ggplot(wls_discontinuity_coefs) + 
  geom_line(aes(x = band , 
                y = estimate) , 
            size = 1) + 
  geom_line(aes(x = band , 
                y = conf.low),
            linetype = 2 , 
            size = 1 ) + 
  geom_line(aes(x = band , 
                y = conf.high),
            linetype = 2 , 
            size = 1 ) +
  geom_hline( aes(yintercept = 0 ) , color = '#00BFC4' , size = 1) +
  labs( title = "Hillenburg 'Jump' Effect: WLS Estimates" , 
        subtitle = "by Bandwidth Around 60th Episode" , 
        y = "Estimate" , 
        x = "Bandwidth" , 
        caption = "(Bandwidth measured in number of episodes.)") + 
  theme(panel.background = element_rect( fill = 'white') , 
        panel.grid.major.y = element_line( color = 'darkgrey' , linetype = 2))

thresh_vary_wls_treat_coefs <- thresh_dt[model=="wls" & term=="treat", 
                                         .(estimate, conf.low, conf.high,statistic,thresh,band)
][
  , true_disc := ifelse(thresh==60,1,0)
]

ggplot(thresh_vary_wls_treat_coefs) + 
  facet_wrap( ~band , 
              nrow = 3) + 
  geom_histogram( aes(x = estimate) ,
                  fill = "#00BFC4" ,
                  color = 'black' ,
                  binwidth = 1) + 
  geom_vline(xintercept = 0 , color = 'black' , size = 1) + 
  geom_vline( data = thresh_vary_wls_treat_coefs[band==20,] , 
              aes(xintercept = 0.21822) ,
              color = '#F8766D' , 
              size = 1,
              linetype = 2) + 
  geom_vline( data = thresh_vary_wls_treat_coefs[band==30,] , 
              aes(xintercept = 0.711767) ,
              color = '#F8766D' , 
              size = 1,
              linetype = 2) + 
  geom_vline( data = thresh_vary_wls_treat_coefs[band==40,] , 
              aes(xintercept = 0.93947) ,
              color = '#F8766D' , 
              size = 1,
              linetype = 2) + 
  labs( title = "Placebo Quit: WLS 'Jump' Estimates" , 
        subtitle = "by Bandwidth (in number of eps.) around 'quit' episode" , 
        caption = "(Red dashed line representing the estimate of the 60th episode)" , 
        x = "(WLS) Estimate" , 
        y = "Count") + 
  theme( panel.background = element_rect( fill = 'white' ) , 
         panel.grid.major.x = element_line( color = "grey" , linetype = 2))

thresh_vary_wls_pvalue <- list()
for(i in 1:3){
  indx = (1+i)*10
  
  band_i_trt_dt <- 
    thresh_vary_wls_treat_coefs[band==indx , ]
  band_pval_trt_dt <- 
    band_i_trt_dt[,`:=`(rank_est = 1+nrow(band_i_trt_dt)-rank(abs(estimate)),
                        p_est = (1/nrow(band_i_trt_dt))*(1+nrow(band_i_trt_dt)-rank(abs(estimate))),
                        rank_t = 1+nrow(band_i_trt_dt)-rank(abs(statistic)) ,
                        p_t = (1/nrow(band_i_trt_dt))*(1+nrow(band_i_trt_dt)-rank(abs(statistic))) ,
                        coef="trend")
    ][
      ,.(rank_est,rank_t,p_est,p_t,band,coef,true_disc)
    ]
  
  thresh_vary_wls_pvalue[[i]] <- band_pval_trt_dt
}

wls_p_val <- rbindlist(thresh_vary_wls_pvalue)
wls_treatment_p_vals <- wls_p_val[true_disc==1,.(coef,band,p_est,p_t,rank_est,rank_t)]

