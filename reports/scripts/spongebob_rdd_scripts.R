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

saveRDS(data_df, "data/raw/spongebob_ratings_szn1-9_8_2020.rds")

# rdd data for plot

rdddf <- data_df[, `:=`(treat=ifelse( episode_nr > 60 , 1 , 0 ),
                        group=cut(episode_nr , breaks = seq(0,204,5) ))
]
bin_scatter_df <- cbind(group_nr = 1:length(unique(rdddf$group)) , rdddf[ , .(rating = mean(rating)), by=group ])
#for plot


library(ggplot2)
library(hrbrthemes)
#plots time 

spongebob_plot <- 
  ggplot( data_df ,
          aes( x = episode_nr , 
               y = rating , 
               size = votes , 
               color = as.factor(season))) +
  geom_point( alpha = 0.7 ) + 
  geom_smooth(method = 'lm' , 
              formula = y ~ poly(x,3) ) +
  labs( x = "Episode Number" , 
        y = "IMDB Score" ,
        title = "Spongebob Squarepants Episode Ratings by Season" , 
        caption = "(Fit is least-squares to a 3rd degree polynomial model)") +
  theme( legend.position = 'none' , 
         panel.background = element_rect(fill='white') , 
         panel.grid.major.y = element_line( color = 'grey' ))
ggsave( plot = spongebob_plot, "d:/Economics/Projects/spongebob-rate.png" , width = 260.3 , height = 126.2 , units = c("mm"))

spong_rdd_plot <- 
  ggplot( rdddf ,
          aes( x = episode_nr , 
               y = rating , 
               size = votes , 
               color = as.factor(treat))) +
  geom_point( alpha = 0.7 ) + 
  geom_smooth(method = 'lm' , 
              formula = y ~ poly(x,3)) +
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
ggsave(plot = spong_rdd_plot, "d:/Economics/Projects/spongebob-rate-rd.png" , width = 260.3 , height = 126.2 , units = c("mm"))


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
             formula = rating ~ treat*x  )
  wls <- lm( data = regression_discontinuity_df , 
             formula = rating ~ treat*x ,
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

ggplot( data_df[ episode_nr >= 50 &
                   episode_nr <= 70 , ][,treat:=ifelse(episode_nr>60,1,0)] ,
        aes( x = episode_nr , 
             y = rating , 
             size = votes , 
             color = as.factor(treat))) +
  geom_point( alpha = 0.7 ) + 
  geom_smooth(method = 'lm') +
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

ggplot( data_df[ episode_nr >= 30 &
                   episode_nr <= 90 , ][,treat:=ifelse(episode_nr>60,1,0)] ,
        aes( x = episode_nr , 
             y = rating , 
             size = votes , 
             color = as.factor(treat))) +
  geom_point( alpha = 0.7 ) + 
  geom_smooth(method = 'lm' ) +
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


bw_vary <-  sapply( seq(5,30,1) , rdd_function, threshhold = 60 , dat = data_df )
bw_outputs <- list()
for(i in 1:ncol(bw_vary)){
  bw_outputs[[i]] <- bw_vary[,i]
}
bw_dt <- rbindlist(bw_outputs)

# ordinary least squares
ols_discontinuity_coefs <- bw_dt[model=="ols"& term=="treat", .(estimate, conf.low, conf.high,band)]
ols_trend_coefs <- bw_dt[model=="ols"&term=="treat:x" , .(estimate, conf.low, conf.high,band)]

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

ggplot(ols_trend_coefs) + 
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
  labs( title = "Hillenburg Trend Effect: OLS Estimates" , 
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

thresh_dt <- rbindlist(lapply(seq(10,30,10), thresh_vary_bw_func))

thresh_vary_ols_treat_coefs <- thresh_dt[model=="ols"&term=="treat", 
                                         .(estimate, conf.low, conf.high,statistic,thresh,band)
][
  , true_disc := ifelse(thresh==60,1,0)
]

thresh_vary_ols_trend_coefs <- thresh_dt[model=="ols"&term=="treat:x", 
                                         .(estimate, conf.low,statistic, conf.high,thresh,band)
][, true_disc := ifelse(thresh==60,1,0)
]


ggplot(thresh_vary_ols_trend_coefs) + 
  facet_wrap( ~band ,
              nrow = 3 ) + 
  geom_histogram( aes(x = estimate) , 
                  fill = "#00BFC4" ,
                  color = 'black' ,
                  binwidth = 0.01) + 
  geom_vline(xintercept = 0 , color = 'black' , size = 1) + 
  geom_vline( data = thresh_vary_ols_trend_coefs[band==10,] , 
              aes(xintercept = -0.04) ,
              color = '#F8766D' , 
              size = 1,
              linetype = 2) + 
  geom_vline( data = thresh_vary_ols_trend_coefs[band==20,] , 
              aes(xintercept = -0.0258) ,
              color = '#F8766D' , 
              size = 1,
              linetype = 2) + 
  geom_vline( data = thresh_vary_ols_trend_coefs[band==30,] , 
              aes(xintercept = -0.0297) ,
              color = '#F8766D' , 
              size = 1,
              linetype = 2)  + 
  labs( title = "Placebo Quit: OLS Trend Estimates" , 
        subtitle = "by Bandwidth (in number of eps.) around 'quit' episode" , 
        caption = "(Red dashed line representing the estimate of the 60th episode)" , 
        x = "Estimate" , 
        y = "Count") + 
  theme( panel.background = element_rect( fill = 'white' ) , 
         panel.grid.major.x = element_line( color = "grey" , linetype = 2))

ggplot(thresh_vary_ols_treat_coefs) + 
  facet_wrap( ~band , 
              nrow = 3) + 
  geom_histogram( aes(x = estimate) ,
                  fill = "#00BFC4" ,
                  color = 'black' ,
                  binwidth = 0.1) + 
  geom_vline(xintercept = 0 , color = 'black' , size = 1) + 
  geom_vline( data = thresh_vary_ols_treat_coefs[band==10,] , 
              aes(xintercept = -0.35952) ,
              color = '#F8766D' , 
              size = 1,
              linetype = 2) + 
  geom_vline( data = thresh_vary_ols_treat_coefs[band==20,] , 
              aes(xintercept = -0.6363) ,
              color = '#F8766D' , 
              size = 1,
              linetype = 2) + 
  geom_vline( data = thresh_vary_ols_treat_coefs[band==30,] , 
              aes(xintercept = -0.8) ,
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
  indx = i*10
  
  band_i_trn_dt <- 
    thresh_vary_ols_trend_coefs[band==indx , ]
  band_pval_trn_dt <- 
    band_i_trn_dt[,`:=`(rank_est = 1+nrow(band_i_trn_dt)-rank(abs(estimate)),
                        p_est = (1/nrow(band_i_trn_dt))*(1+nrow(band_i_trn_dt)-rank(abs(estimate))) , 
                        rank_t = 1+nrow(band_i_trn_dt)-rank(abs(statistic)) ,
                        p_t = (1/nrow(band_i_trn_dt))*(1+nrow(band_i_trn_dt)-rank(abs(statistic))) , 
                        coef="discontinuity")
    ][
      ,.(rank_est,rank_t,p_est,p_t,band,coef,true_disc,thresh)
    ]
  
  band_i_trt_dt <- 
    thresh_vary_ols_treat_coefs[band==indx , ]
  band_pval_trt_dt <- 
    band_i_trt_dt[,`:=`(rank_est = 1+nrow(band_i_trt_dt)-rank(abs(estimate)),
                        p_est = (1/nrow(band_i_trt_dt))*(1+nrow(band_i_trt_dt)-rank(abs(estimate))),
                        rank_t = 1+nrow(band_i_trt_dt)-rank(abs(statistic)) ,
                        p_t = (1/nrow(band_i_trt_dt))*(1+nrow(band_i_trt_dt)-rank(abs(statistic))) ,
                        coef="trend")
    ][
      ,.(rank_est,rank_t,p_est,p_t,band,coef,true_disc,thresh)
    ]
  
  thresh_pval_dt <- rbind(band_pval_trn_dt,
                          band_pval_trt_dt)
  
  thresh_vary_ols_pvalue[[i]] <- thresh_pval_dt
}

ols_p_val <- rbindlist(thresh_vary_ols_pvalue)
treatment_p_vals <- ols_p_val[true_disc==1,.(coef,band,p_est,p_t,rank_est,rank_t)]

# weighted least squares 
wls_discontinuity_coefs <- bw_dt[model=="wls"&term=="treat", .(estimate, conf.low, conf.high,band)]
wls_trend_coefs <- bw_dt[model=="wls"&term=="treat:x" , .(estimate, conf.low, conf.high,band)]

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

ggplot(wls_trend_coefs) + 
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
  labs( title = "Hillenburg Trend Effect: WLS Estimates" , 
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

thresh_vary_wls_trend_coefs <- thresh_dt[model=="wls" & term=="treat:x", 
                                         .(estimate, conf.low,statistic, conf.high,thresh,band)
][, true_disc := ifelse(thresh==60,1,0)
]

ggplot(thresh_vary_wls_trend_coefs) + 
  facet_wrap( ~band ,
              nrow = 3 ) + 
  geom_histogram( aes(x = estimate) , 
                  fill = "#00BFC4" ,
                  color = 'black' ,
                  binwidth = 0.01) + 
  geom_vline(xintercept = 0 , color = 'black' , size = 1) + 
  geom_vline( data = thresh_vary_wls_trend_coefs[band==10,] , 
              aes(xintercept = -0.04) ,
              color = '#F8766D' , 
              size = 1,
              linetype = 2) + 
  geom_vline( data = thresh_vary_wls_trend_coefs[band==20,] , 
              aes(xintercept = -0.0258) ,
              color = '#F8766D' , 
              size = 1,
              linetype = 2) + 
  geom_vline( data = thresh_vary_wls_trend_coefs[band==30,] , 
              aes(xintercept = -0.0297) ,
              color = '#F8766D' , 
              size = 1,
              linetype = 2)  + 
  labs( title = "Placebo Quit: WLS Trend Estimates" , 
        subtitle = "by Bandwidth (in number of eps.) around 'quit' episode" , 
        caption = "(Red dashed line representing the estimate of the 60th episode)" , 
        x = "(WLS) Estimate" , 
        y = "Count") + 
  theme( panel.background = element_rect( fill = 'white' ) , 
         panel.grid.major.x = element_line( color = "grey" , linetype = 2))

ggplot(thresh_vary_wls_treat_coefs) + 
  facet_wrap( ~band , 
              nrow = 3) + 
  geom_histogram( aes(x = estimate) ,
                  fill = "#00BFC4" ,
                  color = 'black' ,
                  binwidth = 0.1) + 
  geom_vline(xintercept = 0 , color = 'black' , size = 1) + 
  geom_vline( data = thresh_vary_wls_treat_coefs[band==10,] , 
              aes(xintercept = -0.35952) ,
              color = '#F8766D' , 
              size = 1,
              linetype = 2) + 
  geom_vline( data = thresh_vary_wls_treat_coefs[band==20,] , 
              aes(xintercept = -0.6363) ,
              color = '#F8766D' , 
              size = 1,
              linetype = 2) + 
  geom_vline( data = thresh_vary_wls_treat_coefs[band==30,] , 
              aes(xintercept = -0.8) ,
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
  
  band_i_trn_dt <- 
    thresh_vary_wls_trend_coefs[band==indx , ]
  band_pval_trn_dt <- 
    band_i_trn_dt[,`:=`(rank_est = 1+nrow(band_i_trn_dt)-rank(abs(estimate)),
                        p_est = (1/nrow(band_i_trn_dt))*(1+nrow(band_i_trn_dt)-rank(abs(estimate))) , 
                        rank_t = 1+nrow(band_i_trn_dt)-rank(abs(statistic)) ,
                        p_t = (1/nrow(band_i_trn_dt))*(1+nrow(band_i_trn_dt)-rank(abs(statistic))) , 
                        coef="discontinuity")
    ][
      ,.(rank_est,rank_t,p_est,p_t,band,coef,true_disc,thresh)
    ]
  
  band_i_trt_dt <- 
    thresh_vary_wls_treat_coefs[band==indx , ]
  band_pval_trt_dt <- 
    band_i_trt_dt[,`:=`(rank_est = 1+nrow(band_i_trt_dt)-rank(abs(estimate)),
                        p_est = (1/nrow(band_i_trt_dt))*(1+nrow(band_i_trt_dt)-rank(abs(estimate))),
                        rank_t = 1+nrow(band_i_trt_dt)-rank(abs(statistic)) ,
                        p_t = (1/nrow(band_i_trt_dt))*(1+nrow(band_i_trt_dt)-rank(abs(statistic))) ,
                        coef="trend")
    ][
      ,.(rank_est,rank_t,p_est,p_t,band,coef,true_disc,thresh)
    ]
  
  thresh_pval_dt <- rbind(band_pval_trn_dt,
                          band_pval_trt_dt)
  
  thresh_vary_wls_pvalue[[i]] <- thresh_pval_dt
}

wls_p_val <- rbindlist(thresh_vary_wls_pvalue)
wls_treatment_p_vals <- wls_p_val[true_disc==1,.(coef,band,p_est,p_t,rank_est,rank_t)]
