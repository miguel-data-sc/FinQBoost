#___________________________________
# load libraries
suppressMessages({
  library(data.table) 
  library(roll)
  library(TTR) # for RSI
  library(xgboost) 
})


# date when inference is made, present time:
last_known_date <- start_date # provided as argument to MakeForecast.R

price_history_csv <- dir("../data", full.names = T) #single price file in data folder
if(length(price_history_csv) != 1 ){ 
  print ("STOPPED: please check there is only one single csv.file in data folder")
  quit(save="no")}

cat("\nloaded price data file: \n", price_history_csv)

#____________________________________________________
# freeze DRE is true for last 4 months of competition, 
#freeze_DRE <- TRUE # true in last 4 months of competition, false otherwise (commented, set by MakeForecast.R depending on current date)

#________________________________________________________________________________
postprocess <- TRUE # minimal postprocess, enforce each quintile columns add to 20

#___________________________________
# load up to date historical EOD adjusted closes, file path:  ../data/uptodate_EOD_Adj.csv
#dt <- fread("../data/update_DT_ffilled_yahoo_1999-01-01_2023-01-06.csv") # sub 12 data
dt <- fread(price_history_csv) # sub 12 data


#___________________________________
#check  first column is date, set name to "index", format to IDate
setnames(dt, names(dt)[1], "index")
dt[, index := as.IDate(index)]

#___________________________________
# data recency,  define train cutting point
dt <- dt[index >= as.Date("2007-01-01")]

#___________________________________
# date when inference is made, present time to forecast from, defaults to last available friday in data:
if (!is.na(last_known_date)){
  dt <- dt[index <= last_known_date]
}


#___________________________________
# make DRE a constant value in all historical data (last 4 months of competition)

if(("DRE" %in% names(dt)) & (freeze_DRE == TRUE)){
  dt[, DRE := DRE[.N]]
  cat("\n\nDRE price history set to constant value\n")
}

#___________________________________
# address the problem of non-modelable noise
target_soften_ndays <- 2 # 1=no softening
q_weights <- c(0, 0, 0, 0, 0) # make no decisions, pending  discretionary decisions manual input
invested_factor <- 0 # make no decisions, pending  discretionary decisions manual input


#___________________________________
# auxiliary functions
# add to one
add_to_one_scale <- function(z) {z/sum(z)}
# find_asset_data
find_asset_data <- function(downloaded_dt, from_included, to_included){
  long_raw_prices <- melt(downloaded_dt, id.vars = "index")
  return(
    na.omit(setcolorder(
      setnames(long_raw_prices[(index >= from_included) & (index <= to_included)], c("date", "symbol", "price")
      ), c("symbol", "date","price")
    ))[,  date := gsub("-", "/", as.character(date))][, symbol := as.character(symbol)] # return expected dates format,  formatted as character with / separators ***
  )                                                                                      # and symbols as character instead of factors 
  
}

#___________________________________
# minimal postprocessing function:
postprocess_sub <- function(submission,
                            balance_columns=TRUE){
  
  # column balance:
  if(balance_columns){  
    submission[, `:=`(Rank5= Rank5-(sum(Rank5)-20)/.N,
                      Rank4= Rank4-(sum(Rank4)-20)/.N,
                      Rank3= Rank3-(sum(Rank3)-20)/.N,
                      Rank2= Rank2-(sum(Rank2)-20)/.N,
                      Rank1= Rank1-(sum(Rank1)-20)/.N
    )]  
  }
  return(submission)
}

#___________________________________
# make template, no decisions

find_template <- function(dt, drop_factor=NA, prop_invested=NA){
  forecasts_dt <- setnames(dcast(dt, prod ~ smooth_quintile, value.var = "preds"), c("ID", paste0("Rank", 1:5)))
  # no decisions, discretionary manual input later
  final_template <- cbind(forecasts_dt, Decision=0 # create all empty decisions
  )[, ID := as.character(ID)] # no lo quiero como factor sino como character para input de las funciones de la comp
}

#___________________________________
#preprocessing and feature engineering function
process_data <- function(dt){
  
  cat("\n***beware: try(, silent=TRUE), expect silent errors. Set silent=FALSE if debugging***\n")
  
  all_non_weekend_dates <- seq.Date(dt$index[1], dt$index[length(dt$index)], by="1 day")[!wday(seq.Date(dt$index[1], dt$index[length(dt$index)], by="1 day")) %in% c(7,1)]
  dt <- dt[data.table(index=all_non_weekend_dates), on="index"][, lapply(.SD, function(x) nafill(x, type = "locf"))]
  (dt_long <- melt(dt,id.vars = "index", variable.name = "prod", value.name = "adj_close"))
  
  #___________________________________
  to_percentile  <- function(x) {
    ecdf(x)(x)
  }
  #___________________________________
  # LABEL 
  dt_long[, four_week_forward_net_returns := (shift(adj_close, type = "lead", 5*4)/adj_close) -1 , by = prod]
  dt_long[, percentile_four_w_ahead_net_returns := try(to_percentile(four_week_forward_net_returns), silent=TRUE), by = index] # beware, try(, silent=TRUE) ERRORS !!! 
  dt_long[, percentile_four_w_ahead_net_returns := as.numeric(percentile_four_w_ahead_net_returns)] 
  dt_long[, quintile_four_w_ahead_net_returns := as.integer(cut(percentile_four_w_ahead_net_returns, seq(0,1,length.out = 6)))]
  
  #___________________________________
  # FEATURE ENGINEERING
  dt_long[, returns1d := (adj_close/shift(adj_close, type = "lag", 1)) -1 , by = prod] # 1d return, auxiliary
  dt_long[, percentile_returns1d := try(to_percentile(returns1d), silent=TRUE), by = index] # 20d return percentile rank
  dt_long[, percentile_returns1d := as.numeric(percentile_returns1d)] # debug character output to numeric
  dt_long[, roll20sd_returns1d := roll_sd(returns1d, width = 20), by = prod ]#   roll20sd of 1d returns
  dt_long[, roll20sd_pctlrank1d := roll_sd(percentile_returns1d, width = 20), by = prod] # ***
  
  #___________________________________
  # rolling standard deviation of percentile returns1d, 15 windows from 1 to 15 months length (months := 4 weeks)
  for (i in 1:15){
    window_size_n4wmonths <- i
    #cat(i, "| ")
    dt_long[, tmp_rollsd_pctlrank1d := roll_sd(percentile_returns1d, width = window_size_n4wmonths*20), by = prod] # ***
    dt_long[, tmp_rollsd_pctlrank1d_prev :=  shift(tmp_rollsd_pctlrank1d,20), by = prod] # ***
    dt_long[, paste0("rollsd_pctlrank1d_prev_windown4wmonths_", window_size_n4wmonths) := tmp_rollsd_pctlrank1d_prev]#   roll20sd of 1d returns
    dt_long[, paste0("rollsd_pctlrank1d_windown4wmonths_", window_size_n4wmonths) := tmp_rollsd_pctlrank1d]#   roll20sd of 1d returns
  }
  
  #___________________________________ 
  # additional feature (for just marginal gains) from competition month 5, RSI ratio feature, single window n=80
  for(n_indicador in c(80)){ 
    dt_long[, paste0("RSI_close_", paste0("n_", n_indicador)) := RSI(adj_close, n=n_indicador), by = prod]
    #average RSI (with that parameter) for the portfolio
    dt_long[, paste0("mean_RSI_close_by_date_", paste0("n_", n_indicador)) := mean(.SD[[paste0("RSI_close_", paste0("n_", n_indicador))]], na.rm=T), by = index]
    dt_long[, paste0("ratio_RSI_vs_mean_RSI_close_by_date_", paste0("n_", n_indicador)) := .SD[[paste0("RSI_close_", paste0("n_", n_indicador))]] / .SD[[paste0("mean_RSI_close_by_date_", paste0("n_", n_indicador))]]]
  }
  
  #___________________________________ 
  quintiles_dt <- setnames(dcast(
    dt_long[, .(index, prod, quintile_four_w_ahead_net_returns)],
    ... ~  quintile_four_w_ahead_net_returns,
    value.var = "quintile_four_w_ahead_net_returns",
    fun.aggregate = length, 
  ),
  as.character(1:5),
  paste0("quintile_", as.character(1:5))
  )
  quintiles_dt[`NA` == 1, paste0("quintile_", as.character(1:5)) := NA]
  quintiles_dt[, paste0("smooth_q_", as.character(1:5)) := frollmean(.SD, target_soften_ndays, align = "right"), # no longer 5 aligned center
               .SDcols = paste0("quintile_", as.character(1:5)),
               by = prod]
  
  
  #________________________________________________
  long_quintiles_dt <- melt(quintiles_dt, measure.vars = grep("smooth", names(quintiles_dt), value=T), variable.name = "smooth_quintile", value.name = "target")[, .(index, prod, smooth_quintile, target)]
  
  #________________________________________________
  #REMOVE FEATURES USED TO BUILD LABEL
  dt_long[, c("four_week_forward_net_returns",
              "percentile_four_w_ahead_net_returns",
              "quintile_four_w_ahead_net_returns"
  ) := NULL]
  #________________________________________________
  # feature selection
  
  keep_features <- c("index", "prod",
                     "rollsd_pctlrank1d_windown4wmonths_3",
                     "rollsd_pctlrank1d_windown4wmonths_4",
                     "rollsd_pctlrank1d_windown4wmonths_5", 
                     "rollsd_pctlrank1d_prev_windown4wmonths_9",
                     "rollsd_pctlrank1d_windown4wmonths_2", 
                     "rollsd_pctlrank1d_windown4wmonths_10",
                     "rollsd_pctlrank1d_prev_windown4wmonths_15", 
                     "rollsd_pctlrank1d_windown4wmonths_13",
                     "rollsd_pctlrank1d_prev_windown4wmonths_6",
                     "ratio_RSI_vs_mean_RSI_close_by_date_n_80" # added from month 5 of competition
                     
  ) 
  
  #######################################################################
  long_quintiles_dt <- dt_long[, .SD, .SDcols = keep_features][long_quintiles_dt, on = c("index", "prod")]
  
  return(long_quintiles_dt) # for feature lab
}




# do process data
suppressWarnings(long_quintiles_dt <- process_data(dt))


#___________________________________ 
#Sample weekly, on fridays, note still keeping all NAS*

cat("\nsampling data weekly on Friday close, \n")
weekly_dt <- long_quintiles_dt[wday(index)==6] #keep fridays

#___________________________________ 
# Brief recap
# all up to date weekly data date ranges: _____________________________________
cat("\n--------------------------------------------------------------------\n")
cat("all weekly data date range: ", weekly_dt[, as.character(range(index))])
first_labeled_date <- weekly_dt[!is.na(target)][1, index]
cat("first labeled date: ", as.character(first_labeled_date))
last_labeled_date <- weekly_dt[!is.na(target)][.N, index]
cat("\nlast labeled date:  ", as.character(last_labeled_date))
cat("\n--------------------------------------------------------------------\n")
total_labeled_n_weeks <- difftime(last_labeled_date, first_labeled_date, units = "weeks")
cat("total labeled data size: ", total_labeled_n_weeks , "weeks\n")


#___________________________________
# Train model for predicting, all available data up to prediction date, no validation set (inference time!)  

#___________________________________
# features to ignore while modeling

excluded_feats <- c("prod")
inference_date <- weekly_dt[.N, index]
cat("inference date: ", as.character(inference_date), "\n\n" ) #last available date

#___________________________________
# COMPLETE TRAIN SET FOR FULL TRAINING BEFORE INFERENCE
full_train_set_weekly <-  weekly_dt[!is.na(target)]


###################################################################################


#___________________________________
# train single model up to most recent labeled data, fixed nrounds, nthreads = 10


system.time({
  trset <- full_train_set_weekly
  
  #backup factor col to join by later (used by comp. evaluation function)  
  tr_prod_col <-  trset[, prod]
  for (i in setdiff(names(trset), c("index"))) {trset[[i]] <- as.numeric(trset[[i]])}
  
  
  if (length(excluded_feats)>0){
    dm_tr <-  xgb.DMatrix(#data = as.matrix(trset[, -c("index", "target")]),
      data = as.matrix(trset[, .SD, .SDcols=-c("index", "target", excluded_feats)]),            
      # -c("sig_id", "kfold", ..all_target_feats) # (nota: aqui uso get para obtener un vector, ..como arriba obtiene una data table, que habría que unlist depues, con get lo extrae directamente como vector) #explicacion, .. vs get() * hack /explicacion
      label = trset[, target]
    )}else{
      dm_tr <-  xgb.DMatrix(#data = as.matrix(trset[, -c("index", "target")]),
        data = as.matrix(trset[, .SD, .SDcols=-c("index", "target", excluded_feats)]),            
        # -c("sig_id", "kfold", ..all_target_feats) # (nota: aqui uso get para obtener un vector, ..como arriba obtiene una data table, que habría que unlist depues, con get lo extrae directamente como vector) #explicacion, .. vs get() * hack /explicacion
        label = trset[, target]
      )  
    }
  
  
  
  #___________________________________
  #PARAMETERS
  num_parallel_tree <- 1
  tree_method <- "hist" 
  max_depth <- 3 # low depth, limit interactions (overfitting is easy in this context)
  colsample_bytree <- 0.8 #Colsample by tree, not by level
  subsample <- 0.5
  #_____________
  eta  <-  0.02 
  nrounds  <- 230 
  gamma <- 0 #(no additional regularization = 0, default)
  min_child_weight <-  1 #(default, sin regularization = 1) 
  plot_importances <- T # plot importances as seen by xgboost  (model-generated importance not reliable but in this case they are ok)
  
  #___________________________________
  #TRAINING
  gc() 
  set.seed(29)   
  
  #system.time(
  boost_model <- xgb.train(
    tree_method = tree_method, 
    data = dm_tr,
    watchlist = list(train = dm_tr), 
    print_every_n = 25,
    num_parallel_tree = num_parallel_tree, 
    max.depth = max_depth, 
    eta = eta,       
    gamma = gamma, 
    min_child_weight= min_child_weight,
    max_delta_step = 0,
    colsample_bytree = colsample_bytree,   
    subsample = subsample,
    nrounds  = nrounds,
    objective   = "binary:logistic",
    eval_metric ="logloss", 
    nthread = 10,
    verbose = 1 
    
  )
  
  
  #___________________________________
  # plot importances as seen by xgboost
  
  if (plot_importances){
    if (length(excluded_feats)>0){
      importance_matrix  <- xgb.importance(feature_names = names(trset[, .SD, .SDcols=-c("index", "target", excluded_feats)]), model = boost_model) #se guarda por si quiero      revisarla luego
    }else{
      importance_matrix <- xgb.importance(feature_names = names(trset[, .SD, .SDcols=-c("index", "target")]), model = boost_model) #se guarda por si quiero      revisarla luego
    }
    #
    
    png("../outputs/xgboost_importances.png", width = 600, height = 600)
    xgb.plot.importance(importance_matrix[-1, ], top_n = 20, cex = 0.8, left_margin = 18, main="FEATURE IMPORTANCE (by Xgboost) ") # [-1, ] remove smooth quintile from the plot
    dev.off()
    
    
    
  }
  
  
  #___________________________________
  
  
  
  val_1out_valset <- weekly_dt[index == inference_date]

  
  #_______________________________
  
  #CREATE DATASETS XGBOOST (DENSE)

  

  #backup factor col to join by later (used by comp. evaluation function)  
  val_prod_col <- val_1out_valset[, prod]
  #to numeric, mandatory for Xgboost
  for (i in setdiff(names(val_1out_valset), c("index"))) {val_1out_valset[[i]] <- as.numeric(val_1out_valset[[i]])}
  
  
  
  if (length(excluded_feats)>0){
    dm_val <-  xgb.DMatrix(
      data = as.matrix(val_1out_valset[, .SD, .SDcols=-c("index", "target", excluded_feats)])
    )}else{
      dm_val <-  xgb.DMatrix(
      data = as.matrix(val_1out_valset[, .SD, .SDcols=-c("index", "target")])
      )
    }
  
  
  
  invisible(gc())
  
  
  #____________________________________________________________________
  # predition
  valpreds <- predict(boost_model, dm_val)
  #recover prods column as factor:
  val_1out_valset[, prod := val_prod_col]
  val_1out_valset <- copy(val_1out_valset) # copy to avoid annoying warning in data.table
  val_1out_valset[, preds := valpreds]
  
  
  template <<- find_template(val_1out_valset, drop_factor = drop_factor, prop_invested = invested_factor) 
  
  #____________________________________________________________________
  # POSTPROCESSING: ADD TO ONE, 
  # just make quantiles add to one for each prod, keep quantile proportions 
  # note that for this task using softmax would have been a bad idea
  Rank_cols <- grep("Rank", names(template), value = T)
  template[, (Rank_cols) :=  add_to_one_scale(.SD), by =ID, .SDcols = Rank_cols]
  
  
})

if(postprocess == TRUE){
  
  postprocess_sub(template,
                  balance_columns=TRUE
  )
}




Rank_cols <- grep("Rank", names(template), value = T)

# All numbers round to 5 decimals, competition requirement
template <- template[, round(.SD, 5), by=ID]


# Rounding breaks probability, must add to 1, simple fix, add the residual to Rank3
precision_residuals <- template[, rowSums(.SD), .SDcols=Rank_cols] - 1
template[, Rank3 := round(Rank3 - precision_residuals, 5)] 

# check
cat("\nall rows add to one: ", all.equal(template[, rowSums(.SD), .SDcols=Rank_cols], rep(1, nrow(template))))
cat("\ncolSums:\n")
print(colSums(template[, Rank1:Rank5]))


fwrite(template, file = "../outputs/template.csv")

# save sessionInfo() output ?
# capture.output(sessionInfo(), file="../sessionInfo.txt")


cat("\nforecast template saved in ../outputs/template.csv")
cat("\n--------------------------------------------------------------------\n\n")




