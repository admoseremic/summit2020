my_colors = c("CallCenter" = "#FD292B",
              "POS" = "#145B65",
              "Online" = "#1A9CA4",
              "Forecast" = "#BBE6EF")


aggregate_products = function(product_name, model_ts, forecast_date) {
  forecast_df = data.frame(DateFrom = model_ts[[product_name]]$forecast_dates) %>%
    mutate(!!product_name := as.vector(model_ts[[product_name]]$final_forecasts$mean)) %>%
    filter(DateFrom == as.Date(forecast_date)) %>%
    select(-DateFrom)
  
  return(forecast_df)
}

donutChart = function(monthly_data, model_ts, forecast_date, product_name, 
                      color_scheme,  col_other = "lightgray",
                      line_rate = "#314150",  line_other = "#314150",  font_color = "white") {

  col_rate = color_scheme[[product_name]]
  forecast_df = data.frame(DateFrom = model_ts[[product_name]]$forecast_dates,
                           Value = as.vector(model_ts[[product_name]]$final_forecasts$mean),
                           Lower = model_ts[[product_name]]$final_forecasts$lower[,2],
                           Upper = model_ts[[product_name]]$final_forecasts$upper[,2])
  actual_df = monthly_data %>%
    select(DateFrom, !!as.name(product_name)) %>%
    rename(Value = !!as.name(product_name)) 
  
  forecast_date = as.Date(forecast_date)
  forecast_year = year(forecast_date)

  ytd_value = actual_df %>%
    filter(year(DateFrom) == forecast_year) %>%
    summarise(ytd_value = sum(Value)) %>%
    pull()
  
  year_total_prediction = ytd_value +
    forecast_df %>%
    filter(DateFrom >= forecast_date,
           year(DateFrom) == forecast_year) %>%
    select(DateFrom, Value) %>%
    summarise(yearly_pred = sum(Value)) %>%
    pull()
  
  my_rate = round(ytd_value/year_total_prediction, 3)
  
  rate_df = data.frame(rate = c(my_rate, 1-my_rate))
  rate_df = rate_df %>% 
    mutate(ymax = cumsum(rate),
           ymin = c(0,head(ymax, n=-1)),
           label_pos = (ymax + ymin)/2,
           label_text = paste0(my_rate*100, "%"))
  
  p1 = ggplot(rate_df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5)) +
    geom_rect(fill=c(col_rate, col_other), color = c(line_rate, line_other), size = c(.01,5)) +
    geom_text(x = -1, aes(y = label_pos, label = label_text, family = "Adobe Clean"), size=14, color = font_color) +
    scale_colour_manual(values = font_color) + 
    coord_polar(theta = "y", start = 0, direction = 1) +
    xlim(c(-1, 4)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "transparent",colour = NA))
 
  return(p1)
}

tsDFManipulation= function(monthly_data, product_name, model_ts, forecast_month) {
  forecast_df = data.frame(DateFrom = model_ts[[product_name]]$forecast_dates,
                           Value = as.vector(model_ts[[product_name]]$final_forecasts$mean),
                           Lower = model_ts[[product_name]]$final_forecasts$lower[,2],
                           Upper = model_ts[[product_name]]$final_forecasts$upper[,2])
  
  forecast_df = forecast_df %>%
    mutate(Lower = ifelse(DateFrom < as.Date(forecast_month), NA, Lower),
           Upper = ifelse(DateFrom < as.Date(forecast_month), NA, Upper))
  
  
  filter_date = as.Date(paste0(year(min(forecast_df$DateFrom)) - 1, "-01-01"))
  actual_df = monthly_data %>%
    select(DateFrom, !!as.name(product_name)) %>%
    rename(Value = !!as.name(product_name)) %>%
    filter(DateFrom >= filter_date)
  
  return(list(actual_df = actual_df,
              forecast_df = forecast_df))
}

tsPlot = function(monthly_data, product_name, model_ts, color_scheme, forecast_month) {
  forecast_df = data.frame(DateFrom = model_ts[[product_name]]$forecast_dates,
                           Value = as.vector(model_ts[[product_name]]$final_forecasts$mean),
                           Lower = model_ts[[product_name]]$final_forecasts$lower[,2],
                           Upper = model_ts[[product_name]]$final_forecasts$upper[,2])
  
  forecast_df = forecast_df %>%
    mutate(Lower = ifelse(DateFrom < as.Date(forecast_month), NA, Lower),
           Upper = ifelse(DateFrom < as.Date(forecast_month), NA, Upper))
  
  
  filter_date = as.Date(paste0(year(min(forecast_df$DateFrom)) - 1, "-01-01"))
  actual_df = monthly_data %>%
    select(DateFrom, !!as.name(product_name)) %>%
    rename(Value = !!as.name(product_name)) %>%
    filter(DateFrom >= filter_date)
  
  color = color_scheme[[product_name]]
  forecast_color = color_scheme[["Forecast"]]
  
  min_date = min(actual_df$DateFrom)
  max_date = max(forecast_df$DateFrom)
  min_y = min(forecast_df$Lower, actual_df$Value, na.rm = TRUE)
  max_y = max(forecast_df$Upper, actual_df$Value, na.rm = TRUE)
  num_breaks = numBreaks(c(min_y, max_y))
  
  x_breaks = seq(from = min_date,
                 to = max_date,
                 by = "3 months")
  x_labels = format(x_breaks, "%b %y")

  p1 = ggplot(data = actual_df, aes(x = DateFrom, y = Value)) +
    geom_line(color = color) +
    geom_point(color = color, fill = "white", shape = 21) +
    geom_ribbon(data = forecast_df, aes(ymin = Lower, ymax =  Upper), fill = forecast_color, alpha = .25) +
    geom_line(data = forecast_df, aes(x = DateFrom, y = Value), color = forecast_color) + 
    geom_point(data = forecast_df, aes(x = DateFrom, y = Value), color = forecast_color, 
               fill = "white", shape = 21) + 
    scale_x_date(breaks = x_breaks,
                 labels = x_labels,
                 limits = c(min_date, max_date)) +
    scale_y_continuous(breaks = extended(dmin = min_y,
                                         dmax = max_y,
                                         m = num_breaks), labels = f <- function(x) paste0("$",x*1e-6,"M")) +
    my_theme()

  return(p1)
}

my_theme = function(base_size = 11, base_family = "") {
  blue <- "#2c3e50"
  green <- "#18BC9C"
  white <- "#FFFFFF"
  grey <- "grey80"
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(line = element_line(colour = blue, size = 0.5, 
                              linetype = 1, lineend = "butt"), 
          axis.line.y = element_blank(), 
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_text(color="white", size=10, family = "Adobe Clean"),
          axis.text.y = element_text(color="white", size=10, family = "Adobe Clean"),
          axis.ticks = element_line(color = grey, size = rel(1/3)), 
          axis.title = element_text(size = rel(1)), 
          panel.background = element_rect(fill = "transparent", color = NA), 
          panel.border = element_rect(fill = "transparent", size = rel(1/2), color = NA), 
          panel.grid.major = element_line(color = grey, size = rel(1/3)), 
          panel.grid.minor = element_line(color = grey, size = rel(1/3)), 
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.spacing = unit(0.75, "cm"), 
          legend.key = element_rect(fill = white, color = NA), 
          legend.position = "none", 
          strip.background = element_rect(fill = "transparent", color = NA),
          strip.text.x = element_blank(),
          strip.text.y = element_blank(),
          plot.title = element_text(size = rel(1.2), hjust = 0), 
          plot.subtitle = element_text(size = rel(0.9), hjust = 0), 
          complete = TRUE)
}

numBreaks = function(value_vector) {
  mag = magnitudeCalc(value_vector)
  num_breaks = ceiling(ceiling(max(value_vector)/(10^mag))/floor(min(value_vector)/(10^mag)))
  return(num_breaks)
}


createForecast = function(monthly_data, forecast_month){
  product_names = monthly_data %>% select(-DateFrom) %>% names()

  model_ts = product_names %>%
    map(~ timeSeriesWrapper(monthly_data, 
                            product_name = .x)) %>%
    set_names(product_names)
  
  monthly_forecast = product_names %>% 
    map(~ aggregate_products(product_name = .x, model_ts = model_ts, forecast_date = forecast_month)) %>%
    unlist() %>%
    as.data.frame() %>%
    set_names("Forecast")    %>%
    mutate(Product = rownames(.),
           DateFrom = forecast_month)
  
  return(list(monthly_forecast = monthly_forecast,
              model_ts = model_ts,
              product_names = product_names,
              monthly_data = monthly_data,
              forecast_month = forecast_month))
}


barChart = function(product_names, model_ts, forecast_month, color_scheme) {
  bar_chart_df = product_names %>%
    map(~ aggregate_products(product_name = .x,
                             model_ts = model_ts,
                             forecast_month)) %>%
    unlist() %>%
    as.data.frame() %>%
    set_names("Value") %>%
    mutate(Product = rownames(.),
           Label = paste0("$",round(Value*1e-6, 2),"M"))
  
  min_y = min(bar_chart_df$Value)
  max_y = max(bar_chart_df$Value)
  num_breaks = numBreaks(c(min_y, max_y))
  
  p1 = ggplot(bar_chart_df, aes(x = Product, y = Value, fill = Product, label = Label)) +
    geom_bar(stat="identity", width = .8) +
    scale_fill_manual(values = my_colors) +
    geom_text(size = 10, color = "white", family = "Adobe Clean") +
    my_theme() +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
  
  return(p1)
}


magnitudeCalc = function(vector) {
  m <- min(trunc(log(abs(vector), 10)))
  m <- ifelse(m == -0, 0, m)
  return(m)
}

mapeCalc = function (forecast_model, actual_values) {
  forecast_values = forecast_model$mean[1:length(actual_values)]
  mape <- mean(abs((actual_values - forecast_values)/actual_values))
  return(mape)
}

arimaModel = function(metric_vector) {
  lag_max =floor(length(metric_vector)/2)
  
  pacf_ts = pacf(metric_vector, lag.max = lag_max, plot = FALSE)
  acf_ts = acf(metric_vector, lag.max = lag_max, plot = FALSE)
  
  m = max(abs(pacf_ts$acf))
  n = max(abs(acf_ts$acf[2:lag_max]))
  
  p = as.numeric(which(abs(pacf_ts$acf[1:lag_max]) == m))
  q = as.numeric(which(abs(acf_ts$acf[2:lag_max]) == n))
  d = ndiffs(metric_vector)
  
  model_arima = arima((metric_vector/1000), order = c(p, d, q), method="ML")
  
  return(model_arima)
}


createTimeSeriesModels = function(metric_vector, metric_dates) {
  train_ts = ts((metric_vector), 
                start = c(year(metric_dates[1]), month(metric_dates[1])), 
                frequency = 12)
  
  model_hwa = HoltWinters(train_ts, optim.start = c(alpha = 0.1, beta = 0.1, gamma = 0.1),
                          optim.control = list("L-BFGS-B"),seasonal="additive")
  model_hwm = HoltWinters(train_ts, optim.start = c(alpha = 0.1, beta = 0.1, gamma = 0.1),
                          optim.control = list("L-BFGS-B"),seasonal = "multiplicative")
  model_des = ets(y = metric_vector, model="AAN", damped=FALSE, opt.crit="lik")
  model_arima = arimaModel(metric_vector)
  
  return(list(hwa = model_hwa, hwm = model_hwm, des = model_des, arima = model_arima))
}

createTimeSeriesForecasts = function(model_list, n_forecast){
  model_forecasts = model_list %>%
    map(~ forecast(object = .x, h = n_forecast)) %>%
    set_names(names(model_list))
  
  return(model_forecasts)
}

calculateModelMAPE = function(model_forecasts, metric_vector){
  model_MAPE = model_forecasts %>%
    map(mapeCalc, actual_values = metric_vector) %>%
    set_names(names(model_forecasts))
  
  return(model_MAPE)
}


timeSeriesWrapper = function(monthly_data, product_name) {
  num_months = nrow(monthly_data)
  num_years = floor(num_months/12)
  train_dates = seq(from = monthly_data$DateFrom[1], 
                    by = "month", 
                    length.out = 12*(num_years-1))
  test_dates = seq(from = tail(train_dates, n = 1) + months(1), 
                   to = tail(monthly_data$DateFrom, n = 1), 
                   by = "month")
  
  train_data = monthly_data %>%
    filter(DateFrom %in% train_dates)
  test_data = monthly_data %>%
    filter(DateFrom %in% test_dates)

  training_metric_vector = train_data[[product_name]]
  training_metric_dates = train_data$DateFrom
  testing_metric_vector = test_data[[product_name]]
  
  n_forecast = max(3, length(testing_metric_vector))
  
  all_ts_models = createTimeSeriesModels(training_metric_vector, training_metric_dates)
  all_ts_forecasts = createTimeSeriesForecasts(all_ts_models, n_forecast = length(testing_metric_vector) + n_forecast)
  mape_models = calculateModelMAPE(all_ts_forecasts, testing_metric_vector)
  
  final_model_name = names(which.min(mape_models))
  final_model = all_ts_models[[final_model_name]]
  final_forecasts = all_ts_forecasts[[final_model_name]]
  final_mape = mape_models[[final_model_name]]
  forecast_dates = seq(from = test_dates[1], 
                       to = tail(test_dates, n = 1) + months(n_forecast), 
                       by = "month")
  
  return(list(final_model_name = final_model_name,
              final_model = final_model, 
              final_forecasts = final_forecasts,
              final_mape = final_mape,
              forecast_dates = forecast_dates))
}

generateVisualizations = function(my_forecast, color_scheme) {
  product_names = my_forecast$product_names
  monthly_data = my_forecast$monthly_data
  model_ts = my_forecast$model_ts
  forecast_month = my_forecast$forecast_month

  product_plots =  product_names %>%
    map(~ tsPlot(monthly_data, product_name = .x, 
                 model_ts, 
                 color_scheme = color_scheme, 
                 forecast_month = forecast_month)) %>%
    set_names(product_names)

  donut_charts = product_names %>%
    map(~ donutChart(monthly_data, model_ts, forecast_month, .x, color_scheme = color_scheme)) %>%
    set_names(product_names)
  
  bar_chart = barChart(product_names, model_ts, forecast_month, color_scheme = color_scheme)
  
  return(list(product_plots = product_plots,
              donut_charts = donut_charts,
              bar_chart = bar_chart))
}



createPPTX = function(my_forecast, color_scheme, pptx_template, pptx_master, pptx_layout, file_out) {
  visualizations = generateVisualizations(my_forecast, color_scheme)
  
  month_text = toupper(format(as.Date(my_forecast$forecast_month), "%B %Y"))
  year_text = toupper(paste0("FY", year(as.Date(my_forecast$forecast_month))))

  forecast_slide_list = list(value = list(visualizations$product_plots$CallCenter,
                                          visualizations$product_plots$Online,
                                          visualizations$product_plots$POS,
                                          visualizations$bar_chart,
                                          visualizations$donut_charts$CallCenter,
                                          visualizations$donut_charts$Online,
                                          visualizations$donut_charts$POS,
                                          month_text, month_text, year_text),
                             location = list(c(left = .51, top = 1.8, height = 1.63, width = 6.68),
                                             c(left = .51, top = 3.79, height = 1.63, width = 6.68),
                                             c(left = .51, top = 5.78, height = 1.63, width = 6.68),
                                             c(left = 7.13, top = 1.76, height = 1.99, width = 6.53),
                                             c(left = 7.25, top = 3.88, height = 4.04, width = 2.39),
                                             c(left = 9.15, top = 3.88, height = 4.04, width = 2.39),
                                             c(left = 11.13, top = 3.88, height = 4.04, width = 2.39),
                                             "Content Placeholder 20", 
                                             "Content Placeholder 22",
                                             "Content Placeholder 2"))
  
  doc = read_pptx(pptx_template)
  doc = doc %>%
    add_slide(layout = pptx_layout, master = pptx_master)
  
  for(i in seq_along(forecast_slide_list$value)){
    doc = doc %>%
      ph_with(value = createValue(forecast_slide_list$value[[i]]),
              location = createLocation(forecast_slide_list$location[[i]]))
  }

  print(doc, file_out)
  
}

createValue = function(value){
  UseMethod("createValue", value)
}

createValue.character <- function(value){
  value
}

createValue.gg <- function(value){
  dml(ggobj = value, bg = "transparent")
}

createLocation <- function(location){
  UseMethod("createLocation", location)
}

createLocation.character <- function(location){
  ph_location_label(location)
}

createLocation.numeric <- function(location){
  ph_location(left = location[['left']], 
              top = location[['top']],
              width = location[['width']],
              height = location[['height']])
}

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

plotlyDFManipulation = function(forecast) {
  monthly_actual_df = forecast$monthly_data
  forecast_dates = forecast$model_ts$Online$forecast_dates
  
  forecast_mean_df = data.frame(DateFrom = forecast$model_ts$Online$forecast_dates,
                                POS = as.vector(forecast$model_ts$POS$final_forecasts$mean),
                                CallCenter = as.vector(forecast$model_ts$CallCenter$final_forecasts$mean),
                                Online = as.vector(forecast$model_ts$Online$final_forecasts$mean)) %>%
    filter(DateFrom > max(forecast$monthly_data$DateFrom))
  
  area_chart_df =  monthly_actual_df %>%
    bind_rows(forecast_mean_df) %>%
    pivot_longer(-DateFrom, names_to = "Revenue_Source", values_to = "Revenue") %>%
    mutate(Month = month(DateFrom),
           Year = year(DateFrom),
           date = decimal_date(DateFrom)) %>%
    mutate(Revenue_Source = ifelse(Revenue_Source == "CallCenter", "Call Center", Revenue_Source)) %>%
    accumulate_by(~ date)
  
  return(area_chart_df)
}

