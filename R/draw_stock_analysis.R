#' draw bollinger band, candle chart, trick char, volume traded chart
#' @param single_stock single stock informations with only one single price information(all OHLC)
#' @param ticker_name ticker name which will be used for the analysis
#' @param sdv standard deviation used to make and interprete BB chart
#' @param n1 numbers for the BBands
#' @param n2 moving average term fomr the first line of SMA
#' @param n3 moving average term fomr the second line of SMA
#' @param months Selected user chosen months to calculate BB plots

#' @return Summary of bollinger band, candle chart, trick char, volume traded chart analysis result
#' @export
#' @examples draw_stock_analysis(single_stock, ticker_name, sdv, n1,n2,n3,last_period_months) When user use sd=2, 95% of the data will fall in between the bollinger bands upper and lower bands.User can use it as a target and analyze the market. Lower bollinger means market will be soon moving into bull market.this will show the moving average graph also. this shows overbought and oversold situations. generally, >60 means overbought, <-50 means oversold.here it will automatically calculate the adjusted OHLC
#' @details This gives all summary of useful plots listed above from analysis information result for the single stock. to interprete bollinger bands graph: use statsitcal terms.

draw_stock_analysis = function(single_stock, ticker_name, sdv, n1,n2,n3,last_period_months){
  x = adjustOHLC(single_stock,use.Adjusted=TRUE)
  #this shows how does the price changed(candle stick), and shows the trade volumne
  chartSeries(x, subset = last_period_months, name = ticker_name, line.type='l', theme = chartTheme('white', up.col = 'green', dn.col = 'red'), major.ticks='months',color.vol=F)
  # this add bollinger bands(n1,sd)

  plot(addBBands(n=n1,sd=sdv))
  plot(addSMA(n=n2, col='blue'))
  plot(addSMA(n=n3, col = 'orange'))
  plot(addTA(TRIX(Cl(x)), col=2:3))
  plot(addCMO()) # add chande momentum oscillator
}
