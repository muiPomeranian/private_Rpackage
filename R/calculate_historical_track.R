library(roxygen2)

#' functions to give out the hitorical record according to the returns calculated from strategy chosen
#' @param dataframe, stocks inputs..
#' @param portfolio, portfolio chosen from the stock calculation as a return base
#' @param weighting, determines the weight of period as a sliding window

#' @return return binded result from historical calculation of stocks based on chosen investment target from portfolio calculation
#' @export
#' @examples calculate_historical_track(dataframe, portfolio, weighting)
#' @details
#' code to calculate returuns bases on a portfolio determined via the function determine_portfolio

calculate_historical_track = function( dataframe,portfolio, weighting)
{
  ## weithging = equal means that you invest the same proportion in each company on each quarter
  if(weighting =='equal')
  {
    print('equal')
    ## we create a matrix of weight where each company receives 1/total number of stocks
    ## if 20 stocks, each gets 1/20 or 5%
    ## if 10 stocks, each gets 10%
    weights = matrix(1/ncol(portfolio), nrow = nrow(portfolio), ncol = ncol(portfolio))

    tt  = time(portfolio)
    prices_kk = c()
    dates_kk = c()
    all_kk = c()

    ##here for each quarter, we compute the return of that quarter
    for( k in 1:nrow(portfolio))
    {
      print(k)
      if(k<nrow(portfolio))
      {
        ## we find out
        ## suppose k = 1 and date is 2016-06-30
        ## so prices_K window would be the price window from that day up to the next quarter
        prices_k = window(dataframe, start = tt[k], end = tt[k+1])
        prices_k = prices_k[,portfolio[k,]]
        ## we have now a matrix with the returns of each company during that quarter
        returns_k = CalculateReturns(prices_k)

        ## we compute the return of the total portfolio by multiplying the weights by returns
        for(w in 2:nrow(returns_k))
        {
          ## here we multiply returns by weights to obtain the aggregated return of the portfolio on day w
          prices_kk = c(prices_kk,sum(returns_k[w,]*weights[k,]))
          dates_kk = c(dates_kk,as.character(time(returns_k[w,])))
          all_kk = cbind(dates_kk,prices_kk)
        }
      }else
      {
        prices_k = window(dataframe, start = tt[k], end = time(dataframe)[length(time(dataframe))])
        prices_k = prices_k[,portfolio[k,]]
        returns_k = CalculateReturns(prices_k)

        for(w in 2:nrow(returns_k))
        {

          prices_kk = c(prices_kk,sum(returns_k[w,]*weights[k,]))
          dates_kk = c(dates_kk,as.character(time(returns_k[w,])))
          all_kk = cbind(dates_kk,prices_kk)
        }
      }

    }
    resuls = c(100)
    for( u in 2:nrow(all_kk))
    {
      resuls = c(resuls, resuls[u-1]*(1+as.numeric(all_kk[u,2])))
    }

    all_kk = cbind(all_kk,resuls)
  }

  ## weithging = size means that you invest more in companies which have higher weight
  ## weight is usually determined by the market capitalization
  if(weighting =='size')
  {
    weights = matrix(0, nrow = nrow(portfolio), ncol = ncol(portfolio))
    print('size')
    for(g in 1:nrow(weights))
    {
      gg= portfolio[g,]
      gg = gsub('.Close','',gg)
      wg = nasdaq[match(gg,nasdaq$Symbol),3]
      wg = wg/sum(wg)

      weights[g,] = wg
    }


    tt  = time(portfolio)
    prices_kk = c()
    dates_kk = c()
    all_kk = c()
    for( k in 1:nrow(portfolio))
    {
      if(k<nrow(portfolio))
      {
        prices_k = window(dataframe, start = tt[k], end = tt[k+1])
        prices_k = prices_k[,portfolio[k,]]
        returns_k = CalculateReturns(prices_k)

        for(w in 2:nrow(returns_k))
        {

          prices_kk = c(prices_kk,sum(returns_k[w,]*weights[k,]))
          dates_kk = c(dates_kk,as.character(time(returns_k[w,])))
          all_kk = cbind(dates_kk,prices_kk)
        }
      }else
      {
        prices_k = window(dataframe, start = tt[k], end = time(dataframe)[length(time(dataframe))])
        prices_k = prices_k[,portfolio[k,]]
        returns_k = CalculateReturns(prices_k)

        for(w in 2:nrow(returns_k))
        {

          prices_kk = c(prices_kk,sum(returns_k[w,]*weights[k,]))
          dates_kk = c(dates_kk,as.character(time(returns_k[w,])))
          all_kk = cbind(dates_kk,prices_kk)
        }
      }

    }
    resuls = c(100)
    for( u in 2:nrow(all_kk))
    {
      resuls = c(resuls, resuls[u-1]*(1+as.numeric(all_kk[u,2])))
    }

    all_kk = cbind(all_kk,resuls)
  }

  return(all_kk)

}
