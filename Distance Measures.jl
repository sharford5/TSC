##==============================================================================
## Distance Measures
## Take in two time series and output the distance between the series
##==============================================================================

##==============================================================================
## Euclidiean Distance
##==============================================================================
function ED(series1, series2)
  return(sqrt(sum((series1 - series2).^2)))
end

##==============================================================================
## Dynamic Time Warping Distance
##==============================================================================
function DTW(series1, series2, out = 0)
  #Create Matrix of Zeros
  l1 = length(series1)
  l2 = length(series2)
  E = zeros(l1,l2)

  #Fill First Cell
  E[1,1] = (series1[1]-series2[1])^2

  #Fill First Column
  for i in 2:l1
    E[i,1] = E[(i-1),1] + (series1[i]-series2[1])^2
  end

  #Fill First Row
  for i in 2:l2
    E[1,i] = E[1,(i-1)] + (series1[1]-series2[i])^2
  end

  for j in 2:l2
    for i in 2:l1
      E[i,j] = min(E[(i-1),j], E[(i-1),(j-1)], E[i,(j-1)]) + (series1[i]-series2[j])^2
    end
  end

  if out == 0
    return(sqrt(E[end,end]))
  else
    return(E)
  end
end

##==============================================================================
## Dynamic Time Warping Distance with Pruning based on a Current Best
##==============================================================================
function DTW_Early(series1, series2, Curent_Best = 10, out = 0)
  #Create Matrix of Zeros
  l1 = length(series1)
  l2 = length(series2)
  E = Inf*ones(l1,l2)

  #Fill First Cell
  E[1,1] = (series1[1]-series2[1])^2

  col_count = 1
  row_count = 1

  #Fill First Column
  for i in 2:l1
    E[i,1] = E[(i-1),1] + (series1[i]-series2[1])^2
    col_count = col_count + 1
    if E[i,1] > Curent_Best
      break
    end
  end

  #Fill First Row
  for i in 2:l2
    row_count
    E[1,i] = E[1,(i-1)] + (series1[1]-series2[i])^2
    if E[1,i] > Curent_Best
      break
    end
  end

  z = 2
  for j in 2:l2
    above = 0
    total = 0
    if col_count > 1
      for i in z:col_count-1
        E[i,j] = min(E[(i-1),j], E[(i-1),(j-1)], E[i,(j-1)]) + (series1[i]-series2[j])^2
        total = total + 1
        if E[i,j] > Curent_Best
          above = above + 1
        end
      end
      for i in col_count:l1
        E[i,j] = min(E[(i-1),j], E[(i-1),(j-1)], E[i,(j-1)]) + (series1[i]-series2[j])^2
        total = total + 1
        if E[i,j] > Curent_Best
          above = above + 1
        end
        if E[i,j] > Curent_Best
          break
        else
          col_count = col_count + 1
        end
      end
      if above == total
        break
      end
      if E[z,j] < Curent_Best
        z = z
      else
        z = z + 1
      end
    else
      break
    end
  end

  if out == 0
    return(sqrt(E[end,end]))
  else
    return(E)
  end
end
