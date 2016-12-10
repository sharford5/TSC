##====================================================
## Lower Bound Techniques
##====================================================
function LB_Kim(series1, series2)
  A = (series1[1] - series2[1])^2
  D = (series1[length(series1)] - series2[length(series2)])^2
  return(sqrt(A+D))
end

function LB_Yi(series1, series2)
  Max = reduce(max, series2)
  Min = reduce(min, series2)
  total = 0

  for i in 1:length(series1)
    if series1[i] > Max
      total = total + (series1[i] - Max)^2
    elseif series1[i] < Min
      total = total + (series1[i] - Min)^2
    end
  end

  return(sqrt(total))
end
