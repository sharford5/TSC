##====================================================
## Classification Algorithm
##   Takes in a full train and test set, the method is either DTW or ED,
##   Optional Lower Bounding Techniques can be used (in list form)
##====================================================
function Classification(train, test, method = "DTW", LB = [])
  train_labels = train[:,1]
  test_labels = test[:,1]

  train = train[:,2:end]
  test = test[:,2:end]

  l_train = length(train[:,1]); l_test = length(test[:,1])

  wrong = 0
  predicted_class = zeros(l_test)

  for j in  1:l_test
    BSF = Inf
    for i in 1:l_train
      if "Kim" in LB
        Kim_distance = LB_Kim(train[i,:], test[j,:])
        if Kim_distance > BSF
          continue
        end
      end

      if "Yi" in LB
        Yi_distance = LB_Yi(train[i,:], test[j,:])
        if Yi_distance > BSF
          continue
        end
      end

      if method == "DTW"
        distance = DTW(train[i,:], test[j,:])
      elseif method == "ED"
        distance = ED(train[i,:], test[j,:])
      end
      if distance < BSF
        predicted_class[j] = train_labels[i]
        BSF = distance
      end
    end
    if predicted_class[j] != test_labels[j]
      wrong = wrong + 1
    end
  end
  return(wrong/l_test)
end
