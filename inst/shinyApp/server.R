server = function(input, output, session){

    # The UI is built for each tab panel.Because the UI is based on input,
    # this information goes into the renderUI() function

    # Set the magnification size for observed and resampled sample plots
    cex.s = 0.9

    # Initiate the reactive Values
    states <- shiny::reactiveValues()
    states$resample <- FALSE
    states$reset = FALSE
    states$plotSample <- FALSE
    states$values = NULL
    states$doNotReset = FALSE
    states$selectedTail = 1
    states$meanPoints = as.data.frame(cbind(x = 0, y = 0, id = 1))
    states$sampleSelect = 1

    # FUNCTIONS

    # FUNCTION TO RESAMPLE FROM THE ORIGINAL SAMPLE(S)
    set.values = function() {
      shiny::req(input$numberSamples)

      v = NULL

      tempData = df()

      if(input$response_var != "NONE" & input$explain_var == "NONE"){
        responsePlace = which(names(df()) == input$response_var)
        tempData = subset(tempData, !is.na(tempData[, responsePlace]))
        responseVar = tempData[, responsePlace]
      }

      if(input$response_var != "NONE" & input$explain_var != "NONE"){
        responsePlace = which(names(df()) == input$response_var)
        explainPlace = which(names(df()) == input$explain_var)
        tempData = subset(tempData, !is.na(tempData[, responsePlace]) & !is.na(tempData[, explainPlace]))
        responseVar = tempData[, responsePlace]
        explainVar = tempData[, explainPlace]
      }

      if(input$response_var != "NONE" & input$responseType == "Quantitative"){

        N = length(responseVar)

        if(input$explain_var == "NONE"){ # One-sample Quantitative
          if(input$simType == "TEST"){
            responseVar = responseVar + (input$nullMean - mean(responseVar))
          }
          v = mosaic::do(input$numberSamples) * mosaic::resample(responseVar)
          v = cbind(rowMeans(v), v)
        }
        else{ # Two Samples Quantitative
          if(input$simType == "CI"){ # Two-Sample CI Quantitative
            theData = subset(responseVar, explainVar == levels(explainVar)[1])
            v1 = mosaic::do(input$numberSamples) * mosaic::resample(theData)
            mean1 = rowMeans(v1)

            theData = subset(responseVar, explainVar == levels(explainVar)[2])
            v2 = mosaic::do(input$numberSamples) * mosaic::resample(theData)
            mean2 = rowMeans(v2)

            if(input$explRefGroup == levels(explainVar)[1]){
              meanDiff = mean1 - mean2
            } else{
              meanDiff = mean2 - mean1
            }

            v = cbind(meanDiff, v1, v2)
          } # End of Two-Sample CI Quantitative
          else{ # Two-Sample TEST Quantitative
            n1 = as.numeric(table(explainVar)[1])
            theReplace = ifelse(input$randomMethod == "Reassign", FALSE, TRUE)

            v = do(input$numberSamples) * resample(responseVar, replace = theReplace)
            mean1 = rowMeans(v[, 1:n1])
            mean2 = rowMeans(v[, (n1 + 1):N])
            if(input$explRefGroup == levels(explainVar)[1]){
              meanDiff = mean1 - mean2
            } else{
              meanDiff = mean2 - mean1
            }
            v = cbind(meanDiff, v)
          }
        } # End of Two-Sample Quantitative
        colnames(v) = c("mean", paste("v", 1:N, sep=""))
        v = as.data.frame(v)
        return(v)
      } # END of Quantitative Response Variable

      if(input$response_var != "NONE" & input$responseType == "Categorical"){

        respLevel = which(levels(responseVar) == input$respRefGroup)
        respOthGroup = ifelse(respLevel == 1, levels(responseVar)[2], levels(responseVar)[1])

        N = length(responseVar)

        if(input$explain_var == "NONE"){ # One-Sample CI or TEST Categorical
          if(input$simType == "CI"){ # One-Sample CI Categorical
            v = mosaic::do(input$numberSamples) * mosaic::resample(responseVar)

            if(respLevel == 1){
              v = -1*(v - 2)
            } else {
              v = v - 1
            }
            p = rowMeans(v)

            v = ifelse(v == 1, input$respRefGroup, respOthGroup)
            v = as.data.frame(cbind(stat = p, v))
            v$stat = as.numeric(v$stat)
          }
          else{ # One-Sample TEST Categorical
            v = do(input$numberSamples) * {prop = rbinom(n = 1, size = N, prob = input$nullMean)
            c(prop, rep(input$respRefGroup, prop), rep(respOthGroup, (N - prop)))}
            v = as.data.frame(v)
            names(v)[1] = "stat"
            v$stat = as.numeric(v$stat)
            v$stat = v$stat/N
          }
        } # End of One-Sample Categorical
        else{ # Two-Sample CI or TEST Categorical
          explLevel = which(levels(explainVar) == input$explRefGroup)
          if(explLevel == 1){
            explOthGroup = levels(explainVar)[2]
          } else{
            explOthGroup = levels(explainVar)[1]
          }
          if(input$simType == "CI"){ # Two-Sample CI Categorical

            theData = subset(responseVar, explainVar == levels(explainVar)[1])
            v1 = mosaic::do(input$numberSamples) * mosaic::resample(theData)

            theData = subset(responseVar, explainVar == levels(explainVar)[2])
            v2 = mosaic::do(input$numberSamples) * mosaic::resample(theData)

            if(respLevel == 1){
              v1 = -1*(v1 - 2)
              v2 = -1*(v2 - 2)
            } else {
              v1 = v1 - 1
              v2 = v2 - 1
            }

            p1 = rowMeans(v1)
            p2 = rowMeans(v2)

            if(explLevel == 1){
              pDiff = p1 - p2
            } else{
              pDiff = p2 - p1
            }

            v = cbind(v1, v2)

            v = ifelse(v == 1, input$respRefGroup, respOthGroup)

            v = as.data.frame(cbind(stat = pDiff, v))
            v$stat = as.numeric(v$stat)
          }
          else{ # Two-sample TEST Categorical
            n = as.numeric(table(explainVar)[1])

            theReplace = ifelse(input$randomMethod == "Reassign", FALSE, TRUE)

            v = mosaic::do(input$numberSamples) * mosaic::resample(responseVar)

            if(respLevel == 1){
              v = -1*(v - 2)
            } else {
              v = v - 1
            }

            p1 = rowMeans(v[, 1:n])
            p2 = rowMeans(v[(n + 1):N])

            if(explLevel == 1){
              pDiff = p1 - p2
            } else{
              pDiff = p2 - p1
            }

            v = ifelse(v == 1, input$respRefGroup, respOthGroup)

            v = as.data.frame(cbind(stat = pDiff, v))
            v$stat = as.numeric(v$stat)
          } # End of Two-sample TEST Categorical
        } # End of Two-Sample Categorical
        colnames(v) = c("mean", paste("v", 1:N, sep=""))
        return(v)
      } # END of Categorical Response Variable
    } #End of set.values()

    reset.values = function(){
      shiny::req(input$numberSamples)

      if(!is.null(states$values)){
        if(nrow(states$values) > 0){

          # Reset for Categorical Response Variable
          if(input$response_var != "NONE" & input$responseType == "Categorical"){
            if(input$explain_var == "NONE"){ # One-Sample
              theNullValue = input$nullMean
              shiny::updateNumericInput(session, "nullMean", value = 1 - theNullValue)
              states$values$mean = 1 - states$values$mean
            }
          }

          # Reset for a Two-Sample Quantitative Response Variable
          if(input$response_var != "NONE" & input$explain_var != "NONE"){
            states$values$mean = -1*states$values$mean
          }
          states$values = arrange(states$values, mean)
        } # nrow(states.values) > 0
      } # states.values != NULL

    } # End of reset.values()

    # FUNCTION TO PLOT A DISTRIBUTION OF SAMPLE MEANS
    dotPlotMeans <- function (x, sampleMean = 0,
                              xlim = NULL, main = NULL, xlab = NULL, ylab = NULL,
                              pch = 20, yaxis = FALSE, n = 1, cex = 1,
                              norm.outline = FALSE, yellow.dot = 1) {
      rawX = x

      boxTop = 17

      pch.unit = (strheight(pch, units = "inches") - 10)
      if (is.null(xlim)){
        (theTicks = pretty(range(x), n = 30)) # Changed from 20 to 30
        (xlim <- range(theTicks))
      }else{
        (theTicks = pretty(range(xlim), n = 30)) # Changed from 20 to 30
      }

      if (is.null(main))
        main <- ""
      if (is.null(xlab))
        xlab <- ""
      if (is.null(ylab))
        ylab <- ""

      the.x = xlim[2] - (xlim[2] - xlim[1])*0.25

      theAdjust = 1
      if(length(x) > 1){
        if(diff(range(x)) < 5){
          (theAdjust = diff(range(x))/50)
          x = x/theAdjust
        }
      }

      id = seq(1, length(x), 1)
      xdf = as.data.frame(cbind(x, id))
      x = x*theAdjust
      xdf = arrange(xdf, x)

      if(length(x) > 2){
        if(diff(range(x)) <= 0.5){
          xs = round(xdf$x, 3)
        }
        if(diff(range(x)) > 0.5 & diff(range(x)) <= 1){
          xs = round(xdf$x, 2)
        }
        if(diff(range(x)) > 1){
          xs = round(xdf$x, 1)
        }
      }else{
        xs = round(xdf$x, 3)
      }

      w <- table(xs)
      w <- unlist(lapply(w, function(n) {1:n}))
      mw <- max(w)
      Nmax <- floor(par()$pin[2]/pch.unit)
      top <- Nmax
      if (mw <= top) {
        plot(range(xs, na.rm = TRUE), c(0, 1), type = "n", xlab = "",
             ylab = "", xlim = xlim, main = main, axes = FALSE)
        yr <- c(0, Nmax)
        par(usr = c(par()$usr[1:2], yr[1], yr[2]))
        y <- pch.unit * w

        axis(side = 1, pos = 0, at = theTicks)
        if (xlab != "")
          axis(side = 1, at = 0.5 * sum(xlim), pos = -2, label = xlab,
               tick = FALSE)
        if (ylab != "")
          axis(side = 2, at = 0.5 * yr[2], line = 2, label = ylab,
               tick = FALSE)
        if (yaxis) {
          b <- max(1, ceiling(top/10))
          ll <- 1 + floor(top/b)
          at <- seq(0, by = b, length = ll)
          axis(side = 2, at = at)
        }

        xs = xs*theAdjust

        points(xs, y, pch = pch)
        lines(c(mean(x), mean(x)), c(0, mw + 1), col = "red")

        text(the.x, mw + 1, paste("n =", n), pos = 4)
        text(the.x, mw + 1 , paste(nrow(states$values), "Samples"), pos = 4)
        text(the.x, mw, paste("mean =", round(mean(x), 2)), pos = 4)
        text(the.x, mw - 1, paste("sd =", round(sd(x), 3)), pos = 4)
      } #END  if (mw <= top)
      else {
        nt <- mw + 1
        if(nt < 40){
          nt = 40
        }
        yr <- c(0, nt)
        plot(xs, w, pch = pch, xlab = "", ylab = "", xlim = xlim, ylim = yr, main = main, axes = FALSE, col = "white")
        pos <- 0
        axis(side = 1, pos = pos, at = theTicks)
        if (xlab != "")
          title(xlab = xlab)
        if (ylab != "")
          title(ylab = ylab)

        if (yaxis) {
          b <- max(1, ceiling(nt/10))
          ll <- 1 + floor(nt/b)
          at <- seq(0, by = b, length = ll)
          axis(side = 2, at = at)
        }
        xs = xs*theAdjust
        points(xs, w, pch = pch)

        the.top = mw
        if(mw < 40){
          the.top = 40
        }
        the.step = the.top/20

        # CONFIDENCE INTERVAL
        if(input$simType == "CI" & shiny::req(input$confLevel)){
          # If CI != "NONE", Highlight points in tails in Red
          if(input$distTail != "NONE"){
            confText = paste(input$confLevel, "%", sep = "")
            confWidth = strwidth(confText)
            fudge = diff(xlim)*0.005
            arrowY = 2.75
            lineHt = par("cxy")[2]
            theMax = length(xs)
            tailPct = 1 - input$confLevel/100
            pctWidth = strwidth(round(tailPct, 5))
            pctHeight = strheight(round(tailPct, 5))

            if(input$distTail == "Two-Tail"){
              tailPct = tailPct/2
              tailCount = round(tailPct*length(xs), 0)
              pctWidth = strwidth(round(tailPct, 5))
              pctHeight = strheight(round(tailPct, 5))

              if(tailCount > 0){
                # LOWER TAIL
                points(xs[which(xdf$id <= tailCount)], w[which(xdf$id <= tailCount)], pch = pch, col = "red")

                if(input$responseType == "Quantitative"){
                  theMeans = sort(states$values$mean)
                  mtext(text = round(theMeans[tailCount], 3), side = 1, line = 1,
                        at = xs[tailCount], adj = 1, col = "red")
                } else{
                  theProps = sort(as.numeric(states$values[, 1]))
                  mtext(text = round(theProps[tailCount], 3), side = 1, line = 1,
                        at = xs[tailCount], adj = 1, col = "red")
                }
                rect(xlim[1], the.top - (boxTop + 0.45)*the.step - pctHeight,
                     xlim[1] + 1.2*pctWidth, the.top - (boxTop + 0.15)*the.step, border = "red")
                text(xlim[1] + 0.6*pctWidth, the.top - boxTop*the.step,
                     round(tailPct, 5), col = "red", pos = 1)
                # UPPER TAIL
                points(xs[which(xdf$id >= (theMax - tailCount + 1))], w[which(xdf$id >= (theMax - tailCount + 1))], pch = pch, col = "red")

                if(input$responseType == "Quantitative"){
                  theMeans = sort(states$values$mean)
                  mtext(text = round(theMeans[(theMax - tailCount + 1)], 3), side = 1, line = 1,
                        at = xs[(theMax - tailCount)], adj = 0, col = "red")
                }else{
                  theProps = sort(as.numeric(states$values[, 1]))
                  mtext(text = round(theProps[(theMax - tailCount + 1)], 3), side = 1, line = 1,
                        at = xs[(theMax - tailCount)], adj = 0, col = "red")
                }
                rect(xlim[2] - 1.2*pctWidth, the.top - (boxTop + 0.45)*the.step - pctHeight,
                     xlim[2], the.top - (boxTop + 0.15)*the.step, border = "red")
                text(xlim[2] - 0.6*pctWidth, the.top - boxTop*the.step,
                     round(tailPct, 5), col = "red", pos = 1)
                #DRAW VERTICAL LINES DOWN TO THE LOWER AND UPPER LIMITS
                par(xpd = TRUE)

                mtext(text = confText, side = 1, line = 1, at = mean(x) + confWidth/2, adj = 1, col = "red")
                arrows(x0 = mean(x) - fudge - confWidth/2, y0 = -arrowY*lineHt, x1 = xs[tailCount] + fudge,
                       y1 = -arrowY*lineHt, angle = 90, length = 0.1, col = "blue")
                arrows(x0 = mean(x) + fudge + confWidth/2, y0 = -arrowY*lineHt, x1 = xs[theMax - tailCount] - fudge,
                       y1 = -arrowY*lineHt, angle = 90, length = 0.1, col = "blue")

                par(xpd = FALSE)
              }
            } # END OF Two-Tail
            else{ # One-Tail
              tailCount = round(tailPct*length(xs), 0)
              if(tailCount > 0){ # Left Tail
                if(input$distTail == "Left"){
                  points(xs[which(xdf$id <= tailCount)], w[which(xdf$id <= tailCount)], pch = pch, col = "red")

                  if(input$responseType == "Quantitative"){
                    theMeans = sort(states$values$mean)
                    mtext(text = round(theMeans[tailCount], 3), side = 1, line = 1,
                          at = xs[tailCount], adj = 1, col = "red")
                  } else{
                    theProps = sort(as.numeric(states$values[, 1]))
                    mtext(text = round(theProps[tailCount], 3), side = 1, line = 1,
                          at = xs[tailCount], adj = 1, col = "red")
                  }
                  mtext(text = expression(infinity), side = 1, line = 1, at = xlim[2], adj = 0, col = "red")
                  #DRAW VERTICAL LINES DOWN TO THE LOWER LIMIT
                  par(xpd = TRUE)
                  arrows(x0 = mean(x) - fudge - confWidth/2, y0 = -arrowY*lineHt, x1 = xs[tailCount] + fudge,
                         y1 = -arrowY*lineHt, angle = 90, length = 0.1, col = "blue")
                  arrows(x0 = mean(x) + fudge + confWidth/2, y0 = -arrowY*lineHt, x1 = xlim[2] - fudge,
                         y1 = -arrowY*lineHt, angle = 30, length = 0.1, col = "blue")
                  par(xpd = FALSE)

                  rect(xlim[1], the.top - (boxTop + 0.45)*the.step - pctHeight,
                       xlim[1] + 1.2*pctWidth, the.top - (boxTop + 0.15)*the.step, border = "red")
                  text(xlim[1] + 0.6*pctWidth, the.top - boxTop*the.step,
                       round(tailPct, 5), col = "red", pos = 1)
                } # END OF Left
                else{ # Right Tail
                  points(xs[which(xdf$id >= (theMax - tailCount + 1))], w[which(xdf$id >= (theMax - tailCount + 1))], pch = pch, col = "red")

                  if(input$responseType == "Quantitative"){
                    theMeans = sort(states$values$mean)
                    mtext(text = round(theMeans[(theMax - tailCount + 1)], 3), side = 1, line = 1,
                          at = xs[(theMax - tailCount)], adj = 0, col = "red")
                  } else{
                    theProps = sort(as.numeric(states$values[, 1]))
                    mtext(text = round(theProps[(theMax - tailCount + 1)], 3), side = 1, line = 1,
                          at = xs[(theMax - tailCount)], adj = 0, col = "red")
                  }
                  mtext(text = expression(-infinity), side = 1, line = 1, at = xlim[1], adj = 1, col = "red")
                  #DRAW VERTICAL LINES DOWN TO THE UPPER LIMIT
                  par(xpd = TRUE)
                  arrows(x0 = mean(x) - fudge - confWidth/2, y0 = -arrowY*lineHt, x1 = xlim[1] + fudge,
                         y1 = -arrowY*lineHt, angle = 30, length = 0.1, col = "blue")
                  arrows(x0 = mean(x) + fudge + confWidth/2, y0 = -arrowY*lineHt, x1 = xs[theMax - tailCount] - fudge,
                         y1 = -arrowY*lineHt, angle = 90, length = 0.1, col = "blue")
                  par(xpd = FALSE)

                  rect(xlim[2] - 1.2*pctWidth, the.top - (boxTop + 0.45)*the.step - pctHeight,
                       xlim[2], the.top - (boxTop + 0.15)*the.step, border = "red")
                  text(xlim[2] - 0.6*pctWidth, the.top - boxTop*the.step,
                       round(tailPct, 5), col = "red", pos = 1)
                } # End of Right
                mtext(text = paste(input$confLevel, "%", sep = ""), side = 1, line = 1,
                      at = mean(xs) + confWidth/2, adj = 1, col = "red")
              }
            } # END OF One-Tail
          }
        }# END OF INDICATE CONFIDENCE INTERVAL

        # HYPOTHESIS TEST
        if(input$simType == "TEST"){
          # If TEST != "NONE", Highlight points in tails in Red
          if(input$distTail != "NONE"){

            arrowY = 2.35
            lineHt = par("cxy")[2]
            theMax = length(xs)

            if(input$explain_var == "NONE"){
              if(input$responseType == "Quantitative"){ # Quant alt hyp tail labels
                #                leftTail = paste("\u03bc <", input$nullMean)
                #                twoTail =  paste("\u03bc ≠", input$nullMean)
                #                rightTail = paste("\u03bc >", input$nullMean)

                leftTail = paste("\u03bc \u003c", input$nullMean)
                twoTail =  paste("\u03bc \u2260", input$nullMean)
                rightTail = paste("\u03bc \u003e", input$nullMean)
              }
              else{ # Categorical alt hyp tail labels
                #                leftTail = paste("p <", input$nullMean)
                #                twoTail =  paste("p ≠", input$nullMean)
                #                rightTail = paste("p >", input$nullMean)

                leftTail = paste("p \u003c", input$nullMean)
                twoTail =  paste("p \u2260", input$nullMean)
                rightTail = paste("p \u003e", input$nullMean)
              }
            } else{ # Two-Sample
              if(input$responseType == "Quantitative"){ # Quant alt hyp tail labels
                #                leftTail = paste("\u03bc1 - \u03bc2 <", input$nullMean)
                #                twoTail =  paste("\u03bc1 - \u03bc2 ≠", input$nullMean)
                #                rightTail = paste("\u03bc1 - \u03bc2 >", input$nullMean)

                leftTail = paste("\u03bc1 - \u03bc2 \u003c", input$nullMean)
                twoTail =  paste("\u03bc1 - \u03bc2 \u2260", input$nullMean)
                rightTail = paste("\u03bc1 - \u03bc2 \u003e", input$nullMean)
              }
              else{ # Categorical alt hyp tail labels
                #                leftTail = paste("p1 - p2 <", input$nullMean)
                #                twoTail =  paste("p1 - p2 ≠", input$nullMean)
                #                rightTail = paste("p1 - p2 >", input$nullMean)

                leftTail = paste("p1 - p2 \u003c", input$nullMean)
                twoTail =  paste("p1 - p2 \u2260", input$nullMean)
                rightTail = paste("p1 - p2 \u003e", input$nullMean)
              }
            }

            nullDiff = abs(sampleMean - input$nullMean)
            leftPlace = NULL
            rightPlace = NULL
            leftPvalue = 0
            rightPvalue = 0

            if(min(rawX) <= (input$nullMean - nullDiff)){
              leftPlace = sum(x <= (input$nullMean - nullDiff))
              leftPvalue = mean(rawX <= (input$nullMean - nullDiff))
            } else{
              leftPlace = 1
            }
            if(max(rawX) >= (input$nullMean + nullDiff)){
              rightPlace = length(x) - sum(x >= (input$nullMean + nullDiff)) + 1
              rightPvalue = mean(rawX >= (input$nullMean + nullDiff))
            } else{
              rightPlace = length(x)
            }

            # LOWER TAIL
            if(input$distTail == twoTail | input$distTail == leftTail){

              if(!is.null(leftPlace)){
                if(leftPvalue > 0){
                  points(xs[which(xdf$id <= leftPlace)], w[which(xdf$id <= leftPlace)], pch = pch, col = "red")
                }
                textWidth = strwidth(round(input$nullMean - nullDiff, 3))
                par(xpd = TRUE)
                mtext(text = round(input$nullMean - nullDiff, 3), side = 1, line = 1,
                      at = input$nullMean - nullDiff - textWidth/2, adj = 0, col = "red")
                arrows(x0 = input$nullMean - nullDiff, y0 = 0.2 - arrowY*lineHt, x1 = input$nullMean - nullDiff,
                       y1 = -0.2, angle = 45, length = 0.1, col = "red")
                par(xpd = FALSE)
                leftWidth = strwidth(round(leftPvalue, 5))
                leftHeight = strheight(round(leftPvalue, 5))
                rect(xlim[1], the.top - (boxTop + 0.45)*the.step - leftHeight,
                     xlim[1] + 1.2*leftWidth, the.top - (boxTop + 0.15)*the.step, border = "red")
                text(xlim[1] + 0.6*leftWidth, the.top - boxTop*the.step,
                     round(leftPvalue, 5), col = "red", pos = 1)
              }
            }
            # UPPER TAIL
            if(input$distTail == twoTail | input$distTail == rightTail){
              if(!is.null(rightPlace)){
                if(rightPvalue > 0){
                  points(xs[which(xdf$id >= rightPlace)], w[which(xdf$id >= rightPlace)], pch = pch, col = "red")
                }
                textWidth = strwidth(round(input$nullMean + nullDiff, 3))
                par(xpd = TRUE)
                mtext(text = round(input$nullMean + nullDiff, 3), side = 1, line = 1,
                      at = input$nullMean + nullDiff - textWidth/2, adj = 0, col = "red")
                arrows(x0 = input$nullMean + nullDiff, y0 = 0.2 - arrowY*lineHt, x1 = input$nullMean + nullDiff,
                       y1 = -0.2, angle = 45, length = 0.1, col = "red")
                par(xpd = FALSE)
                rightWidth = strwidth(round(rightPvalue, 5))
                rightHeight = strheight(round(rightPvalue, 5))
                rect(xlim[2] - 1.2*rightWidth, the.top - (boxTop + 0.45)*the.step - rightHeight,
                     xlim[2], the.top - (boxTop + 0.15)*the.step, border = "red")
                text(xlim[2] - 0.6*rightWidth, the.top - boxTop*the.step,
                     round(rightPvalue, 5), col = "red", pos = 1)
              }
            } # END OF UPPER TAIL

            if(input$distTail != "NONE" & (!is.null(leftPvalue) | !is.null(rightPvalue))){
              thePvalue = 0
              if(input$distTail == leftTail){
                thePvalue = leftPvalue
              }
              if(input$distTail == rightTail){
                thePvalue = rightPvalue
              }
              if(input$distTail == twoTail){
                thePvalue = leftPvalue + rightPvalue
              }
              text(xlim[2], the.top - 5*the.step,
                   paste("p-value =", round(thePvalue, 5)), pos = 2)
            }

          }
        }# END OF INDICATE P-VALUE

        # Plot the mean for the selected sample in yellow
        points(xs[which(xdf$id == yellow.dot)], w[which(xdf$id == yellow.dot)], pch = 19, col = "black")
        points(xs[which(xdf$id == yellow.dot)], w[which(xdf$id == yellow.dot)], pch = pch, col = "yellow")

        # Draw vertical line to indicate location of the mean
        lines(c(mean(x), mean(x)), c(0, mw + 1), col = "red", lwd = 2)

        # Report the Statistics for the Distribution

        se.text = paste("Estimated SE =", round(sd(x), 3))
        se.width = strwidth(se.text)

        text(xlim[2] - 0.5*se.width, the.top , paste(nrow(states$values), "Samples"), pos = 1)

        text(xlim[2], the.top - 2.0*the.step , paste("mean =", round(mean(x), 3)), pos = 2)
        text(xlim[2], the.top - 3.2*the.step, se.text, pos = 2)

        if(input$simType == "CI" & input$distTail != "NONE"){
          text(xlim[2] - 0.5*se.width, the.top - 5.0*the.step,
               paste(input$confLevel, "% CI: SE Method", sep = ""), pos = 1)
          tailPct = (1 - input$confLevel/100)/2

          if(input$distTail == "Two-Tail"){
            low.limit = round(sampleMean + qnorm(tailPct)*sd(x), 3)
            hi.limit = round(sampleMean + qnorm(1 - tailPct)*sd(x), 3)
          }
          if(input$distTail == "Left"){
            low.limit = round(sampleMean + qnorm(2*tailPct)*sd(x), 3)
            hi.limit = "\u221e"
          }
          if(input$distTail == "Right"){
            low.limit = "-\u221e"
            hi.limit = round(sampleMean + qnorm(1 - 2*tailPct)*sd(x), 3)
          }

          ci.text = paste("[", low.limit, ", ", hi.limit, "]", sep = "")
          text(xlim[2] - 0.5*se.width, the.top -6*the.step, ci.text, pos = 1)

        }

        if(norm.outline){
          inc = (xlim[2] - xlim[1])/100
          norm.x = seq(xlim[1], xlim[2], inc)
          norm.y = dnorm(norm.x, mean = mean(xs), sd = sd(xs))
          norm.y = norm.y*(mw/max(norm.y))
          lines(norm.x, norm.y, lwd = 3, col = "blue")
        }
      }

      states$meanPoints = as.data.frame(cbind(x = xs, y = w, id = 1:length(xs)))
    } #END dotPlotMeans()

    set.null.label = function(){
      if(input$explain_var == "NONE"){
        if(input$responseType == "Quantitative"){
          shiny::updateNumericInput(session, "nullMean", label = "\u03bc \u003d", value = 0, step = 1)
        } else{
          shiny::updateNumericInput(session, "nullMean", label = "p \u003d",
                             value = 0.5, min = 0, max = 1, step = 0.01)
        }
      } else{
        if(input$responseType == "Quantitative"){
          shiny::updateNumericInput(session, "nullMean", label = "\u03bc1 - \u03bc2 \u003d",
                             value = 0, min = 0, max = 0, step = 0)
        } else{
          shiny::updateNumericInput(session, "nullMean", label = "p1 - p2 \u003d",
                             value = 0, min = 0, max = 0, step = 0)
        }
      }
    } # END of set.null.label()

    set.selected.tail = function(){
      if(input$simType == "TEST"){
        tailLabel = "Alternative Hypothesis"
        if(input$explain_var == "NONE"){
          if(input$responseType == "Quantitative"){
            tailChoices = c("NONE", paste("\u03bc \u003c", input$nullMean),
                            paste("\u03bc \u2260", input$nullMean),
                            paste("\u03bc \u003e", input$nullMean))
          } else{
            tailChoices = c("NONE", paste("p \u003c", input$nullMean),
                            paste("p \u2260", input$nullMean),
                            paste("p \u003e", input$nullMean))
          }
        } else{
          if(input$responseType == "Quantitative"){
            tailChoices = c("NONE", paste("\u03bc1 - \u03bc2 \u003c", input$nullMean),
                            paste("\u03bc1 - \u03bc2 \u2260", input$nullMean),
                            paste("\u03bc1 - \u03bc2 \u003e", input$nullMean))
          } else{
            tailChoices = c("NONE", paste("p1 - p2 \u003c", input$nullMean),
                            paste("p1 - p2 \u2260", input$nullMean),
                            paste("p1 - p2 \u003e", input$nullMean))
          }
        }
      } else{
        tailLabel = "Confidence Interval Tail: Percentile Method"
        tailChoices = c("NONE", "Left", "Two-Tail", "Right")
      }
      shiny::updateRadioButtons(session, inputId = "distTail", selected = tailChoices[states$selectedTail],
                         label = tailLabel,
                         choices = tailChoices, inline = TRUE)
    } # END of set.selected.tail()

    # OBSERVERS for Reactive Objects

    shiny::observeEvent(input$addMore, {
      if(input$response_var != "NONE"){
        states$resample = TRUE
        responsePlace = which(names(df()) == input$response_var)
        states$values = rbind(states$values, set.values())
        states$values = dplyr::arrange(states$values, mean)
        states$sampleSelect = 1
      }
    })

    shiny::observeEvent(input$resetDist, {
      states$reset = TRUE
      states$resample = TRUE
      states$sampleSelect = 1
    })

    shiny::observeEvent(input$distTail,{
      if(input$simType == "TEST"){
        if(input$explain_var == "NONE"){
          if(input$responseType == "Quantitative"){
            tailChoices = c("NONE", paste("\u03bc \u003c", input$nullMean),
                            paste("\u03bc \u2260", input$nullMean),
                            paste("\u03bc \u003e", input$nullMean))
          } else{
            tailChoices = c("NONE", paste("p \u003c", input$nullMean),
                            paste("p \u2260", input$nullMean),
                            paste("p \u003e", input$nullMean))
          }
        } else{
          if(input$responseType == "Quantitative"){
            tailChoices = c("NONE", paste("\u03bc1 - \u03bc2 \u003c", input$nullMean),
                            paste("\u03bc1 - \u03bc2 \u2260", input$nullMean),
                            paste("\u03bc1 - \u03bc2 \u003e", input$nullMean))
          } else{
            tailChoices = c("NONE", paste("p1 - p2 \u003c", input$nullMean),
                            paste("p1 - p2 \u2260", input$nullMean),
                            paste("p1 - p2 \u003e", input$nullMean))
          }
        }
      } else{
        tailChoices = c("NONE", "Left", "Two-Tail", "Right")
      }
      for(i in 1:length(tailChoices)){
        if(input$distTail == tailChoices[i]){
          states$selectedTail = i
        }
      }
    })

    shiny::observeEvent(input$respRefGroup, {
      states$doNotReset = TRUE
      reset.values()
    })

    shiny::observeEvent(input$explRefGroup, {
      states$doNotReset = TRUE
      reset.values()
    })

    shiny::observeEvent(input$responseType, {
      states$reset = TRUE
      states$resample = TRUE
      states$sampleSelect = 1
      if(input$simType == "CI"){
        shiny::updateRadioButtons(session, inputId = "distTail", selected = "NONE",
                           label = "Confidence Interval Tail: Percentile Method",
                           choices = c("NONE", "Left", "Two-Tail", "Right"), inline = TRUE)
      }
      else{ # HYPOTHESIS TEST
        set.selected.tail()
        set.null.label()
      }
    })

    shiny::observeEvent(input$simType, {
      states$reset = TRUE
      states$resample = TRUE
      states$sampleSelect = 1
      if(input$simType == "CI"){
        shiny::updateRadioButtons(session, inputId = "distTail", selected = "NONE",
                           label = "Confidence Interval Tail: Percentile Method",
                           choices = c("NONE", "Left", "Two-Tail", "Right"), inline = TRUE)
      }
      if(input$simType == "TEST"){
        set.selected.tail()
        set.null.label()
      }
    })

    shiny::observeEvent(input$numberSamples, {
      shiny::req(input$numberSamples)
      if(input$numberSamples > 10000){
        shiny::updateNumericInput(session, "numberSamples", value = 10000)
      }
    })

    shiny::observeEvent(input$confLevel, {
      shiny::req(input$confLevel)
      if(input$confLevel < 1){
        shiny::updateNumericInput(session, "confLevel", value = 1)
      }
      if(input$confLevel > 99){
        shiny::updateNumericInput(session, "confLevel", value = 99)
      }
    })

    shiny::observeEvent(input$nullMean, {
      if(input$simType == "TEST"){
        if(input$explain_var == "NONE"){
          if(states$doNotReset == FALSE){
            states$reset = TRUE
            states$resample = TRUE
            states$sampleSelect = 1
          }
        } else{
          if(input$nullMean != 0){
            shiny::updateNumericInput(session, "nullMean", value = 0)
          }
        }
        set.selected.tail()
        states$doNotReset = FALSE
      }
    })

    shiny::observeEvent(input$randomMethod, {
      states$reset = TRUE
      states$resample = TRUE
      states$sampleSelect = 1
    })

    df <- shiny::reactive({
      shiny::req(input$data_file)
      read.csv(input$data_file$datapath, as.is = FALSE)
    })

    observe({
      input$data_file
      states$values = NULL
      response_list = NULL
      explain_list = NULL

      for(i in 1:length(names(df()))){
        if(class(df()[,i]) == "numeric" | class(df()[,i]) == "integer"){
          response_list = c(response_list, names(df())[i])
        }
        if(class(df()[,i]) == "factor" & length(levels(df()[,i])) == 2){
          explain_list = c(explain_list, names(df())[i])
        }
      }
      if(input$responseType == "Quantitative"){
        shiny::updateSelectInput(session, "response_var", choices = c("NONE", response_list))
      }
      if(input$responseType == "Categorical"){
        shiny::updateSelectInput(session, "response_var", choices = c("NONE", explain_list))
      }
      shiny::updateSelectInput(session, "explain_var", choices = c("NONE", explain_list))
      shiny::updateRadioButtons(session, inputId = "distTail", selected = "NONE")
    })

    shiny::observeEvent(input$response_var,{
      states$values = NULL
      if(input$response_var != "NONE" & input$responseType == "Categorical"){
        responsePlace = which(names(df()) == input$response_var)
        response_levels = levels(df()[, responsePlace])
        shiny::updateRadioButtons(session, inputId = "respRefGroup", choices = response_levels,
                           selected = response_levels[1], inline = TRUE)
      }

      if(input$simType == "TEST"){
        set.selected.tail()
        set.null.label()
      }

      states$sampleSelect = 1
      shiny::updateRadioButtons(session, inputId = "distTail", selected = "NONE")
    })

    shiny::observeEvent(input$explain_var,{
      states$values = NULL
      if(input$explain_var != "NONE"){
        explainPlace = which(names(df()) == input$explain_var)
        explain_levels = levels(df()[, explainPlace])
        shiny::updateRadioButtons(session, inputId = "explRefGroup", choices = explain_levels,
                           selected = explain_levels[1], inline = TRUE)
      }

      if(input$simType == "TEST"){
        set.selected.tail()
        set.null.label()
      }

      states$sampleSelect = 1
      shiny::updateRadioButtons(session, inputId = "distTail", selected = "NONE")
    })

    # PLOT THE DISTRIBUTION OF OBSERVED VALUES
    output$observedPlot <- shiny::renderPlot({

      if(input$response_var == "NONE" & input$explain_var == "NONE"){
        plot(c(0,1), c(0,1), col = "white", col.axis = "white", col.lab = "white",
             xaxt = "n", yaxt = "n", main = "Observed Data Distribution")
      }

      if(input$response_var != "NONE"){
        tempData = df()

        if(input$response_var != "NONE" & input$explain_var == "NONE"){
          responsePlace = which(names(df()) == input$response_var)
          tempData = subset(tempData, !is.na(tempData[, responsePlace]))
          responseVar = tempData[, responsePlace]
        }

        if(input$response_var != "NONE" & input$explain_var != "NONE"){
          responsePlace = which(names(df()) == input$response_var)
          explainPlace = which(names(df()) == input$explain_var)
          tempData = subset(tempData, !is.na(tempData[, responsePlace]) & !is.na(tempData[, explainPlace]))
          responseVar = tempData[, responsePlace]
          explainVar = tempData[, explainPlace]
        }

        # CATEGORICAL Response Variable
        if(input$responseType == "Categorical"){
          # One-Sample
          if(input$response_var != "NONE" & input$explain_var == "NONE"){

            if(class(tempData[, responsePlace]) == "factor"){
              theProps = prop.table(table(tempData[, responsePlace]))

              group1col = ifelse(input$respRefGroup == levels(tempData[, responsePlace])[1], "darkgrey", "lightgrey")
              group2col = ifelse(input$respRefGroup == levels(tempData[, responsePlace])[2], "darkgrey", "lightgrey")

              barPoints = barplot(theProps, xlab = input$response_var, ylab = "Proportion",
                                  main = paste("Observed Data: \n n =", length(tempData[, responsePlace])),
                                  col = c(group1col, group2col), names.arg = c("", ""))

              group1font = ifelse(input$respRefGroup == levels(tempData[, responsePlace])[1], 2, 1)
              group2font = ifelse(input$respRefGroup == levels(tempData[, responsePlace])[2], 2, 1)
              mtext(text = levels(tempData[, responsePlace])[1], side = 1, line = 1,
                    at = 0.75, adj = 1, col = "black", font = group1font)
              mtext(text = levels(tempData[, responsePlace])[2], side = 1, line = 1,
                    at = 2, adj = 1, col = "black", font = group2font)

              text(barPoints[1], theProps[1], round(theProps[1], 3), pos = 1, font = group1font)
              text(barPoints[2], theProps[2], round(theProps[2], 3), pos = 1, font = group2font)
            }
          }

          # Two-Sample
          if(input$response_var != "NONE" & input$explain_var != "NONE"){

            if(class(tempData[, responsePlace]) == "factor"){
              theTable = table(tempData[, explainPlace], tempData[, responsePlace])
              theProp = prop.table(theTable, margin = 1)

              if(input$explRefGroup == levels(tempData[, explainPlace])[1]){
                theProp = rbind(theProp, theProp[1,] - theProp[2,])
              } else{
                theProp = rbind(theProp, theProp[2,] - theProp[1,])
              }

              theProp = round(theProp, 3)
              theProp = cbind(theProp, c(as.numeric(table(tempData[, explainPlace])), 0))

              plot(c(0, 1), c(0, 0.5), ann = F, yaxt = "n", xaxt = "n", col = "white")
              title(main = "Observed Sample")

              text(0.625, 0.5, input$response_var, pos = 1)
              text(0.425, 0.362, input$explain_var, pos = 2)
              colHead = c(levels(tempData[, responsePlace]), "N")
              rowHead = c(levels(tempData[, explainPlace]), "Difference")
              colPlace = c(0.525, 0.725, 0.95)
              rowPlace = c(0.3, 0.2, 0.1)
              # Print the column and row headings
              if(length(colHead) > 1){
                for(i in 1:3){
                  colFontType = 1
                  if(colHead[i] == input$respRefGroup){
                    colFontType = 2
                  }
                  rowFontType = 1
                  if(rowHead[i] == input$explRefGroup){
                    rowFontType = 2
                  }
                  text(colPlace[i], 0.4, colHead[i], pos = 1, font = colFontType)
                  text(0.425, rowPlace[i] - 0.038, rowHead[i], pos = 2, font = rowFontType)
                }
              }
              # Print the proportions
              for(c in 1:2){
                for(r in 1:2){
                  text(colPlace[c], rowPlace[r], theProp[r, c], pos = 1)
                }
              }
              # Print the sample sizes
              for(i in 1:2){
                text(colPlace[3] + 0.025, rowPlace[i], theProp[i, 3], pos = 1)
              }
              # Print the difference between sample proportions for the Reference Group
              if(length(levels(tempData[, responsePlace])) > 0){
                if(input$respRefGroup == levels(tempData[, responsePlace])[1]){
                  text(colPlace[1], rowPlace[3], theProp[3, 1], pos = 1, font = 2)
                } else{
                  text(colPlace[2], rowPlace[3], theProp[3, 2], pos = 1, font = 2)
                }
              }
            } # End of if(responseVar == factor)
          } # End of Tw-Sample Categorical

        } # End of responseType == "Categorical"

        # QUANTITATIVE Response Variable
        if(input$responseType == "Quantitative"){

          if(input$response_var != "NONE" & input$explain_var == "NONE"){ # One-Sample

            if(class(tempData[, responsePlace]) != "factor"){
              BHH2::dotPlot(tempData[, responsePlace], xlab = input$response_var,
                            main = paste("Observed Data: \n n = ", length(tempData[, responsePlace]),
                                         ", mean = ", round(mean(tempData[, responsePlace]),2),
                                         ", sd = ", round(sd(tempData[, responsePlace]),3), sep = ""), pch = 20)
              abline(v = mean(tempData[, responsePlace]), col = "red", lwd = 2)
            }
          }

          if(input$response_var != "NONE" & input$explain_var != "NONE"){ # Two-Sample

            par(mfrow = c(2, 1))

            if(class(tempData[, responsePlace]) != "factor"){
              theMin = min(tempData[, responsePlace])
              theMax = max(tempData[, responsePlace])

              theData = subset(tempData, tempData[, explainPlace] == levels(tempData[, explainPlace])[1])
              meanDiff = mean(theData[, responsePlace])
              BHH2::dotPlot(theData[, responsePlace], xlab = input$response_var,
                            main = paste("Observed: ", input$explain_var, " = ", levels(tempData[, explainPlace])[1],
                                         "\n n = ", length(theData[, responsePlace]),
                                         ", mean = ", round(mean(theData[, responsePlace]),2),
                                         ", sd = ", round(sd(theData[, responsePlace]),3), sep = ""), pch = 20,
                            cex =  cex.s, cex.axis = cex.s, cex.lab = cex.s, cex.main = cex.s,
                            xlim = c(theMin, theMax))
              abline(v = mean(theData[, responsePlace]), col = "red", lwd = 2)

              theData = subset(tempData, tempData[, explainPlace] == levels(tempData[, explainPlace])[2])

              if(input$explRefGroup == levels(tempData[, explainPlace])[1]){
                meanDiff = meanDiff - mean(theData[, responsePlace])
              } else{
                meanDiff = mean(theData[, responsePlace]) - meanDiff
              }

              BHH2::dotPlot(theData[, responsePlace], xlab = input$response_var,
                            main = paste("Observed: ", input$explain_var, " = ", levels(tempData[, explainPlace])[2],
                                         "\n n = ", length(theData[, responsePlace]),
                                         ", mean = ", round(mean(theData[, responsePlace]),2),
                                         ", sd = ", round(sd(theData[, responsePlace]),3), sep = ""), pch = 20,
                            cex =  cex.s, cex.axis = cex.s, cex.lab = cex.s, cex.main = cex.s,
                            xlim = c(theMin, theMax))
              abline(v = mean(theData[, responsePlace]), col = "red", lwd = 2)

              meanText = paste("Mean Difference = ", round(meanDiff, 3))
              meanWidth = strwidth(meanText)
              midPlace = (max(theData[, responsePlace]) - min(theData[, responsePlace]))/2 +
                min(theData[, responsePlace])
              mtext(text = meanText,
                    side = 3, line = 3.5, at = midPlace + meanWidth/2,
                    adj = 1, col = "blue", font = 2)
            } # End of if(responseVar != factor)
          } # End of Two-sample
        } # End of responseType == "Quantitative"
      } # End of response_var != "NONE"
    }) # End of plot observedPlot

    # PLOT THE SELECTED RESAMPLE DISTRIBUTION
    output$resamplePlot <- shiny::renderPlot({

      if(input$response_var != "NONE"){
        thePoints = shiny::nearPoints(states$meanPoints, input$meanPlotHover,
                               xvar = "x", yvar = "y")
        if(!is.na(thePoints[1, "id"])){states$sampleSelect = thePoints[1, "id"]}

        tempData = df()

        if(input$response_var != "NONE" & input$explain_var == "NONE"){
          responsePlace = which(names(df()) == input$response_var)
          tempData = subset(tempData, !is.na(tempData[, responsePlace]))
          responseVar = tempData[, responsePlace]
        }

        if(input$response_var != "NONE" & input$explain_var != "NONE"){
          responsePlace = which(names(df()) == input$response_var)
          explainPlace = which(names(df()) == input$explain_var)
          tempData = subset(tempData, !is.na(tempData[, responsePlace]) & !is.na(tempData[, explainPlace]))
          responseVar = tempData[, responsePlace]
          explainVar = tempData[, explainPlace]
        }

        if(input$simType == "CI"){
          theType = "Bootstrap"
        } else{
          theType = "Randomization"
        }

        if(is.null(states$values)){
          if(input$simType == "CI"){
            mainTitle = "Bootstrap Sample Distribution"
          } else{
            mainTitle = "Randomization Sample Distribution"
          }
          plot(c(0,1), c(0,1), col = "white", col.axis = "white", col.lab = "white",
               xaxt = "n", yaxt = "n", main = mainTitle)
        }

        # QUANTITATIVE Response Variable
        if(input$responseType == "Quantitative"){
          if(input$response_var != "NONE" & input$explain_var == "NONE" & !is.null(states$values)){ # One-Sample

            if(class(tempData[, responsePlace]) != "factor"){
              n = nrow(tempData)
              theMin = min(tempData[, responsePlace])
              theMax = max(tempData[, responsePlace])
              theData = as.numeric(states$values[states$sampleSelect, 2:(1+n)])

              BHH2::dotPlot(x = theData, xlab = input$response_var,
                            main = paste(theType, " Sample #", states$sampleSelect, ": \n n = ", n,
                                         ", mean = ", round(mean(theData), 2),
                                         ", sd = ", round(sd(theData), 3), sep = ""),
                            pch = 20, xlim = c(theMin, theMax))
              abline(v = mean(theData), col = "red", lwd = 2)
            } # End of if(responseVar != factor)
          } # End of One-Sample
          if(input$response_var != "NONE" & input$explain_var != "NONE" & !is.null(states$values)){ # Two-Sample

            if(class(tempData[, responsePlace]) != "factor"){
              par(mfrow = c(2, 1))

              theMin = min(tempData[, responsePlace])
              theMax = max(tempData[, responsePlace])

              n = nrow(tempData)
              sampleData = as.numeric(states$values[states$sampleSelect, 2:(1+n)])

              count1 = sum(tempData[, explainPlace] == levels(tempData[, explainPlace])[1])

              theData = sampleData[1:count1]
              meanDiff = mean(theData)
              BHH2::dotPlot(theData, xlab = input$response_var,
                            main = paste(theType, " #", states$sampleSelect, ": ",
                                         input$explain_var, " = ", levels(tempData[, explainPlace])[1],
                                         "\n n = ", length(theData),
                                         ", mean = ", round(mean(theData),2),
                                         ", sd = ", round(sd(theData),3), sep = ""), pch = 20,
                            cex =  cex.s, cex.axis = cex.s, cex.lab = cex.s, cex.main = cex.s,
                            xlim = c(theMin, theMax))
              abline(v = mean(theData), col = "red", lwd = 2)

              par(xpd = TRUE)
              lines(x = c(theMin, theMax), y = c(4.5, 4.5), col = "red", lwd = 3)
              par(xpd = FALSE)

              theData = sampleData[(count1 + 1):n]

              if(input$explRefGroup == levels(tempData[, explainPlace])[1]){
                meanDiff = meanDiff - mean(theData)
              } else{
                meanDiff =  mean(theData) - meanDiff
              }

              BHH2::dotPlot(theData, xlab = input$response_var,
                            main = paste(theType, " #", states$sampleSelect, ": ",
                                         input$explain_var, " = ", levels(tempData[, explainPlace])[2],
                                         "\n n = ", length(theData),
                                         ", mean = ", round(mean(theData),2),
                                         ", sd = ", round(sd(theData),3), sep = ""), pch = 20,
                            cex =  cex.s, cex.axis = cex.s, cex.lab = cex.s, cex.main = cex.s,
                            xlim = c(theMin, theMax))
              abline(v = mean(theData), col = "red", lwd = 2)

              meanText = paste(theType, "Mean Difference = ", round(meanDiff, 3))
              meanWidth = strwidth(meanText)
              midPlace = (max(theData) - min(theData))/2 + min(theData)
              mtext(text = meanText,
                    side = 3, line = 3.5, at = midPlace + meanWidth/2,
                    adj = 1, col = "blue", font = 2)
            } # End of if(responseVar != factor)
          } # End of Two-Sample Quantitative
        } #END of Quantitative Response Variable

        # CATEGORICAL Response Variable
        if(input$responseType == "Categorical"){
          # One-Sample
          if(input$response_var != "NONE" & input$explain_var == "NONE" & !is.null(states$values)){

            if(class(tempData[, responsePlace]) == "factor"){
              n = nrow(tempData)
              theData = as.character(states$values[states$sampleSelect, 2:(1+n)])

              theProps = prop.table(table(theData))

              if(length(names(theProps)) == 1){
                missGroup = as.table(0)
                if(names(theProps) == levels(tempData[, responsePlace])[1]){
                  names(missGroup) = levels(tempData[, responsePlace])[2]
                  newProps = as.table(cbind(theProps, missGroup))
                  colnames(newProps) = c(names(theProps), names(missGroup))
                  rownames(newProps) = c("")
                }
                if(names(theProps) == levels(tempData[, responsePlace])[2]){
                  names(missGroup) = levels(tempData[, responsePlace])[1]
                  newProps = as.table(cbind(missGroup, theProps))
                  colnames(newProps) = c(names(missGroup), names(theProps))
                  rownames(newProps) = c("")
                }
                theProps = newProps
              }

              group1col = ifelse(input$respRefGroup == levels(tempData[, responsePlace])[1], "darkgrey", "lightgrey")
              group2col = ifelse(input$respRefGroup == levels(tempData[, responsePlace])[2], "darkgrey", "lightgrey")

              barPoints = barplot(theProps, xlab = input$response_var, ylab = "Proportion",
                                  main = paste(theType, " Sample ", states$sampleSelect, ": \n n =",
                                               length(tempData[, responsePlace]), sep = ""),
                                  col = c(group1col, group2col), names.arg = c("", ""))

              group1font = ifelse(input$respRefGroup == levels(tempData[, responsePlace])[1], 2, 1)
              group2font = ifelse(input$respRefGroup == levels(tempData[, responsePlace])[2], 2, 1)
              mtext(text = levels(tempData[, responsePlace])[1], side = 1, line = 1,
                    at = 0.75, adj = 1, col = "black", font = group1font)
              mtext(text = levels(tempData[, responsePlace])[2], side = 1, line = 1,
                    at = 2, adj = 1, col = "black", font = group2font)

              text(barPoints[1], theProps[1], round(theProps[1], 3), pos = 1, font = group1font)
              text(barPoints[2], theProps[2], round(theProps[2], 3), pos = 1, font = group2font)
            } # End of if(responseVar == factor)
          }
          # Two-Sample
          if(input$response_var != "NONE" & input$explain_var != "NONE" & !is.null(states$values)){

            if(class(tempData[, responsePlace]) == "factor"){
              n = nrow(tempData)
              sampleData = as.character(states$values[states$sampleSelect, 2:(1+n)])

              count1 = sum(tempData[, explainPlace] == levels(tempData[, explainPlace])[1])
              count2 = sum(tempData[, explainPlace] == levels(tempData[, explainPlace])[2])

              expVar = c(rep(levels(tempData[, explainPlace])[1], count1), rep(levels(tempData[, explainPlace])[2], count2))

              theData = as.data.frame(expVar)
              theData$respVar = sampleData

              if(length(theData$expVar) == length(theData$respVar)){
                theTable = table(theData$expVar, theData$respVar)
                theProp = prop.table(theTable, margin = 1)

                if(input$explRefGroup == levels(tempData[, explainPlace])[1]){
                  theProp = rbind(theProp, theProp[1,] - theProp[2,])
                } else{
                  theProp = rbind(theProp, theProp[2,] - theProp[1,])
                }

                theProp = round(theProp, 3)
                theProp = cbind(theProp, c(as.numeric(table(tempData[, explainPlace])), 0))

                plot(c(0, 1), c(0, 0.5), ann = F, yaxt = "n", xaxt = "n", col = "white")
                title(main = paste(theType, " Sample ", states$sampleSelect))

                text(0.625, 0.5, input$response_var, pos = 1)
                text(0.425, 0.362, input$explain_var, pos = 2)
                colHead = c(levels(tempData[, responsePlace]), "N")
                rowHead = c(levels(tempData[, explainPlace]), "Difference")
                colPlace = c(0.525, 0.725, 0.95)
                rowPlace = c(0.3, 0.2, 0.1)
                # Print the column and row headings
                for(i in 1:3){
                  colFontType = 1
                  if(colHead[i] == input$respRefGroup){
                    colFontType = 2
                  }
                  rowFontType = 1
                  if(rowHead[i] == input$explRefGroup){
                    rowFontType = 2
                  }
                  text(colPlace[i], 0.4, colHead[i], pos = 1, font = colFontType)
                  text(0.425, rowPlace[i] - 0.038, rowHead[i], pos = 2, font = rowFontType)
                }
                # Print the proportions
                for(c in 1:2){
                  for(r in 1:2){
                    text(colPlace[c], rowPlace[r], theProp[r, c], pos = 1)
                  }
                }
                # Print the sample sizes
                for(i in 1:2){
                  text(colPlace[3] + 0.025, rowPlace[i], theProp[i, 3], pos = 1)
                }
                # Print the difference between sample proportions for the Reference Group
                if(input$respRefGroup == levels(tempData[, responsePlace])[1]){
                  text(colPlace[1], rowPlace[3], theProp[3, 1], pos = 1, font = 2)
                } else{
                  text(colPlace[2], rowPlace[3], theProp[3, 2], pos = 1, font = 2)
                }
              }
            } # End of if(responseVar == factor)
          } # End of Two-Sample Categorical
        } # END of Categorical Response Variable
      } # End of response_var != "NONE"
    }) # End of plot the selected Resample Distribution

    # PLOT THE RESAMPLE MEANS
    output$sampleMeanPlot <- shiny::renderPlot({

      if(input$response_var != "NONE"){

        tempData = df()

        if(input$response_var != "NONE" & input$explain_var == "NONE"){
          responsePlace = which(names(df()) == input$response_var)
          tempData = subset(tempData, !is.na(tempData[, responsePlace]))
          responseVar = tempData[, responsePlace]
        }

        if(input$response_var != "NONE" & input$explain_var != "NONE"){
          responsePlace = which(names(df()) == input$response_var)
          explainPlace = which(names(df()) == input$explain_var)
          tempData = subset(tempData, !is.na(tempData[, responsePlace]) & !is.na(tempData[, explainPlace]))
          responseVar = tempData[, responsePlace]
          explainVar = tempData[, explainPlace]
        }

        if(states$resample){
          states$resample <- FALSE

          if(states$reset){
            states$values = NULL
            states$reset = FALSE
          }
        }

        if(input$responseType == "Quantitative"){
          if(input$simType == "CI"){
            mainTitle = "Distribution of Bootstrap Sample Means"
          } else{
            mainTitle = "Distribution of Randomization Sample Means"
          }
          xTitle = "Sample Mean"
        } else{
          if(input$simType == "CI"){
            mainTitle = "Distribution of Bootstrap Sample Proportions"
          } else{
            mainTitle = "Distribution of Randomization Sample Proportions"
          }
          xTitle = "Sample Proportion"
        }

        if(input$explain_var != "NONE"){
          if(input$simType == "CI"){
            if(input$responseType == "Quantitative"){
              mainTitle = "Distribution of Difference Between Bootstrap Sample Means"
              xTitle = "Sample Mean Difference"
            } else{
              mainTitle = "Distribution of Difference Between Bootstrap Sample Proportions"
              xTitle = "Sample Proportion Difference"
            }
          } else{
            if(input$responseType == "Quantitative"){
              mainTitle = "Distribution of Difference Between Randomization Sample Means"
              xTitle = "Sample Mean Difference"
            } else{
              mainTitle = "Distribution of Difference Between Randomization Sample Proportions"
              xTitle = "Sample Proportion Difference"
            }
          }
        }

        if(is.null(states$values)){
          plot(c(0,1), c(0,1), col = "white", col.axis = "white", col.lab = "white",
               xaxt = "n", yaxt = "n", main = mainTitle)
        }
        else{ # state.values are not NULL

          if(nrow(states$values) > 0){
            if(input$responseType == "Quantitative"){ # One-Sample Context
              sampleMean = mean(tempData[, responsePlace])
            } else{
              sampleMean = mean(tempData[, responsePlace] == input$respRefGroup)
            } # End One-sample Context
            if(input$explain_var != "NONE"){ # Two-Sample Context
              subDF = subset(tempData, tempData[, explainPlace] == levels(tempData[, explainPlace])[1])
              if(input$responseType == "Quantitative"){
                mean1 = mean(subDF[, responsePlace])
              } else{ # Categorical
                mean1 = mean(subDF[, responsePlace] == input$respRefGroup)
              }
              subDF = subset(tempData, tempData[, explainPlace] == levels(tempData[, explainPlace])[2])
              if(input$responseType == "Quantitative"){
                mean2 = mean(subDF[, responsePlace])
              } else{ # Categorical
                mean2 = mean(subDF[, responsePlace] == input$respRefGroup)
              }

              if(input$explRefGroup == levels(tempData[, explainPlace])[1]){
                sampleMean = mean1 - mean2
              } else{
                sampleMean = mean2 - mean1
              }
            } # End Two-Sample Context
            theMeans = states$values$mean

            dotPlotMeans(x = theMeans, sampleMean = sampleMean,
                         main = mainTitle,
                         xlab = xTitle, ylab = NULL, pch = 20, n = nrow(tempData),
                         norm.outline = input$normalOutline, yellow.dot = states$sampleSelect)
          } # End of nrow(states$values > 0)
        } # End of else() for state$values are not NULL

      } # END (input$response_var != "NONE")
    }) #END output$sampleMeanPlot
  } #Ends everything for the server

