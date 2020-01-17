source("./global.R")
input.data <- NULL
old.d <- NULL
coll <- c(1:50)
mterms <- attr(model$terms, 'dataClasses')
n.mterms <- names(mterms)
mclass <- attr(model, 'class')
mspecials <- attr(model$terms, 'specials')
mtermslab <- attr(model$terms, 'term.labels')
n.strata <- length(mspecials$strata)
dim.terms <- length(n.mterms)
tt <- n.mterms[1]
for (i in 2:dim.terms) {
  if (substr(n.mterms[i], 1, 6) == 'strata') {
    nch <- nchar(n.mterms[i])
    n.mterms[i] <- substr(n.mterms[i], 8, (nch - 1))
  }
}

server = function(input, output) {
  observeEvent(input$showPerformance, {
    showModal(modalDialog(
      title = "Performance",
      p("This model provides individualised and time-dependent predictions for the absolute risk of seizure relapse after beginning postsurgical AED withdrawal. The selected predictors are well defined, easily measured, and routinely available. 95% confidence intervals for each inputation are also displayed."), 
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$showHowtouse, {
    showModal(modalDialog(
      title = "How to use",
      p("This is a step-by-step guide on how to use this interactive clinical tool.",
        tags$br(),
        tags$br(),
        tags$strong("Make sure your patient fulfills the eligibility criteria."),
        tags$br(), 
        tags$br(),
        "Obtain the following data from your patient clinical history and fill-in the boxes accordingly.",
        tags$ol(
          tags$li("If your patient had simple partial seizures (e.i. auras) after surgery", tags$i("(excluding the first 30 days inmediately after surgery)"), "and before the date of effective or intented AED reduction type 1, otherwise type 0."),
          tags$li("Calculate the time", tags$i("(in years)"), "between surgery and the date of effective or intended AED reduction. Type-in this number."),
          tags$li("Type-in the number of AEDs the patient was taking", tags$i("at time of surgery.")),
          tags$li("If the patient had generalised tonic clonic seizures in the year before surgery type 1,  otherwise type 0.")),
        tags$br(),
        "Once this is done, check the box next to", tags$strong("'Survival at this Follow Up'"),"and type 10. Once all the boxes are filled-in, click on", tags$strong("'Predict.'")),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$showLimitations, {
    showModal(modalDialog(
      title = "Limitations",
      p(
        "This model permits individualised prediction of achieving complete AED withdrawal following epilepsy surgery.",
        tags$br(),
        "However, its clinical usefulness to influence medical decisions has not been tested."),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$showEligibility, {
    showModal(modalDialog(
      title = "Eligibility",
      p(
        "Inclusion criteria",
        tags$ul(tags$li("My patient has been diagnosed with epilepsy AND"),
                tags$li("he/she received epilepsy surgery AND"),
                tags$li("was aged above 18 years when surgery was performed AND"),
                tags$li("was followed-up for at least 1 year after surgery AND"),
                tags$li("initiated or will initiate AED withdrawal.")),
        p(
          "Exclusion criteria",
          tags$ul(tags$li("My patient experienced 1 or more disabling seizures between surgery and beginning of AED withdrawal (e.i. *not* classifed as ILAE Class 1 or 2).",
                          tags$br(),
                          tags$br(),
                          "*Acute post surgical seizures (e.i. those occurring during the first 30 days inmediately after surgery) are not considered seizure relapses and do not satisafy the exclusion criteria.")
          ))),
      easyClose = TRUE
    ))
    
  })
  
  neededVar <- n.mterms[-1]
  if (length(mtermslab) == 1) {
    input.data <<- data.frame(data[1, neededVar])
    names(input.data)[1] <<- n.mterms[-1]
  } else {
    input.data <<- data[1, neededVar]
  }
  
  input.data[1,] <<- NA
  b <- 1
  i.factor <- NULL
  i.numeric <- NULL
  for (j in 2:length(mterms)) {
    for (i in 1:length(data)) {
      if (n.mterms[j] == names(data)[i]) {
        if (mterms[[j]] == 'factor' |
            mterms[[j]] == 'ordered' |
            mterms[[j]] == 'logical') {
          i.factor <- rbind(i.factor, c(n.mterms[j], j, i, b))
          (break)()
        }
        if (mterms[[j]] == 'numeric') {
          i.numeric <- rbind(i.numeric, c(n.mterms[j], j, i))
          b <- b + 1
          (break)()
        }
      }
    }
  }
  dd <- unlist(strsplit(substr(tt, 6, nchar(tt) - 1), '[,]'))
  tim <- dd[1]
  sts <- substr(dd[2], 2, nchar(dd[2]))
  if (length(mtermslab) == 1) {
    input.data <<-
      data.frame(cbind(stt = NA, ti = NA, cov = NA), NO = NA)
    names(input.data)[3] <<- paste(mtermslab)
    names(input.data)[1:2] <<- c(paste(sts), paste(tim))
  } else {
    data1 <- data[, neededVar]
    input.data <<- cbind(stt = NA,
                         ti = NA,
                         data1[1,],
                         NO = NA)
    names(input.data)[1:2] <<- c(paste(sts), paste(tim))
    input.data[1,] <<- NA
  }
  if (length(i.numeric) == 0) {
    i.numeric <- matrix(ncol = 3)
    i.numeric <- rbind(i.numeric, V1 = paste(tim))
    i.numeric[dim(i.numeric)[1], 3] <-
      which(names(data) == i.numeric[dim(i.numeric)[1], 1])
    i.numeric <- rbind(i.numeric, V1 = paste(sts))
    i.numeric[dim(i.numeric)[1], 3] <-
      which(names(data) == i.numeric[dim(i.numeric)[1], 1])
    i.numeric <- i.numeric[-1,]
  } else {
    i.numeric <- rbind(i.numeric, V1 = paste(tim))
    i.numeric[dim(i.numeric)[1], 3] <-
      which(names(data) == i.numeric[dim(i.numeric)[1], 1])
    i.numeric <- rbind(i.numeric, V1 = paste(sts))
    i.numeric[dim(i.numeric)[1], 3] <-
      which(names(data) == i.numeric[dim(i.numeric)[1], 1])
  }
  
  nn <- nrow(i.numeric)
  if (is.null(nn)) {
    nn <- 0
  }
  nf <- nrow(i.factor)
  if (is.null(nf)) {
    nf <- 0
  }
  if (nf > 0) {
    output$manySliders.f <- renderUI({
      slide.bars <- list(lapply(1:nf, function(j) {
        selectInput(paste('factor', j, sep = ''),
                    names(mterms[as.numeric(i.factor[j, 2])]),
                    model$xlevels[[as.numeric(i.factor[j, 2]) - as.numeric(i.factor[j, 4])]],
                    multiple = FALSE)
      }))
      do.call(tagList, slide.bars)
    })
  }
  if (nn > 1) {
    output$manySliders.n <- renderUI({
      if (covariate == 'slider') {
        if (nn > 2) {
          slide.bars <- list(
            lapply(1:(nn - 2), function(j) {
              sliderInput(
                paste('numeric', j, sep = ''),
                i.numeric[j, 1],
                min = floor(min(na.omit(data[, as.numeric(i.numeric[j, 3])]))),
                max = ceiling(max(na.omit(data[, as.numeric(i.numeric[j, 3])]))),
                value = mean(na.omit(data[, as.numeric(i.numeric[j, 3])]))
              )
            }),
            br(),
            checkboxInput('times', 'Predicted Survival at this Follow Up:'),
            conditionalPanel(
              condition = 'input.times == true',
              sliderInput(
                paste('numeric', (nn - 1), sep = ''),
                i.numeric[(nn - 1), 1],
                min = floor(min(na.omit(data[, as.numeric(i.numeric[(nn - 1), 3])]))),
                max = ceiling(max(na.omit(data[, as.numeric(i.numeric[(nn - 1), 3])]))),
                value = mean(na.omit(data[, as.numeric(i.numeric[(nn - 1), 3])]))
              )
            )
          )
        }
        if (nn == 2) {
          slide.bars <-
            list(
              br(),
              checkboxInput('times', 'Predicted Survival at this Follow Up:'),
              conditionalPanel(
                condition = 'input.times == true',
                sliderInput(
                  paste('numeric', (nn - 1), sep = ''),
                  i.numeric[(nn - 1), 1],
                  min = floor(min(na.omit(data[, as.numeric(i.numeric[(nn - 1), 3])]))),
                  max = ceiling(max(na.omit(data[, as.numeric(i.numeric[(nn - 1), 3])]))),
                  value = mean(na.omit(data[, as.numeric(i.numeric[(nn - 1), 3])]))
                )
              )
            )
        }
      }
      if (covariate == 'numeric') {
        if (nn > 2) {
          slide.bars <- list(
            lapply(1:(nn - 2), function(j) {
              if (i.numeric[j, 1] == "time_begin") {
                numericInput(
                  paste('numeric', j, sep = ''),
                  modellabels[j],
                  min = floor(min(na.omit(
                    data[, as.numeric(i.numeric[j, 3])]
                  ))),
                  max = ceiling(max(na.omit(
                    data[, as.numeric(i.numeric[j, 3])]
                  ))),
                  value = round(mean(na.omit(
                    data[, as.numeric(i.numeric[j, 3])]
                  )))
                )
                
              } else {
                numericInput(
                  paste('numeric', j, sep = ''),
                  modellabels[j],
                  min = floor(min(na.omit(
                    data[, as.numeric(i.numeric[j, 3])]
                  ))),
                  max = ceiling(max(na.omit(
                    data[, as.numeric(i.numeric[j, 3])]
                  ))),
                  value = round(mean(na.omit(
                    data[, as.numeric(i.numeric[j, 3])]
                  )))
                )
              }
            }),
            br(),
            checkboxInput('times', 'Predicted Survival at this Follow Up:'),
            conditionalPanel(
              condition = 'input.times == true',
              numericInput(
                paste('numeric', (nn - 1), sep = ''),
                i.numeric[(nn - 1), 1],
                value = round(mean(na.omit(data[, as.numeric(i.numeric[(nn - 1), 3])])))
              )
            )
          )
        }
        if (nn == 2) {
          slide.bars <-
            list(
              br(),
              checkboxInput('times', 'Predicted Survival at this Follow Up:'),
              conditionalPanel(
                condition = 'input.times == true',
                numericInput(
                  paste('numeric', (nn - 1), sep = ''),
                  i.numeric[(nn - 1), 1],
                  value = round(mean(na.omit(data[, as.numeric(i.numeric[(nn - 1), 3])])))
                )
              )
            )
        }
      }
      do.call(tagList, slide.bars)
    })
  }
 
   a <- 0
  new.d <- reactive({
    input$add
    if (nf > 0) {
      input.f <- vector('list', nf)
      for (i in 1:nf) {
        input.f[[i]] <- isolate({
          input[[paste('factor', i, sep = '')]]
        })
        names(input.f)[i] <- i.factor[i, 1]
      }
    }
    if (nn > 1) {
      input.n <- vector('list', (nn - 1))
      for (i in 1:(nn - 1)) {
        input.n[[i]] <- isolate({
          input[[paste('numeric', i, sep = '')]]
        })
        names(input.n)[i] <- i.numeric[i, 1]
      }
    }
    if (nn == 0) {
      out <- data.frame(do.call('cbind', input.f))
    }
    if (nf == 0) {
      out <- data.frame(do.call('cbind', input.n))
    }
    if (nf > 0 & nn > 0) {
      out <-
        data.frame(do.call('cbind', input.f), do.call('cbind', input.n))
    }
    if (a == 0) {
      wher <- match(names(out), names(input.data)[-1])
      out2 <- cbind(out[wher], NO = input$add)
      input.data <<- rbind(input.data[-1], out2)
    }
    if (a > 0) {
      wher <- match(names(out), names(input.data))
      out2 <- cbind(out[wher], NO = input$add)
      if (isTRUE(compare(old.d, out)) == FALSE) {
        input.data <<- rbind(input.data, out2)
      }
    }
    a <<- a + 1
    out
  })
  p1 <- NULL
  old.d <- NULL
  data2 <- reactive({
    if (input$add == 0)
      return(NULL)
    if (input$add > 0) {
      OUT <- isolate({
        if (isTRUE(compare(old.d, new.d())) == FALSE) {
          new.d <- cbind(stat = 1, new.d())
          names(new.d)[1] <- paste(sts)
          if (n.strata > 0) {
            pred <- predict(
              model,
              newdata = new.d,
              se.fit = TRUE,
              conf.int = 0.95,
              type = 'expected',
              reference = 'strata'
            )
          }
          if (n.strata == 0) {
            pred <- predict(
              model,
              newdata = new.d,
              se.fit = TRUE,
              conf.int = 0.95,
              type = 'expected'
            )
          }
          upb <- exp(-(pred$fit - (1.95996398454005 * pred$se.fit)))
          if (upb > 1) {
            upb <- 1
          }
          lwb <- exp(-(pred$fit + (1.95996398454005 * pred$se.fit)))
          d.p <- data.frame(
            Prediction = exp(-pred$fit),
            Lower.bound = lwb,
            Upper.bound = upb
          )
          old.d <<- new.d[, -1]
          data.p <- cbind(d.p, counter = 1, NO = input$add)
          p1 <<- rbind(p1, data.p)
          p1$count <- seq(1, dim(p1)[1])
          p1
        } else {
          p1$count <- seq(1, dim(p1)[1])
          OUT <- p1
        }
      })
    }
    OUT
  })
  s.fr <- NULL
  old.d2 <- NULL
  b <- 1
  St <- TRUE
  
  dat.p <- reactive({
    if (isTRUE(compare(old.d2, new.d())) == FALSE) {
      fit1 <- survfit(model, newdata = new.d())
      if (n.strata == 0) {
        sff <- as.data.frame(summary(fit1)[c(2:4, 6:7)])
        sff <- cbind(sff, event = 1 - sff$surv, part = b)
        if (sff$time[1] != 0) {
          sff2 <- sff[1,]
          sff2[1,] <- NA
          sff2$time[1] <- 0
          sff2$n.risk[1] <- model$n
          sff2$surv[1] <- 1
          sff2$event[1] <- 0
          sff2$part[1] <- sff$part[1]
          s.f <- rbind(sff2, sff)
        } else {
          s.f <- sff
        }
      }
      if (n.strata > 0) {
        sff <- cbind(sub.fit1(), part = b)
        sff <- cbind(sff, event = 1 - sff$surv)
        if (sff$time[1] != 0) {
          sff2 <- sff[1,]
          sff2[1,] <- NA
          sff2$time[1] <- 0
          sff2$n.risk[1] <- sff[1, 2]
          sff2$surv[1] <- 1
          sff2$event[1] <- 0
          sff2$part[1] <- sff$part[1]
          s.f <- rbind(sff2, sff)
        } else {
          s.f <- sff
        }
        s.f$n.risk <- s.f$n.risk / s.f$n.risk[1]
      }
      if (dim(s.f)[1] < 3) {
        St <<- FALSE
        stop('Error in data structure: There is not enough data in the current strata level')
      }
      s.fr <<- rbind(s.fr, s.f)
      old.d2 <<- new.d()
      b <<- b + 1
    }
    s.fr
  })
  output$plot <- renderPlot({
    if (St == TRUE) {
      if (input$add == 0)
        return(NULL)
      if (input$add > 0) {
        if (input$trans == TRUE) {
          pl <- ggplot(data = dat.p()) +
            geom_step(aes(
              x = time,
              y = surv,
              alpha = n.risk,
              group = part
            ),
            color = coll[dat.p()$part]) +
            ylim(0, 1) + xlim(0, max(dat.p()$time) * 1.05) +
            labs(title = 'Estimated Survival Probability',
                 x = 'Follow Up Time',
                 y = 'S(t)') + theme_bw() +
            theme(text = element_text(face = 'bold', size = 14),
                  legend.position = 'none')
        }
        if (input$trans == FALSE) {
          pl <- ggplot(data = dat.p()) +
            geom_step(aes(
              x = time,
              y = surv,
              group = part
            ), color = coll[dat.p()$part]) +
            ylim(0, 1) + xlim(0, max(dat.p()$time) * 1.05) +
            labs(title = 'Estimated Survival Probability',
                 x = 'Follow Up Time',
                 y = 'S(t)') + theme_bw() +
            theme(text = element_text(face = 'bold', size = 14),
                  legend.position = 'none')
        }
      }
      data2()
      print(pl)
    }
    if (St == FALSE) {
      print('Restart the application')
    }
  })
  output$plot2 <- renderPlotly({
    if (input$add == 0)
      return(NULL)
    if (is.null(new.d()))
      return(NULL)
    lim <- c(0, 1)
    yli <- c(0 - 0.5, 10 + 0.5)
    PredictNO <- 0:(sum(data2()$counter) - 1)
    in.d <- data.frame(input.data[-1, -dim(input.data)[2]])
    xx = matrix(paste(names(in.d), ': ', t(in.d), sep = ''), ncol = dim(in.d)[1])
    text.cov = apply(xx, 2, paste, collapse = '<br />')
    if (dim(input.data)[1] > 11)
      yli <- c(dim(input.data)[1] - 11.5, dim(input.data)[1] - 0.5)
    p <-
      ggplot(
        data = data2(),
        aes(
          x = Prediction,
          y = PredictNO,
          text = text.cov,
          label = Prediction,
          label2 = Lower.bound,
          label3 = Upper.bound
        )
      ) +
      geom_point(size = 2,
                 colour = data2()$count,
                 shape = 15) +
      ylim(yli[1], yli[2]) + coord_cartesian(xlim = lim) +
      geom_errorbarh(
        xmax = data2()$Upper.bound,
        xmin = data2()$Lower.bound,
        size = 1.45,
        height = 0.4,
        colour = data2()$count
      ) +
      labs(title = '95% Confidence Interval for Survival Probability',
           x = 'Survival Probability', y = NULL) +
      theme_bw() + theme(axis.text.y = element_blank(),
                         text = element_text(face = 'bold', size = 10))
    
    
    gp = ggplotly(p, tooltip = c('text', 'label', 'label2', 'label3'))
    dat.p()
    gp
  })
  output$data.pred <- renderPrint({
    if (input$add > 0) {
      if (nrow(data2() > 0)) {
        di <- ncol(input.data)
        data.p <- merge(input.data[-1,], data2()[1:5], by = 'NO')
        data.p <-
          data.p[,!(colnames(data.p) %in% c('NO', 'counter'))]
        stargazer(data.p, summary = FALSE, type = 'text')
      }
    }
  })
  output$summary <- renderPrint({
    coef.c <- exp(model$coef)
    ci.c <- exp(suppressMessages(confint(model, level = 0.95)))
    stargazer(
      model,
      coef = list(coef.c),
      ci.custom = list(ci.c),
      p.auto = F,
      type = 'text',
      omit.stat = c('LL', 'ser', 'f'),
      ci = TRUE,
      ci.level = 0.95,
      single.row = TRUE,
      title = 'coxph(Surv(time_sz, sz) ~ auras + time_begin + drugs + gtcs, data = data)'
    )
  })
}
