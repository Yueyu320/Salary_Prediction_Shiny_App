 #' @title The wage prediction shiny app
 #'
 #' @description This function returns a shiny app, which allows users to input the basic information of a job, and
 #' get the estimated hourly wage for the specific job. The shiny app requires users to import all the needed features
 #' from the job description, then exports the estimated hourly wage (in USD, including 25%, 50%, 75% quantiles of the 100 predictions from bootstrap).
 #'
 #' @details All the input boxes and sliders should have the same data type as what the predict_wage function requires.
 #'
 #' @return A shiny app, containing a density plot for estimated hourly wage.
 #'
 #' @export

DAwage_shiny = function(){
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shinyWidgets::setBackgroundImage(
        src = "https://images.unsplash.com/photo-1589362281138-e3f7ebe47f1a?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=1740&q=80"
      ),
      shiny::titlePanel("Get estimated salary"),

      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          shiny::selectInput(
            "remote",
            label = shiny::h4('Remote or on site'),
            choices = c("Remote","on site"),
            selected="on site"
          ),
          shiny::selectInput(
            "loc",
            label = shiny::h4('State'),
            choices = append(datasets::state.abb,c("DC","unknown")),
            selected="DC"
          ),
          shiny::selectInput(
            "type",
            label = shiny::h4('Type'),
            choices = c("Contract","Full time","mixed","Part time","Per diem","Temporary","unknown"),
            selected="Full time"
          ),
          shiny::selectInput(
            "degree",
            label = shiny::h4('Degree'),
            choices = c("Bachelor","Master","PhD","NotSpecified"),
            selected="Bachelor"
          ),
          shiny::selectInput(
            "data",
            label = shiny::h4('Data analyst or not'),
            choices = c("Yes", "No"),
            selected="Yes"
          ),
          shiny::numericInput("review",label = shiny::h4("Number of review"),value = 10,min = 0,max = 10^6),
          shiny::textInput("desc", label = shiny::h4("Job description"), value = NULL),
          shiny::actionButton("do", label = shiny::h4("Estimate"))
        ),
        shiny::mainPanel(
          shiny::plotOutput("plot")
        )
      )
    ),
    server = function(input, output, session) {
      `%>%` <- magrittr::`%>%`
      shiny::observeEvent(input$do, {

        shiny::withProgress(message = "Making plot", value = 0, {
          set.seed(123)
          salary <- c()
          for (i in 1:100){
            salary[i] <- predict_wage_1(input$desc,input$remote,input$loc,input$type,input$degree,input$data,input$review)
            shiny::incProgress(1/100, detail = paste(i, "%"))
          }
        })

        output$plot <- shiny::renderPlot({
          data.frame(salary) %>%
            ggplot2::ggplot(ggplot2::aes(x = salary), size = 2, col = "red", fill = "lightblue") +
              ggplot2::geom_density() +
              ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                    panel.background = ggplot2::element_rect(fill = "transparent",colour = NA), plot.background = ggplot2::element_rect(fill = "transparent",colour = NA), axis.line = ggplot2::element_line(colour = "black")
                    ) +
              ggplot2::labs(title = "Distribution of Estimated Hourly Salary", x = "hourly salary",
                   y = "Density") +
              ggplot2::geom_vline(xintercept = c(stats::quantile(salary, probs = 0.25, na.rm = TRUE),
                                                 stats::quantile(salary, probs = 0.5, na.rm = TRUE),
                                                 stats::quantile(salary, probs = 0.75, na.rm = TRUE)),
                                  linetype = c("dashed", "dashed", "dashed"),
                                  color = c("Green", "Blue", "Red"),
                                  size = c(1.5, 1.5, 1.5)) +
              ggplot2::annotate(geom = "text",
                     label = c(paste0("25% quantile of estimated salary:", round(stats::quantile(salary, probs = 0.25, na.rm = TRUE), 2)),
                               paste0("Median of estimated salary:",round(stats::quantile(salary, probs = 0.5, na.rm = TRUE), 2)),
                               paste0("75% quantile of estimated salary:", round(stats::quantile(salary, probs = 0.75, na.rm = TRUE), 2))),
                     x = c(stats::quantile(salary, probs = 0.25, na.rm = TRUE),
                           stats::quantile(salary, probs = 0.5, na.rm = TRUE),
                           stats::quantile(salary, probs = 0.75, na.rm = TRUE)),
                     y = c(0.05,0.075,0.1),
                     angle = 0,
                     vjust = 1)
        })
      })
    })

}


# Use a single bootstrap sample to fit AdaBoost and make prediction
predict_wage_1 = function(desc, remote, loc, type, degree, data, review) {
  `%>%` <- magrittr::`%>%`
  if (length(desc) > 1 | length(remote) > 1 | length(loc) > 1 | length(type) > 1 | length(degree) > 1 | length(data) > 1 | length(review) > 1) {
    stop("Couldn't handle more than one input!")
  }
  if (!is.numeric(review) | !is.character(desc)) {
    stop("Plz check review or desc: review has to be a numeric value and desc has to be in the string type!")
  }
  if (!(remote %in% c("Remote", "on site"))) {
    stop("remote variable has to be 'Remote' or 'on site' (Case Sensitive)!")
  }
  states <- append(datasets::state.abb, "DC")
  states <- append(states, "unknown")
  if (!(loc %in% states)) {
    stop("loc variable has to be the abbreviation of states in the U.S. or 'unknown' (ex: CO)!")
  }
  if (!(type %in% c("mixed", "Full time", "unknown", "Contract", "Part time", "Per diem", "Temporary"))) {
    stop("type variable has to be one of 'mixed', 'Full time', 'unknown', 'Contract', 'Part time', 'Per diem','Temporary' types (Case sensitive)!")
  }
  if (!(degree %in% c("Bachelor", "Master", "NotSpecified", "PhD"))) {
    stop("degree variable has to be one of 'Bachelor', 'Master', 'NotSpecified', 'PhD' types (Case sensitive!)")
  }
  if (!(data %in% c("Yes", "No"))) {
    stop("data variable has to be one of 'Yes' or 'No' types (Case sensitive)!")
  }
  tidy_data <- DataAnalyst::tidy_data
  #all_data: delete unnecessary columns of tidy_data. Categorical features in this data frame are not dummified, but it can be used to train tree based models
  data_all <- tidy_data %>%
    dplyr::select(-c(job_des, spark, xml))
  # According to the Cross Validation result, we choose depth = 4 and ntrees = 75
  new_data <- cbind(description(desc),
                    data.frame(job_loc = as.factor(location(loc)),
                               job_remote = as.factor(remote),
                               job_type = as.factor(type),
                               degree = as.factor(degree),
                               data_analyst = as.factor(data_analyst(data)),
                               review = as.numeric(review))
  )

  # sample with replacement to get bootstrap samples
  boot.id <- sample(1:nrow(data_all), size = nrow(data_all), replace = TRUE)
  train.tmp <- data_all[boot.id, ]
  ada.fit <- gbm::gbm(log(salary) ~ ., data = train.tmp, distribution = "gaussian", n.trees = 75, interaction.depth = 4)
  ypred <- exp(stats::predict(ada.fit, newdata = new_data, n.trees = 75))

  return(ypred)
}
