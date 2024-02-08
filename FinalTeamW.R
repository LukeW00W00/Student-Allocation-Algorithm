# BLOCK TO LOAD LIBRARIES

library(ompr)
library(ompr.roi)
library(dplyr)
library(readr)
library(magrittr)
library(ROI.plugin.glpk)
library(ROI)
library(shiny)
library(ggplot2)
library(DT)

# BLOCK TO READ FILES

su_data <- read.csv("Supervisor.csv", header = TRUE)
pro_data <- read.csv("Project.csv", header = TRUE)
stu_data <- read.csv("Student.csv", header = TRUE)

s <- nrow(stu_data)
p <- nrow(pro_data)
limit <- rep.int(1, p) 
# all projects have equal limit

# MMME equal workload feature
extra_Feature <- TRUE

# point assignment feature (2 versions)
assignment_points <- TRUE

#ME project lower limit
ME_limit <- TRUE

# BLOCK TO ASSIGN SUPERVISORS

if (extra_Feature) {
  su_data[su_data$supervisor >= 99017 & su_data$supervisor <= 99030, "su_Allowance"] <- 3
}

# assign supervisors to projects based on conditions
pro_data$supervisor <- NA

if (ME_limit) {
  
  # Assign 10 projects between 81 and 100 to supervisor 99001 to 99015
  for (i in 1:10) {
    available_supervisors <- su_data$supervisor[su_data$su_Allowance > 0 & su_data$supervisor >= 99001 & su_data$supervisor <= 99015]
    
    # Check if the selected supervisor has already been assigned more than 2 projects
    selected_supervisor <- sample(available_supervisors, 1)
    while (sum(pro_data$supervisor == selected_supervisor & pro_data$project_ID >= 81 & pro_data$project_ID <= 100, na.rm = TRUE) >= 2) {
      available_supervisors <- available_supervisors[available_supervisors != selected_supervisor]
      if (length(available_supervisors) == 0) {
        stop("No available supervisors to assign to project")
      }
      selected_supervisor <- sample(available_supervisors, 1)
    }
    
    selected_projects <- which(pro_data$project_ID >= 81 & pro_data$project_ID <= 100 & is.na(pro_data$supervisor))[1:1]
    pro_data$supervisor[selected_projects] <- selected_supervisor
    su_data$su_Allowance[su_data$supervisor == selected_supervisor] <- su_data$su_Allowance[su_data$supervisor == selected_supervisor] - 1
  }
  
  
  # Assign 10 projects between 81 and 100 to supervisor 99016 to 99030
  for (i in 1:10) {
    available_supervisors1 <- su_data$supervisor[su_data$su_Allowance > 0 & su_data$supervisor >= 99016 & su_data$supervisor <= 99030]
    
    # Check if the selected supervisor has already been assigned more than 2 projects
    selected_supervisor <- sample(available_supervisors1, 1)
    while (sum(pro_data$supervisor == selected_supervisor & pro_data$project_ID >= 81 & pro_data$project_ID <= 100, na.rm = TRUE) >= 2) {
      available_supervisors1 <- available_supervisors1[available_supervisors1 != selected_supervisor]
      if (length(available_supervisors1) == 0) {
        stop("No available supervisors to assign to project")
      }
      selected_supervisor <- sample(available_supervisors1, 1)
    }
    
    selected_projects <- which(pro_data$project_ID >= 81 & pro_data$project_ID <= 100 & is.na(pro_data$supervisor))[1:1]
    pro_data$supervisor[selected_projects] <- selected_supervisor
    su_data$su_Allowance[su_data$supervisor == selected_supervisor] <- su_data$su_Allowance[su_data$supervisor == selected_supervisor] - 1
  }
  
} else {
  
  # Assign 10 projects between 81 and 100 to supervisor 99001 to 99015
  for (i in 1:10) {
    available_supervisors <- su_data$supervisor[su_data$su_Allowance > 0 & su_data$supervisor >= 99001 & su_data$supervisor <= 99015]
    selected_supervisor <- sample(available_supervisors, 1)
    selected_projects <- which(pro_data$project_ID >= 81 & pro_data$project_ID <= 100 & is.na(pro_data$supervisor))[1:1]
    pro_data$supervisor[selected_projects] <- selected_supervisor
    su_data$su_Allowance[su_data$supervisor == selected_supervisor] <- su_data$su_Allowance[su_data$supervisor == selected_supervisor] - 1
  }
  
  # Assign 10 projects between 81 and 100 to supervisor 99016 to 99030
  for (i in 1:10) {
    available_supervisors <- su_data$supervisor[su_data$su_Allowance > 0 & su_data$supervisor >= 99016 & su_data$supervisor <= 99030]
    selected_supervisor <- sample(available_supervisors, 1)
    selected_projects <- which(pro_data$project_ID >= 81 & pro_data$project_ID <= 100 & is.na(pro_data$supervisor))[1:1]
    pro_data$supervisor[selected_projects] <- selected_supervisor
    su_data$su_Allowance[su_data$supervisor == selected_supervisor] <- su_data$su_Allowance[su_data$supervisor == selected_supervisor] - 1
  }
  
}

# Assign 30 projects between 1 and 30 to supervisor 99001 to 99015
for (i in 1:30) {
  available_supervisors <- su_data$supervisor[su_data$su_Allowance > 0 & su_data$supervisor >= 99001 & su_data$supervisor <= 99015]
  selected_supervisor <- sample(available_supervisors, 1)
  selected_projects <- which(pro_data$project_ID >= 1 & pro_data$project_ID <= 30 & is.na(pro_data$supervisor))[1:1]
  pro_data$supervisor[selected_projects] <- selected_supervisor
  su_data$su_Allowance[su_data$supervisor == selected_supervisor] <- su_data$su_Allowance[su_data$supervisor == selected_supervisor] - 1
}

# Assign 30 projects between 31 and 60 to supervisor 99016 to 99030
for (i in 31:60) {
  available_supervisors <- su_data$supervisor[su_data$su_Allowance > 0 & su_data$supervisor >= 99016 & su_data$supervisor <= 99030]
  selected_supervisor <- sample(available_supervisors, 1)
  selected_projects <- which(pro_data$project_ID >= 31 & pro_data$project_ID <= 60 & is.na(pro_data$supervisor))[1:1]
  pro_data$supervisor[selected_projects] <- selected_supervisor
  su_data$su_Allowance[su_data$supervisor == selected_supervisor] <- su_data$su_Allowance[su_data$supervisor == selected_supervisor] - 1
}

# Assign 20 projects between 61 and 80 to supervisor 99031 to 99040
for (i in 61:80) {
  available_supervisors <- su_data$supervisor[su_data$su_Allowance > 0 & su_data$supervisor >= 99031 & su_data$supervisor <= 99040]
  selected_supervisor <- sample(available_supervisors, 1)
  selected_projects <- which(pro_data$project_ID >= 61 & pro_data$project_ID <= 80 & is.na(pro_data$supervisor))[1:1]
  pro_data$supervisor[selected_projects] <- selected_supervisor
  su_data$su_Allowance[su_data$supervisor == selected_supervisor] <- su_data$su_Allowance[su_data$supervisor == selected_supervisor] - 1
}

# BLOCK TO CALCULATE PREFERENCES

# Create preference_data list with fixed values
preference_data <- lapply(seq_len(s), function(x) {
  if (x <= 30) {
    # Students 1 to 30 can only pick from projects 1 to 30
    sample(seq_len(30), 10)
  } else if (x <= 60) {
    # Students 31 to 60 can only pick from projects 31 to 60
    sample(seq(31, 60), 10)
  } else if (x <= 80) {
    # Students 61 to 80 can only pick from projects 61 to 80
    sample(seq(61, 80), 10)
  } else {
    # Students 81 to 100 can only pick from projects 81 to 100
    sample(seq(81, 100), 10)
  }
})

preferences <- function(student) preference_data[[student]]

# BLOCK TO ASSIGN WEIGHTS

# the weight of a student choosing a project

if (assignment_points) {
  weight <- function(student, project) {
    w <- which(as.numeric(project) == preferences(as.numeric(student)))
    as.integer(if (length(w) == 0) {
      -10000
    } else {
      w
    })
  }
} else {
  weight <- function(student, project) {
    w <- which(as.numeric(project) == preferences(as.numeric(student)))
    as.integer(if (length(w) == 0) {
      -10000
    } else {
      10
    })
  }
}

# BLOCK TO DEFINE MODEL

library(ompr)
allocation <- MIPModel() %>%
  
  # Decision variables
  add_variable(x[i, j], i = 1:s, j = 1:p, type = "binary") %>%
  
  # Objective function
  set_objective(sum_expr(weight(i, j) * x[i, j], i = 1:s, j = 1:p)) %>%
  
  # Constraints
  add_constraint(sum_expr(x[i, j], i = 1:s) <= limit[j], j = 1:p) %>% 
  
  
  add_constraint(sum_expr(x[i, j], j = 1:p) <= 1, i = 1:s)

allocation

# BLOCK TO SOLVE MODEL

output <- solve_model(allocation, with_ROI(solver = "glpk", verbose = TRUE))

# BLOCK TO SORT RESULTS

FYP <- output %>% 
  get_solution(x[i,j]) %>%
  filter(value > .9) %>%  
  select(i, j) %>% 
  rowwise() %>% 
  mutate(weight = weight(as.numeric(i), as.numeric(j)), 
         preferences = paste0(preferences(as.numeric(i)), collapse = ",")) %>% ungroup

# SHOW TIBBLE

FYP %>% 
  group_by(weight) %>% 
  summarise(count = n())

# BLOCK TO PUT RESULTS IN .csv

FYP <- right_join(FYP, stu_data, by = c("i" = "student_ID")) %>%
  left_join(pro_data, by = c("j" = "project_ID"))

FYP <- left_join(FYP, su_data %>% 
                   select(supervisor, su_Name), 
                 by = "supervisor")

FYP %>% 
  rename(student_ID = i, project = j) %>% 
  select(student_ID, student_Name, student_Course, project, supervisor, su_Name, weight, preferences) %>% 
  arrange(student_ID) %>% 
  write.csv(file = "OutputMatch.csv", row.names = FALSE)

# Shiny Model

# Define UI
ui <- fluidPage(
  titlePanel("Output Information"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      plotOutput("weight_plot"),
      dataTableOutput("matching_data")
    )
  )
)
# Define server
server <- function(input, output) {
  # read in the matching data
  FYP0 <- read.csv("OutputMatch.csv")
  
  # get counts of students per weight
  weight_counts <- FYP0 %>% 
    group_by(weight) %>% 
    summarise(count = n())
  
  # plot bar chart
  output$weight_plot <- renderPlot({
    ggplot(weight_counts, aes(x = weight, y = count, fill = factor(weight))) +
      geom_bar(stat = "identity") +
      xlab("Weight") +
      ylab("Number of Students") +
      scale_fill_discrete(name = "Weight")
  })
  
  # display matching data as a table
  output$matching_data <- renderDataTable({
    datatable(FYP0)
  })
}

# Run app
shinyApp(ui, server)
