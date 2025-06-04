library(treemapify)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmosaic)
library(RColorBrewer)
library(ggforce)
library(cowplot)
library(scales)
library(ggcorrplot)
library(ggridges)
library(viridis)
library(CGPfunctions)
library(lubridate)
library(ggalluvial)
library(plotly)
library(shiny)
library(bslib)
library(factoextra)
library(ggthemes)
library(leaflet)
library(forecast)
library(ggrepel)
library(ggdist)


traffic <- read.csv("Banglore_traffic_Dataset.csv")

ui <- page_sidebar(
  title = "Bangalore Traffic Insights",
  sidebar = sidebar(
    title = "Home",
    open = FALSE,
    selectizeInput(
      "select",
      "Select options below:",
      choices = c(
        "Explore",
        "Traffic Signal Compliance",
        "Traffic Volume",
        "Roadwork and Construction Activity",
        "Weather Conditions",
        "Temporal",
        "Deeper Dive",
        "Expanded Analysis"
      )
    )
  ),
  uiOutput("plot_container"),
  actionButton("prev_button", "Back", style = "position: absolute; bottom: 10px; left: 10px;"),
  actionButton("next_button", "Next", style = "position: absolute; bottom: 10px; right: 10px;")
)


###############server############################
server <- function(input, output, session) {
  steps <- reactiveValues(
    Explore = 1,
    `Traffic Signal Compliance` = 1,
    `Traffic Volume` = 1,
    `Roadwork and Construction Activity` = 1,
    `Weather Conditions` = 1,
    Temporal = 1,
    `Deeper Dive` = 1,
    `Expanded Analysis` = 1
  )
  
  current_step <- reactive({
    steps[[input$select]]
  })
  
  observeEvent(input$next_button, {
    section <- input$select
    if (!is.null(steps[[section]]) && steps[[section]] < 6) {
      steps[[section]] <- steps[[section]] + 1
    }
  })
  
  observeEvent(input$prev_button, {
    section <- input$select
    if (!is.null(steps[[section]]) && steps[[section]] > 1) {
      steps[[section]] <- steps[[section]] - 1
    }
  })
  
  output$plot_container <- renderUI({
    if (input$select == "Roadwork and Construction Activity" && current_step() == 2) {
      plotlyOutput("nested_pie", height = "600px")
      
    } else if (input$select == "Traffic Signal Compliance" && current_step() == 2) {
      plotlyOutput("compliance_heatmap", height = "600px")  
      
    } else if (input$select == "Deeper Dive" && current_step() == 2) {
      leafletOutput("gateway_map", height = "600px")   
      
    } else if (input$select == "Expanded Analysis" && current_step() == 1) {
      plotOutput("hexbin_plot", height = "600px")
    }
    else if (input$select == "Expanded Analysis" && current_step() == 2) {
      plotOutput("parking_density_plot", height = "600px")
    }
    
    else {
      plotOutput("ggplot", height = "600px")
    }
  })
  
  
  output$ggplot <- renderPlot({
    step <- current_step()
    
    if (input$select == "Explore") {
      if (step == 1) {
        ggplot(traffic, aes(x = Congestion.Level)) +
          geom_density(fill = viridis(1), alpha = 0.6) +
          labs(
            title = "Density Plot of Congestion Level", 
            x = "Congestion Level (%)", 
            y = "Density"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 20,hjust = 0.5, face = "bold")
          )
        
      } else if (step == 2) {
        nested_pie_traffic <- traffic %>%
          group_by(Area.Name, Road.Intersection.Name) %>%
          summarise(Traffic.Volume = sum(Traffic.Volume, na.rm = TRUE), .groups = "drop")
        
        ggplot(nested_pie_traffic, aes(area = Traffic.Volume, 
                                       fill = Area.Name, 
                                       label = Road.Intersection.Name, 
                                       subgroup = Area.Name)) +
          geom_treemap() +
          geom_treemap_subgroup_border() +
          geom_treemap_subgroup_text(place = "centre", grow = TRUE, alpha = 0.5, colour = "black") +
          geom_treemap_text(colour = "white", place = "centre", reflow = TRUE) +
          scale_fill_viridis_d() +
          ggtitle("Treemap of Traffic Volume by Area and Intersection") +
          theme(
            plot.title = element_text(size = 20,hjust = 0.5, face = "bold"),
            legend.position = "none"  
          )
        
        
      } else if (step == 3) {
        incident_count <- traffic %>%
          group_by(Road.Intersection.Name) %>%
          summarise(Incident.Count = n(), 
                    Traffic.Volume = sum(Traffic.Volume, na.rm = TRUE))
        
        ggplot(incident_count, aes(area = Traffic.Volume, 
                                   fill = Incident.Count, 
                                   label = Road.Intersection.Name)) +
          geom_treemap() +
          geom_treemap_text(aes(label = paste(Road.Intersection.Name, "\n", Incident.Count)),
                            color = "white", size = 15, place = "centre", grow = TRUE) +
          scale_fill_viridis_c(option = "viridis") +  
          labs(title = "Traffic Incident by Intersection", 
               fill = "Incident Count") +
          theme(
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            legend.position = "right"
          )
        
        
      } else if (step == 4) {
        cor_matrix <- cor(traffic[sapply(traffic, is.numeric)], use = "pairwise.complete.obs")
        ggcorrplot(cor_matrix, type = "upper", method = "circle", 
                   colors = viridis::viridis(3, option = "D"), lab_size = 1,                      
                   title = "Correlation Matrix of Traffic Features",
                  ggtheme = theme_minimal(base_size = 10) +
                  theme(plot.title = element_text(size = 20,hjust = 0.5, face = "bold"))
                  )
        
      } else if (step == 5) {
        numeric_data <- traffic[sapply(traffic, is.numeric)]
        numeric_data <- na.omit(numeric_data)
        
        pca_result <- prcomp(numeric_data, scale. = TRUE)
        
        var <- get_pca_var(pca_result)
        var_df <- data.frame(var$coord)
        var_df$Variable <- rownames(var_df)
        
        n_vars <- nrow(var_df)
        color_palette <- viridis::viridis(n_vars, option = "D")  
        
        ggplot(var_df, aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2, color = Variable)) +
          geom_segment(arrow = arrow(length = unit(0.25, "cm")), size = 1.2) +
          scale_color_manual(values = color_palette) +
          coord_fixed() +
          theme_minimal(base_size = 14) +
          labs(title = "PCA - Variable Arrows with Legend",
               x = "PCA Dimension 1",
               y = "PCA Dimension 2",
               color = "Variables") +
          theme(legend.position = "right",
                plot.title = element_text(hjust = 0.5, face = "bold", size = 20))
        
      } else if (step == 6) {
        traffic$Date <- as.Date(traffic$Date, format = "%m/%d/%Y")
        traffic$Month <- format(traffic$Date, "%m")
        traffic$Year <- format(traffic$Date, "%Y")
        traffic$IncidentSeverity <- cut(
          traffic$Incident.Reports,
          breaks = c(-1, 0, 3, Inf),
          labels = c("None", "Minor", "Major")
        )
        
        aggregated_incident <- traffic %>%
          group_by(Road.Intersection.Name, Year, IncidentSeverity) %>%
          summarize(
            Incident.Reports = sum(Incident.Reports, na.rm = TRUE),
            Travel.Time.Index = mean(Travel.Time.Index, na.rm = TRUE),
            CongestionLevel_numeric = mean(Congestion.Level, na.rm = TRUE),
            .groups = 'drop'
          )
        
        ggplot(aggregated_incident, aes(
          x = Incident.Reports,
          y = Travel.Time.Index,
          size = CongestionLevel_numeric,
          color = IncidentSeverity
        )) +
          geom_point(alpha = 0.7) +
          scale_size_continuous(
            range = c(3, 10),
            limits = c(0, 100),
            name = "Avg. Congestion Level (%)"
          ) +
          scale_color_viridis_d(option = "D", end = 0.85) + 
          labs(
            title = "Impact of Incidents on Travel Time",
            subtitle = "Bubble size = Avg. Congestion | Color = Incident Severity",
            color = "Incident Severity"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
            plot.subtitle = element_text(size = 16, hjust = 0.5)
          )
      }
    }
    
    else if (input$select == "Traffic Signal Compliance") {
      if (step == 1) {
        pie_data_signals <- traffic %>%
          group_by(Area.Name) %>%
          summarise(Traffic.Signal.Compliance = sum(Traffic.Signal.Compliance, na.rm = TRUE)) %>%
          ungroup()
        
        Total_signal <- sum(pie_data_signals$Traffic.Signal.Compliance)
        pie_data_signals <- pie_data_signals %>%
          mutate(Percent = round((Traffic.Signal.Compliance / Total_signal) * 100, 1))
        
        pie_signals <- pie_data_signals %>%
          arrange(Traffic.Signal.Compliance) %>%
          mutate(
            end_angle = 2 * pi * cumsum(Traffic.Signal.Compliance) / sum(Traffic.Signal.Compliance),
            start_angle = lag(end_angle, default = 0),
            mid_angle = 0.5 * (start_angle + end_angle)
          )
        
        rpie <- 1
        rlabel_in <- 0.6 * rpie
        
        ggplot(pie_signals) +
          geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie, start = start_angle, end = end_angle, fill = Area.Name)) +
          geom_text(aes(x = rlabel_in * sin(mid_angle), y = rlabel_in * cos(mid_angle), label = paste0(Percent, "%")), size = 14/.pt) +
          coord_fixed() +
          scale_fill_viridis_d() +
          labs(title = "Traffic Signal Compliance per Area") +
          theme_minimal() +
          theme(legend.position = "right")
        
      } 
    }
    
    else if (input$select == "Traffic Volume") {
      if (step == 1){
        ggplot(traffic, aes(x = Traffic.Volume, y = Road.Intersection.Name, fill = Road.Intersection.Name)) +
          geom_density_ridges() +
          scale_fill_viridis_d(option = "D") +
          theme_ridges() +
          theme(
            legend.position = "none",
            panel.spacing = unit(0.1, "lines"),
            axis.title.x = element_text(hjust = 0.5),
            axis.title.y = element_text(hjust = 0.5)
          ) +
          labs(
            title = "Traffic Volume Across Intersections",
            x = "Traffic Volume",
            y = "Intersection"
          )}
      else if (step == 2){
        ggplot(traffic, aes(x = Environmental.Impact, y = Road.Intersection.Name)) +
          geom_violin(aes(fill = Road.Intersection.Name), color = "black", alpha = 0.6) +
          scale_fill_viridis(discrete = TRUE) +
          scale_x_continuous(breaks = seq(50, 200, by = 25), limits = c(50, 200)) +
          labs(title = "Environmental Impact by Road/Intersection",
               x = "Environmental Impact",
               y = "Road/Intersection Name") +
          theme_minimal() +
          theme(legend.position = "none")
      }
      else if (step == 3){
        traffic$Area.Name <- as.factor(traffic$Area.Name)
        traffic$Roadwork.and.Construction.Activity <- as.factor(traffic$Roadwork.and.Construction.Activity)
        
        traffic_clean <- na.omit(traffic[, c("Area.Name", "Congestion.Level", "Roadwork.and.Construction.Activity")])
        
        ggplot(traffic, aes(x = Road.Intersection.Name, y = Environmental.Impact, fill = Road.Intersection.Name)) + 
          geom_boxplot(show.legend = FALSE) +  
          coord_flip() + 
          scale_fill_viridis_d(option = "D", end = 0.85) +
          labs(
            x = "Intersection Name",
            y = "Environmental Impact",
            title = "Analysis of Traffic's Environmental Impact on Various Roads and Intersections"
          ) + 
          theme_bw()
        
      }
      else if (step == 4){
        ggplot(traffic, aes(x = `Pedestrian.and.Cyclist.Count`)) +
          stat_ecdf(geom = "step", color = "#440154", linewidth = 1) +  
          stat_ecdf(geom = "point", color = "#21908C", size = 1.5) +    
          labs(
            title = "ECDF of Pedestrians and Cyclists",
            x = "Number of Pedestrians and Cyclists",
            y = "Cumulative Frequency"
          ) +
          theme_minimal() +
          theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
      }
    }#end Traffic Volume
    
    else if (input$select == "Roadwork and Construction Activity") {
      if (step == 1){
        stacked_bar_data <- traffic %>%
          group_by(Area.Name, Roadwork.and.Construction.Activity) %>%
          summarise(Congestion.Level = sum(Congestion.Level), .groups = "drop")
        
        return(
          ggplot(stacked_bar_data, aes(x = Area.Name, y = Congestion.Level, fill = Roadwork.and.Construction.Activity)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = paste0(round(Congestion.Level / 1000), "k")),
                      size = 3.5, position = position_stack(vjust = 0.5), color = "white") +
            scale_y_continuous(labels = scales::comma) +
            scale_fill_viridis_d(option = "D", end = 0.6) +
            labs(title = "Congestion by Roadwork and Construction", x = "Area.Name", y = "Congestion Level") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
        )
      }
      if (step==3){
        traffic$Area.Name <- as.factor(traffic$Area.Name)
        traffic$Roadwork.and.Construction.Activity <- as.factor(traffic$Roadwork.and.Construction.Activity)
        
        traffic_clean <- na.omit(traffic[, c("Area.Name", "Congestion.Level", "Roadwork.and.Construction.Activity")])
        
        ggplot(traffic_clean, aes(x = Area.Name, y = Congestion.Level, fill = Roadwork.and.Construction.Activity)) +
          geom_boxplot() +
          theme_minimal(base_size = 13) +
          scale_fill_viridis_d(option = "D", end = 0.6)+
          labs(title = "Congestion Level by Area and Construction Activity",
               x = "Area Name",
               y = "Congestion Level",
               fill = "Construction Activity") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    }#end Roadwork and Construction Activity
    
    else if (input$select == "Weather Conditions") {
      if (step == 1){
        ggplot(traffic, aes(x = Road.Capacity.Utilization, fill = Weather.Conditions)) +
          geom_density(position = "fill", adjust = 4, color = NA) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
          scale_fill_manual(values = c(
            "Windy" = "#fde725",
            "Rain" = "#5ec962",
            "Overcast" = "#21918c",
            "Fog" = "#3b528b",
            "Clear" = "#440154"
          )) +
          labs(
            title = "Smoothed Stacked Density of Road Capacity by Weather",
            x = "Road Capacity Utilization",
            y = "Relative Proportion",
            fill = "Weather Conditions"
          ) +
          theme_minimal()}
      else if (step == 2){
        breaks <- unique(quantile(traffic$Travel.Time.Index, probs = seq(0, 1, 0.25), na.rm = TRUE))
        if (length(breaks) < 4) {
          breaks <- seq(min(traffic$Travel.Time.Index, na.rm = TRUE), max(traffic$Travel.Time.Index, na.rm = TRUE), length.out = 5)
        }
        
        traffic$Travel.Time.Category <- cut(traffic$Travel.Time.Index, 
                                            breaks = breaks, 
                                            labels = c("Low", "Medium", "High", "Very High"), 
                                            include.lowest = TRUE)
        
        ggplot(traffic, aes(axis1 = Area.Name, axis2 = Travel.Time.Category, 
                            axis3 = Weather.Conditions, y = Traffic.Volume)) +
          geom_alluvium(aes(fill = Area.Name)) +
          geom_stratum() +
          geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
          scale_x_discrete(limits = c("Area Name", "Travel Time Category", "Weather Conditions")) +
          scale_fill_viridis_d(option = "D") +
          theme_minimal() +
          labs(title = "Traffic Volume Flow in Bangalore")
      }
      else if (step == 3){
        traffic$Congestion_Level_Bins <- cut(traffic$Congestion.Level, breaks = c(0, 40, 80, 100),
                                             labels = c("Low", "Medium", "High"))
        
        ggplot(data = traffic) +
          geom_mosaic(aes(x = product(Area.Name, Weather.Conditions),
                          fill = Congestion_Level_Bins)) +
          scale_fill_viridis_d(option = "D", end = 0.9) +  # Discrete Viridis palette
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
            panel.grid = element_blank()
          ) +
          labs(
            title = "Congestion Levels by Area and Weather",
            x = "",
            y = "Area Name",
            fill = "Congestion Level"
          )
      }
      
    }# end weather condition 
    else if (input$select == "Temporal"){
      if (step == 1){
        traffic <- mutate(traffic, Year = year(Date))
        traffic_tree <- traffic %>%
          group_by(Year, Road.Intersection.Name) %>%
          summarize(
            Total_Traffic = sum(Traffic.Volume, na.rm = TRUE),
            Avg_Congestion = mean(Congestion.Level, na.rm = TRUE),
            .groups = "drop"
          )
        
        return(
          ggplot(traffic_tree,
                 aes(area = Total_Traffic,
                     fill = Avg_Congestion,
                     label = Road.Intersection.Name,
                     subgroup = Year)) +
            geom_treemap() +
            geom_treemap_subgroup_border(colour = "white", size = 3) +
            geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                                       alpha = 0.25, colour = "black",
                                       fontface = "italic") +
            geom_treemap_text(colour = "white", place = "centre",
                              size = 15, grow = TRUE) +
            scale_fill_viridis_c(option = "D", end = 0.6) +
            labs(title = "Comparison of Traffic Congestion of Intersections over 3 years")
        )
      }
      if (step == 2){
        traffic <- traffic %>%
          mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
                 Year = year(Date),
                 Month = month(Date))
        
        traffic <- traffic %>%
          filter(Year %in% c(2022, 2023, 2024) & Month <= 8)
        
        walkability_data <- traffic %>%
          filter(!is.na(Year) & !is.na(Pedestrian.and.Cyclist.Count)) %>%
          group_by(Area.Name, Year) %>%
          summarise(pad_cyc = sum(Pedestrian.and.Cyclist.Count, na.rm = TRUE), .groups = "drop")
        
        walkability_data$Year <- factor(walkability_data$Year, levels = sort(unique(walkability_data$Year)))
        
        return(
          newggslopegraph(
            dataframe = walkability_data,
            Times = Year,
            Measurement = pad_cyc,
            Grouping = Area.Name,
            Title = "Bangalore Pedestrians and Cyclists by Area (Yearly Trend)",
            SubTitle = "Total number of Pedestrians and Cyclists per area across all years",
            Caption = "Source: Bangalore Traffic Dataset",
            LineColor = viridis(10, option = "D")[3]
          )
        )
      }
      
      if (step == 3){
        traffic <- traffic %>%
          mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
                 Year = year(Date),
                 Month = month(Date))
        
        traffic <- traffic %>%
          filter(Year %in% c(2022, 2023, 2024) & Month <= 8)
        
        accident_slope <- traffic %>%
          filter(!is.na(Year) & !is.na(Incident.Reports)) %>%
          group_by(Area.Name, Year) %>%
          summarise(Accidents = sum(Incident.Reports, na.rm = TRUE), .groups = "drop")
        
        accident_slope$Year <- factor(accident_slope$Year, levels = sort(unique(accident_slope$Year)))
        
        return(
          newggslopegraph(
            dataframe = accident_slope,
            Times = Year,
            Measurement = Accidents,
            Grouping = Area.Name,
            Title = "Bangalore Traffic Accidents by Area (Yearly Trend)",
            SubTitle = "Total reported accidents per area across all years",
            Caption = "Source: Bangalore Traffic Dataset",
            LineColor = viridis(10, option = "D")[3]
          )
        )
      }
      
      if (step == 4){
        ggplot(traffic, aes(x = Incident.Reports)) +
          geom_bar(color = "black", fill = "#440154FF") +
          labs(title = "Count of incident reports in cars", x = "Congestion") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
      }
      
    }# end temporal
    else if(input$select == "Deeper Dive"){
      if(step == 1){
        traffic_monthly <- traffic %>%
          filter(!is.na(`Pedestrian.and.Cyclist.Count`), !is.na(`Congestion.Level`)) %>%
          mutate(YearMonth = floor_date(as.Date(Date), "month")) %>%
          group_by(YearMonth) %>%
          summarise(
            AvgUtilization = mean(`Pedestrian.and.Cyclist.Count`, na.rm = TRUE),
            AvgPedCyclists = mean(`Congestion.Level`, na.rm = TRUE)
          ) %>%
          arrange(YearMonth) %>%
          mutate(id = row_number())  
        
       
        label_subset <- traffic_monthly %>%
          filter(id %% 3 == 0)  
        
        ggplot(traffic_monthly, aes(x = AvgUtilization, y = AvgPedCyclists, color = id)) +
          geom_point(size = 2.5) +
          geom_text_repel(
            data = label_subset,
            aes(
              x = AvgUtilization,
              y = AvgPedCyclists,
              label = format(YearMonth, "%Y-%m")
            ),
            size = 3,
            max.overlaps = 20,
            color = "black",
            inherit.aes = FALSE
          ) +
          geom_segment(
            aes(
              xend = lead(AvgUtilization),
              yend = lead(AvgPedCyclists),
              color = id
            ),
            arrow = arrow(length = unit(0.2, "cm")),
            na.rm = TRUE,
            alpha = 0.6
          ) +
          scale_color_viridis_c(option = "D") +
          labs(
            title = "Monthly Pedestrian and Cyclist Count vs. Congestion Level",
            subtitle = "Exploring the relationship between pedestrian activity and traffic congestion",
            x = "Average Monthly Pedestrian and Cyclist Count",
            y = "Average Monthly Congestion Level",
            color = "Time Progression"
          ) +
          theme_minimal(base_size = 13) +
          theme(
            plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(margin = margin(b = 10))
          )
      }
      else if(step == 2){
        leafletOutput("gateway_map", height = "600px")
        
      }
      else if(step == 3){
        data_clean <- traffic %>%
          filter(!is.na(Travel.Time.Index), !is.na(Traffic.Volume), !is.na(Area.Name))
        
        traffic_summary <- data_clean %>%
          group_by(Area.Name) %>%
          summarise(
            avg_travel_time = mean(Travel.Time.Index, na.rm = TRUE),
            avg_volume = mean(Traffic.Volume, na.rm = TRUE),
            sd_travel_time = sd(Travel.Time.Index, na.rm = TRUE),
            sd_volume = sd(Traffic.Volume, na.rm = TRUE),
            .groups = "drop"
          )
        
        ggplot(traffic_summary, aes(x = avg_travel_time, y = avg_volume)) +
          geom_point(size = 3, aes(color = Area.Name)) +
          geom_errorbar(aes(ymin = avg_volume - sd_volume, ymax = avg_volume + sd_volume, color = Area.Name), width = 0) +
          geom_errorbarh(aes(xmin = avg_travel_time - sd_travel_time, xmax = avg_travel_time + sd_travel_time, color = Area.Name), height = 0) +
          scale_color_viridis_d(option = "D") +  # Apply viridis color scale
          labs(
            title = "Average Traffic Volume vs Travel Time Index by Area",
            x = "Travel Time Index",
            y = "Traffic Volume",
            color = "Area"
          ) +
          theme_minimal(base_size = 14)
      }
      else if(step == 4){
        traffic <- traffic %>%
          mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
          filter(!is.na(Road.Capacity.Utilization))
        
        monthly_capacity <- traffic %>%
          mutate(YearMonth = format(Date, "%Y-%m")) %>%
          group_by(YearMonth) %>%
          summarise(AvgCapacity = mean(Road.Capacity.Utilization, na.rm = TRUE)) %>%
          ungroup()
        
        capacity_ts <- ts(
          monthly_capacity$AvgCapacity,
          start = c(
            as.numeric(substr(monthly_capacity$YearMonth[1], 1, 4)),
            as.numeric(substr(monthly_capacity$YearMonth[1], 6, 7))
          ),
          frequency = 12
        )
        
        autoplot(capacity_ts) +
          ggtitle("Monthly Avg. Road Capacity Utilization") +
          ylab("Avg Capacity Utilization") +
          xlab("Year") +
          theme_minimal()
      }
      else if(step == 5){
        traffic <- traffic %>%
          mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
          filter(!is.na(Road.Capacity.Utilization))
        
        monthly_capacity <- traffic %>%
          mutate(YearMonth = format(Date, "%Y-%m")) %>%
          group_by(YearMonth) %>%
          summarise(AvgCapacity = mean(Road.Capacity.Utilization, na.rm = TRUE)) %>%
          ungroup()
        
        capacity_ts <- ts(
          monthly_capacity$AvgCapacity,
          start = c(
            as.numeric(substr(monthly_capacity$YearMonth[1], 1, 4)),
            as.numeric(substr(monthly_capacity$YearMonth[1], 6, 7))
          ),
          frequency = 12
        )
        decomp <- decompose(capacity_ts, type = "multiplicative")
        plot(decomp)
      }
      else if(step == 6){
        traffic <- traffic %>%
          mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
          filter(!is.na(Road.Capacity.Utilization))
        monthly_capacity <- traffic %>%
          mutate(YearMonth = format(Date, "%Y-%m")) %>%
          group_by(YearMonth) %>%
          summarise(AvgCapacity = mean(Road.Capacity.Utilization, na.rm = TRUE)) %>%
          ungroup()
        
        capacity_ts <- ts(
          monthly_capacity$AvgCapacity,
          start = c(
            as.numeric(substr(monthly_capacity$YearMonth[1], 1, 4)),
            as.numeric(substr(monthly_capacity$YearMonth[1], 6, 7))
          ),
          frequency = 12
        )
        decomp <- decompose(capacity_ts, type = "multiplicative")
        detrended <- capacity_ts / decomp$trend
        
        autoplot(detrended) +
          ggtitle("Detrended Road Capacity Utilization") +
          ylab("Capacity Utilization (detrended)") +
          xlab("Year") +
          theme_minimal() +
          scale_color_manual(values = "blue")
        
      }
    }
    else if(input$select == "Expanded Analysis"){
        if(step == 1){
          plotOutput("hexbin_plot", height = "600px")
        }
        else if(step == 2){
          plotOutput("parking_density_plot", height = "600px")
        }
        else if(step == 3){
          parking_summary <- traffic %>%
            summarise(
              mean_parking = mean(Parking.Usage, na.rm = TRUE),
              sd_parking = sd(Parking.Usage, na.rm = TRUE),
              n = n()
            ) %>%
            mutate(
              se = sd_parking / sqrt(n),
              ci_80 = se * qt(0.90, df = n - 1),
              ci_95 = se * qt(0.975, df = n - 1),
              ci_99 = se * qt(0.995, df = n - 1)
            )
          
          ci_data <- data.frame(
            CI_Level = c("80%", "95%", "99%"),
            Mean = parking_summary$mean_parking,
            Lower = parking_summary$mean_parking - c(parking_summary$ci_80, parking_summary$ci_95, parking_summary$ci_99),
            Upper = parking_summary$mean_parking + c(parking_summary$ci_80, parking_summary$ci_95, parking_summary$ci_99)
          )
          
          ggplot(ci_data, aes(x = CI_Level, y = Mean)) +
            geom_point(size = 4, color = "darkblue") +
            geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1, color = "darkblue") +
            coord_flip() +
            theme_minimal() +
            labs(title = "Confidence Intervals for Parking Usage",
                 x = "Confidence Interval Level",
                 y = "Mean Parking Usage")+
            theme(
              plot.title = element_text(size = 20,hjust = 0.5, face = "bold")  
            )
        }
      else if(step == 4){
        ggplot(traffic, aes(x = Average.Speed)) +
          stat_dots(
            quantiles = 20,
            side = "top",
            layout = "weave",
            scale = 0.75,
            color = "black",
            fill = "#21918c"
          ) +
          labs(
            title = "Quantile Dot Plot of Average Speed",
            x = "Average Speed (km/h)",
            y = NULL
          ) +
          scale_x_continuous(
            expand = expansion(mult = c(0.05, 0.05))  
          ) +
          theme_minimal() +
          theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5)
          )
        
      }
      }
    
    
  })
  
  output$nested_pie <- renderPlotly({
    traffic$Traffic_Volume_Category <- cut(
      traffic$Traffic.Volume,
      breaks = c(4233, 20000, 30000, 40000, 72039),
      labels = c("Very Low", "Low", "Medium", "High"),
      include.lowest = TRUE
    )
    
    outer_data <- traffic %>%
      count(Roadwork.and.Construction.Activity) %>%
      rename(group = Roadwork.and.Construction.Activity, value = n)
    
    inner_data <- traffic %>%
      count(Roadwork.and.Construction.Activity, Traffic_Volume_Category) %>%
      mutate(group = paste(Roadwork.and.Construction.Activity, Traffic_Volume_Category, sep = " - ")) %>%
      rename(value = n)
    
    outer_colors <- viridis(nrow(outer_data), option = "D", end = 0.95)
    inner_colors <- viridis(nrow(inner_data), option = "D", begin = 0.3, end = 1.0)
    
    plot_ly() %>%
      add_pie(
        data = outer_data,
        labels = ~group,
        values = ~value,
        hole = 0.7,
        sort = FALSE,
        marker = list(colors = outer_colors),
        textinfo = "label+percent",
        textfont = list(color = "white", size = 10)
      ) %>%
      add_pie(
        data = inner_data,
        labels = ~group,
        values = ~value,
        domain = list(x = c(0.15, 0.85), y = c(0.15, 0.85)),
        sort = FALSE,
        marker = list(colors = inner_colors),
        textinfo = "label+percent",
        textfont = list(color = "white", size = 10)
      ) %>%
      layout(
        title = list(
          text = "<b>Roadwork and Traffic Volume Comparison</b>",  
          x = 0.5,  
          xanchor = "center",
          font = list(size = 20, color = "black")
        ),
        showlegend = TRUE,
        margin = list(t = 80, b = 0, l = 0, r = 0),  
        paper_bgcolor = "rgba(0,0,0,0)", 
        plot_bgcolor = "rgba(0,0,0,0)"    
      )
  })
  
  
  output$compliance_heatmap <- renderPlotly({
    traffic <- traffic %>% mutate(Date = as.Date(Date), Month = month(Date, label = TRUE, abbr = TRUE))
    
    compliance_data <- traffic %>%
      group_by(Area.Name, Month) %>%
      summarise(Average_Compliance = mean(Traffic.Signal.Compliance, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Tooltip = paste0("Area Name: ", Area.Name, "<br>Month: ", Month, "<br>Compliance: ", round(Average_Compliance, 2)))
    
    p <- ggplot(compliance_data, aes(x = Month, y = factor(Area.Name), fill = Average_Compliance, text = Tooltip)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(name = "Signal Compliance", option = "D") +
      labs(title = "Monthly Traffic Signal Compliance by Area", x = "Month", y = "Area") +
      theme_minimal() +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5), legend.position = "top")
    
    ggplotly(p, tooltip = "text")
  })
  output$gateway_map <- renderLeaflet({
    gateways <- data.frame(
      name = c("Yeshwanthpur Circle", "Silk Board Junction", "Hebbal Flyover", "Marathahalli Bridge",
               "Electronic City Junction", "Tumkur Road Gateway", "ITPL Main Road Junction", "Sony World Junction"),
      lat = c(13.0288, 12.9172, 13.0358, 12.9592, 12.8411, 13.0450, 12.9847, 12.9450),
      lng = c(77.5380, 77.6230, 77.5912, 77.6974, 77.6795, 77.5200, 77.7500, 77.6100),
      congestion_level = c(95, 92, 88, 85, 87, 90, 82, 78),
      gateway_type = c("Primary Highway Gateway", "Southern Corridor Gateway", "Northern Entry Gateway",
                       "IT Corridor Gateway", "Tech Hub Gateway", "Northwestern Highway Gateway",
                       "Eastern Tech Gateway", "Central Area Gateway")
    )
    
    circle_colors <- ifelse(gateways$congestion_level >= 90, "#FF0000",
                            ifelse(gateways$congestion_level >= 85, "#FF8C00",
                                   ifelse(gateways$congestion_level >= 80, "#FFD700", "#32CD32")))
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = 77.5946, lat = 12.9716, zoom = 11) %>%
      addCircles(
        data = gateways,
        lng = ~lng, lat = ~lat,
        radius = ~(congestion_level * 25),
        color = circle_colors,
        opacity = 0.8,
        fillColor = circle_colors,
        fillOpacity = 0.5,
        popup = ~paste0(
          "<b>", name, "</b><br>",
          "Gateway Type: ", gateway_type, "<br>",
          "Congestion Level: ", congestion_level, "%<br>",
          "Status: ", ifelse(congestion_level >= 90, "Critical Bottleneck", 
                             ifelse(congestion_level >= 85, "High Congestion", 
                                    ifelse(congestion_level >= 80, "Moderate Congestion", "Manageable")))
        )
      ) %>%
      addLabelOnlyMarkers(
        data = gateways[gateways$congestion_level >= 85,],
        lng = ~lng, lat = ~lat,
        label = ~name,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "color" = "black",
            "font-weight" = "bold",
            "font-size" = "12px",
            "background-color" = "rgba(255,255,255,0.8)",
            "border" = "1px solid black",
            "border-radius" = "3px",
            "padding" = "2px 4px"
          )
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("#FF0000", "#FF8C00", "#FFD700", "#32CD32"),
        labels = c("Critical (90%+)", "High (85–89%)", "Moderate (80–84%)", "Manageable (<80%)"),
        title = "Congestion Levels",
        opacity = 0.8
      )
  })
  output$hexbin_plot <- renderPlot({
    traffic$Date <- as.Date(traffic$Date, format = "%Y-%m-%d")
    
    traffic <- traffic %>%
      filter(!is.na(Date), !is.na(Congestion.Level))
    
    ggplot(traffic, aes(x = Date, y = Congestion.Level)) +
      geom_hex(bins = 50) +
      scale_fill_viridis_c(option = "inferno", direction = -1, name = "Count") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      labs(
        title = "Hexbin Chart of Congestion Level by Date",
        x = "Date",
        y = "Congestion Level"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16)
      )
  })
  output$parking_density_plot <- renderPlot({
    ggplot(traffic, aes(x = Parking.Usage, y = Public.Transport.Usage)) +
      stat_density_2d(
        aes(fill = after_stat(level)),
        geom = "polygon",
        color = "black",
        bins = 6
      ) +
      scale_fill_viridis_c(
        option = "D",
        direction = -1,
        name = "Density"
      ) +
      geom_point(
        data = traffic,
        aes(x = Parking.Usage, y = Public.Transport.Usage),  
        color = "black",
        alpha = 0.1,
        size = 0.8
      ) +
      labs(
        title = "Pattern of Public Transport Usage vs Parking Availability",
        x = "Parking Usage (%)",
        y = "Public Transport Usage (%)",
        fill = "Density Level"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16, margin = margin(b = 6), hjust = 0.5),
        plot.subtitle = element_text(size = 12, margin = margin(b = 12)),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        legend.position = "right",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90")
      )
  })
  
  
}

shinyApp(ui, server)
