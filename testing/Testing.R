devtools::load_all()

### Initiate Climodr ###
climodr::hubs("C:/Users/Alexander/Documents/Uni/HiWi/Test/data/Input/",
     "C:/Users/Alexander/Documents/Uni/HiWi/Test/data/Output/")

climodr::envi.create("C:/Users/Alexander/Documents/Uni/HiWi/Test/data/")

### Pre-Processing ###

# Cropping tiffs

x <- climodr::crop.all(method = "Input", safe_output = TRUE)

x <- terra::rast(paste0(Output, "tiff_stack.tif"))
x
terra::plot(x[[1:4]])

# Aggregate CSVs

climodr::prep.csv(method = "proc",
                  safe_output = TRUE
                  )

climodr::proc.csv(method = "all",
                  rbind = TRUE,
                  safe_output = TRUE
                  )

climodr::spat.csv(method = "monthly",
                          safe_output = TRUE
                          )

climodr::spat.csv(method = "daily",
                  safe_output = TRUE
                  )

#Processing

calc.model(timespan = c(17:21),
           response = c(6,9,12,13),
           classifier = c("rf", "pls","nnet" ,"lm"),
           seed = 707,
           times = 1,
           p = 0.8,
           fold = "LLO",
           predrows = c(24:57),

           )
