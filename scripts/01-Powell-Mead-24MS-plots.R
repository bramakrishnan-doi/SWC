# library(tidyverse)
library(dplyr)
library(tidyr)
library(rhdb)
library(ggplot2)
library(lubridate)
library(zoo)
library(magick)

## 24-MS MRIDs & Date - UPDATE!
run_date = c('2026-01')

most_mrid <- 3303
min_mrid <- 3304
max_mrid <- 3305

## 24MS Run Date - UPDATE!
most_run_date = c('2026-01')
min_run_date = c('2026-01')
max_run_date = c('2026-01')

## UPDATE! this to add "DROA" to legend. T = add "DROA", F = don't add "DROA"
maxLab_droa = F
minLab_droa = F

## Get month names for chart subheading
if (month(ym(most_run_date)) == month(ym(max_run_date))) {
  month_heading = month.name[month(ym(most_run_date))]
} else {
  month_heading = paste(month.name[month(ym(max_run_date))], "and", month.name[month(ym(most_run_date))])
}

# add year to subheading
month_heading = paste(month_heading, year(ym(most_run_date)))



cloud_model = 'CRMMS'

# nmonths
wy = ifelse(month(ym(run_date))>9, year(ym(run_date)), year(ym(run_date))-1)
wy_start = c(paste0(wy,"-10"))
wy_end = c(paste0(wy+2,"-09"))


hist_nMons = 7 # keep 7 months before start date

#Sets the end date for the chart
if (month(ym(run_date)) > 10) {
  end_date = format(ym(run_date) + months(33-month(ym(run_date))), "%Y-%m")
} else {
  end_date = format(ym(run_date) + months(23), "%Y-%m")
}


# This code checks if the run month is in (Jan, Apr, Aug, or Oct) and sets the 
# end_date to match the max if not
# 
# if (month(ym(run_date)) %in% list(1,4,8,10)) {
#   end_date = end_date
# } else {
#   end_date = format(ym(max_run_date) + months(23), "%Y-%m")
# }


histStart_date = format(ym(run_date) - months(hist_nMons), "%Y-%m")
histEnd_date = format(ym(most_run_date) - months(1), "%Y-%m")
sdis <- c("Mead.Pool Elevation" = 1930, "Powell.Pool Elevation" = 1928)
slots <- names(sdis)
mrid_to_trace <- c("24MS Min", "24MS Max", "24MS Most")
names(mrid_to_trace) <- c(min_mrid, max_mrid, most_mrid)



df_hdb <- bind_rows(
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", most_run_date, end_date, most_mrid),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", most_run_date, end_date, most_mrid),
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", most_run_date, end_date, min_mrid),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", most_run_date, end_date, min_mrid),
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", most_run_date, end_date, max_mrid),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", most_run_date, end_date, max_mrid)
)

df_hdb <- df_hdb %>%
  mutate(slot = names(sdis)[match(sdi, sdis)],
         run = run_date,
         Trace = mrid_to_trace[as.character(mrid)]) %>%
  rename(Date = time_step) %>%
  mutate(Date = as.yearmon(parse_date_time(Date, "m/d/y H:M:S"))) %>%
  select(-sdi, -mrid)

## Read historical data from hdb
df_hist <- bind_rows(
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", histStart_date, histEnd_date),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", histStart_date, histEnd_date)
)

df_hist <- df_hist %>%
  mutate(slot = names(sdis)[match(sdi, sdis)],
         run = run_date,
         Trace = 'Historical') %>%
  rename(Date = time_step) %>%
  mutate(Date = as.yearmon(parse_date_time(Date, "m/d/y H:M:S"))) %>%
  select(-sdi, -mrid) %>% na.omit()




## Connect historical data and initial conditions
df_init = df_hdb %>% 
  filter(Date == run_date) %>% 
  select(-value, -Date) %>% distinct()
df_hist2 = df_hist %>% 
  filter(Date == max(Date)) %>%
  select(slot, Date, value)
df_initAdd = left_join(df_init, df_hist2, by = 'slot')
df_24MS <- rbind(df_hdb, df_initAdd)

df_24MS <- arrange(df_24MS, desc(Trace))

## Add historical data to 24MS df
df_24MS = rbind(df_hist, df_24MS)
# df_24MS = arrange(df_24MS,desc(Trace))



# ########## Get Powell WY Releases ###########################################################################

#If most_run_date month is October, set historical to NULL
#Else get the historical release data from the beginning of the WY

if (month(ym(most_run_date)) == 10) {
  historical <- NULL
} else {
  historical <- hdb_query(1920, "uc", "m", wy_start, format(ym(most_run_date) - months(1), "%Y-%m"))
}

# historical <- hdb_query(1920, "uc", "m", wy_start, format(ym(most_run_date) - months(1), "%Y-%m"))

powRel <- bind_rows(historical,
                    hdb_query(1920, "uc", "m", most_run_date, wy_end, most_mrid))

powRelmin <- bind_rows(historical,
                       hdb_query(1920, "uc", "m", min_run_date, wy_end, min_mrid))

#If max_run_date month is October, set historical to NULL
#Else get the historical release data from the beginning of the WY

if (month(ym(max_run_date)) == 10) {
  maxhistorical <- NULL
} else {
  maxhistorical <- hdb_query(1920, "uc", "m", wy_start, format(ym(max_run_date) - months(1), "%Y-%m"))
}

powRelmax <- bind_rows(maxhistorical,
                       hdb_query(1920, "uc", "m", max_run_date, wy_end, max_mrid))


powRelmost <- powRel %>%
  rename(Date = time_step, Most = value) %>%
  mutate(Date = as.yearmon(parse_date_time(Date, "m/d/y H:M:S"))) %>%
  select(Date, Most)

powRelmin <- powRelmin %>%
  rename(Date = time_step, Min = value) %>%
  mutate(Date = as.yearmon(parse_date_time(Date, "m/d/y H:M:S"))) %>%
  select(Date, Min)

powRelmax <- powRelmax %>%
  rename(Date = time_step, Max = value) %>%
  mutate(Date = as.yearmon(parse_date_time(Date, "m/d/y H:M:S"))) %>%
  select(Date, Max)

powReleaseWY <- inner_join(inner_join(powRelmost,powRelmin, by="Date"),powRelmax, by="Date") %>%
  mutate(WY = ifelse(month(Date)>9,year(Date)+1,year(Date))) %>%
  group_by(WY) %>%
  summarise(MostRel = round(sum(Most)/1000000,2),
            MinRel = round(sum(Min)/1000000,2),
            MaxRel = round(sum(Max)/1000000,2))

mostrd <- ym(most_run_date)
minrd <- ym(min_run_date)
maxrd <- ym(max_run_date)

if (powReleaseWY$MostRel[1] != powReleaseWY$MostRel[2]){
  mostlab <- paste(month.name[month(mostrd)],year(mostrd), "Most Probable Inflow with a Lake Powell release of",
                   format(powReleaseWY$MostRel[1],nsmall = 2), "maf in WY", powReleaseWY$WY[1], "and", 
                   format(powReleaseWY$MostRel[2],nsmall = 2), "maf in WY", powReleaseWY$WY[2])
} else {
  mostlab <- paste(month.name[month(mostrd)],year(mostrd), "Most Probable Inflow with a Lake Powell release of",
                   format(powReleaseWY$MostRel[1],nsmall = 2), "maf in WY", powReleaseWY$WY[1], "and WY", 
                   powReleaseWY$WY[2])
}

if (powReleaseWY$MinRel[1] != powReleaseWY$MinRel[2]){
  minlab <- paste(month.name[month(minrd)],year(minrd), ifelse(minLab_droa, 'DROA Probable Minimum', "Probable Minimum"),
                  "Inflow with a Lake Powell release of",  format(powReleaseWY$MinRel[1],nsmall = 2), "maf in WY", 
                  powReleaseWY$WY[1], "and", format(powReleaseWY$MinRel[2],nsmall = 2), "maf in WY", powReleaseWY$WY[2])
} else {
  minlab <- paste(month.name[month(minrd)],year(minrd), ifelse(minLab_droa, 'DROA Probable Minimum', "Probable Minimum"),
                  "Inflow with a Lake Powell release of",  format(powReleaseWY$MinRel[1],nsmall = 2), "maf in WY", 
                  powReleaseWY$WY[1], "and WY", powReleaseWY$WY[2])
}

if (powReleaseWY$MaxRel[1] != powReleaseWY$MaxRel[2]){
  maxlab <- paste(month.name[month(maxrd)],year(maxrd), ifelse(maxLab_droa, 'DROA Probable Maximum', "Probable Maximum"),
                  "Inflow with a Lake Powell release of",  format(powReleaseWY$MaxRel[1],nsmall = 2), "maf in WY", 
                  powReleaseWY$WY[1], "and", format(powReleaseWY$MaxRel[2],nsmall = 2), "maf in WY", powReleaseWY$WY[2])
} else {
  maxlab <- paste(month.name[month(maxrd)],year(maxrd), ifelse(maxLab_droa, 'DROA Probable Maximum', "Probable Maximum"),
                  "Inflow with a Lake Powell release of",  format(powReleaseWY$MaxRel[1],nsmall = 2), "maf in WY", 
                  powReleaseWY$WY[1], "and WY", powReleaseWY$WY[2])
}

#######################################################################################

lab_names <- c("Historical Elevations",
               maxlab, 
               mostlab,
               minlab
)


names(lab_names) <- c("Historical", "24MS Max", "24MS Most",  "24MS Min")
nn <- lab_names[1:4]

custom_colors <- c('grey20', '#104E8B','#26AE44','#DA3139')
custom_size <- c(1,rep(1, 3))
custom_lt <- c(1,rep(2, 3))
custom_alpha <- c(rep(1, 4))
# Assign the long label names to the custom aesthetic vectors
# This allows ggplot to match the values in 'trace_labels' to the correct color/size etc.
names(custom_colors) <- names(custom_size) <- names(custom_lt) <- 
  names(custom_alpha) <- nn
cloud_color <- '#DDCBA4'#'grey85'

df_24MS_m = df_24MS %>% mutate(trace_labels = lab_names[Trace])

df_24MS_m$trace_labels = factor(df_24MS_m$trace_labels,levels = lab_names[c(1,2,3,4)]) 

# Create cloud range
cloud_name = '24MS Projections Range'

# df_stat <-  df_24MS_m %>% select(c("run","slot","Date", "value")) %>% 
#   # filter(Date >= run_date) %>% 
#   group_by(run, slot, Date) %>%
#   summarise(cloud.max = max(value),
#             cloud.min = min(value)) %>% 
#   mutate(Cloud = factor(cloud_name), Date= as.yearmon(Date))

# df_24MS_m %>% filter(Trace != "24MS Min" & Trace != "Historical") %>% 
#   group_by(run, slot, Date) %>%
#   summarise(cloud.max = max(value),
#             cloud.min = min(value)) %>%
#   filter(case_when(
#     month(maxrd) %in% c(9,10,11,12,1,2,3) ~ as.Date(Date) < maxrd + months(24),
#                     TRUE ~ as.Date(Date) < mostrd + months(24) )) %>% 
#   tail(10)






df_stat_max <- df_24MS_m %>% filter(Trace != "24MS Min" & Trace != "Historical") %>% 
  group_by(run, slot, Date) %>%
  summarise(cloud.max = max(value),
            cloud.min = min(value)) %>%
  filter(case_when(
    month(maxrd) %in% c(9,10,11,12,1,2,3) ~ as.Date(Date) < maxrd + months(24),
    TRUE ~ as.Date(Date) < mostrd + months(24) )) %>% 
  mutate(Cloud = factor(cloud_name), Date= as.yearmon(Date)) 

df_stat_min <- df_24MS_m %>% filter(Trace != "24MS Max" & Trace != "Historical") %>% 
  group_by(run, slot, Date) %>%
  summarise(cloud.max = max(value),
            cloud.min = min(value)) %>%
  #filter(as.Date(Date) < maxrd + months(24)) %>% 
  mutate(Cloud = factor(cloud_name), Date= as.yearmon(Date)) 


df_24MS_m1 = df_24MS_m %>% mutate(Cloud = factor(cloud_name), Date= as.yearmon(Date))

# df_stat_p = df_stat %>% filter(slot == 'Powell.Pool Elevation')
# df_stat_m = df_stat %>% filter(slot == 'Mead.Pool Elevation')

df_stat_p_max = df_stat_max %>% filter(slot == 'Powell.Pool Elevation')
df_stat_p_min = df_stat_min %>% filter(slot == 'Powell.Pool Elevation')

df_stat_m_max = df_stat_max %>% filter(slot == 'Mead.Pool Elevation')
df_stat_m_min = df_stat_min %>% filter(slot == 'Mead.Pool Elevation')

df_24MS_p = df_24MS_m1 %>% filter(slot == 'Powell.Pool Elevation') %>%
  mutate(trace_labels = lab_names[Trace])
df_24MS_m = df_24MS_m1 %>% filter(slot == 'Mead.Pool Elevation') %>%
  mutate(trace_labels = lab_names[Trace])


eq_tier_df <- function() {
  Timestep = matrix(seq.Date(as.Date('2007-10-1'), 
                             as.Date('2026-09-1'), 'months') - 1,
                    byrow = T, ncol = 12)
  Timestep = as.Date(as.vector(t(cbind(Timestep[,1], Timestep))), 
                     origin = '1970-01-01')[-1]
  powell_line = data.frame(
    Date = as.POSIXct(Timestep),
    Eq_elev = rep(c(3636, 3639, 3642, 3643, 3645, 3646, 3648, 3649, 3651, 3652, 
                    3654, 3655, 3657, 3659, 3660, 3662, 3663, 3664, 3666),
                  each=13)[1:length(Timestep)],
    check.names = F,
    stringsAsFactors = FALSE) %>%
    mutate(Date = as.yearmon(Date)) 
  dates_2 = as.yearmon(seq.Date(as.Date('2026-09-1'), 
                                as.Date('2029-09-1'), 'months'))
  postEQ = data.frame(Date = dates_2,
                      Eq_elev = rep(3666, length(dates_2)))
  powell_line = rbind.data.frame(powell_line, postEQ)
  
  return(powell_line)
}

## Powell tier for figures
powell_line = eq_tier_df() %>%
  mutate(Cloud = factor(cloud_name))


res <- c("mead", "powell") # reservoirs

getData <- function(res) 
{
  tmp <- read.csv(file.path("data", paste0(res, "ElevationVolume.csv")))
  tmp
}

# This will fail if the 'data' directory doesn't exist. Add error handling.
tryCatch({
  evTables <- lapply(res, getData)
  names(evTables) <- tolower(res)
}, error = function(e) {
  message("Could not read elevation-volume tables. Creating dummy data.")
  message("Secondary axis for storage will not be accurate.")
  powell_dummy <- data.frame(Elevation = c(3370, 3700), Volume = c(2000000, 24000000))
  mead_dummy <- data.frame(Elevation = c(900, 1220), Volume = c(2000000, 26000000))
  evTables <<- list(powell = powell_dummy, mead = mead_dummy)
})


elevation_to_storage <- function(elevation, reservoir)
{
  # evTables are system data for this package
  e2vFunc <- stats::approxfun(
    evTables[[reservoir]][,1], 
    evTables[[reservoir]][,2]
  )
  
  e2vFunc(elevation)
}







## Powell -----------------------------------------------------------------------
p_breaks <- seq(3350, 3725, 25)
p_breaks2 <- seq(3350, 3725, 5)
yy <- c(3400, 3675) # NULL for default ylim

# Calculate date limits (using df_24MS$Date)
min_date_limit <- min(df_24MS$Date, na.rm = TRUE)
max_date_limit <- max(df_24MS$Date, na.rm = TRUE)

# Construct the plot
gg <- ggplot(mapping = aes(x = Date)) + # Minimal global aesthetic
  
  # Shading rectangle starting Oct 2026
  geom_rect(
    aes(xmin = as.yearmon("Sep 2026"), xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = 'grey90', 
    alpha = 0.5, # Adjust alpha for desired darkness
    inherit.aes = FALSE
  ) +
  
  # Data layers
  geom_ribbon(data = df_stat_p_max,
              aes(ymin = cloud.min, ymax = cloud.max, fill = Cloud),
              alpha = 0.2) +
  geom_ribbon(data = df_stat_p_min,
              aes(ymin = cloud.min, ymax = cloud.max, fill = Cloud),
              alpha = 0.2) +
  geom_line(data = df_24MS_p,
            aes(y = value, color = trace_labels, alpha = trace_labels,
                group = trace_labels, linetype = trace_labels, size = trace_labels)) +
  geom_line(data = powell_line,
            aes(y = Eq_elev),
            colour = "black", linetype = 1) +
  
  # Scales
  scale_fill_discrete(breaks = c(''), type = cloud_color) +
  # CORRECTED: Use unname(lab_names) for the breaks to display the full label text
  scale_color_manual(values = custom_colors, breaks = unname(lab_names)) +
  scale_linetype_manual(values = custom_lt, breaks = unname(lab_names)) +
  scale_size_manual(values = custom_size, breaks = unname(lab_names)) +
  scale_alpha_manual(values = custom_alpha, breaks = unname(lab_names)) +
  scale_x_yearmon(
    expand = c(0,0),
    breaks = unique(df_24MS$Date),
    minor_breaks = unique(df_24MS$Date),
    limits = c(min_date_limit, max_date_limit)
  ) +
  scale_y_continuous(
    labels = scales::comma, breaks = p_breaks, minor_breaks = p_breaks2,
    limits = yy,
    sec.axis = sec_axis(
      ~elevation_to_storage(., "powell"),
      breaks = elevation_to_storage(p_breaks, "powell"),
      labels = scales::comma_format(scale = 1/1000000, accuracy = 0.01),
      name = "Storage (maf)"
    )
  ) +
  # Labels, titles, and caption
  labs(y = "Pool Elevation (ft)", x = NULL, color = NULL, linetype = NULL,
       size = NULL,
       fill = NULL,
       title = bquote('Lake Powell End-of-Month'~Elevations^1),
       subtitle = paste('Projections from', month_heading, '24-Month Study Inflow Scenarios'),
       caption = 'The Drought Response Operations Agreement (DROA) is available online at https://www.usbr.gov/dcp/finaldocs.html.
       
¹For modeling purposes, simulated years beyond 2026 assume a continuation of the 2007 Interim Guidelines including the 2024 Supplement to the 2007 Interim Guidelines (no additional SEIS
conservation is assumed to occur after 2026), the 2019 Colorado River Basin Drought Contingency Plans, and Minute 323 including the Binational Water Scarcity Contingency Plan. With the 
exception of certain provisions related to ICS recovery and Upper Basin Demand management, operations under these agreements are in effect through 2026.'
       
  ) +
  # tier stuff
  geom_hline(
    yintercept = c(3575, 3525), 
    color = 'black', linetype = 'solid'
  ) +
  geom_hline(yintercept = 3490, color = 'grey20', linetype = 2) +
  geom_vline(
    xintercept = as.yearmon(c("Dec 2025", "Dec 2026")), 
    size = 1, color = "#ffdc70",  #"#ffdc70" or "grey45"
    alpha = 0.8
  ) +
  geom_vline( # Vertical line at start of shading
    xintercept = as.yearmon("Sep 2026"),
    color = "gray30", 
    linetype = "solid" 
  ) +
  geom_vline(
    xintercept = as.yearmon(ym(run_date) %m-% months(1)),
    size = 1, color = "black"  #"#ffdc70" or "grey45"
    # alpha = 0.8
  ) +
  #geom_line(
  #   data = powell_line,
  #   aes(x = Date, y=Eq_elev),
  #   colour = "black", linetype = 1
  #) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=3670, label="Equalization Tier (ET)", angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=3620, label="Upper Elevation Balancing\nTier (3,575' to ET)", 
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=3569, label="Mid-Elevation Release Tier", 
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=3548, label="(3,525' to 3,575')", 
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=3510, label="Lower Elevation Balancing\nTier (<3,525')", 
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=3477, label="Minimum Power Pool\n(3,490')", 
           angle=00, size=3, hjust = 0) +
  # legend guides
  guides(alpha = 'none',
         color = guide_legend(nrow = 5, order = 1),
         linetype = guide_legend(nrow = 5, order = 1),
         size = guide_legend(nrow = 5, order = 1)
         #, fill = guide_legend(order = 1)
  ) +
  # Theme settings
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), 
    legend.position = "bottom",
    legend.justification = "left",
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, 'cm'),
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-5,-5,-5,-5),
    plot.margin = unit(c(0.1,1,1,1), "cm"),
    plot.title = element_text(face="bold", hjust = 0.5, size = 13,margin=margin(5,0,0,0)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, margin=margin(5,0,5,0)),
    plot.caption = element_text(hjust = 0, size = 7, face = "italic"),
    legend.text = element_text(size=9)
  )

print("Saving Powell Plot:")
ggsave("Powell24MS.png", width = 11, height = 8)
tryCatch({
    logo_raw <- image_read(file.path("data","BofR-vert.png"))
    crmms_p <- image_read("Powell24MS.png")
    test_plot <- image_composite(crmms_p,image_resize(logo_raw,"325"),offset = "+2860+2060")
    image_write(test_plot, "Powell24MS.png")
    image_write(image_convert(test_plot, format = "pdf"), "Powell24MS.pdf")
}, error = function(e) {
    message("Could not download or apply logo. Skipping image composition.")
})


## Mead -------------------------------------------------------------------------
m_breaks <- seq(900, 1250, 25)
m_breaks2 <- seq(900, 1250, 5)
yy <- c(975, 1125) # NULL for default ylimit

# Calculate date limits (using df_24MS$Date)
min_date_limit <- min(df_24MS$Date, na.rm = TRUE)
max_date_limit <- max(df_24MS$Date, na.rm = TRUE)

# Construct the plot
gg <- ggplot(mapping = aes(x = Date)) + # Minimal global aesthetic
  
  # Shading rectangle starting Dec 2026
  geom_rect(
    aes(xmin = as.yearmon("Dec 2026"), xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = 'grey90', alpha = 0.5, inherit.aes = FALSE
  ) +
  
  # Data layers
  geom_ribbon(data = df_stat_m_max,
              aes(ymin = cloud.min, ymax = cloud.max, fill = Cloud),
              alpha = 0.2) +
  geom_ribbon(data = df_stat_m_min,
              aes(ymin = cloud.min, ymax = cloud.max, fill = Cloud),
              alpha = 0.2) +
  geom_line(data = df_24MS_m,
            aes(y = value, color = trace_labels, alpha = trace_labels,
                group = trace_labels, linetype = trace_labels, size = trace_labels)) +
  
  # Scales
  scale_fill_discrete(breaks = c(''), type = cloud_color) +
  # CORRECTED: Use unname(lab_names) for the breaks to display the full label text
  scale_color_manual(values = custom_colors, breaks = unname(lab_names)) +
  scale_linetype_manual(values = custom_lt, breaks = unname(lab_names)) +
  scale_size_manual(values = custom_size, breaks = unname(lab_names)) +
  scale_alpha_manual(values = custom_alpha, breaks = unname(lab_names)) +
  scale_x_yearmon(
    expand = c(0,0),
    breaks = unique(df_24MS$Date),
    minor_breaks = unique(df_24MS$Date),
    limits = c(min_date_limit, max_date_limit)
  ) +
  scale_y_continuous(
    labels = scales::comma, breaks = m_breaks, minor_breaks = m_breaks2,
    limits = yy, expand = c(0,0),
    sec.axis = sec_axis(
      ~elevation_to_storage(., "mead"),
      breaks = elevation_to_storage(m_breaks, "mead"),
      labels = scales::comma_format(scale = 1/1000000, accuracy = 0.01),
      name = "Storage (maf)"
    )
  ) +
  # Labels, titles, and caption
  labs(y = "Pool Elevation (ft)", x = NULL, color = NULL, linetype = NULL,
       size = NULL,
       fill = NULL,
       title = bquote('Lake Mead End-of-Month'~Elevations^1),
       subtitle = paste('Projections from', month_heading, '24-Month Study Inflow Scenarios'),
       caption ='The Drought Response Operations Agreement (DROA) is available online at https://www.usbr.gov/dcp/finaldocs.html.
       
¹For modeling purposes, simulated years beyond 2026 assume a continuation of the 2007 Interim Guidelines including the 2024 Supplement to the 2007 Interim Guidelines (no additional SEIS
conservation is assumed to occur after 2026), the 2019 Colorado River Basin Drought Contingency Plans, and Minute 323 including the Binational Water Scarcity Contingency Plan. With the 
exception of certain provisions related to ICS recovery and Upper Basin Demand management, operations under these agreements are in effect through 2026.'
       
  )+
  # Reference lines and annotations
  geom_hline(yintercept = c(1110, 1090, 1045), colour = 'black', linetype = 'dashed') +
  geom_hline(yintercept = c(1145, 1075, 1050, 1025), color = "black", linetype = "solid") +
  geom_vline(xintercept = as.yearmon(c("Dec 2025")), size = 1, color = "#ffdc70", alpha = 0.8) +
  geom_vline( # Vertical line at start of shading
    xintercept = as.yearmon("Dec 2026"),
    color = "gray30", 
    linetype = "solid" 
  ) +
  geom_vline(xintercept = as.yearmon(lubridate::ym(run_date) %m-% months(1)), size = 1, color = "black") +
  
  annotate("text", 
           x = as.yearmon(lubridate::ym(run_date) - months(6)), y=1147.5, 
           label="Surplus Condition (>1,145')", angle=00, size=3, hjust = 0) +
  annotate("text", 
           x = as.yearmon(lubridate::ym(run_date) - months(6)), y=1100, 
           label="Normal Condition\n(1,075' to 1,145')", angle=00, size=3, hjust = 0) +
  annotate("text", 
           x = as.yearmon(lubridate::ym(run_date)), y=1107, 
           label="Elevation 1,110 ft", angle=00, size=3, hjust = 0) +
  annotate("text", 
           x = as.yearmon(lubridate::ym(run_date)), y=1087, 
           label="Elevation 1,090 ft", angle=00, size=3, hjust = 0) +
  annotate("text", 
           x = as.yearmon(lubridate::ym(run_date)), y=1042, 
           label="Elevation 1,045 ft", angle=00, size=3, hjust = 0) +
  annotate("text", 
           x = as.yearmon(lubridate::ym(run_date) - months(6)), y=1073, 
           label="Level 1", angle=00, size=3, hjust = 0) +
  annotate("text", 
           x = as.yearmon(lubridate::ym(run_date) - months(6)), y=1068, 
           label="Shortage Condition", angle=00, size=3, hjust = 0) +
  annotate("text", 
           x = as.yearmon(lubridate::ym(run_date) - months(6)), y=1063, 
           label="(1,050' to 1,075')", angle=00, size=3, hjust = 0) +
  annotate("text", 
           x = as.yearmon(lubridate::ym(run_date) - months(6)), y=1036, 
           label="Level 2 \nShortage Condition\n(1,025' to 1,050')", angle=00, size=3, hjust = 0) +
  annotate("text", 
           x = as.yearmon(lubridate::ym(run_date) - months(6)), y=1015, 
           label="Level 3 \nShortage Condition\n(<1,025')", angle=00, size=3, hjust = 0) +
  
  # Legend guides
  guides(
    alpha = 'none',
    color = guide_legend(nrow = 5, order = 1),
    linetype = guide_legend(nrow = 5, order = 1),
    size = guide_legend(nrow = 5, order = 1)
  ) +
  
  # Theme settings
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "bottom",
    legend.justification = "left",
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, 'cm'),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(-5,-5,-5,-5),
    plot.margin = unit(c(0.1,1,1,1), "cm"),
    plot.title = element_text(face="bold", hjust = 0.5, size = 13, margin=margin(5,0,0,0)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, margin=margin(5,0,5,0)),
    plot.caption = element_text(hjust = 0, size = 7, face = "italic"),
    legend.text = element_text(size=9)
  )


ggsave("Mead24MS.png", 
       width = 11, height = 8)

tryCatch({
    crmms_m <- image_read("Mead24MS.png")
    logo_raw <- image_read(file.path("data","BofR-vert.png"))
    test_plot <- image_composite(crmms_m,image_resize(logo_raw,"325"),offset = "+2860+2060")
    image_write(test_plot, "Mead24MS.png")
    image_write(image_convert(test_plot, format = "pdf"), "Mead24MS.pdf")
}, error = function(e) {
    message("Could not download or apply logo. Skipping image composition.")
})
