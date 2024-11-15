library(rhdb)
library(tidyverse)
library(lubridate)
library(zoo)

## 24-MS MRIDs & Date - UPDATE!
run_date = c('2024-11')

most_mrid <- 3269
min_mrid <- 3270
max_mrid <- 3268

## 24MS Run Date - UPDATE!
most_run_date = c('2024-11')
min_run_date = c('2024-11')
max_run_date = c('2024-10')

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

#
if (month(ym(run_date)) > 10) {
  end_date = format(ym(run_date) + months(33-month(ym(run_date))), "%Y-%m")
} else {
  end_date = format(ym(run_date) + months(23), "%Y-%m")
}


# This code checks if the run month is in (Jan, Apr, Aug, or Oct) and sets the 
# end_date to match the max if not

if (month(ym(run_date)) %in% list(1,4,8,10)) {
  end_date = end_date
} else {
  end_date = format(ym(max_run_date) + months(23), "%Y-%m")
}


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

## Add historical data to 24MS df
df_24MS = rbind(df_24MS, df_hist)
df_24MS = arrange(df_24MS,Trace)



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
custom_size <- c(1,rep(1.2, 3))
custom_lt <- c(1,rep(2, 3))
custom_alpha <- c(rep(1, 4))
names(custom_colors) <- names(custom_size) <- names(custom_lt) <- 
  names(custom_alpha) <- nn
cloud_color <- '#DDCBA4'#'grey85'

df_24MS_m = df_24MS %>% mutate(trace_labels = lab_names[Trace])

df_24MS_m$trace_labels = factor(df_24MS_m$trace_labels,levels = lab_names[c(4,1,2,3)]) 

# Create cloud range
cloud_name = '24MS Projections Range'
df_stat <-  df_24MS_m %>% select(c("run","slot","Date", "value")) %>% 
  # filter(Date >= run_date) %>% 
  group_by(run, slot, Date) %>%
  summarise(cloud.max = max(value),
            cloud.min = min(value)) %>% 
  mutate(Cloud = factor(cloud_name), Date= as.yearmon(Date))

df_24MS_m1 = df_24MS_m %>% mutate(Cloud = factor(cloud_name), Date= as.yearmon(Date))

df_stat_p = df_stat %>% filter(slot == 'Powell.Pool Elevation')
df_stat_m = df_stat %>% filter(slot == 'Mead.Pool Elevation')
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

evTables <- lapply(res, getData)
names(evTables) <- tolower(res)


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
yy <- NULL #c(3400, 3675) # NULL for default ylim
gg <-
  ggplot(df_stat_p, aes(x = Date, fill = Cloud)) +
  scale_fill_discrete(breaks = c(''), type = cloud_color) +
  geom_ribbon(aes(ymin = cloud.min, ymax = cloud.max), alpha = 0.2) +
  geom_line(data = df_24MS_p, 
            aes(x = Date, y = value, color = trace_labels, 
                alpha = trace_labels, group = trace_labels,
                linetype = trace_labels, size = trace_labels)) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_lt) +
  scale_size_manual(values = custom_size) +
  scale_alpha_manual(values = custom_alpha) +
  scale_x_yearmon(expand = c(0,0), breaks = unique(df_24MS$Date),
                  minor_breaks = unique(df_24MS$Date),
                  limits = c(min(df_24MS$Date), max(df_24MS$Date))) +
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
  labs(y = "Pool Elevation (ft)", x = NULL, color = NULL, linetype = NULL,
       size = NULL,
       fill = NULL,
       title = bquote('Lake Powell End-of-Month'~Elevations),
       subtitle = paste('Projections from', month_heading, '24-Month Study Inflow Scenarios'),
       # caption = "The Drought Response Operations Agreement (DROA) is available online at https://www.usbr.gov/dcp/finaldocs.html.                  "
  ) +
  # tier stuff
  geom_hline(
    yintercept = c(3575, 3525), 
    color = 'black', linetype = 'solid'
  ) +
  geom_hline(yintercept = 3490, color = 'grey20', linetype = 2) +
  geom_vline(
    xintercept = as.yearmon(c("Dec 2024", "Dec 2025")), 
    size = 1, color = "#ffdc70",  #"#ffdc70" or "grey45"
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.yearmon(ym(run_date) %m-% months(1)),
    size = 1, color = "black"  #"#ffdc70" or "grey45"
    # alpha = 0.8
  ) +
  geom_line(
    data = powell_line,
    aes(x = Date, y=Eq_elev),
    colour = "black", linetype = 1
  ) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=3670, label="Equalization Tier (ET)", angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=3620, label="Upper Elevation Balancing Tier\n(3,575' to ET)", 
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=3544, label="Mid-Elevation Release Tier\n(3,525' to 3,575')", 
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=3510, label="Lower Elevation Balancing Tier\n(<3,525')", 
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=3477, label="Minimum Power Pool\n(3,490')", 
           angle=00, size=3, hjust = 0) +
  theme_bw(base_size = 14) +
  guides(alpha = 'none',
         color = guide_legend(nrow = 5, order = 1),
         linetype = guide_legend(nrow = 5, order = 1),
         size = guide_legend(nrow = 5, order = 1)
         #, fill = guide_legend(order = 1)
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5), 
    legend.position = "bottom",
    legend.justification = "left",
    legend.key.width = unit(1.2, "cm"),
    plot.margin = unit(c(0.2,1,1,1), "cm"),
    plot.title = element_text(face="bold", hjust = 0.5, size = 14,margin=margin(5,0,0,0)),
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin=margin(5,0,5,0)),
    plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
    legend.text = element_text(size=10)
  )

print("Saving Powell Plot:")
ggsave("Powell24MS.png", width = 11, height = 8)
