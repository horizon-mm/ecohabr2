library(R6)
library(data.table)
library(lubridate)
library(ggplot2)
library(DT)
library(shiny)

# testing codes
idfile <- file.choose()
timefile <- file.choose()
rawdir <- choose.dir()
files <- list.files(rawdir, pattern = "0000\\.txt$", full.names = TRUE)

system.time(ehd <- EcoHAB$new(files, idfile, timefile))
system.time(ehd$calc_events())
system.time(ehd$calc_single())
system.time(ehd$calc_pair())
system.time(ehd$calc_follow())

ehd$set_binsize(3600)
ehd$calc_activity("events")
ehd$calc_activity("single")
ehd$calc_activity("pair")
ehd$calc_activity("follow")

ehd$all_results()

events_visits <- ehd$get_result("events_visits")
events_time <- ehd$get_result("events_time")

events_time[, rfid := paste0("x", substr(rfid, 1, 4))]

fwrite(dcast(events_time[location %in% c("A", "B", "C", "D")], phase + rfid ~ location, value.var = "duration_ms"),
       "cage_time.csv")

# single_dt <- ehd$get_result("single")
# pair_dt <- ehd$get_result("pair")
# follow_dt <- ehd$get_result("follow")


for (name in ehd$all_results())
  fwrite(ehd$get_result(name), paste0(name, ".csv"))

## testing graph
# cage occupancy
ggplot(events_time[location %in% c("A", "B", "C", "D")], aes(rfid, phase)) +
  geom_raster(aes(fill = duration_ms/3600000)) +
  geom_hline(yintercept = seq(12.5, 84.5, 24), color = "black", linetype = "dashed") +
  geom_hline(yintercept = seq(24.5, 72.5, 24), color = "black") +
  xlab("Mouse ID") +
  scale_fill_gradient(name = "Time ratio", low = "white", high = "deepskyblue", limits = c(0, 1)) +
  scale_y_discrete(name = "Hour", limits = rev,
                   breaks = function(x) x[seq_along(x) %% 24 == 1],
                   labels = seq(96, 24, -24)) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12, angle = 90),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14)) +
  facet_wrap(~location)

# tube activity
ggplot(events_visits[location %in% c("ab","ba", "bc", "cb", "cd", "dc", "da", "ad"),
                     .(N = sum(N)), by = .(phase, rfid)],
       aes(phase, N, group = 1)) +
  geom_ribbon(stat = "summary", fun.data = mean_se, fill = "grey70") +
  geom_line(stat = "summary", fun = mean, linewidth = 1) +
  geom_point() +
  scale_x_discrete(breaks = function(x) x[seq_along(x) %% 24 == 0],
                   labels = seq(12, 96, 12)) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text = element_text(size = 12))




# test reading efficiency
dt_list <- lapply(files, fread)
raw_data <- rbindlist(dt_list,
                      use.names = TRUE, fill = TRUE)
setnames(raw_data, c("V1", "V2", "V3"),
         c("timestamp", "rfid", "id_reader"))
raw_data <- unique(raw_data)

# more efficient
raw_data[, time := as.numeric(fast_strptime(substr(timestamp, 1, 15),
                                            format = "%Y%m%d_%H%M%S",
                                            tz = Sys.timezone()))]
# 2x time
raw_data[, time := as.numeric(as.POSIXct(substr(timestamp, 1, 15),
                                         format = "%Y%m%d_%H%M%S"))]
