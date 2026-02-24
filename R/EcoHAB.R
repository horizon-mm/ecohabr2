#' R6 class processing Eco-HAB experimental data
#' 
#' @description
#' This class stores raw data and convert them into events and summaries.
#' 
#' @details
#' The EcoHAB class is generated from a list of raw data files, a ID file and a time line file, and store them as private members. External access is granted by active bindings to protect the reference nature of R6 class and data.table. Analysis results are stored as private and accessed by functions that return a copy of data.table.
#' 
#' @import R6
#' @import data.table
#' @importFrom lubridate ymd_hms force_tz
#' @import shiny
#' @importFrom DT datatable DTOutput renderDT JS dataTableProxy selectRows replaceData formatRound
#' @import ggplot2
#' @export
EcoHAB <- R6::R6Class("EcoHAB",
                      private = list(
                        .raw = NULL,
                        .idlist = NULL,
                        .timeline = NULL,
                        .timeline_bin = NULL,
                        .events = NULL,
                        results = list(),
                        timezone = NULL,
                        loc_threshold = NULL,
                        follow_threshold = NULL,
                        n_partial = 0,
                        n_fixed = 0,
                        key_cols = c("rfid", "start"),
                        config = data.table::data.table(id_reader = 1:8,
                                                        tube = c("ab", "ba", "bc", "cb",
                                                                 "cd", "dc", "da", "ad"),
                                                        cage = c("A", "B", "B", "C",
                                                                 "C", "D", "D", "A")),
                        locmap = matrix(c(NA, "ba", "B", "B", "err", "err", "A", "A",
                                          "ab", NA, "B", "B", "err", "err", "A", "A",
                                          "B", "B", NA, "cb", "C", "C", "err", "err",
                                          "B", "B", "bc", NA, "C", "C", "err", "err",
                                          "err", "err", "C", "C", NA, "dc", "D", "D",
                                          "err", "err", "C", "C", "cd", NA, "D", "D",
                                          "A", "A", "err", "err", "D", "D", NA, "ad",
                                          "A", "A", "err", "err", "D", "D", "da", NA),
                                        nrow = 8, dimnames = list(1:8, 1:8)),
                        loc_rev = setNames(c("ab", "ba", "bc", "cb", "cd", "dc", "da", "ad"),
                                           c("ba", "ab", "cb", "bc", "dc", "cd", "ad", "da")),
                        
                        divide = function(row_dt, binsize) {
                          # row_dt strictly has only one row
                          p_start <- row_dt$start
                          p_end <- row_dt$end
                          # get the number of bins from binzize
                          # note that the last bin may be smaller though usually the same
                          n_bin <- ceiling(as.numeric(p_end - p_start, "secs")/binsize)
                          res <- data.table(phase = paste(row_dt$phase, "div",
                                                          sprintf(paste0("%0", nchar(n_bin), "d"), seq_len(n_bin)),
                                                          sep = "_"),
                                            start = p_start + (seq_len(n_bin) - 1)*binsize,
                                            end = p_start + seq_len(n_bin)*binsize)
                          # force "end" of the last bin to be the end of row_dt
                          res[n_bin, end := p_end]
                        },
                        
                        fix_partial = function(rfid) {
                          # the tag ID is a 10-digit hex number
                          # and the rfid reader gets the number in 2-digit increment from the lower bits
                          # when the reading time is insufficient it gets only partial ID filled into the higher bits
                          # e.g. 123456789A may be read as 3456789A00, 56789A0000, etc
                          # as the lower 6 digits are always the same (possibly render ID)
                          # only ID of which the first 2 digits are missing can be fixed
                          # only unique partial rfid can be fixed
                          # non-unique partial rfid will be set to NA
                          ref_id <- substr(private$.idlist$rfid, 3, 10)
                          fixable <- !duplicated(ref_id) & !duplicated(ref_id, fromLast = TRUE)
                          partial_id <- substr(rfid, 1, 8)
                          fixmap <- setNames(private$.idlist$rfid[fixable], ref_id[fixable])
                          fixmap[partial_id]
                        },
                        
                        raw_events = function(sub_dt) {
                          dup_dt <- sub_dt
                          # add a vitual rfid read to make the event before the mouse is first detected
                          row_first <- data.table(sub_dt[1, "id_reader"],
                                                  private$.raw[1, "time"] - private$loc_threshold)
                          # add a virtual rfid read to make the event after the mouse is last detected
                          row_last <- data.table(sub_dt[.N, "id_reader"],
                                                 private$.raw[.N, "time"] + private$loc_threshold)
                          # make the "from" half of the data.table
                          dup_dt <- rbindlist(list(row_first, dup_dt), use.names = TRUE, fill = TRUE)
                          setnames(dup_dt, c("id_reader", "time"),
                                   c("id_reader_start", "start"))
                          # make the "to" half of the data.table
                          sub_dt <- rbindlist(list(sub_dt, row_last), use.names = TRUE, fill = TRUE)
                          setnames(sub_dt, c("id_reader", "time"),
                                   c("id_reader_end", "end"))
                          # build the from-to data table
                          dup_dt[, names(sub_dt) := sub_dt]
                          dup_dt[, duration := as.numeric(difftime(end, start, units = "secs"))]
                          dup_dt[, location := private$loc(id_reader_start, id_reader_end, duration)]
                          # find duplicated events at the same id reader
                          # make the first event as combined and delete the others 
                          dup_dt[, dup := rleid(id_reader_start, id_reader_end, location)]
                          dup_dt[, ':='(id_reader_end = id_reader_end[.N],
                                        end = end[.N],
                                        duration = sum(duration),
                                        location = ifelse(seq_len(.N) == 1, location, NA)),
                                 by = dup]
                          dup_dt <- dup_dt[!is.na(location) & duration > 0]
                          dup_dt[, dup := NULL]
                        },
                        
                        loc = function(from, to, duration) {
                          # calculate location for each event
                          # 1. If the event was at the same id_reader, and the length was greater than the
                          # threshold (2s by default), the mouse is assigned to the nearest cage
                          # 2. If the event was at different id_readers, and was in the same tube,
                          # the mouse is assigned to that tube
                          # 3. If the event was in two adjacent tubes connected to the same cage,
                          # the mouse is assigned to that cage
                          # 4. If the event was at different id_readers but not in case 3 or 4,
                          # an "err" tag will be assigned (at least two records were missed)
                          # note: in case that records were missed in the same tube, the mouse would
                          # appear to be in that tube for unexpected long period and may be omitted manually
                          same_id_reader <- from == to
                          res <- character(length(from))
                          res[same_id_reader] <- ifelse(duration[same_id_reader] >= private$loc_threshold,
                                                        private$config$cage[from[same_id_reader]],
                                                        as.character(from[same_id_reader]))
                          res[!same_id_reader] <- private$locmap[cbind(from[!same_id_reader], to[!same_id_reader])]
                          res
                        },
                        
                        reduce_dt = function(dt) {
                          dt <- copy(dt)
                          check_required_columns(dt, c("start", "end"))
                          setorder(dt, start, end)
                          # [start, end), left-closed and right-open
                          # check if each start is greater than the previous end (e.g. a new group)
                          dt[, group := cumsum(start > shift(end, fill = -Inf))]
                          # merge start and end for each group
                          dt[, .(start = min(start), end = max(end)), by = group][, group := NULL]
                          # note that param dt has columns "rfid", "start" and "end"
                          # return dt has columns "start" and "end"
                        },
                        
                        setdiff_dt = function(x, y) {
                          x <- copy(x)
                          y <- copy(y)
                          check_required_columns(x, c("start", "end"))
                          check_required_columns(y, c("start", "end"))
                          setkey(x, start, end)
                          setkey(y, start, end)
                          xovy <- foverlaps(x, y, type = "any", nomatch = 0L)
                          
                          # non-overlapping
                          # find rows in x that are not in xovy
                          x_nov <- x[!xovy, on = .(start = i.start, end = i.end)]
                          x_nov <- x_nov[, .(start, end)]
                          
                          # overlapping
                          # calculate all possible intervals that are 1. only within x;
                          # 2. only within y; 3. within both x and y
                          cuts <- sort(unique(c(xovy$start, xovy$end,
                                                xovy$i.start, xovy$i.end)))
                          pieces <- data.table(start = head(cuts, -1),
                                               end = tail(cuts, -1))
                          # include intervals within x
                          setkey(pieces, start, end)
                          x_ov <- foverlaps(pieces, x, type = "within", nomatch = 0L)
                          x_ov <- x_ov[, .(start = i.start, end = i.end)]
                          # exclude intervals within y
                          x_ov <- x_ov[!y, on = .(start >= start, end <= end)]
                          
                          # combine results
                          rbindlist(list(x_nov, x_ov), use.names = TRUE)
                          # return dt has columns "start" and "end"
                        }
                      ),
                      
                      active = list(
                        #' @field raw Processed raw data in a data.table (read-only)
                        raw = function(val) {
                          if (missing(val))
                            copy(private$.raw)
                          else
                            warning("Raw data are read-only.")
                        },
                        #' @field idlist Animal ID information in a data.table (read-only)
                        idlist = function(val) {
                          if (missing(val))
                            copy(private$.idlist)
                          else
                            warning("ID list is read-only.")
                        },
                        #' @field timeline Experimental time line setting in a data.table (read-only)
                        timeline = function(val) {
                          if (missing(val))
                            copy(private$.timeline)
                          else
                            warning("Timeline is read-only.")
                        },
                        #' @field timeline_bin Subdivided time line setting in a data.table (read-only)
                        timeline_bin = function(val) {
                          if (missing(val))
                            copy(private$.timeline_bin)
                          else
                            warning("Binned timeline is read-only.")
                        }
                      ),
                      public = list(
                        #' @description
                        #' Create a new Eco-HAB data set from input files
                        #' @param raw_files A list of RFID recording text files
                        #' @param idfile A text file containing animal ID information
                        #' @param timefile A text file containing experimental time line information
                        #' @param timezone The time zone of RFID recording 
                        initialize = function(raw_files = NULL, idfile = NULL,
                                              timefile = NULL, timezone = Sys.timezone()) {
                          if (is.null(raw_files)) {
                            warning("Raw files unavailable.")
                            return()
                          }  
                          if (is.null(idfile)) {
                            warning("ID file unavailable.")
                            return()
                          } 
                          if (is.null(timefile)) {
                            warning("Timeline file unavailable.")
                            return()
                          }
                          
                          # read the raw data text files
                          raw_data <- rbindlist(lapply(sort(raw_files), fread,
                                                       colClasses = "character"),
                                                use.names = TRUE, fill = TRUE)
                          setnames(raw_data, c("V1", "V2", "V3"),
                                   c("timestamp", "rfid", "id_reader"))
                          raw_data <- unique(raw_data)
                          raw_data[, id_reader := as.integer(id_reader)]
                          # convert the time stamps into POSIXct time
                          raw_data[, time := ymd_hms(sub("(.{3})$", ".\\1", timestamp))]
                          # ymd_hms converts time stamps "YYYYMMDD_HHMMSS" as UTC by default
                          
                          time_pre <- force_tz(raw_data[, time], tzone = timezone, roll_dst = "pre")
                          time_post <- force_tz(raw_data[, time], tzone = timezone, roll_dst = "post")
                          
                          # default time zone is DST (which happens earlier)
                          raw_data[, time := time_pre]
                          # eliminate ambiguity when there is more than 1 ambiguous time stamp
                          t_ambiguous <- time_pre != time_post
                          n_ambiguous <- sum(t_ambiguous)
                          if (n_ambiguous > 1) {
                            t_delta <- diff(as.numeric(time_pre[t_ambiguous]))
                            n_switch <- which(t_delta < 0)
                            raw_data[t_ambiguous, time := c(time_pre[t_ambiguous][seq_len(n_switch)],
                                                            time_post[t_ambiguous][(n_switch+1) : n_ambiguous])]
                          }
                          private$.raw <- raw_data[order(time, rfid)]
                          
                          # read mouse id file and timeline config file
                          private$.idlist <- fread(idfile, colClasses = "character")
                          private$.timeline <- fread(timefile)
                          # fread recognize times tamps "YYYY-MM-DD HH:MM:SS" as UTC by default
                          # convert it to the system time zone (where the data were acquired)
                          private$.timeline[, ':='(start = force_tz(start, tzone = timezone),
                                                   end = force_tz(end, tzone = timezone))]
                          private$.timeline[]
                          private$timezone <- timezone
                          
                          # validate the structure of raw data
                          check_required_columns(private$.raw, c("rfid", "id_reader", "time"))
                          check_required_columns(private$.idlist, c("rfid", "mid", "loc"))
                          check_required_columns(private$.timeline, c("phase", "start", "end"))
                        },
                        #' @description
                        #' Create a dummy Eco-HAB data set from pre-set parameters
                        #' @param n_subject Number of animals
                        #' @param p_step Probability of animal moving forwards on the RFID reader chain
                        #' @param mean_step Mean value of total movements
                        #' @param sd_step Standard deviation of movements
                        #' @param t Total recording time
                        #' @param p_cage Probability of time stayed in cages
                        #' @param alpha_cage Alpha of individual cage event time distribution (gamma)
                        #' @param alpha_tube Alpha of individual tube event time distribution (gamma)
                        #' @param theta Theta of individual cage or tube event time distribution (gamma)
                        #' @param timezone The time zone of dummy RFID recording
                        #' @param seed Random seed to generate dummy data
                        #' @return The current EcoHAB object
                        dummy = function(n_subject = 10, p_step = 0.5, mean_step = 1000,
                                         sd_step = 100, t = 86400, p_cage = 0.99, alpha_cage = 10,
                                         alpha_tube = 2, theta = 1, timezone = "UTC", seed = 123) {
                          if (exists(".Random.seed", envir = .GlobalEnv)) {
                            old_seed <- .GlobalEnv$.Random.seed
                            on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv))
                          } else {
                            on.exit(rm(".Random,seed", envir = .GlobalEnv))
                          }
                          # initiate subjects and activities
                          set.seed(seed)
                          subject <- paste0("AE",
                                            sprintf(paste0("%0", nchar(n_subject), "d"),
                                                          1:n_subject),
                                            "000000")
                          n_step <- round(rnorm(n_subject, mean_step, sd_step))
                          
                          # make raw recordings
                          raw <- list()
                          for (i in seq_len(n_subject)) {
                            step <- ifelse(runif(n_step[i]) < p_step, 1, -1)
                            position <- cumsum(step) %% 8 + 1
                            from <- c(1, position[-n_step[i]])
                            to <- position
                            loc <- private$locmap[cbind(from, to)]
                            duration <- numeric(n_step[i])
                            
                            in_cage <- loc %in% unique(private$config$cage)
                            n_cage <- sum(in_cage)
                            
                            g_cage <- rgamma(n_cage, shape = alpha_cage, scale = theta)
                            duration[in_cage] <- t * p_cage * g_cage / sum(g_cage)
                            g_tube <- rgamma(n_step[i] - n_cage, shape = alpha_tube, scale = theta)
                            duration[!in_cage] <- t * (1 - p_cage) * g_tube / sum(g_tube)
                            
                            raw[[i]] <- data.table(timestamp = cumsum(duration),
                                                   rfid = subject[i],
                                                   id_reader = as.integer(position))
                          }
                          raw <- rbindlist(raw, use.names = TRUE, fill = TRUE)
                          raw <- raw[order(timestamp, rfid)]
                          raw[, time := as.POSIXct(timestamp, tz = timezone)]
                          raw[, timestamp := format(time, "%Y%m%d_%H%M%OS3")]
                          raw[, timestamp := gsub("\\.", "", timestamp)]
                          setcolorder(raw, c("timestamp", "rfid", "id_reader", "time"))
                          private$.raw <- raw[]
                          
                          # make idlist
                          private$.idlist <- data.table(rfid = subject,
                                                        mid = substr(subject, 1, 4),
                                                        loc = "A")
                          
                          # make timeline
                          private$.timeline <- data.table(phase = "dummy_phase",
                                                          start = as.POSIXct("1970-01-01 00:00:00",
                                                                             tz = timezone),
                                                          end = as.POSIXct("1970-01-02 00:00:00",
                                                                           tz = timezone))
                          private$timezone <- timezone
                          
                          # validate the structure of raw data
                          check_required_columns(private$.raw, c("rfid", "id_reader", "time"))
                          check_required_columns(private$.idlist, c("rfid", "mid", "loc"))
                          check_required_columns(private$.timeline, c("phase", "start", "end"))
                          
                          invisible(self)
                        },
                        #' @description
                        #' Divide the experimental time line according to bin size
                        #' @param binsize The duration of each bin in seconds
                        #' @return The current EcoHAB object
                        set_binsize = function(binsize) {
                          if (is.null(private$.timeline)) {
                            warning("Timeline data.table does not exist")
                            return()
                          }
                          private$.timeline_bin <- private$.timeline[, private$divide(.SD, binsize), by = .I]
                          private$.timeline_bin[, ':='(I = NULL,
                                                       phase = factor(phase, levels = phase),
                                                       binsize = end - start)]
                          invisible(self)
                        },
                        #' @description
                        #' Populate events from RFID recordings
                        #' @param threshold The maximal time the animal stays at a RFID reader in seconds
                        #' @return The current EcoHAB object
                        calc_events = function(threshold = 2) {
                          if (!is.numeric(threshold)) {
                            warning("'threshold' must be numeric or integer")
                            return()
                          }
                          private$loc_threshold <- threshold
                          
                          # Step 1. Fix partial rfid reads
                          uid <- unique(private$.raw$rfid)
                          valid <- setNames(uid %in% private$.idlist$rfid, uid)
                          partial <- !valid[private$.raw$rfid]
                          private$n_partial <- sum(partial)
                          private$.raw[partial, rfid := private$fix_partial(rfid)]
                          unfixed <- is.na(private$.raw$rfid)
                          private$n_fixed <- private$n_partial - sum(unfixed)
                          cat(sprintf("%d partial records found, %d records fixed\n",
                                      private$n_partial, private$n_fixed))
                          private$.raw <- private$.raw[!unfixed]
                          
                          # Step 2. Get individual events
                          private$.events <- private$.raw[, private$raw_events(.SD),
                                                          by = rfid,
                                                          .SDcols = c("id_reader", "time")]
                          setkeyv(private$.events, private$key_cols)
                          private$results[["events"]] <- copy(private$.events)
                          invisible(self)
                        },
                        #' @description
                        #' Show and edit events in a ShinyApp
                        edit_events = function() {
                          events_original <- copy(private$.events)
                          events_edit <- copy(private$results[["events"]])
                          if (is.null(events_original) || is.null(events_edit)) {
                            warning("Events data.table does not exist")
                            return()
                          }
                          uid <- unique(events_original$rfid)
                          eloc <- c(private$config$tube, "err")
                          events_prepared <- events_original[location %in% eloc]
                          events_saved <- NULL
                          # Shiny UI: define layout, plain format (no auto-sort)
                          ui <- fluidPage(tags$style(HTML("input[type=number]::-webkit-inner-spin-button,
                                                           input[type=number]::-webkit-outer-spin-button {
                                                            -webkit-appearance: none; margin:0;
                                                           }
                                                           input[type=number]{ -moz-appearance:textfield; }")),
                                          
                                          titlePanel("Events editor"),
                                          # leftTable: original data, read-only
                                          # rightTable: editing area
                                          fluidRow(column(6, h4("Original events"),
                                                          fluidRow(column(5,
                                                                          selectInput("rfid","Filter by RFID",
                                                                                      choices = c("All" = "", uid))),
                                                                   column(4,
                                                                          numericInput("duration","Minimal duration (s)",
                                                                                       value = 60,
                                                                                       min = 0,
                                                                                       step = 1)),
                                                                   column(3,
                                                                          div(style = "display: flex;
                                                                                       flex-direction: column;",
                                                                              tags$label("Concurrent events"),
                                                                              actionButton("showBtn", "Show")))),
                                                          DTOutput("leftTable")),
                                                   column(6, h4("Editing..."),
                                                          actionButton("saveBtn","Save"),
                                                          actionButton("resetBtn","Reset"),
                                                          actionButton("finishBtn","Submit"),
                                                          actionButton("cancelBtn","Cancel"),
                                                          DTOutput("rightTable"))))
                          
                          server <- function(input, output, session){
                            rv <- reactiveValues(data = events_edit,
                                                 prepared = events_prepared,
                                                 backup = events_original,
                                                 rtPageLength = 25,
                                                 rtRow = 0,
                                                 edit_count = 0)
                            # leftTable data: filtered by location, rfid and duration_ms
                            filteredLeft <- reactive({
                              d <- rv$prepared
                              if (nzchar(input$rfid))
                                d <- d[rfid == input$rfid]
                              d[duration >= input$duration]
                            })
                            #rightTable data: filtered by the same rfid as leftTable
                            filteredRight <- reactive({
                              count <- rv$edit_count
                              d <- rv$data
                              if (nzchar(input$rfid))
                                d <- d[rfid == input$rfid]
                              d
                            })
                            # showTable data: filtered by the same time as selected row in leftTable
                            filteredShow <- reactive({
                              d <- rv$backup
                              #time_cols = c("start", "end")
                              select_left <- input$leftTable_rows_selected
                              if (length(select_left) > 0) {
                                select_start <- filteredLeft()[select_left, start]
                                select_end <- filteredLeft()[select_left, end]
                                select_loc <- filteredLeft()[select_left, location]
                                d <- d[start <= select_end & end >= select_start]
                                d <- {
                                  if (select_loc == "err")
                                    d[location %in% private$config$tube]
                                  else
                                    d[location == select_loc | location == private$loc_rev[select_loc]]
                                  }
                              }
                              d
                            })
                            # create a proxy for rows selection
                            proxy <- dataTableProxy("rightTable")
                            # leftTable output: filtered original data, read-only
                            output$leftTable <- renderDT({
                              filteredLeft() |>
                                datatable(editable = FALSE,
                                          selection = "single",
                                          options = list(dom = 'ltip',
                                                         ordering = FALSE,
                                                         scrollX = TRUE,
                                                         autoWidth = TRUE)) |>
                                formatRound("duration", 3)
                            }, server = TRUE)
                            # rightTable output: only editing location is allowed
                            # monitor page length changes
                            # jump to page
                            output$rightTable <- renderDT({
                              filteredRight() |>
                                datatable(editable = list(target = "cell",
                                                          disable = list(columns = 1:6)),
                                          selection = "single",
                                          options = list(dom = 'ltip',
                                                         ordering = FALSE,
                                                         scrollX = TRUE,
                                                         autoWidth = TRUE,
                                                         pageLength = isolate(rv$rtPageLength), 
                                                         displayStart = isolate(rv$rtRow),
                                                         initComplete = JS(
                                                           "function(settings, json) {
                                                             var table = this.api();
                                                             Shiny.setInputValue('rightTable_dt_ready', true);
                                                             Shiny.setInputValue('rightTable_pageLength', table.page.len());
                                                             table.on('length.dt', function(e, settings, len) {
                                                               Shiny.setInputValue('rightTable_pageLength', len);
                                                               });
                                                             Shiny.addCustomMessageHandler('gotoPage', function(page){
                                                               if(page === null || isNaN(page)) return;
                                                               table.page(page).draw(false);
                                                               });
                                                             }"
                                                           ))) |>
                                formatRound("duration", 3)
                            }, server = TRUE)
                            # showTable output: all concurrent events with selected events in leftTable
                            output$showTable <- renderDT({
                              filteredShow() |>
                                datatable(editable = FALSE,
                                          selection = "none",
                                          options = list(dom = 'ltip',
                                                         ordering = FALSE,
                                                         scrollX = TRUE,
                                                         autoWidth = TRUE)) |>
                                formatRound("duration", 3)
                            })
                            # monitor single row selection on leftTable and turn rightTable to page of the same raw
                            observeEvent(input$leftTable_rows_selected, {
                              req(input$rightTable_dt_ready)
                              # get the selected row in leftTable
                              select_left <- input$leftTable_rows_selected
                              if(length(select_left) == 0)
                                return()
                              # find the same row in rightTable
                              key_cols <- private$key_cols
                              key_data <- filteredLeft()[select_left, ..key_cols]
                              select_right <- filteredRight()[key_data, on = key_cols, which = TRUE]
                              if (length(select_right) == 0)
                                return()
                              # go to the page containing the hit
                              # technically there will be only one hit
                              pageLength <- input$rightTable_pageLength
                              req(pageLength)
                              page_num <- (select_right[1] - 1) %/% pageLength
                              if (is.na(page_num) || page_num < 0)
                                return()
                              session$sendCustomMessage("gotoPage", page_num)
                              selectRows(proxy, select_right[1])
                            })
                            # show concurrent events at selected
                            observeEvent(input$showBtn, {
                              select_left <- input$leftTable_rows_selected
                              if(length(select_left) == 0)
                                return()
                              showModal(modalDialog(title = "Concurrent events",
                                                    DTOutput("showTable"),
                                                    easyClose = TRUE,
                                                    size = "l"))
                            })
                            # process data editing in rightTable
                            observeEvent(input$rightTable_cell_edit, {
                              # get the edited cell and value
                              info <- input$rightTable_cell_edit
                              i<-info$row
                              j<-info$col
                              v<-info$value
                              # find the edited row in rightTable
                              key_cols <- private$key_cols
                              col<-names(filteredRight())[j]
                              key_data <- filteredRight()[i, ..key_cols]
                              edit_row <- rv$data[key_data, on = key_cols, which = TRUE]
                              if (length(edit_row) == 0)
                                return()
                              # modify the backend data and update rightTable
                              # no need to coerce info$value
                              # technically there is only one hit
                              set(rv$data, edit_row, col, v)
                              rv$rtPageLength <- input$rightTable_pageLength
                              rv$rtRow <- input$rightTable_rows_current[1] - 1
                              # force to refresh filteredRight()
                              # as change of rv$data by set() is not deemed as reactive
                              rv$edit_count <- rv$edit_count + 1
                            })
                            # save the modifications on rightTable to backend data
                            observeEvent(input$saveBtn,{
                              events_edit <<- rv$data
                            })
                            # reset the backend data (and rightTable) to original
                            observeEvent(input$resetBtn,{
                              rv$data <- copy(rv$backup)
                            })
                            # exit and return the modified backend data to the parent environment
                            observeEvent(input$finishBtn,{
                              events_saved <<- events_edit
                              stopApp(events_saved)
                            })
                            # exit and do not return the modified backend data
                            observeEvent(input$cancelBtn,{
                              stopApp(NULL)
                            })
                          }
                          
                          res <- runApp(shinyApp(ui, server))
                          if(!is.null(res))
                            private$results[["events"]] <- res
                        },
                        #' @description
                        #' Populate the single events from individual events
                        #' @param loc The locations where single events are counted
                        #' @return The current EcoHAB object
                        calc_single = function(loc = c("cages", "all")) {
                          if (is.null(private$results[["events"]])) {
                            warning("Events data.table does not exist")
                            return()
                          }
                          # argument loc must be "cages" (by default) or "all"
                          loc <- switch(match.arg(loc, c("cages", "all")),
                                        cages = unique(private$config$cage),
                                        all = unique(private$results[["events"]]$location))
                          
                          # Step 1. Get the filtered events data.table
                          events_dt <- copy(private$results[["events"]])
                          events_dt <- events_dt[location %in% loc]
                          
                          # Step 2. Remove overlaps
                          single_dt <- events_dt[, {
                            dtl <- split(.SD, by = "rfid")
                            diff_dtl <- lapply(seq_along(dtl), function(i) {
                              this_dt <- dtl[[i]]
                              other_dt <- private$reduce_dt(rbindlist(dtl[-i]))
                              private$setdiff_dt(this_dt, other_dt)
                            })
                            names(diff_dtl) <- names(dtl)
                            res <- rbindlist(diff_dtl, use.names = TRUE, idcol = "rfid")
                          }, by = location, .SDcols = c("rfid", "start", "end")]
                          
                          # Step 3. Clean up the results data.table
                          single_dt[, duration := as.numeric(difftime(end, start, units = "secs"))]
                          single_dt <- single_dt[duration > 0][order(start, end, rfid)]
                          setcolorder(single_dt, c("rfid", "start", "end", "duration", "location"))
                          private$results[["single"]] <- single_dt
                          invisible(self)
                        },
                        #' @description
                        #' Populate the pair-wise events from individual events
                        #' @param loc The locations where pair-wise events are counted
                        #' @return The current EcoHAB object
                        calc_pair = function(loc = c("cages", "all")) {
                          if (is.null(private$results[["events"]])) {
                            warning("Events data.table does not exist")
                            return()
                          }
                          # argument loc must be "cages" (by default) or "all"
                          loc <- switch(match.arg(loc, c("cages", "all")),
                                        cages = unique(private$config$cage),
                                        all = unique(private$results[["events"]]$location))
                          
                          # Step 1. Get the filtered events data.table
                          events_dt <- copy(private$results[["events"]])
                          events_dt <- events_dt[location %in% loc]
                          
                          # Step 2. Calculate overlaps
                          # foverlaps treats intervals as closed at both start and end
                          # and generates overlaps with 0 duration
                          setkey(events_dt, location, start, end)
                          events_overlap <- foverlaps(events_dt, events_dt,
                                                      by.x = c("location", "start", "end"),
                                                      by.y = c("location", "start", "end"),
                                                      type = "any",
                                                      nomatch = 0L)
                          pair_dt <- events_overlap[rfid != i.rfid, .(location,
                                                                      rfid1 = pmin(rfid, i.rfid),
                                                                      rfid2 = pmax(rfid, i.rfid),
                                                                      start = pmax(start, i.start),
                                                                      end = pmin(end, i.end))]
                          
                          # Step 3. Clean up the results data.table
                          # remove overlaps with 0 duration
                          # sort the data.table by .(start, end)
                          pair_dt <- unique(pair_dt)
                          pair_dt[, ':=' (rfid = paste(rfid1, rfid2, sep = "_"),
                                          duration = as.numeric(difftime(end, start, units = "secs")))]
                          pair_dt <- pair_dt[duration > 0][order(start, end, rfid)]
                          setcolorder(pair_dt, c("rfid1", "rfid2", "rfid", "start",
                                                 "end", "duration", "location"))
                          private$results[["pair"]] <- pair_dt
                          invisible(self)
                        },
                        #' @description
                        #' Populate the following events from individual events
                        #' @param loc The locations where following events are counted
                        #' @param mode The mode of following events identification
                        #' @param threshold The maximal interval bwtween two animals in the "delayed" mode
                        #' @return The current EcoHAB object
                        calc_follow = function(loc = c("tubes", "all"),
                                               mode = c("in_place", "delayed"),
                                               threshold = 2) {
                          if (is.null(private$results[["events"]])) {
                            warning("Events data.table does not exist")
                            return()
                          }
                          # argument "loc" must be either "tubes" (by default) or "all"
                          loc <- switch(match.arg(loc, c("tubes", "all")),
                                        tubes = unique(private$config$tube),
                                        all = unique(private$results[["events"]]$location))
                          # argument "mode" must be either "in_place" (by default) or "delayed"
                          # follower should enter the tube after the leader and within threshold
                          # in case of "in_place", before the leader exits the tube
                          # in case of "delay", before certain time (default 2s) has passed
                          mode <- match.arg(mode, c("in_place", "delayed"))
                          
                          if (!is.numeric(threshold)) {
                            warning("'threshold' must be numeric or integer")
                            return()
                          }
                          private$follow_threshold <- threshold
                          
                          # Step 1. Prepare the followee and follower data
                          followee_dt <- copy(private$results[["events"]])
                          followee_dt <- followee_dt[location %in% loc]
                          follower_dt <- copy(followee_dt)
                          
                          if (mode == "delayed") {
                            followee_dt[, end := start + threshold]
                          }
                          
                          # Step 2. Calculate overlaps
                          # foverlaps treats intervals as closed at both start and end
                          # in the case of follow it makes sense
                          setkey(followee_dt, location, start, end)
                          setkey(follower_dt, location, start, end)
                          follow_overlap <- foverlaps(follower_dt, followee_dt,
                                                      by.x = c("location", "start", "end"),
                                                      by.y = c("location", "start", "end"),
                                                      type = "any",
                                                      nomatch = 0L)
                          follow_dt <- follow_overlap[rfid != i.rfid & start < i.start,
                                                      .(location,
                                                        rfid1 = rfid,
                                                        rfid2 = i.rfid,
                                                        start,
                                                        end = i.start)]
                          # TODO: check if the followee stays at the reader after passing the tube
                          # TODO: check if the follower exit the tube later than the followee
                          
                          # Step 3. Clean up the results
                          # rfid1 (before "|") as the followee; rfid2 (after "|") as the follower
                          # duration_ms is the delay of entries in each followee-follower pair
                          follow_dt[, ':='(rfid = paste(rfid1, rfid2, sep = "|"),
                                           duration = as.numeric(difftime(end, start, units = "secs")))]
                          follow_dt <- follow_dt[duration > 0][order(start, end, rfid)]
                          setcolorder(follow_dt, c("rfid1", "rfid2", "rfid", "start",
                                                   "end", "duration", "location"))
                          private$results[["follow"]] <- follow_dt
                          invisible(self)
                        },
                        #' @description
                        #' Calculate the counts and duration of given events in each time bin
                        #' @param name The name of events data.table in the "results" list
                        #' @return The current EcoHAB object
                        calc_activity = function(name = c("events", "single", "pair", "follow")) {
                          name <- match.arg(name, c("events", "single", "pair", "follow"))
                          if (is.null(private$.timeline_bin)) {
                            warning("'timeline_bin' is missing")
                            return()
                          }
                          activity_dt <- self$get_result(name)
                          
                          # make all possible combinations of phase, rfid and location
                          # some "pair" or "follow" id pairs may be missing at a location or phase
                          # phase and rfid are extracted from metadata md
                          # location is extracted from the events data.table
                          bin_breaks <- c(private$.timeline_bin$start,
                                          private$.timeline_bin$end[nrow(private$.timeline_bin)])
                          bin_labels <- private$.timeline_bin$phase
                          bin_size <- setNames(private$.timeline_bin$binsize, bin_labels)
                          bin_size <- as.numeric(bin_size, "secs")
                          all_rfid <- private$.idlist$rfid
                          rfid_pair <- CJ(rfid1 = all_rfid, rfid2 = all_rfid)
                          rfid_pair <- rfid_pair[rfid1 != rfid2]
                          if (name == "pair") {
                            rfid_pair[, ':='(rfid1 = pmin(rfid1, rfid2),
                                             rfid2 = pmax(rfid1, rfid2))]
                            rfid_pair <- rfid_pair[!duplicated(rfid_pair)]
                            all_rfid <- paste(rfid_pair$rfid1, rfid_pair$rfid2, sep = "_")
                          }
                          if (name == "follow") {
                            all_rfid <- paste(rfid_pair$rfid1, rfid_pair$rfid2, sep = "|")
                          }
                          all_combo <- CJ(phase = bin_labels, rfid = all_rfid,
                                          location = unique(activity_dt$location))
                          setorder(all_combo, phase, rfid, location)
                          
                          # calculate the event counts at each location and in each phase
                          # assign each event into one interval defined by timeline_bin and count
                          # depending on where start is
                          visits_dt <- copy(activity_dt)
                          visits_dt[, idx := findInterval(start, bin_breaks)]
                          visits_dt <- visits_dt[idx > 0 & idx < length(bin_breaks)]
                          visits_dt[, phase := bin_labels[idx]]
                          visits_res <- visits_dt[, .N, by = .(phase, rfid, location)]
                          visits_res <- merge(all_combo, visits_res,
                                              by = c("phase", "rfid", "location"), all.x = TRUE)
                          private$results[[paste(name, "visits", sep = "_")]] <-
                            visits_res[is.na(N), N := 0]
                          
                          # calculate the event time at each location and in each phase
                          # assign each event into one or more intervals defined by timeline_bin and count
                          # depending on where start and end are
                          # if an event [start, end) spans more than one interval split it into multiple ones
                          time_dt <- copy(activity_dt)
                          time_dt[, ':='(idx_start = findInterval(start, bin_breaks),
                                         idx_end = findInterval(end, bin_breaks))]
                          time_dt <- time_dt[idx_start <= idx_end, .(idx = idx_start : idx_end),
                                             by = .(rfid, location, start, end)]
                          time_dt <- time_dt[idx > 0 & idx < length(bin_breaks)]
                          time_dt[, ':='(phase = bin_labels[idx],
                                         new_start = pmax(start, bin_breaks[idx]),
                                         new_end = pmin(end, bin_breaks[idx + 1]))]
                          time_dt[, duration := as.numeric(difftime(new_end, new_start, units = "secs"))]
                          time_res <- time_dt[, .(duration = sum(duration)),
                                              by = .(phase, rfid, location)]
                          time_res <- merge(all_combo, time_res,
                                            by = c("phase", "rfid", "location"), all.x = TRUE)
                          time_res[is.na(duration), duration := 0]
                          time_res[, ratio := duration/bin_size[phase]]
                          if (name == "pair") {
                            co_dt <- self$get_result("events_time")[location %in%
                                                                      unique(private$results[["pair"]]$location)]
                            co_dt <- co_dt[, {
                              idx <- combn(.N, 2)
                              rfid1 = rfid[idx[1,]]
                              rfid2 = rfid[idx[2,]]
                              data.table(rfid = paste0(pmin(rfid1, rfid2), "_", pmax(rfid1, rfid2)),
                                         co_duration = duration[idx[1,]] * duration[idx[2,]])
                            }, by = .(phase, location)]
                            co_dt[, adjust := co_duration/bin_size[phase]**2]
                            setkey(co_dt, phase, rfid, location)
                            time_res <- time_res[co_dt, on = .(phase, rfid, location),
                                                 .(phase, rfid, location, duration, ratio = ratio - i.adjust)]
                          }
                          private$results[[paste(name, "time", sep = "_")]] <- time_res
                          invisible(self)
                        },
                        #' @description
                        #' Get the names of all results
                        #' @return A vector containing names of all elements in the "results" list
                        all_results = function() {
                          names(private$results)
                        },
                        #' @description
                        #' Get a result in data.table
                        #' @param result_name The name of events or analyses data.table in the "results" list
                        #' @return A copy of data.table in the "results" list
                        get_result = function(result_name) {
                          if (!result_name %in% names(private$results)) {
                            warning(sprintf("Result '%s' not found", result_name))
                            return()
                          } 
                          copy(private$results[[result_name]])
                        },
                        #' @description
                        #' Delete all analysis results
                        #' @param prompt Instructions shown in the console
                        #' @return TRUE if results are deleted or FALSE otherwise
                        reset_results = function(prompt = "Delete all results?: (y/n): ") {
                          while (TRUE) {
                            answer <- readline(prompt)
                            answer <- tolower(trimws(answer))
                            if (answer == "y") {
                              private$.timeline_bin <- NULL
                              private$results <- NULL
                              cat("All results have been deleted.\n")
                              return(invisible(TRUE))
                            } else if (answer == "n") {
                              cat("Operation cancelled.\n")
                              return(invisible(FALSE))
                            } else {
                              cat("Invalid input. Please press 'y' or 'n'.\n")
                            }
                          }
                        },
                        #' @description
                        #' Plot the cage occupancy in heatmap
                        #' @return A "ggplot2" object containing the plot
                        plot_occupancy = function() {
                          dt <- self$get_result("events_time")
                          if (is.null(dt)) {
                            warning("Events_time not available, Will not plot")
                            return()
                          }
                          dt <- dt[location %in% c("A", "B", "C", "D")]
                          map_mid <- setNames(private$.idlist$mid, private$.idlist$rfid)
                          dt[, mid := map_mid[rfid]]
                          # TODO: global mid
                          
                          n_phase <- nrow(private$.timeline_bin)
                          n_day <- nrow(private$.timeline)/2
                          n_bin <- n_phase/n_day
                          y_within <- seq(n_bin/2+0.5, n_phase-n_bin/2+0.5, n_bin)
                          y_between <- seq(n_bin+0.5, n_phase-n_bin+0.5, n_bin)
                          # for regular binsize e.g. 1hr, 4hr, 12hr
                          # TODO: dynamic detection
                          
                          ggplot(dt, aes(mid, phase)) +
                            geom_raster(aes(fill = ratio)) +
                            geom_hline(yintercept = y_within, color = "black", linetype = "dashed") +
                            geom_hline(yintercept = y_between, color = "black") +
                            xlab("Mouse ID") +
                            scale_fill_gradient(name = "Time ratio", low = "white", high = "deepskyblue", limits = c(0, 1)) +
                            scale_y_discrete(name = "Time bin", limits = rev,
                                             breaks = function(x) x[seq_along(x) %% n_bin == 1],
                                             labels = seq(n_phase, n_bin, -n_bin)) +
                            theme_minimal() +
                            theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
                                  axis.title = element_text(size = 14),
                                  axis.text.x = element_text(size = 12, angle = 90),
                                  axis.text = element_text(size = 12),
                                  legend.title = element_text(size = 14),
                                  legend.text = element_text(size = 12),
                                  strip.text = element_text(size = 12)) +
                            facet_wrap(~location)
                        }
                      )
)
