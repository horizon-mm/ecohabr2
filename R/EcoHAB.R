#' @import R6
#' @import data.table
#' @importFrom lubridate fast_strptime force_tz
#' @import shiny
#' @importFrom DT datatable DTOutput renderDT JS dataTableProxy selectRows replaceData
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
                        key_cols = c("rfid", "time_start", "delay_start"),
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
                        divide = function(row_dt, binsize) {
                          # row_dt strictly has only one row
                          p_start <- as.numeric(row_dt$start)
                          p_end <- as.numeric(row_dt$end)
                          # get the number of bins from binzize
                          # note that the last bin may be smaller though usually the same
                          n_bin <- ceiling((p_end - p_start)/binsize)
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
                                                  private$.raw[1, "time"] - private$loc_threshold,
                                                  delay = 0)
                          # add a virtual rfid read to make the event after the mouse is last detected
                          row_last <- data.table(sub_dt[.N, "id_reader"],
                                                 private$.raw[.N, "time"] + private$loc_threshold,
                                                 delay = 0)
                          # make the "from" half of the data.table
                          dup_dt <- rbindlist(list(row_first, dup_dt), use.names = TRUE, fill = TRUE)
                          setnames(dup_dt, c("id_reader", "time", "delay"),
                                   c("id_reader_start", "time_start", "delay_start"))
                          # make the "to" half of the data.table
                          sub_dt <- rbindlist(list(sub_dt, row_last), use.names = TRUE, fill = TRUE)
                          setnames(sub_dt, c("id_reader", "time", "delay"),
                                   c("id_reader_end", "time_end", "delay_end"))
                          # build the from-to data table
                          dup_dt[, names(sub_dt) := sub_dt]
                          dup_dt[, duration_ms := (time_end - time_start)*1000 + delay_end - delay_start]
                          dup_dt[, location := private$loc(id_reader_start, id_reader_end, duration_ms)]
                          # find duplicated events at the same id reader
                          # make the first event as combined and delete the others 
                          dup_dt[, dup := rleid(id_reader_start, id_reader_end, location)]
                          dup_dt[, ':='(id_reader_end = id_reader_end[.N],
                                        time_end = time_end[.N],
                                        delay_end = delay_end[.N],
                                        duration_ms = sum(duration_ms),
                                        location = ifelse(seq_len(.N) == 1, location, NA)),
                                 by = dup]
                          dup_dt <- dup_dt[!is.na(location) & duration_ms > 0]
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
                          res[same_id_reader] <- ifelse(duration[same_id_reader] >= 1000*private$loc_threshold,
                                                        private$config$cage[from[same_id_reader]],
                                                        as.character(from[same_id_reader]))
                          res[!same_id_reader] <- private$locmap[cbind(from[!same_id_reader], to[!same_id_reader])]
                          res
                        },
                        
                        reduce_dt = function(dt) {
                          dt <- copy(dt)
                          #check_required_columns(dt, c("start", "end"))
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
                          #check_required_columns(x, c("start", "end"))
                          #check_required_columns(y, c("start", "end"))
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
                        raw = function(val) {
                          if (missing(val))
                            copy(private$.raw)
                          else
                            stop("Raw data are read-only.")
                        },
                        
                        idlist = function(val) {
                          if (missing(val))
                            copy(private$.idlist)
                          else
                            stop("ID list is read-only.")
                        },
                        timeline = function(val) {
                          if (missing(val))
                            copy(private$.timeline)
                          else
                            stop("Timeline is read-only.")
                        },
                        timeline_bin = function(val) {
                          if (missing(val)) {
                            tlb <- copy(private$.timeline_bin)
                            if (is.null(tlb))
                              return(tlb)
                            tlb[, ':='(start = as.POSIXct(start, origin = "1970-01-01", tz = private$timezone),
                                       end = as.POSIXct(end, origin = "1970-01-01", tz = private$timezone))]
                            tlb[]
                          }
                          else
                            stop("Binned timeline is read-only.")
                        }
                      ),
                      public = list(
                        initialize = function(raw_files = NULL, idfile = NULL,
                                              timefile = NULL, timezone = Sys.timezone()) {
                          if (is.null(raw_files))
                            stop("Raw files unavailable.")
                          if (is.null(idfile))
                            stop("ID file unavailable.")
                          if (is.null(timefile))
                            stop("Timeline file unavailable.")
                          
                          # read the raw data text files
                          raw_data <- rbindlist(lapply(sort(raw_files), fread),
                                                use.names = TRUE, fill = TRUE)
                          setnames(raw_data, c("V1", "V2", "V3"),
                                   c("timestamp", "rfid", "id_reader"))
                          raw_data <- unique(raw_data)
                          # convert the timestamp into POSIXct time (in second) and millisecond
                          raw_data[, time := as.numeric(fast_strptime(substr(timestamp, 1, 15),
                                                                      format = "%Y%m%d_%H%M%S",
                                                                      tz = timezone))]
                          raw_data[, delay := as.integer(substr(timestamp, 16, 18))]
                          # TODO: address winter DST ambiguity. R treat it as non-DST
                          private$.raw <- raw_data[order(time, delay)]
                          
                          # read mouse id file and timeline config file
                          private$.idlist <- fread(idfile)
                          private$.timeline <- fread(timefile)
                          # fread recognize time string "YYYY-MM-DD HH:MM:SS"as UTC by default
                          # convert it to the system time zone (where the data were acquired)
                          private$.timeline[, ':='(start = force_tz(start, tzone = timezone),
                                                   end = force_tz(end, tzone = timezone))]
                          private$.timeline[]
                          private$timezone <- timezone
                          
                          # validate the structure of raw data
                          check_required_columns(private$.raw, c("rfid", "id_reader", "time", "delay"))
                          check_required_columns(private$.idlist, c("rfid", "mid", "loc"))
                          check_required_columns(private$.timeline, c("phase", "start", "end"))
                        },
                        
                        set_binsize = function(binsize) {
                          private$.timeline_bin <- private$.timeline[, private$divide(.SD, binsize), by = .I]
                          private$.timeline_bin[, ':='(I = NULL,
                                                       phase = factor(phase, levels = phase),
                                                       binsize = end - start)]
                        },
                        
                        calc_events = function(threshold = 2) {
                          if (!is.numeric(threshold)) {
                            stop("'threshold' must be numeric or integer")
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
                          cat(sprintf("%d partial records found, %d records fixed",
                                      private$n_partial, private$n_fixed))
                          private$.raw <- private$.raw[!unfixed]
                          
                          # Step 2. Get individual events
                          private$.events <- private$.raw[, private$raw_events(.SD),
                                                          by = rfid,
                                                          .SDcols = c("id_reader", "time", "delay")]
                          setkeyv(private$.events, private$key_cols)
                          private$results[["events"]] <- copy(private$.events)
                        },
                        
                        edit_events = function() {
                          events_original <- copy(private$.events)
                          events_edit <- copy(private$results[["events"]])
                          if (is.null(events_original) || is.null(events_edit)) {
                            stop("Events data.table does not exist")
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
                                                          fluidRow(column(6,
                                                                          selectInput("rfid","Filter by RFID",
                                                                                      choices = c("All" = "", uid))),
                                                                   column(6,
                                                                          numericInput("duration","Minimal duration (s)",
                                                                                       value = 60,
                                                                                       min = 0,
                                                                                       step = 1))),
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
                                                 edit_count = 0)
                            # leftTable data: filtered by location, rfid and duration_ms
                            filteredLeft <- reactive({
                              d <- rv$prepared
                              if (nzchar(input$rfid))
                                d <- d[rfid == input$rfid]
                              d[duration_ms >= input$duration * 1000]
                            })
                            #rightTable data: filtered by the same rfid as leftTable
                            filteredRight <- reactive({
                              count <- rv$edit_count
                              d <- rv$data
                              if (nzchar(input$rfid))
                                d <- d[rfid == input$rfid]
                              d
                            })
                            
                            proxy <- dataTableProxy("rightTable")
                            # leftTable output: filtered original data, read-only
                            output$leftTable <- renderDT({
                              datatable(filteredLeft(), editable = FALSE, selection = "single",
                                        options = list(dom = 'ltip', ordering = FALSE,
                                                       scrollX = TRUE, autoWidth = TRUE))
                            }, server = TRUE)
                            # rightTable output: only editing location is allowed
                            # monitor page length changes
                            # jump to page
                            output$rightTable <- renderDT({
                              datatable(filteredRight(),
                                        editable = list(target = "cell",
                                                        disable = list(columns = 1:8)),
                                        selection = "single",
                                        options = list(dom = 'ltip', ordering = FALSE,
                                                       scrollX = TRUE, autoWidth = TRUE,
                                                       initComplete = JS(
                                                         "function(settings, json) {",
                                                         "  var table = this.api();",
                                                         "  Shiny.setInputValue('rightTable_dt_ready', true);",
                                                         "  Shiny.setInputValue('rightTable_pageLength', table.page.len());",
                                                         "  table.on('length.dt', function(e, settings, len) {",
                                                         "    Shiny.setInputValue('rightTable_pageLength', len);",
                                                         "  });",
                                                         "  Shiny.addCustomMessageHandler('gotoPage', function(page){",
                                                         "    if(page === null || isNaN(page)) return;",
                                                         "    table.page(page).draw(false);",
                                                         "  });",
                                                         "}"
                                                       )))
                            }, server = TRUE)
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
                              set(rv$data, edit_row, col, v)
                              rv$edit_count <- rv$edit_count + 1
                              replaceData(proxy, filteredRight(), resetPaging = FALSE)
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
                        
                        calc_single = function(loc = c("cages", "all")) {
                          if (is.null(private$results[["events"]])) {
                            stop("Events data.table does not exist")
                          }
                          # argument loc must be "cages" (by default) or "all"
                          loc <- switch(match.arg(loc, c("cages", "all")),
                                        cages = unique(private$config$cage),
                                        all = unique(private$results[["events"]]$location))
                          
                          # Step 1. Convert start and end time to milliseconds from the beginning
                          events_dt <- copy(private$results[["events"]])
                          events_dt <- events_dt[location %in% loc]
                          offset <- min(events_dt$time_start)
                          events_dt[, ':='(start = (time_start - offset)*1000 + delay_start,
                                           end = (time_end - offset)*1000 + delay_end)]
                          
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
                          single_dt[, ':='(time_start = offset + start %/% 1000,
                                           delay_start = start %% 1000,
                                           time_end = offset + end %/% 1000,
                                           delay_end = end %% 1000,
                                           duration_ms = end - start)]
                          single_dt <- single_dt[duration_ms > 0][order(start, end)]
                          single_dt[, c("start", "end") := NULL]
                          setcolorder(single_dt, c("rfid", "time_start",
                                                   "delay_start", "time_end", "delay_end",
                                                   "duration_ms", "location"))
                          private$results[["single"]] <- single_dt
                        },
                        
                        calc_pair = function(loc = c("cages", "all")) {
                          if (is.null(private$results[["events"]])) {
                            stop("Events data.table does not exist")
                          }
                          # argument loc must be "cages" (by default) or "all"
                          loc <- switch(match.arg(loc, c("cages", "all")),
                                        cages = unique(private$config$cage),
                                        all = unique(private$results[["events"]]$location))
                          
                          # Step 1. Convert start and end time to milliseconds from the beginning
                          events_dt <- copy(private$results[["events"]])
                          events_dt <- events_dt[location %in% loc]
                          offset <- min(events_dt$time_start)
                          events_dt[, ':='(start = (time_start - offset)*1000 + delay_start,
                                           end = (time_end - offset)*1000 + delay_end)]
                          
                          # Step 2. Calculate overlaps
                          # foverlaps treats intervals as closed at both start and end
                          # and generates overlaps with 0 duration_ms
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
                          # remove overlaps with 0 duration_ms
                          # sort the data.table by .(start, end)
                          pair_dt <- unique(pair_dt)
                          pair_dt[, ':=' (rfid = paste(rfid1, rfid2, sep = "_"),
                                          time_start = offset + start %/% 1000,
                                          delay_start = start %% 1000,
                                          time_end = offset + end %/% 1000,
                                          delay_end = end %% 1000,
                                          duration_ms = end - start)]
                          pair_dt <- pair_dt[duration_ms > 0][order(start, end)]
                          pair_dt[, c("start", "end") := NULL]
                          setcolorder(pair_dt, c("rfid1", "rfid2", "rfid",
                                                 "time_start", "delay_start",
                                                 "time_end", "delay_end",
                                                 "duration_ms", "location"))
                          private$results[["pair"]] <- pair_dt
                        },
                        
                        calc_follow = function(loc = c("tubes", "all"),
                                               mode = c("in_place", "delayed"),
                                               threshold = 2) {
                          if (is.null(private$results[["events"]])) {
                            stop("Events data.table does not exist")
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
                            stop("'threshold' must be numeric or integer")
                          }
                          private$follow_threshold <- threshold
                          
                          # Step 1. Prepare the followee and follower data
                          followee_dt <- copy(private$results[["events"]])
                          followee_dt <- followee_dt[location %in% loc]
                          follower_dt <- copy(followee_dt)
                          offset <- min(followee_dt$time_start)
                          if (mode == "delayed") {
                            followee_dt[, ':='(time_end = time_start + threshold, delay_end = delay_start)]
                          }
                          followee_dt[, ':='(start = (time_start - offset)*1000 + delay_start,
                                             end = (time_end - offset)*1000 + delay_end)]
                          follower_dt[, ':='(start = (time_start - offset)*1000 + delay_start,
                                             end = (time_start - offset)*1000 + delay_start)]
                          
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
                                                        time_start,
                                                        delay_start,
                                                        time_end = i.time_start,
                                                        delay_end = i.delay_start,
                                                        start,
                                                        end = i.start)]
                          # to do: check if the followee stays at the reader after passing the tube
                          
                          # Step 3. Clean up the results
                          # rfid1 (before "|") as the followee; rfid2 (after "|") as the follower
                          # duration_ms is the delay of entries in each followee-follower pair
                          follow_dt[, ':='(rfid = paste(rfid1, rfid2, sep = "|"),
                                           duration_ms = end - start)]
                          follow_dt <- follow_dt[duration_ms > 0][order(start, end)]
                          follow_dt[, c("start", "end") := NULL]
                          setcolorder(follow_dt, c("rfid1", "rfid2", "rfid", "time_start",
                                                   "delay_start", "time_end", "delay_end",
                                                   "duration_ms", "location"))
                          private$results[["follow"]] <- follow_dt
                        },
                        
                        calc_activity = function(name = c("events", "single", "pair", "follow")) {
                          name <- match.arg(name, c("events", "single", "pair", "follow"))
                          if (is.null(private$.timeline_bin)) {
                            stop("'timeline_bin' is missing")
                          }
                          activity_dt <- self$get_result(name)
                          
                          # make all possible combinations of phase, rfid and location
                          # some "pair" or "follow" id pairs may be missing at a location or phase
                          # phase and rfid are extracted from metadata md
                          # location is extracted from the events data.table
                          offset <- min(private$.timeline_bin$start)
                          bin_breaks <- (c(private$.timeline_bin$start,
                                           private$.timeline_bin$end[nrow(private$.timeline_bin)]) -
                                           offset)*1000
                          bin_labels <- private$.timeline_bin$phase
                          bin_size <- setNames(private$.timeline_bin$binsize,
                                               private$.timeline_bin$phase)
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
                          visits_dt[, idx := findInterval((time_start - offset)*1000 +
                                                            delay_start, bin_breaks)]
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
                          time_dt[, ':='(start = (time_start - offset)*1000 + delay_start,
                                         end = (time_end - offset)*1000 + delay_end)]
                          time_dt[, ':='(idx_start = findInterval(start, bin_breaks),
                                         idx_end = findInterval(end - 1, bin_breaks))]
                          time_dt <- time_dt[idx_start <= idx_end, .(idx = idx_start : idx_end),
                                             by = .(rfid, location, start, end)]
                          time_dt <- time_dt[idx > 0 & idx < length(bin_breaks)]
                          time_dt[, ':='(phase = bin_labels[idx],
                                         new_start = pmax(start, bin_breaks[idx]),
                                         new_end = pmin(end, bin_breaks[idx + 1]))]
                          time_dt[, duration_ms := new_end - new_start]
                          time_res <- time_dt[, .(duration_ms = sum(duration_ms)),
                                              by = .(phase, rfid, location)]
                          time_res <- merge(all_combo, time_res,
                                            by = c("phase", "rfid", "location"), all.x = TRUE)
                          time_res[is.na(duration_ms), duration_ms := 0]
                          time_res[, ratio := duration_ms/bin_size[phase]/1000]
                          if (name == "pair") {
                            co_dt <- self$get_result("events_time")[location %in%
                                                                      unique(private$results[["pair"]]$location)]
                            co_dt <- co_dt[, {
                              idx <- combn(.N, 2)
                              rfid1 = rfid[idx[1,]]
                              rfid2 = rfid[idx[2,]]
                              data.table(rfid = paste0(pmin(rfid1, rfid2), "_", pmax(rfid1, rfid2)),
                                         co_duration = duration_ms[idx[1,]] * duration_ms[idx[2,]])
                            }, by = .(phase, location)]
                            co_dt[, adjust := co_duration/bin_size[phase]**2/1e6]
                            setkey(co_dt, phase, rfid, location)
                            time_res <- time_res[co_dt, on = .(phase, rfid, location),
                                                 .(phase, rfid, location, duration_ms, ratio = ratio - i.adjust)]
                          }
                          private$results[[paste(name, "time", sep = "_")]] <- time_res
                        },
                        
                        all_results = function() {
                          names(private$results)
                        },
                        
                        get_result = function(result_name) {
                          if (!result_name %in% names(private$results))
                            stop(sprintf("Result '%s' not found", result_name))
                          copy(private$results[[result_name]])
                        },
                        
                        plot_occupancy = function() {
                          dt <- self$get_result("events_time")[location %in% c("A", "B", "C", "D")]
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