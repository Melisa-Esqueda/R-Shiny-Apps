
# Title: Text Analysis Shiny App
# Description: A Shiny App that visualizes the results of a text analysis
# based on the names and titles of R packages. 
# Author: Melisa Esqueda
# Date: 4/28/21

library(dplyr)
library(ggplot2)
library(stringr)

# import csv of package names and descriptions
dat <-read.csv("rpackages.csv", stringsAsFactors = FALSE)


ui <- fluidPage(
    
    titlePanel("Text Analysis of R Packages"),
    fluidRow(
        column(3,
               p(em("Main filter")),
               radioButtons(inputId = "Initial", 
                            label = "Initial letter of name", 
                            choices = c("lower case" = "ini_lower",
                                        "upper case" = "ini_upper",
                                        "both (lower & upper)" = "ini_both"), 
                            selected = "ini_both")
        ),
        
        column(3,
               p(em("More filters")),
               selectInput(inputId = "numbers", 
                           label = "Name contains numbers",
                           choices = c("optional" = "num_opt",
                                       "only numbers" = "num_yes",
                                       "no numbers" = "num_no")),
               selectInput(inputId = "dot", 
                           label = "Name contains dot(s)",
                           choices = c("optional" = "dot_opt",
                                       "yes dot(s)" = "dot_yes",
                                       "no dot(s)" = "dot_no"))
        ),
        
        column(3,
               p(em("Barchart options")),
               radioButtons(inputId = "arrange", 
                            label = "Order bars by:", 
                            choices = c("decreasing freq" = "arr_dec",
                                        "increasing freq" = "arr_inc",
                                        "alphabetical a-z" = "arr_a2z",
                                        "alphabetical z-a" = "arr_z2a"),
                            selected = "arr_dec")
        ),
        
        column(3,
               p(em("Histogram options")),
               sliderInput(inputId = "binwidth",
                           label = "Binwidth",
                           min = 1,
                           max = 20,
                           value = 1),
               checkboxInput(inputId = "facets",
                             label = strong("Facet by letter"),
                             value = FALSE)
        )
    ),
    hr(),
    
    tabsetPanel(type = "tabs",
                tabPanel("Barchart",
                         h3("Number of packages by initial letter"),
                         plotOutput("barplot"),
                         hr(),
                         verbatimTextOutput('table1')),
                tabPanel("Histogram", 
                         h3("Length of package names"),
                         plotOutput("histogram"),
                         hr(),
                         verbatimTextOutput('table2'))
    )
)



server <- function(input, output){
    
    dat_freq <- reactive({
        first_letter = str_extract(dat$Package, regex('^[A-Z|a-z]{1}'))
        dat$Initial = first_letter
        if ((input$dot == "dot_yes") & ((input$numbers == "num_no")|(input$numbers == "num_opt"))){
            dots = str_extract(dat$Package, regex('[A-Za-z]+[\\.][a-zA-Z]+'))
            dots_only = filter(dat, Package == dots)
            dat = dots_only
            
        }else if (input$dot == "dot_yes" & input$numbers == "num_yes"){
            nums_dots = str_extract(dat$Package, regex('([0-9a-z]+[\\.]+[a-zA-Z]+[0-9]+)'))
            nums_dots_only = filter(dat, Package == nums_dots)
            dat = nums_dots_only
            
        }else if ((input$dot == "dot_no")|(input$dot == "dot_opt") & (input$numbers == "num_no")|(input$numbers == "num_opt")){
            no_dots_nums = str_extract(dat$Package, regex('[a-zA-Z]+'))
            no_dots_nums_only = filter(dat, Package == no_dots_nums)
            dat = no_dots_nums_only
            
        }
        else if (((input$dot == "dot_no")|(input$dot == "dot_opt")) & input$numbers == "num_yes"){
            nums = str_extract(dat$Package, regex('([a-zA-Z]+[0-9]+[a-zA-Z]+)|([a-zA-Z]+[0-9]+)'))
            nums_only = filter(dat, Package == nums)
            dat = nums_only

        }else{
            dat
        }
        if (input$Initial == "ini_lower"){
            lowers = str_extract(dat$Package, regex('^[a-z]{1}'))
            freq = filter(dat, Initial == lowers)
            lowerss_count = freq %>% group_by(Initial) %>% tally()
            lowerss_count$Count = lowerss_count$n
            lowerss_count$n = NULL
            freqs = lowerss_count
           
        }else if(input$Initial == "ini_upper"){
            uppers = str_extract(dat$Package, regex('^[A-Z]{1}'))
            freq = filter(dat, Initial == uppers)
            freq
            upperss_count = freq %>% group_by(Initial) %>% tally()
            upperss_count$Count = upperss_count$n
            upperss_count$n = NULL
            freqs = upperss_count
           
        }else{
            both = str_extract(dat$Package, regex('^[a-zA-z]{1}'))
            freq = filter(dat, Initial == both)
            freq
            lowers= str_extract(dat$Package, regex('^[a-z]{1}'))
            uppers = str_extract(dat$Package, regex('^[A-Z]{1}'))
            freq$Upper_case = uppers
            freq$Lower_Case = lowers
            freq
            if(nrow(freq) == 644){
                count_upper = freq %>% group_by(Upper_case) %>% tally()
                count_upper$count_1 = count_upper$n
                count_upper$n = NULL
                count_upper
                
                k = c("K", 0)
                y = c("Y", 0)
                add_ups_1 = rbind(count_upper, k)
                add_ups_2 = rbind(add_ups_1,y)
                remove_na = na.omit(add_ups_2)
                remove_na
                uppers_order = arrange(remove_na, Upper_case)
                uppers_order
                
                count_lower = freq %>% group_by(Lower_Case) %>% tally()
                count_lower$count_2 = count_lower$n
                count_lower$n = NULL
                count_lower
                y = c("y", 0)
                add_lows = rbind(count_lower, y)
                remove_na_2 = na.omit(add_lows)
                remove_na_2
                lowers_order =arrange(remove_na_2, Lower_Case)
                lowers_order
                
                ups_and_lows = cbind(uppers_order, lowers_order)
                ups_and_lows
                ups_and_lows$Initial <- paste(ups_and_lows$Lower_Case, "|", ups_and_lows$Upper_case)
                ups_and_lows
                ups_and_lows$Count= as.numeric(ups_and_lows$count_1) + as.numeric(ups_and_lows$count_2) 
                ups_and_lows_df = select(ups_and_lows, Initial, Count)
                freqs = ups_and_lows_df
                
            }else if(nrow(freq) == 476){
                count_upper = freq %>% group_by(Upper_case) %>% tally()
                count_upper$count_1 = count_upper$n
                count_upper$n = NULL
                count_upper
                
                j = c("J", 0)
                k = c("K", 0)
                w = c("W", 0)
                x = c("X", 0)
                y = c("Y", 0)
                z = c("Z", 0)
                add_ups_1 = rbind(count_upper, j)
                add_ups_2 = rbind(add_ups_1, k)
                add_ups_3 = rbind(add_ups_2, w)
                add_ups_4 = rbind(add_ups_3, x)
                add_ups_5 = rbind(add_ups_4, y)
                add_ups_6 = rbind(add_ups_5, z)
                remove_na = na.omit(add_ups_6)
                remove_na
                uppers_order = arrange(remove_na, Upper_case)
                uppers_order
                
                count_lower = freq %>% group_by(Lower_Case) %>% tally()
                count_lower$count_2 = count_lower$n
                count_lower$n = NULL
                count_lower
                v = c("v", 0)
                y = c("y", 0)
                add_lows_1 = rbind(count_lower, v)
                add_lows_2 = rbind(add_lows_1, y)
                remove_na_2 = na.omit(add_lows_2)
                remove_na_2
                lowers_order =arrange(remove_na_2, Lower_Case)
                lowers_order
                
                ups_and_lows = cbind(uppers_order, lowers_order)
                ups_and_lows
                ups_and_lows$Initial <- paste(ups_and_lows$Lower_Case, "|", ups_and_lows$Upper_case)
                ups_and_lows
                ups_and_lows$Count= as.numeric(ups_and_lows$count_1) + as.numeric(ups_and_lows$count_2) 
                ups_and_lows_df = select(ups_and_lows, Initial, Count)
                freqs = ups_and_lows_df
            }else if(nrow(freq) == 504){
                count_upper = freq %>% group_by(Upper_case) %>% tally()
                count_upper$count_1 = count_upper$n
                count_upper$n = NULL
                count_upper
                
                j = c("J", 0)
                k = c("K", 0)
                w = c("W", 0)
                x = c("X", 0)
                y = c("Y", 0)
                z = c("Z", 0)
                add_ups_1 = rbind(count_upper, j)
                add_ups_2 = rbind(add_ups_1, k)
                add_ups_3 = rbind(add_ups_2, w)
                add_ups_4 = rbind(add_ups_3, x)
                add_ups_5 = rbind(add_ups_4, y)
                add_ups_6 = rbind(add_ups_5, z)
                remove_na = na.omit(add_ups_6)
                remove_na
                uppers_order = arrange(remove_na, Upper_case)
                uppers_order
                
                count_lower = freq %>% group_by(Lower_Case) %>% tally()
                count_lower$count_2 = count_lower$n
                count_lower$n = NULL
                count_lower
                v = c("v", 0)
                y = c("y", 0)
                add_lows_1 = rbind(count_lower, v)
                add_lows_2 = rbind(add_lows_1, y)
                remove_na_2 = na.omit(add_lows_2)
                remove_na_2
                lowers_order =arrange(remove_na_2, Lower_Case)
                lowers_order
                
                ups_and_lows = cbind(uppers_order, lowers_order)
                ups_and_lows
                ups_and_lows$Initial <- paste(ups_and_lows$Lower_Case, "|", ups_and_lows$Upper_case)
                ups_and_lows
                ups_and_lows$Count= as.numeric(ups_and_lows$count_1) + as.numeric(ups_and_lows$count_2) 
                ups_and_lows_df = select(ups_and_lows, Initial, Count)
                freqs = ups_and_lows_df
                
            }else if(nrow(freq) == 16232){
                count_upper = freq %>% group_by(Upper_case) %>% tally()
            count_upper$count_1 = count_upper$n
            count_upper$n = NULL
            count_upper
            
            remove_na = na.omit(count_upper)
            remove_na
            uppers_order = arrange(remove_na, Upper_case)
            uppers_order
            
            count_lower = freq %>% group_by(Lower_Case) %>% tally()
            count_lower$count_2 = count_lower$n
            count_lower$n = NULL
            count_lower
            remove_na_2 = na.omit(count_lower)
            remove_na_2
            lowers_order =arrange(remove_na_2, Lower_Case)
            lowers_order
            
            ups_and_lows = cbind(uppers_order, lowers_order)
            ups_and_lows
            ups_and_lows$Initial <- paste(ups_and_lows$Lower_Case, "|", ups_and_lows$Upper_case)
            ups_and_lows
            ups_and_lows$Count= as.numeric(ups_and_lows$count_1) + as.numeric(ups_and_lows$count_2) 
            ups_and_lows_df = select(ups_and_lows, Initial, Count)
            freqs = ups_and_lows_df
            }else if(nrow(freq) == 7){
                count_upper = freq %>% group_by(Upper_case) %>% tally()
                count_upper$count_1 = count_upper$n
                count_upper$n = NULL
                count_upper
                
                a = c("A", 0)
                b = c("B", 0)
                c = c("C", 0)
                d = c("D", 0)
                e = c("E", 0)
                f = c("F", 0)
                g = c("G", 0)
                h = c("H", 0)
                i = c("I", 0)
                j = c("J", 0)
                k = c("K", 0)
                l = c("L", 0)
                m = c("M", 0)
                n = c("N", 0)
                o = c("O", 0)
                p = c("P", 0)
                q = c("Q", 0)
                r = c("R", 0)
                s = c("S", 0)
                t = c("T", 0)
                u = c("U", 0)
                v = c("V", 0)
                w = c("W", 0)
                x = c("X", 0)
                y = c("Y", 0)
                z = c("Z", 0)
                add_ups_1 = rbind(count_upper, a)
                add_ups_2 = rbind(add_ups_1,b)
                add_ups_3 = rbind(add_ups_2, c)
                add_ups_4 = rbind(add_ups_3,d)
                add_ups_5 = rbind(add_ups_4, e)
                add_ups_6 = rbind(add_ups_5,f)
                add_ups_7 = rbind(add_ups_6, g)
                add_ups_8 = rbind(add_ups_7,h)
                add_ups_9 = rbind(add_ups_8, i)
                add_ups_10 = rbind(add_ups_9,j)
                add_ups_11 = rbind(add_ups_10, k)
                add_ups_12 = rbind(add_ups_11,l)
                add_ups_13 = rbind(add_ups_12, m)
                add_ups_14 = rbind(add_ups_13,n)
                add_ups_15 = rbind(add_ups_14, o)
                add_ups_16 = rbind(add_ups_15,p)
                add_ups_17 = rbind(add_ups_16, q)
                add_ups_18 = rbind(add_ups_17,r)
                add_ups_19 = rbind(add_ups_18, s)
                
                add_ups_20 = rbind(add_ups_19,t)
                add_ups_21 = rbind(add_ups_20, u)
                add_ups_22 = rbind(add_ups_21,v)
                add_ups_23 = rbind(add_ups_22, w)
                add_ups_24 = rbind(add_ups_23,x)
                add_ups_25 = rbind(add_ups_24, y)
                add_ups_26 = rbind(add_ups_25,z)
                remove_na = na.omit(add_ups_26)
                remove_na
                uppers_order = arrange(remove_na, Upper_case)
                uppers_order
                
                count_lower = freq %>% group_by(Lower_Case) %>% tally()
                count_lower$count_2 = count_lower$n
                count_lower$n = NULL
                count_lower
                c = c("c", 0)
                d = c("d", 0)
                e = c("e", 0)
                f = c("f", 0)
                g = c("g", 0)
                h = c("h", 0)
                j = c("j", 0)
                k = c("k", 0)
                m = c("m", 0)
                n = c("n", 0)
                o = c("o", 0)
                p = c("p", 0)
                q = c("q", 0)
                r = c("r", 0)
                t = c("t", 0)
                u = c("u", 0)
                v = c("v", 0)
                w = c("w", 0)
                x = c("x", 0)
                y = c("y", 0)
                z = c("z", 0)
                
                add_lows_1 = rbind(count_lower, c)
                add_lows_2 = rbind(add_lows_1,d)
                add_lows_3 = rbind(add_lows_2, e)
                add_lows_4 = rbind(add_lows_3,f)
                add_lows_5 = rbind(add_lows_4, g)
                add_lows_6 = rbind(add_lows_5,h)
                add_lows_7 = rbind(add_lows_6, j)
                add_lows_8 = rbind(add_lows_7,m)
                add_lows_9 = rbind(add_lows_8, n)
                add_lows_10 = rbind(add_lows_9,o)
                add_lows_11 = rbind(add_lows_10, p)
                add_lows_12 = rbind(add_lows_11,q)
                add_lows_13 = rbind(add_lows_12, r)
                add_lows_14 = rbind(add_lows_13,t)
                add_lows_15 = rbind(add_lows_14, u)
                add_lows_16 = rbind(add_lows_15,v)
                add_lows_17 = rbind(add_lows_16, w)
                add_lows_18 = rbind(add_lows_17,x)
                add_lows_19 = rbind(add_lows_18, y)
                
                add_lows_20 = rbind(add_lows_19,z)
                remove_na_2 = na.omit(add_lows_20)
                remove_na_2
                lowers_order =arrange(remove_na_2, Lower_Case)
                lowers_order
                
                ups_and_lows = cbind(uppers_order, lowers_order)
                ups_and_lows
                ups_and_lows$Initial <- paste(ups_and_lows$Lower_Case, "|", ups_and_lows$Upper_case)
                ups_and_lows
                ups_and_lows$Count= as.numeric(ups_and_lows$count_1) + as.numeric(ups_and_lows$count_2) 
                ups_and_lows_df = select(ups_and_lows, Initial, Count)
                freqs = ups_and_lows_df
                freqs
            }else{
                count_upper = freq %>% group_by(Upper_case) %>% tally()
                count_upper$count_1 = count_upper$n
                count_upper$n = NULL
                count_upper
                
                remove_na = na.omit(count_upper)
                remove_na
                uppers_order = arrange(remove_na, Upper_case)
                uppers_order
                
                count_lower = freq %>% group_by(Lower_Case) %>% tally()
                count_lower$count_2 = count_lower$n
                count_lower$n = NULL
                count_lower
                remove_na_2 = na.omit(count_lower)
                remove_na_2
                lowers_order =arrange(remove_na_2, Lower_Case)
                lowers_order
                
                ups_and_lows = cbind(uppers_order, lowers_order)
                ups_and_lows
                ups_and_lows$Initial <- paste(ups_and_lows$Lower_Case, "|", ups_and_lows$Upper_case)
                ups_and_lows
                ups_and_lows$Count= as.numeric(ups_and_lows$count_1) + as.numeric(ups_and_lows$count_2) 
                ups_and_lows_df = select(ups_and_lows, Initial, Count)
                freqs = ups_and_lows_df
                
            
        }
        dat_tbl = freqs
        
        if (input$arrange == "arr_dec"){
            order_by = arrange(dat_tbl, desc(Count))
            order_by
        }else if (input$arrange == "arr_inc"){
            order_by = arrange(dat_tbl, Count)
            order_by
        }else if(input$arrange == "arr_a2z"){
            order_by = arrange(dat_tbl, Initial)
            order_by
        }else{
            order_by = arrange(dat_tbl, desc(Initial))
            order_by
        }
        
        
        
    }})
    
    dat_len <- reactive({
    first_letter = str_extract(dat$Package, regex('^[A-Z|a-z]{1}'))
    dat$Initial = first_letter
    remove_newlines = str_replace_all(dat$Title, "\n", "")
    remove_punc = str_replace_all(remove_newlines, "[[:punct:]\\s]", "")
    remove_punc
    dat$Title = remove_punc
    dat$Length = nchar(dat$Title)
    dat$length = NULL
    dat$Start = dat$Initial
    dat_len_df = select(dat, Start, Length)
    dat_len_df
    
    
    })
    
    
    output$barplot <- renderPlot({
        if (input$arrange == "arr_dec"){
        ggplot(data = dat_freq(),aes(x = reorder(Initial, -Count), y = Count, color = "black", fill = "red")) + 
           geom_col()+labs(title = "number of packages by initial letter") +
           guides(color = FALSE, fill=FALSE) + xlab("Initial") + 
                geom_hline(aes(yintercept = mean(Count)),col='blue',size=0.5)
        }else if (input$arrange == "arr_inc"){
        ggplot(data = dat_freq(),aes(x = reorder(Initial, Count), y = Count, color = "black", fill = "red")) + 
           geom_col()+labs(title = "number of packages by initial letter") + 
            guides(color = FALSE, fill=FALSE) + xlab("Initial") + geom_hline(aes(yintercept = mean(Count)),col='blue',size=0.5)
        }else if(input$arrange == "arr_a2z"){
        ggplot(data = dat_freq(),aes(x = Initial, y = reorder(Count, Initial), color = "black", fill = "red")) + 
           geom_col()+labs(title = "number of packages by initial letter") + 
            guides(color = FALSE, fill=FALSE) + ylab("Count") + geom_hline(aes(yintercept = mean(Count)),col='blue',size=0.5)
        }else{
       ggplot(data = dat_freq(),aes(x = reorder(Initial, desc(Initial)), y = Count, color = "black", fill = "red")) + 
        geom_col()+labs(title = "number of packages by initial letter") + 
        guides(color = FALSE, fill=FALSE) + xlab("Initial") + geom_hline(aes(yintercept = mean(Count)),col='blue',size=0.5)
        }
        
    })
    
    output$table1 <- renderPrint({
       summary(dat_freq())
    })
    
    
    output$histogram <- renderPlot({
        data = dat_len()
       if (input$Initial == "ini_lower"){
           lowers = str_extract(data$Start, regex('^[a-z]{1}'))
           freq = filter(data, Start %in% lowers)
           dat_lens = freq
           dat_lens
        }else if(input$Initial == "ini_upper"){
            uppers = str_extract(data$Start, regex('^[A-Z]{1}'))
            freq = filter(data, Start %in% uppers)
            dat_lens = freq
            dat_lens
            
        }else{
            both = str_extract(data$Start, regex('^[a-zA-z]{1}'))
            freq = filter(data, Start %in% both)
            dat_lens = freq
            dat_lens
        }

        if (input$facets == TRUE){
        ggplot(data = dat_lens, aes(x = Length, color = "black", fill = "red")) +
            geom_histogram(binwidth = input$binwidth) + guides(color = FALSE, fill=FALSE) + facet_wrap(~ Start) + 
                geom_vline(aes(xintercept = mean(Length)),col='blue',size= 0.5)
        }else{
        
        ggplot(data = dat_lens, aes(x = Length, color = "black", fill = "red")) +
            geom_histogram(binwidth = input$binwidth) + guides(color = FALSE, fill=FALSE) + 
                geom_vline(aes(xintercept = mean(Length)),col='blue',size=0.5)
    }})
    
    output$table2 <- renderPrint({
        summary(dat_len())
    })
    
}



shinyApp(ui = ui, server = server)

