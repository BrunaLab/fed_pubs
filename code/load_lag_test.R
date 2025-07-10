
# overview ----------------------------------------------------------------

# assess lag in loading papers to scopus to detemrine if drop off is "real" or
# is due to a delay in loading



# libraries ---------------------------------------------------------------


# install.packages("rscopus")
# install.packages("tidyverse")
library(rscopus)
library(tidyverse)



# direct search in scopus -------------------------------------------------

(AF-ID ( 60006577 ) OR AF-ID ( 60000870 ) OR AF-ID ( 60028170 ) OR
AF-ID ( 60013409 ) OR AF-ID ( 60027053 ) OR AF-ID ( 60104446 ) OR
AF-ID ( 60016619 ) OR AF-ID ( 60160460 ) OR AF-ID ( 60018432 ) OR
AF-ID ( 60007651 ) OR AF-ID ( 60005281 ) OR AF-ID ( 60022386 ) OR
AF-ID ( 60078461 ) OR AF-ID ( 60078460 ) OR AF-ID ( 60078458 ) OR
AF-ID ( 60078457 ) OR AF-ID ( 60078459 ) OR AF-ID ( 60013369 ) OR
AF-ID ( 60017336 ) OR AF-ID ( 60032254 ) OR AF-ID ( 60005449 ) OR
AF-ID ( 60027085 ) OR AF-ID ( 60017777 ) OR AF-ID ( 60023653 ) OR
AF-ID ( 60008438 ) OR AF-ID ( 60028652 ) OR AF-ID ( 60026049 ) OR
AF-ID ( 60003158 ) OR AF-ID ( 60013780 ) OR AF-ID ( 60024906 ) OR
AF-ID ( 60017252 ) OR AF-ID ( 60072521 ) OR AF-ID ( 60016730 ) OR
AF-ID ( 60013123 ) OR AF-ID ( 60020468 ) OR AF-ID ( 60021862 ) OR
AF-ID ( 60030317 ) OR AF-ID ( 60001536 ) OR AF-ID ( 60087823 ) OR
AF-ID ( 60011528 ) OR AF-ID ( 60019280 ) OR AF-ID ( 60013335 ) OR
AF-ID ( 60006614 ) OR AF-ID ( 60016257 ) OR AF-ID ( 60076790 )) 

AND (AF-ID ( 60006577 ) OR AF-ID ( 60000870 ) OR AF-ID ( 60028170 ) OR
AF-ID ( 60013409 ) OR AF-ID ( 60027053 ) OR AF-ID ( 60104446 ) OR
AF-ID ( 60016619 ) OR AF-ID ( 60160460 ) OR AF-ID ( 60018432 ) OR
AF-ID ( 60007651 ) OR AF-ID ( 60005281 ) OR AF-ID ( 60022386 ) OR
AF-ID ( 60078461 ) OR AF-ID ( 60078460 ) OR AF-ID ( 60078458 ) OR
AF-ID ( 60078457 ) OR AF-ID ( 60078459 ) OR AF-ID ( 60013369 ) OR
AF-ID ( 60017336 ) OR AF-ID ( 60032254 ) OR AF-ID ( 60005449 ) OR
AF-ID ( 60027085 ) OR AF-ID ( 60017777 ) OR AF-ID ( 60023653 ) OR
AF-ID ( 60008438 ) OR AF-ID ( 60028652 ) OR AF-ID ( 60026049 ) OR
AF-ID ( 60003158 ) OR AF-ID ( 60013780 ) OR AF-ID ( 60024906 ) OR
AF-ID ( 60017252 ) OR AF-ID ( 60072521 ) OR AF-ID ( 60016730 ) OR
AF-ID ( 60013123 ) OR AF-ID ( 60020468 ) OR AF-ID ( 60021862 ) OR
AF-ID ( 60030317 ) OR AF-ID ( 60001536 ) OR AF-ID ( 60087823 ) OR
AF-ID ( 60011528 ) OR AF-ID ( 60019280 ) OR AF-ID ( 60013335 ) OR
AF-ID ( 60006614 ) OR AF-ID ( 60016257 ) OR AF-ID ( 60076790 )) 

AND PUBYEAR = 2023

AND LOAD-DATE > 20240101


# AF-ID ( "University of Florida" 60013959 ) AND LOAD-DATE > 20240101 AND py<2024


# NIH
# 
affil<-c("AF-ID ( 60006577 ) OR AF-ID ( 60000870 ) OR AF-ID ( 60028170 ) OR
AF-ID ( 60013409 ) OR AF-ID ( 60027053 ) OR AF-ID ( 60104446 ) OR
AF-ID ( 60016619 ) OR AF-ID ( 60160460 ) OR AF-ID ( 60018432 ) OR
AF-ID ( 60007651 ) OR AF-ID ( 60005281 ) OR AF-ID ( 60022386 ) OR
AF-ID ( 60078461 ) OR AF-ID ( 60078460 ) OR AF-ID ( 60078458 ) OR
AF-ID ( 60078457 ) OR AF-ID ( 60078459 ) OR AF-ID ( 60013369 ) OR
AF-ID ( 60017336 ) OR AF-ID ( 60032254 ) OR AF-ID ( 60005449 ) OR
AF-ID ( 60027085 ) OR AF-ID ( 60017777 ) OR AF-ID ( 60023653 ) OR
AF-ID ( 60008438 ) OR AF-ID ( 60028652 ) OR AF-ID ( 60026049 ) OR
AF-ID ( 60003158 ) OR AF-ID ( 60013780 ) OR AF-ID ( 60024906 ) OR
AF-ID ( 60017252 ) OR AF-ID ( 60072521 ) OR AF-ID ( 60016730 ) OR
AF-ID ( 60013123 ) OR AF-ID ( 60020468 ) OR AF-ID ( 60021862 ) OR
AF-ID ( 60030317 ) OR AF-ID ( 60001536 ) OR AF-ID ( 60087823 ) OR
AF-ID ( 60011528 ) OR AF-ID ( 60019280 ) OR AF-ID ( 60013335 ) OR
AF-ID ( 60006614 ) OR AF-ID ( 60016257 ) OR AF-ID ( 60076790 )")

DOCS<-c("AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))")

LOAD-DATE<-c(" > 20240101")
AND (LOAD-DATE > 20230101 AND NOT < 20231231 ) BEF pr AFT
PY<- ("= 2023")
LOAD-DATE AFT 20250601 LOAD-DATE BEF 20250608

best ORIG-LOAD-DATE
# 2022
2022 articles: 9827
9659


# 2023
# 2023 articles: 9139
# loaded after 1 jan 2024: 7869
after 1 jan 2024 but < 2025: 3149

# NEEDTO: download list of load-date > 20230601 and see what months they were from (i.e., how many from each month before BEFORE 0601)  

AND PUBYEAR = 2025 AND ORIG-LOAD-DATE > 20250601
# 2024
# 2024 articles: 8668
# loaded after 1 jan 2025: 5456


  search_term<-c(
                 '127295808',
                 '106720691'

                 )
    
    
    yr1=2019
    yr2=2025
    
    date_range <- seq(yr1,yr2)
    year <- seq_along(date_range)
    term <- seq_along(search_term)
    
    for (h in term){
      
      for (j in year) {
        
        
        a<-paste("(AF-ID('",search_term[h],"')"," AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))",sep="")
        

        c <- " AND PUBYEAR = "
        
        query_string <-paste0(a, c, date_range[j],")",sep = "")
      
        
        scopus_data <- rscopus::scopus_search(query_string,
                                              max_count=8000,
                                              view = "COMPLETE",
                                              api_key = "")
        
        
        
        
        scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
        
        if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
          next
        }else{
        scopus_papers <- scopus_data_raw$df
  
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")
        papers <- paste("./data_raw/papers/",term_for_file,"_papers", ".csv", sep = "")
        write_csv(scopus_papers, papers)
        
        scopus_affiliations <- scopus_data_raw$affiliation
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")
        affils <- paste("./data_raw/affils/",term_for_file,"_affils", ".csv", sep = "")
        write_csv(scopus_affiliations, affils)
        
        scopus_authors <- scopus_data_raw$author
        term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],sep="")
        authors <- paste("./data_raw/authors/",term_for_file,"_author", ".csv", sep = "")
        write_csv(scopus_authors, authors)
        }
    }
    }
  

    
    

# Universities ------------------------------------------------------------

    library(rscopus)
    library(tidyverse)
    
    
    
    # inst, affil_code, inst_code
    # Harvard Medical School, 441574, 449547
    # search_term<-c('60002746', '60017087', '60100090', '60010869', '60020482',
    # '60145297', '60008361', '60014986', '60012535', '60104690', '60097350', 
    # '60019829', '60028457', '60016830' )

    Need 60009982 2024, 2023 60002746 2023-2025
    # Harvard University, 191469, 726046 (includes med school)
    search_term<-c( '60097058', '60001001', '60023563', '60079465',
    '60019666', '60138961', '60000650', '60077735', '60003705', '60010402',
    '60006303', '60145297', '60007624', '60077572', '60287955', '60007773',
    '60029819', '60099481', '60145405', '60017150', '60017087',
    '60100090', '60010869', '60020482', '60145297', '60008361', '60014986',
    '60012535', '60104690', '60097350', '60019829', '60028457', '60016830',
    '60005856', '60027452', '60032499', '60120583', '60005478', '60002564',
    '60021507', '60022098', '60027409', '60107878', '60107841', '60028031',
    '60077569', '60020680', '60031821', '60002294', '60006332', '60131697',
    '60013733', '60272417', '60122560', '60027589', '60018380', '60008668',
    '60028849', '60008789', '60100185', "60009982" ,'60002746')
    # 
    # STILL NEED: 60033182, 60025778
    # University of Michigan - Ann Arbor, 347907, 418348
    # search_term<-c('60073999', '60009933', '60155336', '60155338',
    # '60023614', '60007200', '60019230', '60008966', '60018937', '60012328',
    # '60013871', '60155338', '60013060', '60022590', '60009861',
    # '60027017', '60112769', '60019369', '60032546', '60016660', '60008535')
    
    # STILL NEED 60028548, 60015481
    # University of Washington, 329371, 373766
    # search_term<-c('60156837', '60011986', '60292186', '60116364',
    # '60014462', '60032180', '60073077', '60091926', '60138971', '60138687',
    # '60008032', '60008980', '60156836', '60001212', '60032180', '60012095',
    # '60028548', '60138689', '60033293', '60138688', '60000864', '60091917',
    # '60006602', '60279025', '60016643', '60278845', '60028661', '60156836')
    
    # STILL NEED 60006297
    # University of Pennsylvania, 269101, 368273, 60003711
    # search_term<-c('60107840', '60271725', '60073168', '60138958',
    # '60023009', '60274218', '60138417', '60278651', '60278649', '60274225',
    # '60020284', '60138419', '60138418', '60274219', '60102562', '60156402',
    # '60156401', '60156403', '60136676', '60030118', '60139001', '60004517',
    # '60120789', '60015802', '60102564', '60156402', '60156401',
    # '60139001', '60096942', '60102561', '60031115', '60138421', '60138420',
    # '60102563', '60022452', '60156403')
    
    # STILL NEED: 60012708, 60032838
    # Stanford University, 306475, 416740
    # search_term<-c('60075286', '60026163', '60091251', '60122630',
    # '60121420', '60025590', '60091251', '60023012', '60287975', '60091250',
    # '60141503', '60141511', '60141505', '60141708', '60141701', '60141711',
    # '60141508', '60078422', '60104839', '60003743', '60141509', '60141511',
    # '60091253', '60074650', '60091252', '60287975', '60020928', '60025911',
    # '60010417', '60274003', '60032063', '60010984', '60015615', '60015156',
    # '60005585', '60076515', '60141509', '60028364', '60091249',
    # '60141701' )
    
    # STILL NEED: '60027550', '60016081'
    # University of California - Los Angeles, 312016, 392573
    # search_term<-c('60117623', '60102435', '60085729',
    # '60101299', '60121420', '60078126', '60109559', '60102801', '60029270', 
    # '60032655', '60005247', '60087034', '60006511', '60020418', '60032023', 
    # '60085730', '60138296', '60026999', '60277579', '60153950', '60023227', 
    # '60138295' )
    

    # STILL NEED 60029929 for all years
    # Massachusetts General Hospital, 209891, 215786	
    # search_term<-c('60026147', '60007377', '60102779',
    # '60008130', '60159760' )
    ----

    # STILL NEED 60023691 2024,2023    60031970 in 2024,2023
    # University of California - San Francisco, 241713, 265358
    # search_term<-c('60076081', '60226504', '60120814', '60008261', 
    # '60002860', '60014751', '60028203', '60029881', '60001294', '60030839', 
    # '60017805', '60033399', '60033283', '60006491', '60074643', '60031970', 
    # '60280125', '60280126', '60280125' )
    
    
    
    
    
    # STILL NEED 60013959 2024-2023
    # University of Florida, 245598, 283685	
    # search_term<-c('60013959', '60138923', '60154289', '60154389', '60027033', 
    # '60154244', '60138923', '60012284', '60024013', '60122658', '60122659', 
    # '60154389', '60122771', '60001462', '60006298', '60019530', '60122773', 
    # '60122770', '60122772', '60024153', '60008420', '60007567', '60154389', 
    # '60019838', '60007929', '60010177', '60018327', '60026237', '60075538', 
    # '60004181', '60030502', '60017574', '60122769' )
    
    
    # DONE STILL NEED 60030612 2024,2023
    # University of California - San Diego, 264376, 288722
    # search_term<-c('60121553', '60121641', '60121557', '60121643', 
    #                '60121646', '60121648', '60121504', '60121532', '60121638', 
    #                '60121565', '60121556', '60121657', '60121536', '60121535', 
    #                '60121538', '60121541', '60121562', '60121659', '60121672', 
    #                '60121640', '60121542', '60121545', '60121529', '60121580', 
    #                '60121654', '60121687', '60121510', '60121681', '60121608', 
    #                '60024752', '60121509', '60116256', '60121491', '60121490', 
    #                '60121489', '60121656', '60121488', '60121507', '60121537', 
    #                '60025135', '60120814', '60121482', '60121483', '60121613', 
    #                '60121481', '60121484', '60121486', '60121487', '60121485', 
    #                '60121480', '60121611', '60121501', '60121503', '60121511', 
    #                '60121508', '60121502', '60121512', '60121514', '60121515', 
    #                '60121517', '60121518', '60121516', '60121513', '60121642', 
    #                '60121644', '60121645', '60121639', '60281121', '60121533', 
    #                '60121539', '60121543', '60121526', '60121524', '60121527', 
    #                '60121528', '60121612', '60121530', '60121607', '60121609', 
    #                '60121610', '60121525', '60023018', '60121582', '60121583', 
    #                '60121585', '60121584', '60121574', '60121577', '60121581', 
    #                '60121578', '60121575', '60121576', '60121579', '60121610', 
    #                '60121573', '60121647', '60121506', '60121544', '60121591', 
    #                '60121505', '60121531', '60121534', '60121540', '60121684', 
    #                '60121665', '60121664', '60121568', '60121569', '60110665', 
    #                '60086300', '60003262', '60001708', '60002974', '60018246', 
    #                '60014754', '60121596', '60121595', '60121560', '60121561', 
    #                '60121564', '60179288', '60121590', '60121563', '60121597', 
    #                '60121599', '60121598', '60121547', '60121548', '60121549', 
    #                '60121550', '60121551', '60121552', '60121554', '60121686', 
    #                '60002582', '60121555', '60121592', '60121559', '60121558', 
    #                '60121682', '60121594', '60121593', '60121663', '60121662', 
    #                '60121669', '60121667', '60121670', '60121671', '60121665', 
    #                '60281105', '60121673', '60121650', '60281107', '60121652', 
    #                '60121651', '60121668', '60121653', '60121660', '60121661', 
    #                '60281109', '60121655', '60121666', '60281121', '60121656', 
    #                '60121664', '60281120', '60121658', '60271729' )
    
    
    
    
    

    
    
    
    

    
      

     
      
    
    
    

      
      
    
    
      search_term<-c(
      
        
      )
    yr1=2024
    yr2=2024
    
    date_range <- seq(yr1,yr2)
    year <- seq_along(date_range)
    term <- seq_along(search_term)
    
    month_vec<-c('January',
                 'February', 
                 'March', 
                 'April', 
                 'May', 
                 'June', 
                 'July', 
                 'August', 
                 'September', 
                 'October', 
                 'November',
                 'December')    
    
    # load_date1<-c('0101','0410','0701','1001')
    # load_date2<-c('0401','0701','1001','1231')
    month<-seq_along(month_vec)
    
    for (h in term){
      
      for (j in year) {
        
        for (k in month) {
          
          a<-paste("(AF-ID('",search_term[h],"')"," AND (DOCTYPE(ar) OR DOCTYPE(re) OR DOCTYPE(ch) OR DOCTYPE(ed) OR DOCTYPE(le) OR DOCTYPE(dp) OR DOCTYPE(no))",sep="")
          
          
          c <- " AND PUBYEAR = "
          
          # d<- paste(" AND LOAD-DATE > ", 
          #           date_range[j],
          # load_date1[quarter[k]], 
          # " AND LOAD-DATE <  ",
          # date_range[j],
          # load_date2[quarter[k]],
          # sep="")
          
          d<- paste(" AND PUBDATETXT(", 
                    month_vec[k],
                    date_range[j], 
                    ")",
                    sep=" ")
          
          
          query_string <-paste0(a, c, date_range[j],d,")",sep = "")
          
          
          scopus_data <- rscopus::scopus_search(query_string,
                                                max_count=15000,
                                                view = "COMPLETE",
                                                api_key = "1c46380453a72efcc8b15d447371e646")
          
          
          
          
          scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
          
          if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
            next
          }else{
            scopus_papers <- scopus_data_raw$df
          
          term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],"_",month[k],sep="")
          papers <- paste("./data_raw/scopus_api/unis_files/papers/",term_for_file,"_papers", ".csv", sep = "")
          write_csv(scopus_papers, papers)
          
          scopus_affiliations <- scopus_data_raw$affiliation
          term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],"_",month[k],sep="")
          affils <- paste("./data_raw/scopus_api/unis_files/affils/","/",term_for_file,"_affils", ".csv", sep = "")
          write_csv(scopus_affiliations, affils)
          
          scopus_authors <- scopus_data_raw$author
          term_for_file<-paste("scopus_affil_", search_term[h],"_",date_range[j],"_",month[k],sep="")
          authors <- paste("./data_raw/scopus_api/unis_files/authors/",term_for_file,"_authors", ".csv", sep = "")
          write_csv(scopus_authors, authors)
        }
      }
    }
    }
    
    
    
    