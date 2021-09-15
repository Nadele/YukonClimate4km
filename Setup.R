library(profvis)
profvis(shiny::runApp("E:\\Research\\9.ShinyApp\\YukonClimate"))
library(rsconnect)
rsconnect::setAccountInfo(name='nadele-shiny',
                          token='490072E6B2ED6BE2B2063C6373F623A1',
                          secret='FrdEeF61d5QerSRDU++wAYG/6XGIbd8dq2mz+tLn')

forgetDeployment('E:/Research/9.ShinyApp/YukonClimate4km')

rsconnect::deployApp('E:/Research/9.ShinyApp/YukonClimate4km')
