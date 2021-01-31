library(shiny)
library(dplyr)
library(tidyr)
library(tidyverse)
library(DT)
library(plotly)
library(ggplot2)

pop <- read.csv('C:/FAO/data/Population.csv')
cereals <-  read.csv("C:/FAO/data/Cereals.csv")
alim <- read.csv("C:/FAO/data/Dispo_alim(big_data).csv")
sous_nut <- read.csv("C:/FAO/data/Sous_nutrition.csv")

#Question 1
pop <- pop %>% mutate(Population=Population * 1000)
pop_mondiale <- pop %>%
    filter(Country != 'China') %>%
    select(Year,Population) %>% 
    group_by(Year) %>%
    summarise(total=sum(Population))

#print(c(animal_food*100, human_food*100))

ui <- fluidPage(

    # Application title
    titlePanel("Projet FAO"),
            tabsetPanel(
                tabPanel(title = "Question 1",
                         h1("Question 1:"),
                         p("En se basant sur le dataframe construit avec Python ou R (à partir des CSV brutes), calculer la taille de la population mondiale (pour chaque année) ?"),
                         DT::dataTableOutput("pop_mondiale")),
                tabPanel(title = "Question 2",
                         tabsetPanel(
                        tabPanel("Question 2.1.1",
                         p("Établir la liste des produits (ainsi que leur code) considéré comme des céréales selon la FAO et créer une colonne de type booléen nommée 'is_cereal' ?"),
                         DT::dataTableOutput("alimentation_cer")),
                         tabPanel("Question 2.1.2",
                         p("En ne prenant en compte que les céréales destinées à l'alimentation (humaine et animale), quelle proportion (en termes de poids) est destinée à l'alimentation animale ?"),
                         htmlOutput("prop")),
                        tabPanel("Question 2.2.1",
                         p("Calculer (pour chaque pays et chaque produit) la disponibilité alimentaire en kcal puis en kg de protéines ?"),
                         DT::dataTableOutput("foodsup")),
                        tabPanel("Question 2.2.2",
                         p("A partir de ces dernières informations, et à partir du poids de la disponibilité alimentaire (pour chaque pays et chaque produit), calculez pour chaque produit le ratio 'énergie/poids', que vous donnerez en kcal ?"),
                         DT::dataTableOutput("renkcl")),
                        tabPanel("Question 2.2.3",
                         p('En suivant la même méthodologie, calculez le pourcentage de protéines de chaque produit (pour chaque pays)'),
                         DT::dataTableOutput("rpropre")),
                        tabPanel("Question 2.3.1",
                        p('Citez 5 aliments parmi les 20 aliments les plus caloriques, en utilisant le ratio énergie/poids ?'),
                        DT::dataTableOutput("renpoi")),
                        tabPanel("Question 2.3.2",
                        p('Citez 5 aliments parmi les 20 aliments les plus riches en protéines ?'),
                        DT::dataTableOutput("tbmosprot")),
                        tabPanel("Question 2.4.1",
                        p('Calculer, pour les produits végétaux uniquement, la disponibilité intérieure mondiale exprimée en kcal et en Kg protéines pour chaque année et tracer la viz correspondante ?'),
                        tabsetPanel(
                            tabPanel('KCal',
                                DT::dataTableOutput("tbdispomond"),
                                plotly::plotlyOutput('pltdispomond')
                                ),
                            tabPanel('Kg',
                                DT::dataTableOutput("tbdispomondper"),
                                plotly::plotlyOutput('pltdispomondper')
                            )
                        )#tabsetPanel
                        )#Fin2.4.1
                        )#TabsetSub2
                        ),#Question2
                tabPanel(title = "Question 3",
                    tabsetPanel(
                        tabPanel("Question 2.4-3.1:",
                         h1("Question 2.4-3.1:"),
                    p("Combien d'humains pourraient être nourris si toute la disponibilité intérieure mondiale de produits végétaux était utilisée pour de la nourriture ? Donnez les résultats en termes de calories, puis de protéines, et exprimez ensuite ces 2 résultats en pourcentage de la population mondiale.
Combien d'humains pourraient être nourris si toute la disponibilité alimentaire en produits végétaux (Food), la nourriture végétale destinée aux animaux (Feed) et les pertes de produits végétaux (Waste) étaient utilisés pour de la nourriture ? Donnez les résultats en termes de calories, puis de protéines, et exprimez ensuite ces 2 résultats en pourcentage de la population mondiale.
Combien d'humains pourraient être nourris avec la disponibilité alimentaire mondiale ? Donnez les résultats en termes de calories, puis de protéines, et exprimez ensuite ces 2 résultats en pourcentage de la population mondiale.
Quelle proportion de la population mondiale est considérée comme étant en sous-nutrition ?"),
                    DT::dataTableOutput("sous_nut"), 
                    DT::dataTableOutput("pm_sous_nut")
                        ),
                        tabPanel(title = "Quesion 3.2",
                            p("Sélectionnez parmi les données des bilans alimentaires les informations relatives aux pays dans
lesquels la FAO recense des personnes en sous-nutrition."),
                            DT::dataTableOutput("c_sous_nut")),
                        tabPanel(title = "Quesion 3.3",
                             p("Repérer les 15 produits les plus exportés par ce groupe de pays."),
                             DT::dataTableOutput("tbLpropluex")
                        ),
                        tabPanel(title = "Question 3.4",
                            p("Parmi les données des bilans alimentaires au niveau mondial, sélectionner les 200 plus grandes importations de ces produits (1 importation = une quantité d'un produit donné importée par un pays donné sur l'année choisie)"),
                            DT::dataTableOutput("tbplusgrimp")
                        ),
                        tabPanel(title = "Question 3.5-3.7",
                                 p("Grouper ces importations par produit, afin d'avoir une table contenant 1 ligne pour chacun des 15 produits. Ensuite, calculer pour chaque produit les 2 quantités suivantes :
                                       
                                       Le ratio entre la quantité destinés aux 'Autres utilisations' (Other uses) et la disponibilité intérieure.
                                   
                                   Le ratio entre la quantité destinée à la nourriture animale et la quantité destinée à la nourriture (animale + humaine)
                                   
                                   Donnez les 3 produits qui ont la plus grande valeur pour chacun des 2 ratios (vous aurezdonc 6 produits à citer)
                                   
                                   Combien de tonnes de céréales pourraient être libérées si les USA diminuaient leur production de produits animaux de 10% ?"),
                                 DT::dataTableOutput("tbGCUSA")
                                 ),
                    tabPanel(title = "Question 3.8",
                             p("En Thaïlande, quelle proportion de manioc est exportée ? Quelle est la proportion de personnes en sous-nutrition ?"),
                             DT::dataTableOutput("tbalimthi"),
                             DT::dataTableOutput("tbppthi")
                            )
                        )
                )#Question3
            )#TabsetPanel
)#fluidePage
# Define server logic required to draw a histogram
server <- function(input, output) {

    #Question 1
    output$pop_mondiale <- DT::renderDataTable({pop_mondiale})
    #Question2
    alim_is_cereal <- alim %>%
        select(country, feed, food, item_code, item, year, origin) %>%
        mutate(is_cereal=is.element(item_code, cereals$Item.Code))
    
    test <- alim_is_cereal %>%
        replace_na(list(feed=0, food=0)) %>%
        filter(is_cereal==TRUE) %>%
        select(feed, food)
    
    sum_feed <- sum(test$feed)
    sum_food <- sum(test$food)
    
    total <- sum_feed + sum_food
    animal_food <- sum_feed / total
    human_food <- sum_food / total
    
    output$prop <- renderText({ paste("<b>Animal Food :",round(animal_food*100,2),"%","<br>","Human Food :",round(human_food*100,2),"% </b>") })
    output$alimentation_cer <- DT::renderDataTable({alim_is_cereal})
    
    food_supply <- merge(x=alim, y=pop, by.x=c("country_code", "year"), by.y=c("Country_code", "Year")) %>%
        mutate(food_supply_kcal=food_supply_kcalcapitaday * 365.0 * Population,
               protein_supply_kgprotein=protein_supply_quantity_gcapitaday / 1000 * 365.0 * Population) %>%
        replace_na(list(food_supply_kcal=0.0, protein_supply_kgprotein=0.0))
    
    output$foodsup <- DT::renderDataTable({food_supply})
    
    #Q2.2.2
    #Calcule
    ratio_en_kcal <- food_supply %>%
        mutate(food_weight=food_supply_quantity_kgcapitayr * Population) %>%
        replace_na(list(food_weight=0.0)) %>%
        mutate(kcal_par_kg=food_supply_kcal / food_weight) %>%
        replace_na(list(kcal_par_kg=0.0)) %>%
        arrange(desc(kcal_par_kg)) %>%
        rowwise %>% filter(!any(is.infinite(c_across(where(is.numeric)))))
    #Output
    output$renkcl <- DT::renderDataTable({ratio_en_kcal})
    
    #Q2.2.3
    #Calcule
    ratio_protein_percent <- food_supply %>%
        mutate(food_weight=food_supply_quantity_kgcapitayr * Population) %>%
        replace_na(list(food_weight=0.0)) %>%
        mutate(protein_par_kg=protein_supply_kgprotein / food_weight) %>% replace_na(list(protein_par_kg=0.0)) %>%
        arrange(desc(protein_par_kg)) %>%
        rowwise %>% filter(!any(is.infinite(c_across(where(is.numeric)))))
    #Output
    output$rpropre <- DT::renderDataTable({ratio_protein_percent})
    
    #Q2.3.1
    #Calcule
    ratio_en_kcal %>%
        arrange(desc(kcal_par_kg)) %>%
        head(20) -> renergypoids
    #Output
    output$renpoi <- DT::renderDataTable({renergypoids})
    
    #Q2.3.2
    #Calcule
    ratio_protein_percent %>%
        arrange(desc(protein_par_kg)) %>%
        head(5)->mostprot
    #Output
    output$tbmosprot <- DT::renderDataTable({mostprot})
    
    
    #Q2.4.1
    #Calcule
    ratio_en_kcal %>%
        mutate(domestic_supply_kcal=domestic_supply_quantity * kcal_par_kg) %>%
        filter(origin=="vegetal") %>%
        select(year, domestic_supply_kcal) %>%
        group_by(year) %>%
        summarise(domestic_supply_kcal=sum(domestic_supply_kcal))->dispomond
    
    ratio_protein_percent %>%
        mutate(domestic_supply_prot=domestic_supply_quantity * protein_par_kg) %>%
        filter(origin=="vegetal") %>%
        select(year, domestic_supply_prot) %>%
        group_by(year) %>%
        summarise(domestic_supply_prot=sum(domestic_supply_prot))->dispomondper
    
    #Output
    output$tbdispomond <- DT::renderDataTable({dispomond})
    output$pltdispomond <- plotly::renderPlotly({dispomond %>% 
            ggplot(aes(x = year, y = domestic_supply_kcal)) +
            geom_col()+ ylab("Domestic supply (KCal)")})
    
    output$tbdispomondper <- DT::renderDataTable({dispomondper})
    output$pltdispomondper <- plotly::renderPlotly({dispomondper %>% 
            ggplot(aes(x = year, y = domestic_supply_prot)) +
            geom_col()+ ylab("Domestic supply (Protéin) (Kg)")})
    #Question 2.4-3.1
    sous_nut$Year = strtoi(substr(sous_nut$Year, 1, 4)) + 1
    sous_nut$Population <- suppressWarnings(replace_na(as.numeric(as.character(sous_nut$Population)), 0.0) * 1e+6)
    output$sous_nut <- DT::renderDataTable({sous_nut})
    
    pop_mondiale_sous_nut <- sous_nut %>%
        filter(Country != 'China') %>%
        select(Year,Population) %>% 
        group_by(Year) %>%
        summarise(total=sum(Population)) %>%
        mutate(sous_nut_pourcent=total/pop_mondiale$total*100)
    
    output$pm_sous_nut <- DT::renderDataTable({pop_mondiale_sous_nut})
    
    country_in_sounut <- sous_nut %>%
        filter(Population!=0) %>%
        select(Country, Country_code) %>%
        unique()
    
    output$c_sous_nut <- DT::renderDataTable({country_in_sounut})
    
    #Q3.3
    
    alim %>%
        mutate(is_in_sousnut=is.element(country_code, country_in_sounut$Country_code)) %>%
        filter(is_in_sousnut==TRUE) %>%
        select(item_code, item, export_quantity) %>%
        group_by(item_code, item) %>%
        replace_na(list(export_quantity=0)) %>%
        summarise(total_exported_quantity=sum(export_quantity)) %>%
        arrange(desc(total_exported_quantity)) %>%
        head(15) ->Lpropluex
    output$tbLpropluex <- DT::renderDataTable({Lpropluex})
    
    
    #Q3.4
    alim %>%
        arrange(desc(import_quantity)) %>%
        head(200)->plusgrimp
    
    output$tbplusgrimp <- DT::renderDataTable({plusgrimp})
    
    #Q3.5-3.7
    alim_is_cereal %>%
        filter(country=="United States of America", is_cereal==TRUE) %>%
        replace_na(list(feed=0)) %>%
        summarise(cereal_eaten_by_animals=sum(feed)) ->GCUSA
    output$tbGCUSA <- DT::renderDataTable({GCUSA})
    
    #Q3.8
    alim %>%
        filter(country=="Thailand", item_code==2532) %>%
        select(export_quantity, production) %>%
        replace_na(list(export_quantity=0, production=0)) %>%
        summarise(export=sum(export_quantity), production_=sum(production)) %>%
        summarise(manioc_exported=export/production_) -> alimthi
    output$tbalimthi <- DT::renderDataTable({alimthi})
    
    pop_thai <- pop %>% filter(Country=="Thailand")
    pop_thai_sous_nut <- sous_nut %>% filter(Country=="Thailand")
    pop_thai %>% mutate(sous_nut_pourcent=pop_thai_sous_nut$Population/pop_thai$Population*100)->ppthi
    output$tbppthi <- DT::renderDataTable({ppthi})
}

# Run the application 
shinyApp(ui = ui, server = server)
