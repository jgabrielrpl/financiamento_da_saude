###
## Gráfico indicadores do financiameno da saude do Brasil em perspctiva comparada
###

####
##
# Pacote ----

library(tidyverse)

####
##
# Elaboracao gráfica ----

# Manipulando os dados

dados <- data.frame(paises = c("Argentina", "Brasil", "Canadá", "Chile", "Cuba", "EUA", "França", "Alemanha", "Itália", "Reino Unido"),
                    `Despesas governamentais em saúde como % das despesas totais em saúde` = c(61, 42, 73, 51, 89, 50, 73, 78, 74, 79),
                    `Despesas privadas em saúde como % da despesa total em saúde` = c(38, 58, 27, 49, 11, 50, 27, 22, 26, 21),
                    `Out-ofpocket como % da despesa total em saúde` = c(28, 28, 15, 33, 11, 11, 9, 13, 24, 17),
                    `Despesas governamentais em saúde como % do PIB` = c(6, 4, 8, 5, 10, 9, 8, 9, 6, 8),
                    `Despesas governamentais em saúde como % das despesas governamentais` = c(15, 10, 20, 18, 15, 23, 15, 20, 13, 19),
                    `Despesas governamentais per capita em saúde (em US$)` = c(693, 354, 3671, 740, 877, 5356, 3441, 4251, 2209, 3392),
                    #`Despesas governamentais per capita em saúde (em PPP Int$)` = c(1222, 638, 3822, 1172, 2240, 5356, 3852, 4737, 2678, 3631),
                    `Despesas privadas per capita em saúde (em US$)` = c(432, 494, 1324, 716, 109, 5268, 1249, 1221, 780, 923
                                                                         ))
                    #,
                    #`Despesas privadas per capita em saúde (em PPP Int$)` = c(762, 892, 1378, 1134, 277, 5268, 1398, 1361, 946, 988)
                     
dados_tidy <- gather(dados, key = "variavel", value = "valor", -paises) |> 
  mutate(variavel_novo =  case_when( 
    variavel ==  "Despesas.governamentais.em.saúde.como...das.despesas.governamentais" ~ "% gasto público em saúde no gasto governamental total",
    variavel ==  "Despesas.governamentais.em.saúde.como...das.despesas.totais.em.saúde" ~ "% gasto público em saúde no gasto total em saúde ",
    variavel ==  "Despesas.governamentais.em.saúde.como...do.PIB"  ~  "Gasto público em saúde como % do PIB",
    variavel ==  "Despesas.governamentais.per.capita.em.saúde..em.US.."  ~  "Gasto privado em saúde per capita (em US$)",
    variavel ==  "Despesas.privadas.em.saúde.como...da.despesa.total.em.saúde"  ~  "% gasto privado em saúde no gasto total em saúde",
    variavel ==  "Despesas.privadas.per.capita.em.saúde..em.US.."  ~  "Gasto público em saúde per capita (em US$)",
    variavel ==  "Out.ofpocket.como...da.despesa.total.em.saúde"  ~  "% out-ofpocket no gasto total em saúde"),
    
    paises = factor(paises,
                 levels = rev(c("Alemanha",
                             "Canadá",
                             "EUA",
                             "França",
                             "Itália",
                             "Reino Unido",
                             "Brasil",
                             "Argentina",
                             "Chile",
                             "Cuba"))),
    
    variavel_novo = factor(variavel_novo,
                    levels = (c(
                      "% gasto público em saúde no gasto total em saúde ",
                      "% gasto privado em saúde no gasto total em saúde",
                      "% out-ofpocket no gasto total em saúde",
                      
                      "Gasto público em saúde per capita (em US$)",
                      "Gasto privado em saúde per capita (em US$)",
                      
                      "% gasto público em saúde no gasto governamental total",
                      "Gasto público em saúde como % do PIB"
                      ))),
    
    classificacao = case_when(
      paises == "Argentina" ~ "América Latina",
      paises == "Brasil" ~ "Brasil",
      paises == "Alemanha" ~ "Compõe o G7",
      paises == "Chile" ~ "América Latina",
      paises == "Cuba" ~ "América Latina",
      paises == "Canadá" ~ "Compõe o G7",
      paises == "EUA" ~ "Compõe o G7",
      paises == "França" ~ "Compõe o G7",
      paises == "Itália" ~ "Compõe o G7",
      paises == "Reino Unido" ~ "Compõe o G7"))

  
  # Graficando
  dados_tidy |> 
  ggplot(aes(x= paises, y = valor, fill = classificacao)) +
  geom_col()+
  facet_wrap(vars(variavel_novo),  scales = "free")+ # importante 
  coord_flip()+
    geom_text(aes(label= round(valor)), size = 2, position = position_stack(vjust = 1))+
    scale_fill_manual(values = c(
      "#DCD6D6",
      "#CBADA0",
      "#BCCEE0"
      ))+
    #scale_fill_manual(values = c( # mudar a cord do fill 
    labs (title = "Financiamento da saúde no Brasil em perspectiva comparada",
          y = NULL,
          x = NULL,
          fill= NULL,
          caption = "Fonte: Organização Mundial da saúde. Acesse: <l1nq.com/indicadores>. Ano base: 2018. Acesso Abr 2021.")+
    theme(legend.title = element_text(family = "Times New Roman"), 
          legend.position = "bottom",
          title = element_text(size = 10, family = "Times New Roman"),
          axis.title.y = element_text(size = 10, family = "Times New Roman"),
          axis.text.y = element_text(size = 10, family = "Times New Roman"),
          axis.text.x = element_text(size = 10, family = "Times New Roman"),
          axis.title.x = element_text(size = 10,family = "Times New Roman"),
          legend.text = element_text(size = 10, family = "Times New Roman"),
          plot.caption = element_text(hjust = 0, size = 11, family = "Times New Roman" ),
          panel.background = element_rect(fill = "white", colour = "white", color = "white"),
          plot.background = element_rect(fill = "white", colour = "white", color = "white"),
          legend.background = element_rect(fill = "white"),
          panel.grid.major.x = element_line(colour = "#eceff2", size = 0.7),
          panel.grid.major.y = element_line(colour = "#eceff2", size = 0.7),
          legend.key = element_rect(fill = "white", color = "white"),
          strip.background = element_rect(fill = "white", colour = "white")) +
    guides(fill = guide_legend(reverse=T))
 
# Nome das variáveis 
#   Nota:
#   Despesas governamentais em saúde como % das despesas totais em saúde -> % gasto público em saúde no gasto total em saúde  
#   Despesas privadas em saúde como % da despesa total em saúde -> % gasto privado em saúde no gasto total em saúde
#   Out-ofpocket como % da despesa total em saúde -> % out-ofpocket no gasto total em saúde
#   Despesas governamentais em saúde como % do PIB -> Gasto público em saúde como % do PIB
#   Despesas governamentais em saúde como % das despesas governamentais -> % gasto público em saúde no gasto governamental total
#   Despesas governamentais per capita em saúde (em US$) -> Gasto público em saúde per capita (em US$)
#   Despesas privadas per capita em saúde (em US$) -> Gasto privado em saúde per capita (em US$)
#   

# Salvar 
#ggsave("trabalhos/trabalho_grafico_aula/figura_brasil.png", width = 33, height = 20, units = "cm")
#ggsave("trabalhos/trabalho_grafico_aula/figura__brasil.svg", width = 33, height = 20, units = "cm")
  



