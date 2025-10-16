################################################################################
#                                                                              #
#    Script para Ler o arquivo de exportação do Zotero e criar uma página      #
#    individual para cada artigo.                                              #
#                                                                              #
#    Autor: Mário O. de Menezes                                                #
#    Data: Abril de 2025                                                       3
#                                                                              # 
################################################################################



library(readr)
publicacoes <- read_csv("~/docs/Projetos/IPEN/INCT_INTERAS_202212/publicacoesINCT/exportZotero/INTERAS20250402.csv")

