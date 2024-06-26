
# Export Database

![Static Badge](https://img.shields.io/badge/development-abap-blue)
![GitHub commit activity (branch)](https://img.shields.io/github/commit-activity/t/edmilson-nascimento/export-database-cluster-background)


Um programa será chamado duas vezes. Na segunda vez, provavelmente em uma execução por job, tera acesso a dados que foram lidos de um arquivo/ficheiro ~~neste caso, so uma tabela interna porque eu não vou criar tudo~~ na primeira vez.

## Necessidade ##
Em alguns casos, existe a necessidade de:
- Informar dados para processamento
- Informar também um arquivo
- Habilitar uma opção de processamento em background

Dessa forma, o programa irá buscar os dados de um arquivo excel por exemplo, e isso irá gerar um processamento em background com esses dados e/ou os dados da tela inicial. Para esse tipo de solução, é comum usar uma chamada do mesmo programa, criando um job ou algo nessa linha. 

Para casos assim, é interessante buscar os dados do arquivo antes de fazer a chamada do programa novamente (via job e etc). Infelizmente não é possível um job ler um arquivo que esta no computador do usuário (apenas arquivos no próprio servidor), por isso, seria interessante ler esses dados e guardar de forma que possam ser acessados pelo job em um segundo momento. Para esse necessidade que este programa foi criado.


## Tecnologia adotada ##
ABAP usando `export/import ... database` ~~por usar export memory so da trabalho e os dados ficam perdidos em nárnia~~ para gerir o export/import de dados. 


## Solução ##
O programa será executado duas vezes e em cada uma tera uma funcionalidade diferente.
1- Buscar, exportar os dados e gerar o job
2- Recuperar os dados e processa-los

### Fase 1 - busca, exportação e criação de job ### 
Para essa parte, o fluxo seguido será
- Informar os dados de processamento
    - Arquivo
    - Dados necessários
    - Etc
- Executar o programa para importação dos dados do arquivo ~~que nesse caso, eu vou informar manualmente porque eu não quis criar as rotinas de importar do excel~~
- Exportar os dados para a tabela `Cluster DataBase`
- Criar o job passando o `ID` gerado para esses dados
```mermaid
flowchart LR
    Start((start)) --> SAPData[(Buscar dados)]
    SAPData --> SAPExpor(Exportar dados)
    SAPExpor --> Job(Criar Job)
    Job --> FinishNew((( )))
```

### Fase 2 - Importação e processamento ###
Essa parte ja será executa como job. A partir desse ponto segue-se o seguinte fluxo:
- Importar os dados da tabela `Cluster DataBase`
- Eliminar os dados da tabela
- Processar os dados 

```mermaid
flowchart LR
    Start((start)) --> SAPData[(Importar dados)]
    SAPData --> Del(Deletar dados)
    Del --> Process(Processar dados)
    Process --> FinishNew((( )))
```

**Esse código é aberto, sujeito a alterações ~~a hora que me der na telha~~ assim que houver uma necessidade que trará ganho didático ao conteúdo e deixe o algoritmo com melhor leitura e compreensão.**

## Example

You can see the example on this [file](/class/cluster.abap).