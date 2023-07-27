
# Double call program

[![N|Solid](https://wiki.scn.sap.com/wiki/download/attachments/1710/ABAP%20Development.png?version=1&modificationDate=1446673897000&api=v2)](https://www.sap.com/brazil/developer.html)

## Descr
Em alguns casos, existe a necessidade de, em um report:
- Informar dados para processamento
- Informar tambem um arquivo
- Habilitar uma opção de processamento em background

Dessa forma, o programa ira buscar os dados do arquivo excel por exemplo, e isso ira gerar um processamento em background com esses dados e/ou os dados da tela inicial. Para esse tipo de solução, é comum usar uma chamada do mesmo programa, criando um job ou algo assim. 

Para casos assim, é interessante buscar os dados do arquivo antes de fazer a chamada do programa novamente (via job ou algo assim). Infelizmente não é possivel um job ler um arquivo que esta no computador do usuario (apenas arquivos no proprio servidor), por isso, seria interessante ler esses dados e guardar de forma que possam ser lidos pelo job em um segundo momento. Para esse necesssidade que este programa foi criado.

Isso seguira os seguintes passos


That will use a dynamic table as a filter on the select instruction, only the screen fields that have been filled. The empty screen fields are going not to be considered.

## Example

You can see the example on this [file](cluster.abap).