# municipios_brasil
Banco de dados com estatísticas básicas sobre os municípios brasileiros em uma série histórica (1995 - 2017)

Esse projeto compreende a construção de um banco de dados contendo estatísticas básicas sobre os municípios brasileiros dentro de uma série histórica. A princípio desde os anos de 1995 até 2017. O projeto está em sua primeira versão e contém apenas os dados de contagem da população nos anos citados. A ideia é que o banco possa auxiliar os pesquisadores oferecendo acesso a dados básicos, usados em grande parte dos projetos, de maneira rápida e condensada.

Banco em R: histmun.RData
Banco em csv: histmun.csv
Dicionário:  

04 de julho de 2018

v1.0: variáveis adicionadas ao banco:

    "Ano",
    "Unidade da federação (UF)/ Estado",
    "Join das variáveis ano e uf separados por _",
    "Código IBGE da UF",
    "Código do Tribunal Superior Eleitoral (TSE) do município (5 dígitos)",
    "Código IBGE do município sem o dígito verificador (6 dígitos)",
    "Código IBGE do município com o dígito verificador (7 dígitos)",
    "Nome do município sem acento, em maiúscula no padrão usado pelo IBGE",
    "Absoluto da população residente do município",
    "Absoluto da população residente da uf",
    "Absoluto da população residente no Brasil",
    "Percentual da população do município sobre o total na uf",
    "Percentual da população do município sobre o total no Brasil"
