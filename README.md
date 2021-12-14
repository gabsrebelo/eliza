# Eliza
Projeto da disciplina de Paradigmas de Linguagem de Programação - 2021/2. Eliza é um programa desenvolvido em Prolog que consume entradas do usuário e fornece respostas de acordo com um conjunto interno de regras. 

**Exemplo de interação**
```
> oi
Como vai você? Por favor, me fale do seu problema.
> Você é um computador?
Computadores te incomodam?
> Não... é só que você tem o nome de um programa de computador
Você diz não só para ser negativo?
> Não, desculpa
Por favor, não se desculpe.
> Mas então, você é um programa de computador?
O que máquinas tem a ver com o seu problema?
> Eu lembro da minha infância
Lembrar da sua infância traz alguma outra lembrança à sua mente?
> Não sei... você lembra da sua?
Você acha que eu esqueceria da minha ??
> Não, desculpa
Desculpas não são necessárias.
> Bye
Bye
```

## Setup
```
sudo add-apt-repository ppa:swi-prolog/stable
sudo apt-get update
sudo apt-get install swi-prolog
```

## Execução
* Compilar o programa usando:
`swipl eliza.pl`

* Chamar a regra "interagir"
`?- interagir.`

* O símbolo `>` indica que o usuário pode conversar com a Eliza.

* Usar "Bye" para encerrar a conversa
