% Trabalho Pratico III - Programacao Logica
% Projeto Eliza em Prolog
% Autora: Gabriela Rebelo

/*############################################# Trocas #############################################*/
troca_por(meu,seu).
troca_por(seu,meu).
troca_por(minha,sua).
troca_por(sua,minha).
troca_por(eu,você).
troca_por(você,eu).
troca_por(voce,eu).
troca_por(sou,é).
troca_por(é,sou).
troca_por(fui,foi).
troca_por(foi,fui).
troca_por(estou,está).
troca_por(está,estou).
troca_por(tenho,tem).
troca_por(tem,tenho).
troca_por(tive,teve).
troca_por(teve,tive).
troca_por(fomos,eram).
troca_por(eram,fomos).
troca_por(meus,seus).
troca_por(seus,meus).
troca_por(minhas,suas).
troca_por(suas,minhas).
troca_por(teus,meus).
troca_por(meus,teus).


/*############################################# Utils #############################################*/
tratar_interrogacao([], []).
% include space before interrogation
tratar_interrogacao([63|R], [32,63|NR]) :-
    tratar_interrogacao(R, NR).
tratar_interrogacao([H|R], [H|NR]) :-
    tratar_interrogacao(R, NR).

remover_pontuacao([],[]).
remover_pontuacao([H|R],NR) :-
    H > 32,
    H < 48,
    remover_pontuacao(R,NR).
remover_pontuacao([H|R],[H|NR]):-
    remover_pontuacao(R,NR).

normalizar_input(CodesInput,Normalizado):-
    remover_pontuacao(CodesInput,SemPontuacao),
    tratar_interrogacao(SemPontuacao,Tratado),
    atom_codes(A,Tratado),
    string_lower(A,Normalizado).

sublist([P|R], L) :-
    append([_, [P|R], _], L).
sublist(T, L) :-
    append([_, [T], _], L).

%alterar contexto para pronomes ou alguns verbos. A saída é usada para responder o usuario
mudar_contexto([],[]).
mudar_contexto([H|R],[NH|NR]):-
    troca_por(H,NH),
    mudar_contexto(R,NR).
mudar_contexto([H|R],[H|NR]):-
    mudar_contexto(R,NR).


/*############################################# DCG #############################################*/

sair --> [bye].

%*eu lembro*
frase(eu_lembro, L) --> [eu, lembro], palavras(L).
frase(eu_lembro, L) --> palavras(_), [eu, lembro], palavras(L).

%*voce lembra*
frase(vc_lembra, L) --> [você, lembra], palavras(L).
frase(vc_lembra, L) --> [voce, lembra], palavras(L).
frase(vc_lembra, L) --> palavras(_), [você, lembra], palavras(L).
frase(vc_lembra, L) --> palavras(_), [voce, lembra], palavras(L).

%*se*
frase(se_, L) --> [se], palavras(L).
frase(se_, L) --> palavras(_), [se], palavras(L).

%*eu sonhei com*
frase(eu_sonhei_com, L) --> [eu, sonhei, com], palavras(L).
frase(eu_sonhei_com, L) --> palavras(_), [eu, sonhei, com], palavras(L).

%*eu sonhei*
frase(eu_sonhei, L) --> [eu, sonhei], palavras(L).
frase(eu_sonhei, L) --> palavras(_), [eu, sonhei], palavras(L).

%*minha mae*
frase(minha_mae, L) --> [minha, mae], palavras(L).
frase(minha_mae, L) --> [minha, mãe], palavras(L).
frase(minha_mae, L) --> palavras(_), [minha, mae], palavras(L).
frase(minha_mae, L) --> palavras(_), [minha, mãe], palavras(L).

%*eu quero*
frase(eu_quero, L) --> [eu, quero], palavras(L).
frase(eu_quero, L) --> palavras(_), [eu, quero], palavras(L).

%*eu era*?
frase(eu_era_perg, L) --> [eu, era], palavras(L), [?].
frase(eu_era_perg, L) --> palavras(_), [eu, era], palavras(L), [?].

%*eu era*
frase(eu_era, L) --> [eu, era], palavras(L).
frase(eu_era, L) --> palavras(_), [eu, era], palavras(L).

%*eu sou*?
frase(eu_sou_perg, L) --> [eu, sou], palavras(L), [?].
frase(eu_sou_perg, L) --> palavras(_), [eu, sou], palavras(L), [?].

%*eu sou*
frase(eu_sou, L) --> [eu, sou], palavras(L).
frase(eu_sou, L) --> palavras(_), [eu, sou], palavras(L).

%*você é*?
frase(vc_e_perg, L) --> [você, é], palavras(L), [?].
frase(vc_e_perg, L) --> [voce, é], palavras(L), [?].
frase(vc_e_perg, L) --> palavras(_), [você, é], palavras(L), [?].
frase(vc_e_perg, L) --> palavras(_), [voce, é], palavras(L), [?].

%*você é*
frase(vc_e, L) --> [você, é], palavras(L).
frase(vc_e, L) --> [voce, é], palavras(L).
frase(vc_e, L) --> palavras(_), [você, é], palavras(L).
frase(vc_e, L) --> palavras(_), [voce, é], palavras(L).

%*você era*?
frase(vc_era, L) --> [você, era], palavras(L), [?].
frase(vc_era, L) --> [voce, era], palavras(L), [?].
frase(vc_era, L) --> palavras(_), [você, era], palavras(L), [?].
frase(vc_era, L) --> palavras(_), [voce, era], palavras(L), [?].

%*eu nao posso*
frase(eu_nao_posso, L) --> [eu, não, posso], palavras(L).
frase(eu_nao_posso, L) --> [eu, nao, posso], palavras(L).
frase(eu_nao_posso, L) --> palavras(_), [eu, não, posso], palavras(L).
frase(eu_nao_posso, L) --> palavras(_), [eu, nao, posso], palavras(L).

%*eu sinto*
frase(eu_sinto, L) --> [eu, sinto], palavras(L).
frase(eu_sinto, L) --> palavras(_), [eu, sinto], palavras(L).

%*porque você não*
frase(pq_vc_nao, L) --> [porque, você, não], palavras(L).
frase(pq_vc_nao, L) --> palavras(_), [porque, você, não], palavras(L).
frase(pq_vc_nao, L) --> [porque, voce, nao], palavras(L).
frase(pq_vc_nao, L) --> palavras(_), [porque, voce, nao], palavras(L).


%*são*
frase(sao, L) --> [são], palavras(L).
frase(sao, L) --> palavras(_), [são], palavras(L).
frase(sao, L) --> [sao], palavras(L).
frase(sao, L) --> palavras(_), [sao], palavras(L).


%*sao como*
frase(sao_como, L1, L2) --> palavras(L1), [são, como], palavras(L2).
frase(sao_como, L1, L2) --> palavras(L1), [sao, como], palavras(L2).

%*eh como*
frase(e_como, L1, L2) --> palavras(L1), [é, como], palavras(L2).
frase(e_como, L1, L2) --> palavras(L1), [eh, como], palavras(L2).

palavras([]) --> [].
palavras([P|R]) --> [P], palavras(R).

/*############################################# Interpretacoes #############################################*/

%*oi*
interpretar(E,oi):-
    sublist([oi],E).

%*computador*
interpretar(E,computador):-
    sublist([computador],E).

%*nome*
interpretar(E,nome):-
    sublist([nome],E).

%*desculpe*
interpretar(E,desculpe):-
    sublist([desculpe],E).

%*eu lembro*
interpretar(E,objeto(eu_lembro,L)):-
    frase(eu_lembro,L,E,[]).

%*voce lembra*
interpretar(E,objeto(vc_lembra,L)):-
    frase(vc_lembra,L,E,[]).

%*se*
interpretar(E,objeto(se_,L)):-
    frase(se_,L,E,[]).

%*eu sonhei com* 
interpretar(E,objeto(eu_sonhei_com,L)):-
    frase(eu_sonhei_com,L,E,[]).

%*eu sonhei*
interpretar(E,objeto(eu_sonhei,L)):-
    frase(eu_sonhei,L,E,[]).

%*sonho*
interpretar(E,sonho):-
    sublist([sonho],E).

%*minha mae*
interpretar(E,objeto(minha_mae,L)):-
    frase(minha_mae,L,E,[]).

%*meu pai*
interpretar(E,pai):-
    sublist([meu,pai],E).

%*eu quero*
interpretar(E,objeto(eu_quero,L)):-
    frase(eu_quero,L,E,[]).

%*estou feliz*
interpretar(E,feliz):-
    sublist([estou,feliz],E).

%*estou triste*
interpretar(E,triste):-
    sublist([estou, triste],E).

%*sao como*
interpretar(E,objeto(sao_como,L1,L2)):-
    frase(sao_como,L1,L2,E,[]).

%*é como*
interpretar(E,objeto(e_como,L1,L2)):-
    frase(e_como,L1,L2,E,[]).

%*parece*
interpretar(E,parece):-
    sublist([parece],E).

%*mesmo*
interpretar(E,mesmo):-
    sublist([mesmo],E).

%*eu era*?
interpretar(E,objeto(eu_era_perg,L)):-
    frase(eu_era_perg,L,E,[]).

%*eu era*
interpretar(E,objeto(eu_era,L)):-
    frase(eu_era,L,E,[]).

%*eu sou*?
interpretar(E,objeto(eu_sou_perg,L)):-
    frase(eu_sou_perg,L,E,[]).

%*eu sou*
interpretar(E,objeto(eu_sou,L)):-
    frase(eu_sou,L,E,[]).
%*sou*
interpretar(E,sou):-
    sublist([sou],E).

%*voce é*?
interpretar(E,objeto(vc_e_perg,L)):-
    frase(vc_e_perg,L,E,[]).

%*voce é* 
interpretar(E,objeto(vc_e,L)):-
    frase(vc_e,L,E,[]).

%*por causa*
interpretar(E,por_causa):-
    sublist([por, causa],E).

%*voce era*?
interpretar(E,objeto(vc_era,L)):-
    frase(vc_era,L,E,[]).

%*eu nao posso*
interpretar(E,objeto(eu_nao_posso,L)):-
    frase(eu_nao_posso,L,E,[]).

%*eu sinto*
interpretar(E,objeto(eu_sinto,L)):-
    frase(eu_sinto,L,E,[]).

%*eu sentia*
interpretar(E,sentia):-
    sublist([eu, sentia],E).

%*porque voce nao*
interpretar(E,objeto(pq_vc_nao,L)):-
    frase(pq_vc_nao,L,E,[]).

%*sim*
interpretar(E,sim):-
    sublist([sim],E).

%*nao*
interpretar(E,nao):-
    sublist([nao],E).

%*alguem*
interpretar(E,alguem):-
    sublist([alguem],E).

%*todos*
interpretar(E,todos):-
    sublist([todos],E).

%*sempre*
interpretar(E,sempre):-
    sublist([sempre],E).

%*o que*
interpretar(E,o_que):-
    sublist([o, que],E).

%*talvez*
interpretar(E,talvez):-
    sublist([talvez],E).

%*sao*
interpretar(E,objeto(sao,L)):-
    frase(sao,L,E,[]).

%bye
interpretar(E,frase_sair):-
    sair(E,[]).

%frase nao reconhecida
interpretar(_,frase_nao_reconhecida).

/*############################################# Respostas #############################################*/

responder(oi):-
    write('Como vai você? Por favor, me fale do seu problema.'),nl,nl,
    interagir.

responder(computador):-
    random_member(Resp,['Computadores te incomodam?',
                        'O que você acha sobre máquinas?',
                        'Porque você menciona computadores?',
                        'O que você acha que máquinas tem a ver com o seu problema?']),
    write(Resp),nl,nl,
    interagir.

responder(nome):-
    write('Não estou interessado em nomes.'),nl,nl,
    interagir.

responder(desculpe):-
    random_member(Resp,['Por favor, não se desculpe',
                        'Desculpas não são necessárias',
                        'Como você se sente quando se desculpa?']),
    write(Resp),nl,nl,
    interagir.

responder(objeto(eu_lembro,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['Você normalmente lembra ',S,'?'],R1),
    atomic_list_concat(['Lembrar ',S,' traz alguma outra lembrança à sua mente?'],R2),
    atomic_list_concat(['Porque você lembra ',S,' nesse momento?'],R3),
    atomic_list_concat(['O que na situação atual faz você lembrar ',S],R4),
    atomic_list_concat(['Qual a conexão entre lembrar ',S,' e eu?'],R5),
    random_member(Resp, [R1,R2,R3,R4,R5,
                         'Que outras coisas você lembra?']),
    write(Resp),nl,nl,
    interagir.

responder(objeto(vc_lembra,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['Você acha que eu me esqueceria ',S,'?'],R1),
    atomic_list_concat(['Porque você acha que eu deveria lembrar ',S,' agora?'],R2),
    atomic_list_concat(['E que tal ',S,'?'],R3),
    atomic_list_concat(['Você mencionou ',S,'?'],R4),
    random_member(Resp, [R1,R2,R3,R4]),
    write(Resp),nl,nl,
    interagir.

%*se*
responder(objeto(se_,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['De verdade? se ',S,'?'],R1),
    random_member(Resp, [R1,'Sério?']),
    write(Resp),nl,nl,
    interagir.

%*eu sonhei*
responder(objeto(eu_sonhei,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['Realmente? ',S,'?'],R1),
    atomic_list_concat(['Você já sonhou ',S,' enquanto acordado?'],R2),
    atomic_list_concat(['Você já havia sonhado ',S,' antes?'],R3),
    random_member(Resp, [R1,R2,R3]),
    write(Resp),nl,nl,
    interagir.

%*eu sonhei com*
responder(objeto(eu_sonhei_com,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['Como você se sente em relação a ',S,' na verdade?'],Resp),
    write(Resp),nl,nl,
    interagir.

responder(sonho):-
    random_member(Resp,['O que este sonho sugere a você?',
                        'Você sonha com frequencia?',
                        'Que pessoas aparecem em seus sonhos?',
                        'Você não acha que sonhos tem algo a ver com o seu problema?']),
    write(Resp),nl,nl,
    interagir.

%*minha mae*
responder(objeto(minha_mae,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['Quem mais na sua família ',S,'?'],R1),
    random_member(Resp, [R1,'Fale-me mais sobre a sua família']),
    write(Resp),nl,nl,
    interagir.

responder(pai):-
    random_member(Resp,['Seu pai?',
                        'Ele influencia você fortemente?',
                        'O que mais vem à sua mente quando você pensa no seu pai?']),
    write(Resp),nl,nl,
    interagir.

%*eu quero*
responder(objeto(eu_quero,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['Qual a importância de ter ',S,'?'],R1),
    atomic_list_concat(['Porque você quer ',S,'?'],R2),
    atomic_list_concat(['Acho que você terá ',S,' logo'],R3),
    random_member(Resp, [R1,R2,R3]),
    write(Resp),nl,nl,
    interagir.

%*estou feliz*
responder(feliz):-
    random_member(Resp,['Eu tenho alguma influência nisso?',
                        'O que te faz feliz?',
                        'Você pode explicar o porque de feliz?']),
    write(Resp),nl,nl,
    interagir.

%*estou triste*
responder(triste):-
    random_member(Resp,['Sinto que você se sinta assim',
                        'Estou certo de que não é prazeroso estar assim']),
    write(Resp),nl,nl,
    interagir.

%*sao como*
responder(objeto(sao_como,L1,L2)) :-
    mudar_contexto(L1,NL1),
    mudar_contexto(L2,NL2),
    atomic_list_concat(NL1,' ',S1),
    atomic_list_concat(NL2,' ',S2),
    %concatena cada uma das respostas
    atomic_list_concat(['Que semelhança você vê entre ',S1,' e ',S2],Resp),
    write(Resp),nl,nl,
    interagir.

%*é como*
responder(objeto(e_como,L1,L2)) :-
    mudar_contexto(L1,NL1),
    mudar_contexto(L2,NL2),
    atomic_list_concat(NL1,' ',S1),
    atomic_list_concat(NL2,' ',S2),
    %concatena cada uma das respostas
    atomic_list_concat(['De que forma ',S1,' é como ',S2,'?'],R1),
    random_member(Resp,[R1,
                        'Que semelhança você vê?',
                        'Será que há realmente alguma coisa em comum?',
                        'Como?']),
    write(Resp),nl,nl,
    interagir.

%*parece*
responder(parece):-
    random_member(Resp,['De que forma?',
                        'Que similaridades há?']),
    write(Resp),nl,nl,
    interagir.

%*mesmo*
responder(mesmo):-
    write('Que outras conexões você observa?'),nl,nl,
    interagir.

%*eu era*?
responder(objeto(eu_era_perg,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['E se você fosse ',S,'?'],R1),
    atomic_list_concat(['Você acha que era ',S],R2),
    atomic_list_concat(['E qual o siginificado de ser ',S,'?'],R3),
    random_member(Resp, [R1,R2,R3]),
    write(Resp),nl,nl,
    interagir.

%*eu era*
responder(objeto(eu_era,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['Talvez eu soubesse que você fosse ',S],R1),
    atomic_list_concat(['Porque você está me dizendo que era ',S,' agora?'],R2),
    random_member(Resp, ['Você era realmente?',R1,R2]),
    write(Resp),nl,nl,
    interagir.

%*eu sou*?
responder(objeto(eu_sou_perg,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['Você acredita que você seja ',S,'?'],R1),
    atomic_list_concat(['Você gostaria de ser ',S,' ?'],R2),
    atomic_list_concat(['Você gostaria que eu dissesse que você é ',S,' ?'],R3),
    atomic_list_concat(['E qual o siginificado de ser ',S,' ?'],R4),
    random_member(Resp, [R1,R2,R3,R4]),
    write(Resp),nl,nl,
    interagir.

%*eu sou*
responder(objeto(eu_sou,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['E como você é ',S],R1),
    atomic_list_concat(['Você quer ser ',S,' ?'],R2),
    random_member(Resp, [R1,R2]),
    write(Resp),nl,nl,
    interagir.

responder(sou):-
    random_member(Resp,['Porque você está dizendo "SOU"?',
                        'Nao entendi']),
    write(Resp),nl,nl,
    interagir.

%*você é*?
responder(objeto(vc_e_perg,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['E qual o seu interesse em saber se sou ',S, ' ou não?'],R1),
    atomic_list_concat(['Você iria preferir se eu não fosse ',S,' ?'],R2),
    atomic_list_concat(['Talvez eu seja ',S,' em suas fantasias'],R3),
    random_member(Resp, [R1,R2,R3]),
    write(Resp),nl,nl,
    interagir.

%*você é*
responder(objeto(vc_e,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['O que faz você pensar que eu sou ',S, '?'],Resp),
    write(Resp),nl,nl,
    interagir.

%*por causa*
responder(por_causa):-
    random_member(Resp,['Essa é a razão?',
                        'Que outras razões você acha que poderiam haver?',
                        'E isto explica tudo?']),
    write(Resp),nl,nl,
    interagir.

%*voce era*?
responder(objeto(vc_era,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['Talvez eu fosse ',S],R1),
    atomic_list_concat(['E se eu fosse ',S,'?'],R2),
    random_member(Resp, [R1,R2,'O que você acha?']),
    write(Resp),nl,nl,
    interagir.

%*eu nao posso*?
responder(objeto(eu_nao_posso,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['Talvez você pudesse ',S,' agora'],R1),
    atomic_list_concat(['E se eu pudesse ',S,'?'],R2),
    random_member(Resp, [R1,R2]),
    write(Resp),nl,nl,
    interagir.

%*eu sinto*
responder(objeto(eu_sinto,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['Você sempre sente ',S, '?'],Resp),
    write(Resp),nl,nl,
    interagir.

%*eu sentia*
responder(sentia):-
    write('Que outras coisas você sente?'),nl,nl,
    interagir.

%*porque voce nao*
responder(objeto(pq_vc_nao,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['Você ',S,'?'],R1),
    atomic_list_concat(['Você acredita que eu não ',S],R2),
    atomic_list_concat(['Talvez eu vá ',S,' depois'],R3),
    random_member(Resp, [R1,R2,R3]),
    write(Resp),nl,nl,
    interagir.

%*sim*
responder(sim):-
    random_member(Resp,['Você parece uma pessoa bem positiva',
                        'Tem certeza?',
                        'Entendo']),
    write(Resp),nl,nl,
    interagir.

%*nao*
responder(nao):-
    random_member(Resp,['Porque não?',
                        'Você está sendo um pouco negativo',
                        'Você diz não só pra ser negativo?']),
    write(Resp),nl,nl,
    interagir.

%*alguem*
responder(alguem):-
    write('Você pode ser mais específico?'),nl,nl,
    interagir.

%*todos*
responder(todos):-
    random_member(Resp,['Com certeza não todos',
                        'Pode pensar em alguém em particular?',
                        'Quem por exemplo?',
                        'Você está pensando em alguém em particular?']),
    write(Resp),nl,nl,
    interagir.

%*sempre*
responder(sempre):-
    random_member(Resp,['Você pode dar um exemplo específico',
                        'Quando?',
                        'Sobre o que você está pensando?',
                        'Realmente sempre?']),
    write(Resp),nl,nl,
    interagir.

%*o que*
responder(o_que):-
    random_member(Resp,['Porque você pergunta?',
                        'Essa pergunta é interessante para você?',
                        'O que você quer saber de verdade?',
                        'O que você acha?',
                        'O que vem à sua mente quando pergunta isso?']),
    write(Resp),nl,nl,
    interagir.

%*talvez*
responder(talvez):-
    write('Você não parece muito certo'),nl,nl,
    interagir.

%*sao*
responder(objeto(sao,L)) :-
    mudar_contexto(L,NL),
    atomic_list_concat(NL,' ',S), %alterar para formato string
    %concatena cada uma das respostas
    atomic_list_concat(['Você acha que eles poderiam não ser ',S,'?'],R1),
    atomic_list_concat(['Possivelmente eles são ',S],R2),
    random_member(Resp, [R1,R2]),
    write(Resp),nl,nl,
    interagir.


responder(frase_sair):-
    write("Tchau!"),nl,nl.

responder(frase_nao_reconhecida):-
    random_member(Resp,['Muito interessante.',
                        'Não sei se entendi direito',
                        'O que isso sugere a você?',
                        'Por favor, continue.',
                        'Continue',
                        'Você quer mesmo falar sobre isso?',
                        'Elabore melhor']),
    write(Resp),nl,nl,
    interagir.

/*############################################# IO #############################################*/
prompt(L) :-
    write("> "),
    read_line_to_codes(user_input,Codes),
    normalizar_input(Codes,Normalizado),
    atomic_list_concat(L,' ',Normalizado).

/*################################## Interacao com usuario #########################################*/
interagir :-
    prompt(E),
    interpretar(E,I),
    responder(I).

