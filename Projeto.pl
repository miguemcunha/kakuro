:- [codigo_comum].

% ------------------------------------------------------ 
% Estrutura que tem um numero (Soma) e as variaveis que 
% compoem o espaco (Vars).
% ------------------------------------------------------

espaco(Soma, Vars):-
    number(Soma),
    sumlist(Vars, Soma).

% ------------------------------------------------------ 
% Encontra todas as permutacoes cuja soma e' Soma.
% ------------------------------------------------------

combinacoes_soma(N, Els, Soma, Combs):-
    sort(Els, Temp),
    findall(Comb,(combinacao(N, Temp, Comb),
        sum_list(Comb, X), X =:= Soma),
        Combs).

% ------------------------------------------------------ 
% Encontra todas as permutacoes cuja soma e' igual a Soma.
% ------------------------------------------------------

permutacoes_soma(N, Els, Soma, Perms):-
    combinacoes_soma(N, Els, Soma, Combs),
    findall(Perm,
        (combinacao(N, Els, Comb), permutation(Comb, Perm), member(Comb, Combs))
        ,Perms_aux),
    sort(Perms_aux, Perms).

% ------------------------------------------------------ 
% Procura um espaco numa fila.
% ------------------------------------------------------

number_of_sum([Sum|_], v, [Sum]).

number_of_sum([_|Sum], h, Sum).

lista_vars([El | R1], [El | R2]) :-
  var(El), !,
  lista_vars(R1, R2).

lista_vars(_, []).

espaco_fila_aux([Head|Tail], espaco(Sum, Vars), H_V):-
    (
        is_list(Head),
        number_of_sum(Head, H_V, Sum),
        lista_vars(Tail, Vars),
        Vars \== []
    );
    ( 
        espaco_fila_aux(Tail,espaco(Sum, Vars), H_V)
    ).

espaco_fila([Head|Tail], espaco(Sum, Vars), H_V):-
    espaco_fila_aux([Head|Tail], espaco([Sum], Vars), H_V),
    (
        (
            \+(is_list(Vars)),
            espaco_fila_aux([Head|Tail], espaco([Sum], Vars), H_V)
        );
        (
            is_list(Vars)
        )
    ).

% ------------------------------------------------------ 
% Encontra todos os espacos de uma fila
% ------------------------------------------------------

espacos_fila(H_V, Fila, Esps):-
    bagof(Esp, espaco_fila(Fila, Esp, H_V), Esps).

% ------------------------------------------------------ 
% Encontra todos os espacos de um puzzle
% ------------------------------------------------------

espacos_puzzle(Puzzle, Espacos):-
    append(Puzzle, Linha),
    espacos_fila(h, Linha, Esps1),
    mat_transposta(Puzzle, Mat_trans),
    append(Mat_trans, Colunas),
    espacos_fila(v, Colunas, Esps2),
    append(Esps1, Esps2, Espacos).

% ------------------------------------------------------ 
% Encontra todos os espacos com variaveis em comum com o 
% espaco principal.
% ------------------------------------------------------

membro(E, [Q | _]):- E == Q.
membro(E, [_ | R]):- membro(E, R).

verifica_comum(Var, espaco(_, Vars)):-
    membro(Var, Vars).

espacos_com_posicoes_comuns_aux(_, [], _):- !.

espacos_com_posicoes_comuns_aux(Var, [Head|_], Head):-
    verifica_comum(Var, Head), 
    nonvar(Head).

espacos_com_posicoes_comuns_aux(Var, [_|Tail], H):-
    espacos_com_posicoes_comuns_aux(Var, Tail, H).

espacos_com_posicoes_comuns(Espacos, Esp1, Esps_com):-
    Esp1 = espaco(_, Vars),
    bagof(Esps,
        X^(member(X, Vars), espacos_com_posicoes_comuns_aux(X, Espacos, Esps), Esps \== [], Esps \== Esp1,
        nonvar(Esps)),
        Esps_com).

% ------------------------------------------------------ 
% Encontra todas as permutacoes possiveis para uma lista
% de espacos.
% ------------------------------------------------------

permutacoes_soma_espacos([], []).

permutacoes_soma_espacos([Esp|Tail], [Temp|R]):-
    Esp = espaco(Sum, Vars),
    length(Vars, X),
    permutacoes_soma(X, [1, 2, 3, 4, 5, 6, 7, 8, 9], Sum, Perm),
    Temp = [Esp, Perm],
    permutacoes_soma_espacos(Tail, R).

% ------------------------------------------------------ 
% Aplica permutacao_possivel_espaco para descobrir todas as 
% possiveis para um espaco principal.
% ------------------------------------------------------

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma):-
    findall(Y, (member(Perms_aux, Perms_soma), Perms_aux = [Esp, Y]), [X]),
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
    member(Perm, X),
    bagof(Pos, Esp1^(member(Esp1, Esps_com), nth0(Pos, Esps_com, Esp1)), Lista_pos),
    forall(member(Pos, Lista_pos),
            (nth0(Pos, Perm, Num), nth0(Pos, Esps_com, Esp_com),
            member(Perms_coms, Perms_soma), Perms_coms = [Esp_com, Perms_com],
            append(Perms_com, Nums_com), member(Num, Nums_com))).

% ------------------------------------------------------ 
% Aplica permutacao_possivel_espaco para descobrir todas as 
% possiveis para um espaco principal.
% ------------------------------------------------------

permutacoes_possiveis_espaco(Espacos, Perms_soma, espaco(Sum, Vars), Perms_poss):-
    bagof(Perm, permutacao_possivel_espaco(Perm, espaco(Sum, Vars), Espacos, Perms_soma), 
        Perms_possiveis),
    Perms_poss = [Vars, Perms_possiveis].

% ------------------------------------------------------ 
% Aplica permutacoes_possiveis_espaco para descobrir todas as 
% possiveis para todos os espacos.
% ------------------------------------------------------

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps):-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    bagof(Perms, 
        Esp^(member(Esp, Espacos), permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms)),
        Perms_poss_esps).

% ------------------------------------------------------ 
% Atribui numeros comuns entre todas as permuutacoes. Estes
% numeros tem de estar todos no mesmo indice.
% ------------------------------------------------------

numeros_comuns(Lst_Perms, Numeros_comuns):-
    nth1(1, Lst_Perms, X),
    findall(Par, (member(N, X), nth1(I, X, N), Par = (I, N)), Pares1),
    sort(Pares1, Pares),
    findall(Par_aux, (member(Par_aux, Pares), Par_aux = (Pos, Num),
    member(Perm, Lst_Perms), \+(nth1(Pos, Perm, Num))), Pares_wrong),
    subtract(Pares, Pares_wrong, Numeros_comuns).

atribui_variaveis([], _).

atribui_variaveis([Par|T], Vars):-
    Par = (Pos, Num), 
    nth1(Pos, Vars, Num), 
    atribui_variaveis(T, Vars).

atribui_comuns([]).

atribui_comuns([Perms_Possiveis|T]):-
    nth1(1, Perms_Possiveis, Vars),
    nth1(2, Perms_Possiveis, Perms),
    numeros_comuns(Perms, Pares),
    atribui_variaveis(Pares, Vars),
    atribui_comuns(T).

% ------------------------------------------------------ 
% Retira permutacoes que nao unificam com as Vars deseignadas
% para o espaco em questao.
% ------------------------------------------------------

verifica_impossivel([], []).

verifica_impossivel([Var|T], [Num|Q]):-
    Var == Num, 
    verifica_impossivel(T, Q).

verifica_impossivel([Var|T], [_|Q]):-
    var(Var),
    verifica_impossivel(T, Q).

retira_impossiveis([], []).

retira_impossiveis([H|T], [Novas_Perms_Possiveis|Q]):-  
    H = [Vars, Perms_aux],
    include(verifica_impossivel(Vars), Perms_aux, Perms),
    Novas_Perms_Possiveis = [Vars, Perms],
    retira_impossiveis(T, Q).

% ------------------------------------------------------ 
% Simplifica uma lista de permutacoes atribuindo comuns e retirando
% permutacoes impossiveis sucessivamente
% ------------------------------------------------------

simplifica(Perms_Possiveis, Novas_Perms_Possiveis):-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, Perms_Possiveis_aux),
    Perms_Possiveis == Perms_Possiveis_aux,
    Novas_Perms_Possiveis = Perms_Possiveis_aux.

simplifica(Perms_Possiveis, Novas_Perms_Possiveis):-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, Perms_Possiveis_aux),
    Perms_Possiveis \== Perms_Possiveis_aux,
    simplifica(Perms_Possiveis_aux, Novas_Perms_Possiveis).

% ------------------------------------------------------ 
% Usa os predicados anteriores para inicializar o Puzzle
% ------------------------------------------------------

inicializa(Puzzle, Perms_Possiveis):-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Perms_Possiveis_aux),
    simplifica(Perms_Possiveis_aux, Perms_Possiveis).

% ------------------------------------------------------ 
% Escolhe menos alternativas e as suas auxiliares que encontram
% dentro de uma lista de espacos e as suas permutacoes o elemento
% com o menor numero de permutacoes possiveis para um espaco.
% ------------------------------------------------------

numero_minimo([], _, _).

numero_minimo([H|T], Z):-
    nth1(2, H, Perms),
    length(Perms, X),
    X >= Z,
    numero_minimo(T, Z).

numero_minimo([H|T], Z):-
    nth1(2, H, Perms),
    length(Perms, X),
    X < Z,
    numero_minimo(T, X).

encontra_primeiro_z(Perms_Possiveis, Z, Escolha):-
    member(Escolha, Perms_Possiveis),
    nth1(2, Escolha, Perms),
    length(Perms, Z).

escolhe_menos_alternativas(Perms_Possiveis, Escolha):-
    bagof(Perm_Possivel, (member(Perm_Possivel, Perms_Possiveis), 
        nth1(2, Perm_Possivel, Perms1), length(Perms1, X), X \== 1),
        Perms_Possiveis_aux),
    findall(X, (member(Perm_Possivel, Perms_Possiveis), 
        nth1(2, Perm_Possivel, Perms1), length(Perms1, X), X \== 1),
        List_X),
    min_list(List_X, Y),
    nth1(Pos, List_X, Y),
    nth1(Pos, Perms_Possiveis_aux, Escolha), !.

% ------------------------------------------------------ 
% Exprimenta uma permutacao para um espaco
% ------------------------------------------------------

experimenta_perm([Esp, Lst_Perms], Perms_Possiveis, Novas_Perms_Possiveis):-
    member(Perm, Lst_Perms),
    Esp = Perm,
    select([Esp, Lst_Perms], Perms_Possiveis, [Esp, [Perm]], Novas_Perms_Possiveis).

% ------------------------------------------------------ 
% Encontra os valores certos e possiveis para todos os espacos
% ------------------------------------------------------

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis):-
    \+(escolhe_menos_alternativas(Perms_Possiveis, _)),
    Novas_Perms_Possiveis = Perms_Possiveis.

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis):-
    escolhe_menos_alternativas(Perms_Possiveis, Escolha),
    experimenta_perm(Escolha, Perms_Possiveis, Perms_Possiveis_aux),
    simplifica(Perms_Possiveis_aux, Temp_Perms_Possiveis),
    resolve_aux(Temp_Perms_Possiveis, Novas_Perms_Possiveis).

% ------------------------------------------------------ 
% Resolve o Puzzle
% ------------------------------------------------------

resolve(Puzzle):-
    inicializa(Puzzle, Perms_Possiveis),
    resolve_aux(Perms_Possiveis, _).

