
digit(0) --> [""].
digit(1) --> ["one"].
digit(2) --> ["two"].
digit(3) --> ["three"].
digit(4) --> ["four"].
digit(5) --> ["five"].
digit(6) --> ["six"].
digit(7) --> ["seven"].
digit(8) --> ["eight"].
digit(9) --> ["nine"].

teen(10) --> ["ten"].
teen(11) --> ["eleven"].
teen(12) --> ["twelve"].
teen(13) --> ["thirteen"].
teen(14) --> ["fourteen"].
teen(15) --> ["fifteen"].
teen(16) --> ["sixteen"].
teen(17) --> ["seventeen"].
teen(18) --> ["eighteen"].
teen(19) --> ["nineteen"].

ten(20) --> ["twenty"].
ten(30) --> ["thirty"].
ten(40) --> ["fourty"].
ten(50) --> ["fifty"].
ten(60) --> ["sixty"].
ten(70) --> ["seventy"].
ten(80) --> ["eighty"].
ten(90) --> ["ninety"].

num(N) --> trinum(R), {N is R}.
num(N) --> trinum(M), ["thousand"], trinum(R), {N is M*1000+R}.

trinum(N) --> twonum(DU), {N is DU}.
trinum(N) --> digit(C), ["hundred"], twonum(DU), {N is C*100+DU}.

twonum(N) --> ten(D), digit(U), {N is D+U}.
twonum(N) --> teen(D), {N is D}.
twonum(N) --> digit(U), {N is U}.

%num2string(N,S) :- var(N), split_string(S," ","",S2), mnum(N,S2,[]), !.
%num2string(N,S) :- mnum(N,S2,[]), phrase().

concat([A],A).
concat([A|B],C) :- concat(B,D), string_concat(A," ",A2), string_concat(A2,D,C).

num2string(N,S) :- var(S), phrase(num(N),S2), concat(S2,S), !.
num2string(N,S) :- phrase(num(N),S), !.

%separacao

/*
number(0)--> [zero].
number(N)--> xxx(N).
number(N)--> xx(N).

xxx(N) --> digit(D), [hundred], rest_xxx(N1), {N is D*100 + N1}.

rest_xxx(0)-->[].
rest_xxx(N)-->[and], xx(N).

xx(N)--> digit(N).
xx(N)--> teen(N).
xx(N)--> tens(T), rest_xx(N1), {N is T + N1}.

rest_xx(0)--> [].
rest_xx(N)--> digit(N).

digit(1) --> [one].
digit(2) --> [two].
digit(3) --> [three].
digit(4) --> [four].
digit(5) --> [five].
digit(6) --> [six].
digit(7) --> [seven].
digit(8) --> [eight].
digit(9) --> [nine].

teen(10) --> [ten].
teen(11) --> [eleven].
teen(12) --> [twelve].
teen(13) --> [thirteen].
teen(14) --> [fourteen].
teen(15) --> [fifteen].
teen(16) --> [sixteen].
teen(17) --> [seventeen].
teen(18) --> [eighteen].
teen(19) --> [nineteen].
tens(20) --> [twenty].

tens(30) --> [thirty].
tens(40) --> [forty].
tens(50) --> [fifty].
tens(60) --> [sixty].
tens(70) --> [seventy].
tens(80) --> [eighty].
tens(90) --> [ninety].
*/








