/**
  Diogo Pereira 201605323
  Ricardo Pereira 201604583
*/

:- dynamic(polynomials/2).

%check if it a lonely variable or if it has an exponent
power(X) :- belongs(X), !.
power(X^Y) :- belongs(X), integer(Y), Y>1, !.

%checks if it is a monomial
monomial(N) :- number(N), !.
monomial(X) :- power(X), !.
monomial(-X) :- power(X), !.
monomial(K*X) :- number(K), power(X), !.

%Divides the monomial in coefficient and exponent (X^Y)
split(N,N,ind) :- number(N), !.
split(P,1,P) :- power(P), !.
split(-P,-1,P) :- power(P), !.
split(N*P,N,P).

%Auxiliar function of list2poly2, concatenates two polynomials without operation sign in the middle
merge(X,Y,Z) :- term_to_atom(X,X1), term_to_atom(Y,Y1),
                atom_string(X1,X2), atom_string(Y1,Y2),
                string_chars(X2,X3), string_chars(Y2,Y3), append(X3,Y3,W2),
                string_chars(W,W2), atom_string(Z1,W), term_to_atom(Z,Z1).

%Auxiliar function of poly2list, converts a polynomial as a list to a polynomial as an expression
list2poly2(M,[M]) :- monomial(M),!.
list2poly2(-M,[-M]) :- monomial(M),!.
list2poly2(P+M,[M|L]) :- monomial(M), split(M,K,_), K>0, 
                         list2poly2(P,L), !.
list2poly2(C,[M|L]) :- monomial(M), split(M,K,_), K<0, 
                       list2poly2(P,L), merge(P,M,C), !.

%Auxiliar function of poly2list, converts a polynomial as an expression to a polynomial as a list
poly2list2(M,[M]) :- monomial(M), !.
poly2list2(-M,[-M]) :- monomial(M), !.
poly2list2(P+M,[M|L]) :- monomial(M), poly2list2(P,L), !.
poly2list2(P-M,[M2|L]) :- monomial(M), split(M,K,Po), K2 is -K,
                          ((Po==ind,M2=K2); (K2 is -1,merge(-,M,M2)); M2=K2*Po),
                          poly2list2(P,L).

%main function to convert polynomials as expressions to polynomials as lists and vice-versa
poly2list(0,[]) :- !.
poly2list(P,L) :- var(P), reverse(L,L2), list2poly2(P,L2),!.
poly2list(P,L) :- poly2list2(P,L2), reverse(L,L2),!.

%Given a list of monomials and an power, returns the sum of the coefficient and the list of monomials with that power
sumE([],_,0,[]) :- !.
sumE([M|P],Po,K,P2) :- split(M,N,Po), sumE(P,Po,N2,P2), K is N+N2, !.
sumE([M|P],Po,K,[M|P2]) :- sumE(P,Po,K,P2).

%Auxiliar function to simpoly_list. Sums the monomials with the same powers
joinExp([],[]).
joinExp([M|P],[M2|P2]) :- split(M,_,Po), sumE([M|P],Po,K,P3),
                          M2=K*Po, joinExp(P3,P2).

%Auxiliar function to simpoly_list. Cuts useless constants and eliminates null monomials
cutK([],[]).
cutK([M|P],P2) :- split(M,N,_), (N is 0; N is (0.0)), cutK(P,P2), !.
cutK([M|P],[M2|P2]) :- split(M,N,Po), Po=ind, M2=N, cutK(P,P2), !.
cutK([M|P],[M2|P2]) :- split(M,N,Po), (N is 1; N is (1.0)), M2=Po, cutK(P,P2), !.
cutK([M|P],[-M2|P2]) :- split(M,N,Po), (N is -1; N is (-1.0)), M2=Po, cutK(P,P2), !.
cutK([M|P],[M|P2]) :- cutK(P,P2).

%Simplifies a polynomial as a list
simpoly_list(L,L2) :- joinExp(L,L3), cutK(L3,L2).

%Simplifies a polynomial as an expression
simpoly(P,P2) :- poly2list(P,L), simpoly_list(L,L2), poly2list(P2,L2).


%Multiplies a polynomial by a constant. It returns a simplified polynomial
scalepoly2([],_,[]).
scalepoly2([M|P],F,[M2|P2]) :- split(M,N,Po), N2 is N*F, M2=N2*Po,
                               scalepoly2(P,F,P2).
scalepoly(P,F,P2) :- poly2list(P,P3), simpoly_list(P3,P4),
                     scalepoly2(P4,F,P5), simpoly_list(P5,P6), poly2list(P2,P6).

%Auxiliar function to addpoly. It appends 2 lists, in this case, 2 polynomials
append([], L, L).
append([X|L1], L2, [X|L3]) :- append(L1, L2, L3).

%It adds 2 polynomials and returns a third simplified polynomial that is the sum of the previous ones.
addpoly(P1,P2,R) :- poly2list(P1,P1L), poly2list(P2,P2L), append(P1L,P2L,RL), simpoly_list(RL, RS), poly2list(R,RS).




%----------------------Parte 2-------------------------------------


%Group of acceptable variable names
variables([a,b,c,d,e,f,g,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
  aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,as,at,au,av,aw,ax,ay,az,
  ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bw,bx,by,bz,
  ca,cb,cc,cd,ce,cf,cg,ch,ci,cj,ck,cl,cm,cn,co,cp,cq,cr,cs,ct,cu,cv,cw,cx,cy,cz,
  da,db,dc,dd,de,df,dg,dh,di,dj,dk,dl,dm,dn,do,dp,dq,dr,ds,dt,du,dv,dw,dx,dy,dz,
  ea,eb,ec,ed,ee,ef,eg,eh,ei,ej,ek,el,em,en,eo,ep,eq,er,es,et,eu,ev,ew,ex,ey,ez,
  fa,fb,fc,fd,fe,ff,fg,fh,fi,fj,fk,fl,fm,fn,fo,fp,fq,fr,fs,ft,fu,fv,fw,fx,fy,fz,
  ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,gk,gl,gm,gn,go,gp,gq,gr,gs,gt,gu,gv,gw,gx,gy,gz,
  ha,hb,hc,hd,he,hf,hg,hh,hi,hj,hk,hl,hm,hn,ho,hp,hq,hr,hs,ht,hu,hv,hw,hx,hy,hz,
  ia,ib,ic,id,ie,if,ig,ih,ii,ij,ik,il,im,in,io,ip,iq,ir,is,it,iu,iv,iw,ix,iy,iz,
  ja,jb,jc,jd,je,jf,jg,jh,ji,jj,jk,jl,jm,jn,jo,jp,jq,jr,js,jt,ju,jv,jw,jx,jy,jz,
  ka,kb,kc,kd,ke,kf,kg,kh,ki,kj,kk,kl,km,kn,ko,kp,kq,kr,ks,kt,ku,kv,kw,kx,ky,kz,
  la,lb,lc,ld,le,lf,lg,lh,li,lj,lk,ll,lm,ln,lo,lp,lq,lr,ls,lt,lu,lv,lw,lx,ly,lz,
  ma,mb,mc,md,me,mf,mg,mh,mi,mj,mk,ml,mm,mn,mo,mp,mq,mr,ms,mt,mu,mv,mw,mx,my,mz,
  na,nb,nc,nd,ne,nf,ng,nh,ni,nj,nk,nl,nm,nn,no,np,nq,nr,ns,nt,nu,nv,nw,nx,ny,nz,
  oa,ob,oc,od,oe,of,og,oh,oi,oj,ok,ol,om,on,oo,op,oq,or,os,ot,ou,ov,ow,ox,oy,oz,
  pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pp,pq,pr,ps,pt,pu,pv,pw,px,py,pz,
  qa,qb,qc,qd,qe,qf,qg,qh,qi,qj,qk,ql,qm,qn,qo,qp,qq,qr,qs,qt,qu,qv,qw,qx,qy,qz,
  ra,rb,rc,rd,re,rf,rg,rh,ri,rj,rk,rl,rm,rn,ro,rp,rq,rr,rs,rt,ru,rv,rw,rx,ry,rz,
  sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn,so,sp,sq,sr,ss,st,su,sv,sw,sx,sy,sz,
  ta,tb,tc,td,te,tf,tg,th,ti,tj,tk,tl,tm,tn,to,tp,tq,tr,ts,tt,tu,tv,tw,tx,ty,tz,
  ua,ub,uc,ud,ue,uf,ug,uh,ui,uj,uk,ul,um,un,uo,up,uq,ur,us,ut,uu,uv,uw,ux,uy,uz,
  va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk,vl,vm,vn,vo,vp,vq,vr,vs,vt,vu,vv,vw,vx,vy,vz,
  wa,wb,wc,wd,we,wf,wg,wh,wi,wj,wk,wl,wm,wn,wo,wp,wq,wr,ws,wt,wu,wv,ww,wx,wy,wz,
  xa,xb,xc,xd,xe,xf,xg,xh,xi,xj,xk,xl,xm,xn,xo,xp,xq,xr,xs,xt,xu,xv,xw,xx,xy,xz,
  ya,yb,yc,yd,ye,yf,yg,yh,yi,yj,yk,yl,ym,yn,yo,yp,yq,yr,ys,yt,yu,yv,yw,yx,yy,yz,
    za,zb,zc,zd,ze,zf,zg,zh,zi,zj,zk,zl,zm,zn,zo,zp,zq,zr,zs,zt,zu,zv,zw,zx,zy,zz]).

digit(0) --> ["zero"].
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
ten(40) --> ["forty"].
ten(50) --> ["fifty"].
ten(60) --> ["sixty"].
ten(70) --> ["seventy"].
ten(80) --> ["eighty"].
ten(90) --> ["ninety"].


%Generates a number between 1000 and 9999
tnum(N) --> trinum(R),
            {N is R}.
tnum(N) --> trinum(M), ["thousand"],
            {N is M*1000}.
tnum(N) --> trinum(M), ["thousand"], trinum(R),
            {N is M*1000+R}.

%Generates a number between 100 and 999
trinum(N) --> twonum(DU),
              {N is DU}.
trinum(N) --> digit(C), ["hundred"],
              {N is C*100}.
trinum(N) --> digit(C), ["hundred"], twonum(DU),
              {N is C*100+DU}.

%Generates a number between 0 and 99
twonum(N) --> ten(D), digit(U),
              {N is D+U}.
twonum(N) --> ten(D),
              {N is D}.
twonum(N) --> teen(D),
              {N is D}.
twonum(N) --> digit(U),
              {N is U}.

%Converts a number represented by a string to a real number
number2string(Number,String) :- tnum(Number,String,[]), !.

%Checks if a variable belongs to the list of valid variables
belongs(VarS) :- variables(List), (string(VarS) , atom_string(Var,VarS); Var = VarS), member(Var,List), !.

%Converts a string to the polynomial that it represents
text2poly(String,Poly) :- split_string(String," ","",List), expr(Poly,List,[]), !.

%Prints the list of operations given by the user's input
print([]).
print([Op|List]) :- Op==".", writeln(""), !, print(List).
print([Op|List]) :- (Op=="";write(Op)) , print(List).

%Pritns the list of variables stored in memory
writeList([]).
writeList([[ID,Poly]|List]) :- write(ID), write(" = "), writeln(Poly), writeList(List).

%In case a variable is already in memory, it prints an error
polyNameUsed(ID) :- polynomials(ID,_), write(ID), writeln(" is used").

group(L) --> [X], {L = [X]}.
group(L) --> [A], group(B), {append([A],B,L)}.

start(String) :- split_string(String," ","",ListS), list(ListS,[]), !.

%Generates the list of operations
list --> group(L1), ["and"], {parse(L,L1,[]), print(L)}, list.
list --> parse(L), {print(L)}.

%Part of the grammar that identifies the type of operation
parse(L) --> ["show"], ["variables"],
             {findall([ID,Poly], polynomials(ID,Poly),L1), writeList(L1), L=[""]}.
parse(L) --> ["forget"], [ID],
             {retract(polynomials(ID,_)), L=[""];
             append(["It doesn't exists"],["."],L)}.
parse(L) --> cmd(Poly), ["as"], [ID],
             {polyNameUsed(ID), !; assertz(polynomials(ID,Poly)),
             append([ID],[" = "],L1), append(L1,[Poly],L2), append(L2,["."],L), !}.
parse(L) --> cmd(Poly),
             {append([Poly],["."],L), !}.

%Part of the grammar that identifies the type of operation within the operations with polynomials
cmd(Poly) --> ["show"], expr(Poly).
cmd(PolySimp) --> ["simplify"], expr(Poly),
              {simpoly(Poly,PolySimp)}.
cmd(PolySum) --> ["add"], expr(Poly1), ["to"], expr(Poly2),
           {addpoly(Poly1,Poly2,PolySum)}.
cmd(PolyMul) --> ["multiply"], group(Factor), ["by"], expr(Poly1),
           {number2string(Num,Factor), scalepoly(Poly1,Num,PolyMul)}.

%Part of the grammar that builds the polynomail.
expr(Poly) --> term(T1), ["plus"], expr(T2),
              {Poly = T2+T1}.
expr(Poly) --> term(T1), ["minus"], expr(T2),
            {Poly = T2-T1}.
expr(Poly) --> term(Poly).


term(Poly) --> group(NumS), ["times"], raised(Exp),
              {number2string(NumR,NumS), Poly = NumR*Exp}.
term(Poly) --> group(NumS), raised(Exp),
              {number2string(NumR,NumS), Poly = NumR*Exp}.
term(Poly) --> raised(Poly).
term(Poly) --> group(NumS),
              {number2string(Poly,NumS)}.
term(Poly) --> [ID],
              {polynomials(ID,Poly)}.


raised(Term) --> [VarS], ["raised"], ["to"], group(NumS),
              {belongs(VarS), atom_string(Var,VarS), number2string(NumR,NumS), Term = Var^NumR}.
raised(Term) --> [VarS], ["squared"],
              {belongs(VarS), atom_string(Var,VarS), Term = Var^2}.
raised(Term) --> [VarS], ["cubed"],
              {belongs(VarS), atom_string(Var,VarS), Term = Var^3}.
raised(Term) --> [VarS],
              {belongs(VarS), atom_string(Var,VarS), Term = Var}.



polyplay :- retractall(polynomials(_,_)), polyplay_aux, !.

polyplay_aux :- writeln("Your operation:"), flush_output, read_string(user_input, "\n", " ", _, Input),
                ((Input \= "leave", start(Input), polyplay_aux);
                (Input == "leave", writeln("Goodbye"))).

