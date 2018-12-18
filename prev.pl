/**
  Diogo Pereira 201605323
  Ricardo Pereira 201604583
*/


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

number2string2(1000,"one thousand").
number2string2(900,"nine hundred").
number2string2(800,"eigth hundred").
number2string2(700,"seven hundred").
number2string2(600,"six hundred").
number2string2(500,"five hundred").
number2string2(400,"four hundred").
number2string2(300,"three hundred").
number2string2(200,"two hundred").
number2string2(100,"one hundred").
number2string2(90,"ninety").
number2string2(80,"eighty").
number2string2(70,"seventy").
number2string2(60,"sixty").
number2string2(50,"fifty").
number2string2(40,"forty").
number2string2(30,"thirty").
number2string2(20,"twenty").
number2string2(19,"nineteen").
number2string2(18,"eighteen").
number2string2(17,"seventeen").
number2string2(16,"sixteen").
number2string2(15,"fifteen").
number2string2(14,"fourteen").
number2string2(13,"thirteen").
number2string2(12,"twelve").
number2string2(11,"eleven").
number2string2(10,"ten").
number2string2(9,"nine").
number2string2(8,"eight").
number2string2(7,"seven").
number2string2(6,"six").
number2string2(5,"five").
number2string2(4,"four").
number2string2(3,"three").
number2string2(2,"two").
number2string2(1,"one").
number2string2(0,"zero").

number2string(0,"zero"):- !.
number2string(X,S) :- var(S), number2string2(A,B), X >= A, !, X2 is X-A, ((X2 == 0, B2="");(number2string(X2,B3),string_concat(" ",B3,B2))), string_concat(B,B2,S), !.
number2string(X,S) :- number2string2(A,B), string_concat(B,C,S), ((C == "", X2 is 0); (string_concat(" ", C2, C), number2string(X2,C2))), X is X2+A, !.

belongs(X) :- variables(V), (string(X) , atom_string(X2,X); X2 = X), member(X2,V), !.

writeList([]).
writeList([[X,Y]|L2]) :- write(X), write(" = "), writeln(Y), writeList(L2).

operation(S,P) :- split_string(S," ","",S2), opr(P,S2,[]).
operation(S,P) :- text2poly(S,P).

text2poly(S,P) :- split_string(S," ","",S2), build(P,S2,[]), !.

opr(X) --> ["add"], build(X2), ["to"], build(X3), {addpoly(X2,X3,X4), simpoly(X4,X5), X = X5}.
opr(X) --> ["simplify"], ["polynomial"], build(X2), {simpoly(X2, SP), X = SP}.
opr(X) --> ["show"], build(X2), ["as"], pvar(X3), {write(X3), write("= "), assertz(polynomials(X3,X2)), X = X2}.
opr(X) --> ["show"], ["stored"], ["polynomials"], {findall([ID,P], polynomials(ID,P),L), writeList(L), X = ""}.

build(X) --> expr(X2), {X = X2}.
build(X) --> expr(X2), ["plus"], expr(X3), {X = X2 + X3}.
build(X) --> expr(X2), ["minus"], expr(X3), {X = X2 - X3}.

expr(X) --> factor(X2), ["plus"], factor(X3), {X = X2+X3}.
expr(X) --> factor(X2), ["minus"], factor(X3), {X = X2-X3}.
expr(X) --> factor(X2), {X = X2}.	

factor(X) --> num(X2), {number2string(X3,X2), X = X3}.
factor(X) --> raised(X2), {X = X2}.
factor(X) --> num(X2), ["times"], raised(X3), {number2string(N,X2), X = N*X3}.

raised(X) --> pvar(X2), {belongs(X2), atom_string(V,X2), X = V}.
raised(X) --> pvar(X2), ["raised"], ["to"], num(X3) , {belongs(X2), atom_string(V,X2), number2string(N,X3), X = V^N}.
raised(X) --> pvar(X2), ["squared"], {belongs(X2), atom_string(V,X2), X = V^2}.
raised(X) --> pvar(X2), ["cubed"], {belongs(X2), atom_string(V,X2), X = V^3}.

pvar(X) --> [X].

num(X) --> [X].

polyplay :- writeln("Your operation:"), flush_output, read(X), ((X \= "leave", operation(X,P), writeln(P), polyplay); (X == "leave", retractall(polynomials(_,_)), writeln("Goodbye"))). 


