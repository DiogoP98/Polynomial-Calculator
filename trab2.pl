/**
  Diogo Pereira 201605323
  Ricardo Pereira 201604583
*/


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

belongs(X) :- variables(V), atom_string(X2,X), member(X2,V), !.

text2poly(S,P) :- split_string(S," ","",S2), build(P,S2,[]), !.

build(X) --> expr(X2), {X = X2}.

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


polyplay :- writeln("Your operation:"), flush_output, read(X), (X \= "leave", text2poly(X,P), writeln(P), polyplay; X == "leave", writeln("Goodbye")). 









