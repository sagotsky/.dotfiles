#!/bin/bash

(
  echo \"{\\C-,\\e}{a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z}\"%--------------------------------  | tr ' ' "\n" | tr '%' ' '

  bind -p | grep -v self | grep -v '^#'
  bind -s
) | sort
