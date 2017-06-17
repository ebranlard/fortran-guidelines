#!/bin/bash
git rev-parse --short HEAD       | awk '{print "\\newcommand{\\gitrevision}{"$1"}"}'     > tVersion.tex 
git rev-parse --abbrev-ref HEAD  | awk '{print "\\newcommand{\\gitbranch}{"$1"}"}'      >> tVersion.tex 
git describe --dirty=-dev        | awk '{print "\\newcommand{\\gitversion}{"$1"}"}'     >> tVersion.tex 

