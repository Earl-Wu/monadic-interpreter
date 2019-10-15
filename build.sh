#!/bin/bash -e
# The top line tells the shell that this program will be run in Bash
# (You can put flags for Bash there as well!)

alex Lexer.x;
happy -i Parser.y;
ghc Fb.hs;
