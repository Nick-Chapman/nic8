#!/bin/bash

make top.exe; vvp -n top.exe +steps=0 +prog=$1
