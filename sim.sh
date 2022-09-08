#!/bin/bash

make top.exe; vvp -n top.exe +change +verbose +steps=100 +prog=$1
