#!/bin/bash

make top.exe; vvp -n top.exe +change +verbose +steps=5 +prog=$1
