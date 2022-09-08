#!/bin/bash

make simulation.exe; vvp -n simulation.exe +steps=0 +prog=$1
