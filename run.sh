#!/bin/bash

make simulation.exe; vvp -n simulation.exe +steps=10 +prog=$1
