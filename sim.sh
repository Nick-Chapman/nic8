#!/bin/bash

make simulation.exe; vvp -n simulation.exe +verbose +steps=100 +prog=$1
