#!/bin/bash

make simulation.exe; vvp -n simulation.exe +verbose +steps=25 +prog=$1
