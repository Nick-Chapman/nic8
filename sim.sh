#!/bin/bash

make simulation.exe; vvp -n simulation.exe +change +verbose +steps=100 +prog=$1
