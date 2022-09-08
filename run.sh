#!/bin/bash

make simulation.exe; vvp -n simulation.exe +prog=$1
