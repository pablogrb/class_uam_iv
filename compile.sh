#!/bin/bash
rm class_pmcamx_met.mod
rm test_run_met

ifort class_pmcamx_met.f90 test_run_met.f90 -o test_run_met -g -mieee-fp -align dcommons -convert big_endian -static_intel -extend-source
