#!/bin/sh

# This script will generate the unicode case mappings tables using unicode's 
# CaseFolding <https://unicode.org/Public/UNIDATA/CaseFolding.txt> 
# and SpecialCasing.txt <https://unicode.org/Public/UNIDATA/SpecialCasing.txt> files.
#
# Those two files should be downloaded and placed in the same directory as this script.
#
stack runghc -- -XNoImplicitPrelude -XRebindableSyntax -XTypeFamilies -XBangPatterns -XDeriveDataTypeable CaseMapping.hs
