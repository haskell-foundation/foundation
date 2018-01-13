#!/bin/sh

# This script will generate the unicode case mappings tables using unicode's 
# CaseFolding <https://unicode.org/Public/UNIDATA/CaseFolding.txt> 
# and SpecialCasing.txt <https://unicode.org/Public/UNIDATA/SpecialCasing.txt> files.
#
curl "https://unicode.org/Public/UNIDATA/CaseFolding.txt" >> CaseFolding.txt
curl "https://unicode.org/Public/UNIDATA/SpecialCasing.txt" >> SpecialCasing.txt

stack runghc -- -XNoImplicitPrelude -XRebindableSyntax -XTypeFamilies -XBangPatterns -XDeriveDataTypeable CaseMapping.hs

if [ "$(head -n 7 "NewCaseMapping.hs")" = "$(head -n 7 "../../basement/Basement/String/CaseMapping.hs")" ]
  then
    echo "Unicode case mapping table up to date."
  else
    echo "A new Unicode case mapping has been released."
    echo "Please run /scripts/caseMapping/generateCaseMapping.sh locally, then move the generated file to /basement/Basement/String/NewCaseMapping.hs"
    exit 1
fi
