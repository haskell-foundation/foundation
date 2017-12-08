#!/bin/sh

stack runghc -- -XNoImplicitPrelude -XRebindableSyntax -XTypeFamilies -XBangPatterns -XDeriveDataTypeable CaseMapping.hs


