#!/bin/bash
spago -x ./test.dhall test # --purs-args "--censor-codes=MissingTypeDeclaration,UnusedName,ShadowedName,UserDefinedWarning,WildcardInferredType,ImplicitQualifiedImport"