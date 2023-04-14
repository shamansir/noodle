#!/bin/bash
spago -x ./spago.dhall build # --purs-args "--censor-codes=MissingTypeDeclaration,UnusedName,ShadowedName,UserDefinedWarning,WildcardInferredType,ImplicitQualifiedImport,<ImplicitImport>"