#!/bin/bash
spago-legacy -x ./test.dhall test # --purs-args "--censor-codes=MissingTypeDeclaration,UnusedName,ShadowedName,UserDefinedWarning,WildcardInferredType,ImplicitQualifiedImport"