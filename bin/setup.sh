#!/usr/bin/env bash

echo Generating a new keychain for you at $(ale-path keychain)
ale-keychain generate

echo Generating a server key pair at $(ale-path server-secret-key)
ale-keygen -p $(ale-path server-secret-key)

echo Generating Genesis data with a uniform distribution at $(ale-path genesis)
ale-distribute -n 10 key1 key2 key3 key4 | ale-genesis -
