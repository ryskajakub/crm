#!/bin/bash

PGPASSWORD='haskell' psql -h localhost -U haskell -d crm < create-tables.sql
