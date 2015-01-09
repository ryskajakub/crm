#!/bin/bash

PGPASSWORD='haskell' psql -h localhost -U haskell -d crm < schema.sql 
