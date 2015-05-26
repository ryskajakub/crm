#!/bin/bash
STR='{"instance":"Login","password":"pass1"}'
curl -i -X POST -H "Content-Type: application/json" -d "${STR}" http://crm/api/v1.0.0/login/perform/
