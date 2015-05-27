#!/bin/bash
STR='{"instance":"Employee","name":"Alois12", "contact":"contact", "capabilities":"caps"}'
curl -X POST -H "Cookie: passx" -H "Content-Type: application/json" -d "${STR}" http://crm/api/v1.0.0/employees/
