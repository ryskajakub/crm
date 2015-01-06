#!/bin/bash
STR='{"companyName":"Changed name","companyPlant":"I","instance":"Company","companyAddress":"adreas1","companyPhone":"777","companyPerson":"novák"}'
curl -X PUT -H "Content-Type: application/json" -d "${STR}" http://crm/api/v1.0.0/companies/3/
