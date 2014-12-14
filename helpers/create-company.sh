#!/bin/bash

STR="{\"companyName\":\"721 650 894\",\"companyPlant\":\"I\",\"instance\":\"Company\"}"
curl -X POST -H "Content-Type: application/json" -d "${STR}" http://crm/api/v1.0.0/companies
