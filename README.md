# 2e crm app #

## Purpose ##

Application for planning maintenances customer's factories.

This application serves to keep track of facilities in different factories and suggest plan of doing the maintenance work in the factories.

The information, that are being tracked are:

1. The companies, their name, address 
1. Machines which belong to the company, one company can have multiple machines
1. Maintenances - this is a record of planned or done maintenance. Has information about which employee did the upkeep, how much time it took, which machines in the factory were maintained, etc.

Apart from the CRUD, the application computes, when there should occur the next maintenance in the companies. It does it based on this reasoning
1. manually planned maintenances overrides the application computation
1. the date of latest machine maintenance, the expected level of usage and the need for maintenance defined by the machine type is taken into account during the next maintenance date computation.

## Tech stack ##

Haskell

### Serveside ###
1. opaleye
1. rest

### Clientside ###
1. Fay
1. React
1. Bootstrap
