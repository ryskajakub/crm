module Crm.Server.Database.Types where

import Opaleye (Column, Nullable, PGInt4, PGInt8, PGText, PGDate, PGBool)

type DBInt = Column PGInt4
type DBInt8 = Column PGInt8
type DBText = Column PGText
type DBDate = Column PGDate
type DBBool = Column PGBool

type MBInt = Column (Nullable PGInt4)
type MBInt8 = Column (Nullable PGInt8)
type MBText = Column (Nullable PGText)
type MBDate = Column (Nullable PGDate)
type MBBool = Column (Nullable PGBool)
