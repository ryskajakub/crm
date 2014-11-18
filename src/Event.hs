module Event where

import FFI (ffi)

data SyntheticEvent
data SyntheticMouseEvent

eventValue :: SyntheticEvent -> Fay String
eventValue = ffi " %1['target']['value'] "
