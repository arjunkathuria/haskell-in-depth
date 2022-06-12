{-# LANGUAGE RankNTypes #-}
{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language GADTs #-}
{-# Language ScopedTypeVariables #-}

module SingMan where

data DoorState = Closed | Opened
  deriving Show

data SDoorState (s :: DoorState) where
  SClosed :: SDoorState 'Closed    -- SDoorClosed 'Closed type is a singleton, with only one value, SClosed
  SOpened :: SDoorState 'Opened    -- (So is this)

-- There exists only one value of these types, namely, SClosed and SOpened respectively

-- The above are explicit singletons, sometimes it's better to work with
-- Implicit singletons, via as follows

class SDoorStateI (s :: DoorState) where
  sDoorState :: SDoorState s


instance SDoorStateI 'Opened where
  sDoorState = SOpened

instance SDoorStateI 'Closed where
  sDoorState = SClosed


-- describing the Door itself

data Door (s :: DoorState) where
  MKDoor :: SDoorStateI s => Door s


doorState :: forall (s :: DoorState) . Door s -> DoorState
doorState MKDoor =
  case (sDoorState :: SDoorState s) of
    SOpened -> Opened
    SClosed -> Closed

instance Show (Door s) where
  show d = "Door " <> show (doorState d)

-- sometimes we don't need to refer to singletons, controlling types is enough

open :: Door 'Closed -> Door 'Opened
open _ = MKDoor

close :: Door 'Opened -> Door 'Closed
close _ = MKDoor
