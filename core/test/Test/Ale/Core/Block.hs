module Test.Ale.Core.Block where

-- import Universum

-- import Hedgehog (Property)
-- import Hedgehog.Internal.Property (forAllT)

-- import Ale.Core.Block (mkPointer)

-- import Test.Ale.Core.Block.Gen (genBlock)
-- import Test.Ale.Core.DHT.Pure (propInPureDHT)
-- import Test.Serialise (serialising)


-- TODO: We can't generate blocks now because we need access to state

----------------------------------------
--- Block
----------------------------------------

-- hprop_BlockSerialise :: Property
-- hprop_BlockSerialise = propInPureDHT $
--     forAllT genBlock >>= serialising

----------------------------------------
--- BlockPointer
----------------------------------------

-- hprop_BlockPointerSerialise :: Property
-- hprop_BlockPointerSerialise = propInPureDHT $
--     (mkPointer <$> forAllT genBlock) >>= serialising
