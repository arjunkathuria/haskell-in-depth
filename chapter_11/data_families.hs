{-# Language TypeFamilies #-}

data family XList a

newtype instance XList () = XListUnit Integer
data instance XList Bool = XBits Integer Integer


data YList a = YListUnit Integer | YBits Integer Integer

class XListable a where
  xempty :: XList a
  xcons :: a -> XList a -> XList a
  xheadMay :: XList a -> Maybe a

instance XListable () where
  xempty = XListUnit 0
  xcons () (XListUnit n) = XListUnit (n+1)
  xheadMay (XListUnit 0) = Nothing
  xheadMay _ = Just ()

instance XListable Bool where
