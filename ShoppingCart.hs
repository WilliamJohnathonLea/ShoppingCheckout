
data Item = Item {
    price :: Int,
    tag :: Char
} deriving (Show)


type OfferF = Maybe Char -> Checkout -> Checkout


data Offer = Offer {
    apply :: OfferF,
    offerTag :: Maybe Char
}


data Checkout = Checkout {
    total :: Int,
    items :: [Item]
} deriving (Show)


inOffer :: Maybe Char -> [Item] -> [Item]
inOffer Nothing is = is
inOffer (Just t) is = filter ((== t) . tag) is


notInOffer :: Maybe Char -> [Item] -> [Item]
notInOffer Nothing is = is
notInOffer (Just t) is = filter ((/= t) . tag) is


bogof :: OfferF
bogof Nothing c = c -- Do nothing because the offer needs a filter tag
bogof t (Checkout ct is) = Checkout (ct + offerSum + spareSum) (notInOffer t is)
    where
        offerItems = inOffer t is
        rem = (length offerItems) `mod` 2
        spareSum = foldr ( (+) . price ) 0 (take rem offerItems)
        offerSum = (foldr ( (+) . price ) 0 (drop rem offerItems)) `div` 2


xForY :: Int -> Int -> OfferF
xForY _ _ Nothing c = c -- Do nothing because the offer needs a filter tag
xForY n p t (Checkout ct is) = Checkout (ct + offerSum + spareSum) (notInOffer t is)
    where
        offerItems = inOffer t is
        rem = (length offerItems) `mod` n
        spareSum = foldr ( (+) . price ) 0 (take rem offerItems)
        offerSum = (length (drop rem offerItems) `div` n) * p


checkout :: Checkout -> [Offer] -> Checkout
checkout c@(Checkout _ []) _ = c
checkout (Checkout t is) [] = Checkout finalTotal []
    where finalTotal = foldr ( (+) . price ) t is
checkout c (o:os) = checkout ( (apply o) (offerTag o) c ) os


startingCheckout = Checkout 0 [(Item 30 'B'), (Item 30 'B'), (Item 30 'B'), (Item 50 'A')]
demoCheckout = checkout startingCheckout [(Offer (2 `xForY` 45) (Just 'B')), (Offer (3 `xForY` 130) (Just 'A'))]
