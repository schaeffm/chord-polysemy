module RoutingTable where

data RoutingTable addr = RoutingTable {
    address :: addr,
    fingers :: [addr],
    predecessor :: Maybe addr,
    successors :: [addr]
}

createRoutingTable :: addr -> RoutingTable addr
createRoutingTable n = RoutingTable 
    { address = n
    , successors = [n]
    , predecessor = Nothing
    , fingers = []
    }

