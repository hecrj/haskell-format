module Main where

create :: Name -> IO ( Customer, [Event] )
create name =
    do
        id <- Id <$> Uuid.generate
        let
            customer =
                Customer id name

            events =
                [ Created customer ]

        return ( customer, events )


create2 :: Name -> IO ( Customer, [Event] )
create2 name =
    do
        id <- Id <$> Uuid.generate
        let
            customer =
                Customer id name

            events =
                [ Created customer ]

            in
            return ( customer, events )
