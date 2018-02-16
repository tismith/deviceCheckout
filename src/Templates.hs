{-# LANGUAGE OverloadedStrings #-}
module Templates (deviceList) where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy as TL (Text)
import Data.Maybe (fromMaybe)

import Types(Device(..), ReservationStatus(..))

bootstrap4 :: H.AttributeValue
bootstrap4 = "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css"

invertReservationStatus :: Maybe ReservationStatus -> String
invertReservationStatus r =
    case r of
        Just Reserved -> "Available"
        _ -> "Reserved"

deviceRow :: Device -> H.Markup
deviceRow device =
    H.form
        H.! A.name (H.toValue $ deviceName device)
        H.! A.action "/devices"
        H.! A.method "post" $ do
        --hidden deviceName
        H.input H.! A.type_ "hidden" H.! A.name "deviceName"
            H.! A.value (H.toValue $ deviceName device)
            H.! A.class_ "form-control"
        --hidden reservationStatus
        H.input H.! A.type_ "hidden" H.! A.name "reservationStatus"
            H.! A.value (H.toValue $ invertReservationStatus (reservationStatus device))
            H.! A.class_ "form-control"
        --readonly deviceName
        H.td $ H.toMarkup $ deviceName device
        --readonly deviceUrl
        H.td $ H.a H.! A.href (H.toValue $ fromMaybe "" $ deviceUrl device)
            $ H.toHtml $ fromMaybe "" $ deviceUrl device
        --editable deviceOwner
        H.td $ H.input H.! A.type_ "text" H.! A.name "deviceOwner"
            H.! A.class_ "form-control"
            H.! A.value (H.toValue $ fromMaybe "" $ deviceOwner device)
        --editable comments
        H.td $ H.input H.! A.type_ "text" H.! A.name "comments"
            H.! A.class_ "form-control"
            H.! A.value (H.toValue $ fromMaybe "" $ comments device)
        --submit button
        H.td $ H.input H.! A.type_ "submit"
            H.! A.class_ (case reservationStatus device of
                        Just Reserved -> "btn btn-danger"
                        _ -> "btn btn-primary")
            H.! case reservationStatus device of
                Just Reserved -> A.value "RETURN"
                _ -> A.value "CLAIM"

deviceList :: (Foldable t) => t Device -> TL.Text
deviceList devices = renderHtml $ do
    H.head $ do
        H.title "Device list"
        H.link H.! A.rel "stylesheet"
            H.! A.href bootstrap4

    H.body $
        H.div H.! A.class_ "container" $ do
            H.h1 "Devices"
            H.table H.! A.class_ "table table-bordered" $ do
                H.thead H.! A.class_ "thead-dark" $ H.tr $ do
                    H.th "Device name"
                    H.th "URL"
                    H.th "Owner"
                    H.th "Comments"
                    H.th ""
                mapM_ (H.tr . deviceRow) devices
