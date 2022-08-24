-- |

module Validations.Base where

import Control.Monad.Except (ExceptT)
import Utils.TypeLevel


data Validation
  = AuthUser
  | ValidCart
  | ValidPaymentDetails
  | ConfirmedPurchase

data CheckoutState
  = New
  | Paid
  | Complete

data ClientError
  = ConnectionTimeout
  | NotFound


data Cart
  = MkCart [Product]

data User

data Product

data PaymentDetails

data CheckoutSummary (a :: [Validation]) (b :: CheckoutState)
  = MkSummary User Cart PaymentDetails


type AppM = ExceptT ClientError IO


authUser :: CheckoutSummary a b
          -> AppM (CheckoutSummary (a |+| 'AuthUser) b)
authUser = undefined

validateCart :: CheckoutSummary a b
              -> AppM (CheckoutSummary (a |+| 'ValidCart) b)
validateCart = undefined

checkPaymentDetails :: CheckoutSummary a b
                     -> AppM (CheckoutSummary (a |+| 'ValidPaymentDetails) b)
checkPaymentDetails = undefined

confirmPurchase :: CheckoutSummary a b
                 -> AppM (CheckoutSummary (a |+| 'ConfirmedPurchase) b)
confirmPurchase = undefined


checkout :: Must (ContainsMany a ['AuthUser, 'ValidCart])
          => CheckoutSummary a 'New
          -> AppM (CheckoutSummary a 'Paid)
checkout = undefined

completePurchase :: Must (ContainsMany a ['AuthUser, 'ValidCart,       'ValidPaymentDetails, 'ConfirmedPurchase])
                 => CheckoutSummary a 'Paid -> AppM (CheckoutSummary a 'Complete)
completePurchase = undefined
