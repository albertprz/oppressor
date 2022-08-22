-- |

module Validations.Base where

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



authUser :: User
          -> CheckoutSummary a b
          -> Either ClientError (IO (CheckoutSummary (a |+| 'AuthUser) b))
authUser = undefined

validateCart :: CheckoutSummary a b
              -> Either ClientError (IO (CheckoutSummary (a |+| 'ValidPaymentDetails) b))
validateCart = undefined

checkPaymentDetails :: CheckoutSummary a b
                     -> Either ClientError (IO (CheckoutSummary (a |+| 'ValidPaymentDetails) b))
checkPaymentDetails = undefined

confirmPurchase :: CheckoutSummary a b
                 -> Either ClientError (IO (CheckoutSummary (a |+| 'ConfirmedPurchase) b))
confirmPurchase = undefined


checkout :: Must (ContainsMany a ['AuthUser, 'ValidPaymentDetails])
          => CheckoutSummary a 'New
          -> Either ClientError (IO (CheckoutSummary a 'Paid))
checkout = undefined

completePurchase :: Must (ContainsMany a ['AuthUser, 'ValidCart,       'ValidPaymentDetails, 'ConfirmedPurchase])
                 => CheckoutSummary a 'Paid -> Either ClientError (IO (CheckoutSummary a 'Complete))
completePurchase = undefined
