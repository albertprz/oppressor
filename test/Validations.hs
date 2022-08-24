module Validations where

import Control.Monad.Except
import Validations.Base



checkoutStep :: CheckoutSummary _ 'New
              -> AppM (CheckoutSummary _ 'Paid)
checkoutStep =
  authUser >=>
  validateCart >=>
  checkout


completePurchaseStep :: CheckoutSummary _ 'Paid
                      -> AppM (CheckoutSummary _ 'Complete)
completePurchaseStep =
  checkPaymentDetails >=>
  confirmPurchase >=>
  completePurchase


fullProgram :: CheckoutSummary '[] 'New
             -> AppM (CheckoutSummary _ 'Complete)
fullProgram =
  checkoutStep >=>
  completePurchaseStep
