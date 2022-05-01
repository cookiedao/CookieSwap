(impl-trait .cookie-swap-trait-v1.swap-trait)

(define-fungible-token wstx-cookie)

(define-constant ERR-NOT-AUTHORIZED u21401)

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) (err ERR-NOT-AUTHORIZED))

    (match (ft-transfer? wstx-cookie amount sender recipient)
      response (begin
        (print memo)
        (ok response)
      )
      error (err error)
    )
  )
)

(define-read-only (get-name)
  (ok "Cookie V1 wSTX cookie LP Token")
)

(define-read-only (get-symbol)
  (ok "COOKIEWSTXCOOK")
)

(define-read-only (get-decimals)
  (ok u6)
)

(define-read-only (get-balance (owner principal))
  (ok (ft-get-balance wstx-cookie owner))
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply wstx-cookie))
)

(define-read-only (get-token-uri)
  (ok (some u"https://arkadiko.finance/tokens/wstx-diko-token.json"))
)
;; {
;;   "name":"wSTX-DIKO",
;;   "description":"wSTX-DIKO Arkadiko LP token",
;;   "image":"url",
;;   "vector":"url"
;; }

;; one stop function to gather all the data relevant to the LP token in one call
(define-read-only (get-data (owner principal))
  (ok {
    name: (unwrap-panic (get-name)),
    symbol: (unwrap-panic (get-symbol)),
    decimals: (unwrap-panic (get-decimals)),
    uri: (unwrap-panic (get-token-uri)),
    supply: (unwrap-panic (get-total-supply)),
    balance: (unwrap-panic (get-balance owner))
  })
)

;; the extra mint method used when adding liquidity
;; can only be used by cookie swap main contract
(define-public (mint (recipient principal) (amount uint))
  (begin
    (print "cookie-swap-token.mint")
    (print contract-caller)
    (print amount)

    (asserts! (is-eq contract-caller (unwrap-panic (contract-call? .cookie-dao get-qualified-name-by-name "swap"))) (err ERR-NOT-AUTHORIZED))
    (ft-mint? wstx-cookie amount recipient)
  )
)


;; the extra burn method used when removing liquidity
;; can only be used by cookie swap main contract
(define-public (burn (recipient principal) (amount uint))
  (begin
    (print "cookiw-swap-token.burn")
    (print contract-caller)
    (print amount)

    (asserts! (is-eq contract-caller (unwrap-panic (contract-call? .cookie-dao get-qualified-name-by-name "swap"))) (err ERR-NOT-AUTHORIZED))
    (ft-burn? wstx-cookie amount recipient)
  )
)