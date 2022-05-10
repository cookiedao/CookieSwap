;; @contract Stake Registry - Keep track of all staking pools
;; Users can stake, unstake and claim rewards from active pools.
;; DAO can activate a new pool or deactivate an existing one.
;; When a pool is deactivated, users can not stake but they can unstake.
;; @version 1.1

(use-trait ft-trait .sip-010-trait-ft-standard.sip-010-trait)
(use-trait stake-pool-trait .cookieswap-stake-pool-trait-v1.stake-pool-trait)
(use-trait stake-registry-trait .cookieswap-stake-registry-trait-v1.stake-registry-trait)
(impl-trait .cookieswap-stake-registry-trait-v1.stake-registry-trait)

;; Errors
(define-constant ERR-INVALID-POOL (err u19001))
(define-constant ERR-POOL-EXIST (err u19002))
(define-constant ERR-POOL-INACTIVE (err u19003))
(define-constant ERR-WRONG-REGISTRY (err u19004))
(define-constant ERR-NOT-AUTHORIZED u19401)

;; Variables
(define-data-var pool-count uint u0)

;; Pool maps
(define-map pools-data-map
  { pool: principal }
  {
    name: (string-ascii 256),
    deactivated-block: uint,
    deactivated-rewards-per-block: uint,
    rewards-percentage: uint
  }
)

;; Get pool info
(define-read-only (get-pool-data (pool principal))
  (unwrap-panic (map-get? pools-data-map { pool: pool }))
)

;; Set pool info
(define-public (set-pool-data
  (pool principal)
  (name (string-ascii 256))
  (deactivated-block uint)
  (deactivated-rewards-per-block uint)
  (rewards-percentage uint)
)
  (begin
    (asserts! (is-eq tx-sender (contract-call? .cookie-dao get-dao-owner)) (err ERR-NOT-AUTHORIZED))

    (map-set pools-data-map
      { pool: pool }
      {
        name: name,
        deactivated-block: deactivated-block,
        deactivated-rewards-per-block: deactivated-rewards-per-block,
        rewards-percentage: rewards-percentage
      }
    )
    (ok true)
  )
)

;; Get pool rewards per block
(define-read-only (get-rewards-per-block-for-pool (pool principal))
  (let (
    (total-staking-rewards (contract-call? .cookieswap-cookie-guardian-v1 get-staking-rewards-per-block))
    (pool-percentage (get rewards-percentage (get-pool-data pool)))
    (deactivated-block (get deactivated-block (get-pool-data pool)))
    (deactivated-rewards-per-block (get deactivated-rewards-per-block (get-pool-data pool)))
  )
    (if (is-eq deactivated-block u0)
      (ok (/ (* total-staking-rewards pool-percentage) u1000000))
      (ok deactivated-rewards-per-block)
    )
  )
)

;; Get pool deactivated block
(define-read-only (get-pool-deactivated-block (pool principal))
  (let (
    (pool-info (unwrap! (map-get? pools-data-map { pool: pool }) ERR-POOL-EXIST))
    (block (get deactivated-block pool-info))
  )
    (ok block)
  )
)

;; @desc stake tokens
;; @param registry-trait; current stake registry, to be used by stake pool
;; @param pool-trait; pool to stake tokens in
;; @param token-trait; token to stake in pool
;; @param amount; amount of tokens to stake
;; @post uint; returns amount of staked tokens
(define-public (stake (registry-trait <stake-registry-trait>) (pool-trait <stake-pool-trait>) (token-trait <ft-trait>) (amount uint))
  (begin
    (let (
      (pool (contract-of pool-trait)) 
      (pool-info (unwrap! (map-get? pools-data-map { pool: pool }) ERR-POOL-EXIST))
    )
      (asserts! (is-eq (as-contract tx-sender) (contract-of registry-trait)) ERR-WRONG-REGISTRY)
      (asserts! (is-eq (get deactivated-block pool-info) u0) ERR-POOL-INACTIVE)

      (print {
        type: "pool",
        action: "stake",
        data: { registry: registry-trait, pool: pool-trait, token: token-trait, amount: amount, owner: tx-sender }
      })
      (contract-call? pool-trait stake registry-trait token-trait tx-sender amount)
    )
  )
)

;; @desc unstake tokens
;; @param registry-trait; current stake registry, to be used by pool
;; @param pool-trait; pool to unstake tokens from
;; @param token-trait; token to unstake from pool
;; @param amount; amount of tokens to unstake
;; @post uint; returns amount of unstaked tokens
(define-public (unstake (registry-trait <stake-registry-trait>) (pool-trait <stake-pool-trait>) (token-trait <ft-trait>) (amount uint))
  (begin
    (let (
      (pool (contract-of pool-trait)) 
      (pool-info (unwrap! (map-get? pools-data-map { pool: pool }) ERR-POOL-EXIST))
    )
      (asserts! (is-eq (as-contract tx-sender) (contract-of registry-trait)) ERR-WRONG-REGISTRY)

      (print {
        type: "pool",
        action: "unstake",
        data: { registry: registry-trait, pool: pool-trait, token: token-trait, amount: amount, owner: tx-sender }
      })
      (contract-call? pool-trait unstake registry-trait token-trait tx-sender amount)
    )
  )
)

;; @desc get amount of pending COOKIE rewards for pool
;; @param registry-trait; current stake registry, to be used by pool
;; @param pool-trait; pool to get pending rewards from
;; @post uint; returns amount of pending rewards
(define-public (get-pending-rewards (registry-trait <stake-registry-trait>) (pool-trait <stake-pool-trait>))
  (begin
    (let (
      (pool (contract-of pool-trait)) 
      (pool-info (unwrap! (map-get? pools-data-map { pool: pool }) ERR-POOL-EXIST))
    )
      (asserts! (is-eq (as-contract tx-sender) (contract-of registry-trait)) ERR-WRONG-REGISTRY)
      (contract-call? pool-trait get-pending-rewards registry-trait tx-sender)
    )
  )
)

;; @desc claim pending COOKIE rewards for pool
;; @param registry-trait; current stake registry, to be used by pool
;; @param pool-trait; pool to claim rewards on
;; @post uint; returns amount of claimed rewards
(define-public (claim-pending-rewards (registry-trait <stake-registry-trait>) (pool-trait <stake-pool-trait>))
  (begin
    (asserts! (is-eq (as-contract tx-sender) (contract-of registry-trait)) ERR-WRONG-REGISTRY)
    (contract-call? pool-trait claim-pending-rewards registry-trait tx-sender)
  )
)

;; @desc claim pending COOKIE rewards for pool and immediately stake in COOKIE pool
;; @param registry-trait; current stake registry, to be used by pool
;; @param pool-trait; pool to claim rewards on
;; @param cookie-pool-trait; COOKIE pool to stake rewards
;; @param cookie-token-trait; COOKIE token contract
;; @post uint; returns amount of claimed/staked rewards
(define-public (stake-pending-rewards 
    (registry-trait <stake-registry-trait>) 
    (pool-trait <stake-pool-trait>) 
    (cookie-pool-trait <stake-pool-trait>)
    (cookie-token-trait <ft-trait>)
  )
  (let (
    (claimed-rewards (unwrap-panic (contract-call? pool-trait claim-pending-rewards registry-trait tx-sender)))
  )
    (asserts! (is-eq (as-contract tx-sender) (contract-of registry-trait)) ERR-WRONG-REGISTRY)

    (print {
      type: "pool",
      action: "stake-pending-rewards",
      data: { registry: registry-trait, pool: pool-trait, owner: tx-sender }
    })
    (contract-call? cookie-pool-trait stake registry-trait cookie-token-trait tx-sender claimed-rewards)
  )
)
