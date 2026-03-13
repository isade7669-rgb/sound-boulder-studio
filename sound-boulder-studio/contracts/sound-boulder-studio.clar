;; Sound Boulder Studio - Collaborative Art Platform
;; Tracks contributions, ownership, and royalty distribution for collaborative works.

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROJECT-NOT-FOUND (err u101))
(define-constant ERR-CONTRIBUTION-NOT-FOUND (err u102))
(define-constant ERR-INVALID-SHARE (err u103))
(define-constant ERR-ALREADY-FINALIZED (err u104))
(define-constant ERR-NOT-FINALIZED (err u105))
(define-constant ERR-INSUFFICIENT-FUNDS (err u106))
(define-constant ERR-INVALID-PARAMS (err u107))
(define-constant ERR-DISPUTE-EXISTS (err u108))
(define-constant ERR-NO-DISPUTE (err u109))

;; Share basis points: 10000 = 100%
(define-constant BASIS-POINTS u10000)

;; ============================================================
;; DATA MAPS
;; ============================================================

;; Project registry
(define-map projects
  { project-id: uint }
  {
    owner: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    total-shares: uint,         ;; Must equal BASIS-POINTS when finalized
    finalized: bool,
    royalty-pool: uint,         ;; STX in microstacks
    created-at: uint,
    is-disputed: bool
  }
)

;; Contributor shares per project
(define-map contributor-shares
  { project-id: uint, contributor: principal }
  {
    share-bps: uint,            ;; Basis points (e.g. 2500 = 25%)
    contribution-hash: (buff 32), ;; Cryptographic fingerprint of contribution
    contribution-type: (string-ascii 50), ;; e.g. "music", "lyrics", "code", "art"
    joined-at: uint,
    claimed-royalties: uint     ;; Total STX claimed by this contributor
  }
)

;; Track all contributors per project (via counter)
(define-map project-contributor-count
  { project-id: uint }
  { count: uint }
)

;; Indexed contributor list per project
(define-map project-contributors
  { project-id: uint, index: uint }
  { contributor: principal }
)

;; Reputation scores (cumulative, updated on successful payouts)
(define-map reputation-scores
  { user: principal }
  { score: uint, projects-completed: uint }
)

;; Dispute records
(define-map disputes
  { project-id: uint }
  {
    raised-by: principal,
    reason: (string-ascii 200),
    raised-at: uint,
    resolved: bool
  }
)

;; Project ID counter
(define-data-var project-nonce uint u0)

;; ============================================================
;; READ-ONLY FUNCTIONS
;; ============================================================

(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

(define-read-only (get-contributor-share (project-id uint) (contributor principal))
  (map-get? contributor-shares { project-id: project-id, contributor: contributor })
)

(define-read-only (get-reputation (user principal))
  (default-to
    { score: u0, projects-completed: u0 }
    (map-get? reputation-scores { user: user })
  )
)

(define-read-only (get-dispute (project-id uint))
  (map-get? disputes { project-id: project-id })
)

(define-read-only (get-contributor-count (project-id uint))
  (default-to
    { count: u0 }
    (map-get? project-contributor-count { project-id: project-id })
  )
)

(define-read-only (get-project-contributor (project-id uint) (index uint))
  (map-get? project-contributors { project-id: project-id, index: index })
)

(define-read-only (calculate-claimable (project-id uint) (contributor principal))
  (match (map-get? projects { project-id: project-id })
    project
    (match (map-get? contributor-shares { project-id: project-id, contributor: contributor })
      share-data
      (let
        (
          (pool (get royalty-pool project))
          (share-bps (get share-bps share-data))
          (already-claimed (get claimed-royalties share-data))
          (entitled (/ (* pool share-bps) BASIS-POINTS))
        )
        (if (> entitled already-claimed)
          (ok (- entitled already-claimed))
          (ok u0)
        )
      )
      ERR-CONTRIBUTION-NOT-FOUND
    )
    ERR-PROJECT-NOT-FOUND
  )
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

(define-private (is-project-owner (project-id uint) (caller principal))
  (match (map-get? projects { project-id: project-id })
    project (is-eq (get owner project) caller)
    false
  )
)

(define-private (update-reputation (user principal))
  (let
    (
      (current (get-reputation user))
    )
    (map-set reputation-scores
      { user: user }
      {
        score: (+ (get score current) u10),
        projects-completed: (+ (get projects-completed current) u1)
      }
    )
  )
)

;; ============================================================
;; PUBLIC FUNCTIONS
;; ============================================================

;; Create a new collaborative project
(define-public (create-project
    (title (string-ascii 100))
    (description (string-ascii 500))
  )
  (let
    (
      (project-id (+ (var-get project-nonce) u1))
    )
    (var-set project-nonce project-id)
    (map-set projects
      { project-id: project-id }
      {
        owner: tx-sender,
        title: title,
        description: description,
        total-shares: u0,
        finalized: false,
        royalty-pool: u0,
        created-at: block-height,
        is-disputed: false
      }
    )
    (map-set project-contributor-count { project-id: project-id } { count: u0 })
    (ok project-id)
  )
)

;; Add or update a contributor's share (owner only, pre-finalization)
(define-public (add-contributor
    (project-id uint)
    (contributor principal)
    (share-bps uint)
    (contribution-hash (buff 32))
    (contribution-type (string-ascii 50))
  )
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
      (current-count (get count (get-contributor-count project-id)))
    )
    (asserts! (is-eq (get owner project) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (not (get finalized project)) ERR-ALREADY-FINALIZED)
    (asserts! (> share-bps u0) ERR-INVALID-PARAMS)
    (asserts! (<= share-bps BASIS-POINTS) ERR-INVALID-PARAMS)

    ;; Register contributor index if new
    (if (is-none (map-get? contributor-shares { project-id: project-id, contributor: contributor }))
      (begin
        (map-set project-contributors
          { project-id: project-id, index: current-count }
          { contributor: contributor }
        )
        (map-set project-contributor-count
          { project-id: project-id }
          { count: (+ current-count u1) }
        )
      )
      true
    )

    ;; Upsert contributor share record
    (map-set contributor-shares
      { project-id: project-id, contributor: contributor }
      {
        share-bps: share-bps,
        contribution-hash: contribution-hash,
        contribution-type: contribution-type,
        joined-at: block-height,
        claimed-royalties: u0
      }
    )

    ;; Update total shares on project
    (map-set projects
      { project-id: project-id }
      (merge project { total-shares: (+ (get total-shares project) share-bps) })
    )
    (ok true)
  )
)

;; Finalize a project - locks shares and enables royalty claims
;; Total shares must equal BASIS-POINTS (10000) exactly
(define-public (finalize-project (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    )
    (asserts! (is-eq (get owner project) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (not (get finalized project)) ERR-ALREADY-FINALIZED)
    (asserts! (is-eq (get total-shares project) BASIS-POINTS) ERR-INVALID-SHARE)

    (map-set projects
      { project-id: project-id }
      (merge project { finalized: true })
    )
    (update-reputation tx-sender)
    (ok true)
  )
)

;; Deposit STX royalties into a project's pool
(define-public (deposit-royalties (project-id uint) (amount uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    )
    (asserts! (get finalized project) ERR-NOT-FINALIZED)
    (asserts! (not (get is-disputed project)) ERR-DISPUTE-EXISTS)
    (asserts! (> amount u0) ERR-INVALID-PARAMS)

    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    (map-set projects
      { project-id: project-id }
      (merge project { royalty-pool: (+ (get royalty-pool project) amount) })
    )
    (ok true)
  )
)

;; Claim earned royalties for a contributor
(define-public (claim-royalties (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
      (share-data (unwrap! (map-get? contributor-shares { project-id: project-id, contributor: tx-sender }) ERR-CONTRIBUTION-NOT-FOUND))
      (claimable (unwrap! (calculate-claimable project-id tx-sender) ERR-CONTRIBUTION-NOT-FOUND))
    )
    (asserts! (get finalized project) ERR-NOT-FINALIZED)
    (asserts! (not (get is-disputed project)) ERR-DISPUTE-EXISTS)
    (asserts! (> claimable u0) ERR-INSUFFICIENT-FUNDS)

    (try! (as-contract (stx-transfer? claimable tx-sender tx-sender)))

    (map-set contributor-shares
      { project-id: project-id, contributor: tx-sender }
      (merge share-data {
        claimed-royalties: (+ (get claimed-royalties share-data) claimable)
      })
    )
    (update-reputation tx-sender)
    (ok claimable)
  )
)

;; Raise a dispute on a project (any contributor can raise)
(define-public (raise-dispute (project-id uint) (reason (string-ascii 200)))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    )
    (asserts! (is-some (map-get? contributor-shares { project-id: project-id, contributor: tx-sender })) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-disputed project)) ERR-DISPUTE-EXISTS)

    (map-set disputes
      { project-id: project-id }
      {
        raised-by: tx-sender,
        reason: reason,
        raised-at: block-height,
        resolved: false
      }
    )
    (map-set projects
      { project-id: project-id }
      (merge project { is-disputed: true })
    )
    (ok true)
  )
)

;; Resolve a dispute (contract owner acts as arbiter)
(define-public (resolve-dispute (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
      (dispute (unwrap! (map-get? disputes { project-id: project-id }) ERR-NO-DISPUTE))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (get is-disputed project) ERR-NO-DISPUTE)

    (map-set disputes
      { project-id: project-id }
      (merge dispute { resolved: true })
    )
    (map-set projects
      { project-id: project-id }
      (merge project { is-disputed: false })
    )
    (ok true)
  )
)

;; Transfer project ownership
(define-public (transfer-ownership (project-id uint) (new-owner principal))
  (let
    (
      (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    )
    (asserts! (is-eq (get owner project) tx-sender) ERR-NOT-AUTHORIZED)
    (map-set projects
      { project-id: project-id }
      (merge project { owner: new-owner })
    )
    (ok true)
  )
)
