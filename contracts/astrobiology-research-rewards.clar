;; title: astrobiology-research-rewards
;; Token incentives for exoplanet discovery, atmospheric analysis, and astrobiology research
;; This contract manages reward distribution, research validation, and community governance

;; Constants
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_INVALID_DATA (err u401))
(define-constant ERR_INSUFFICIENT_FUNDS (err u402))
(define-constant ERR_RESEARCH_NOT_FOUND (err u403))
(define-constant ERR_ALREADY_CLAIMED (err u404))
(define-constant ERR_NOT_VALIDATED (err u405))
(define-constant ERR_VOTING_CLOSED (err u406))
(define-constant CONTRACT_OWNER tx-sender)

;; Reward amounts (in micro-tokens)
(define-constant DISCOVERY_BASE_REWARD u1000000) ;; 1000 tokens
(define-constant ANALYSIS_BASE_REWARD u300000) ;; 300 tokens
(define-constant VALIDATION_REWARD u100000) ;; 100 tokens
(define-constant BIOSIGNATURE_BONUS u2000000) ;; 2000 token bonus
(define-constant PEER_REVIEW_REWARD u50000) ;; 50 tokens
(define-constant GOVERNANCE_REWARD u25000) ;; 25 tokens

;; Quality multipliers (percentage)
(define-constant HIGH_QUALITY_MULTIPLIER u200) ;; 2x
(define-constant MEDIUM_QUALITY_MULTIPLIER u150) ;; 1.5x
(define-constant LOW_QUALITY_MULTIPLIER u100) ;; 1x

;; Token management
(define-fungible-token exoplanet-research-token)

;; Data Structures
(define-map research-contributions
  { contribution-id: uint }
  {
    contributor: principal,
    research-type: (string-ascii 30),
    data-reference: (string-ascii 100),
    quality-score: uint,
    peer-reviews: uint,
    validation-status: (string-ascii 20),
    submission-date: uint,
    reward-amount: uint,
    claimed: bool,
    multiplier-applied: uint
  }
)

(define-map reward-pool
  { pool-type: (string-ascii 20) }
  {
    total-allocated: uint,
    total-distributed: uint,
    remaining-balance: uint,
    last-updated: uint,
    pool-manager: principal
  }
)

(define-map researcher-profiles
  { researcher: principal }
  {
    total-contributions: uint,
    total-rewards-earned: uint,
    reputation-score: uint,
    specialization: (string-ascii 50),
    verified-discoveries: uint,
    peer-review-count: uint,
    governance-participation: uint,
    member-since: uint,
    status: (string-ascii 20)
  }
)

(define-map governance-proposals
  { proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposal-type: (string-ascii 30),
    voting-start: uint,
    voting-end: uint,
    yes-votes: uint,
    no-votes: uint,
    total-votes: uint,
    status: (string-ascii 20),
    execution-date: uint,
    minimum-participation: uint
  }
)

(define-map research-bounties
  { bounty-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    reward-amount: uint,
    deadline: uint,
    requirements: (string-ascii 300),
    target-planet: (string-ascii 50),
    status: (string-ascii 20),
    claimed-by: (optional principal),
    evaluation-criteria: (string-ascii 200)
  }
)

(define-map milestone-achievements
  { achievement-id: uint }
  {
    researcher: principal,
    milestone-type: (string-ascii 50),
    achievement-date: uint,
    description: (string-ascii 200),
    reward-earned: uint,
    verification-required: bool,
    verified-by: (list 3 principal),
    special-recognition: bool
  }
)

;; Data Variables
(define-data-var contribution-counter uint u0)
(define-data-var proposal-counter uint u0)
(define-data-var bounty-counter uint u0)
(define-data-var achievement-counter uint u0)
(define-data-var token-supply uint u100000000000000) ;; 100M tokens
(define-data-var min-peer-reviews uint u3)
(define-data-var governance-threshold uint u1000000) ;; 1000 tokens to vote
(define-data-var reputation-decay-rate uint u5) ;; 5% per year

;; Read-only functions
(define-read-only (get-research-contribution (contribution-id uint))
  (map-get? research-contributions { contribution-id: contribution-id })
)

(define-read-only (get-reward-pool (pool-type (string-ascii 20)))
  (map-get? reward-pool { pool-type: pool-type })
)

(define-read-only (get-researcher-profile (researcher principal))
  (map-get? researcher-profiles { researcher: researcher })
)

(define-read-only (get-governance-proposal (proposal-id uint))
  (map-get? governance-proposals { proposal-id: proposal-id })
)

(define-read-only (get-research-bounty (bounty-id uint))
  (map-get? research-bounties { bounty-id: bounty-id })
)

(define-read-only (get-milestone-achievement (achievement-id uint))
  (map-get? milestone-achievements { achievement-id: achievement-id })
)

(define-read-only (get-token-balance (account principal))
  (ft-get-balance exoplanet-research-token account)
)

(define-read-only (get-total-supply)
  (ft-get-supply exoplanet-research-token)
)

(define-read-only (get-contribution-counter)
  (var-get contribution-counter)
)

;; Public functions
(define-public (submit-research-contribution
    (research-type (string-ascii 30))
    (data-reference (string-ascii 100))
    (quality-evidence (string-ascii 200)))
  (let
    (
      (new-contribution-id (+ (var-get contribution-counter) u1))
      (base-reward (calculate-base-reward research-type))
    )
    (begin
      (map-set research-contributions
        { contribution-id: new-contribution-id }
        {
          contributor: tx-sender,
          research-type: research-type,
          data-reference: data-reference,
          quality-score: u0,
          peer-reviews: u0,
          validation-status: "pending",
          submission-date: burn-block-height,
          reward-amount: base-reward,
          claimed: false,
          multiplier-applied: u100
        }
      )
      
      (update-researcher-profile tx-sender "contribution")
      (var-set contribution-counter new-contribution-id)
      (ok new-contribution-id)
    )
  )
)

(define-public (peer-review-contribution
    (contribution-id uint)
    (quality-score uint)
    (review-comments (string-ascii 300)))
  (let
    (
      (contribution (unwrap! (get-research-contribution contribution-id) ERR_RESEARCH_NOT_FOUND))
      (updated-reviews (+ (get peer-reviews contribution) u1))
      (updated-quality (/ (+ (* (get quality-score contribution) (get peer-reviews contribution)) quality-score) updated-reviews))
    )
    (begin
      (asserts! (not (is-eq (get contributor contribution) tx-sender)) ERR_UNAUTHORIZED)
      (asserts! (and (>= quality-score u1) (<= quality-score u10)) ERR_INVALID_DATA)
      (asserts! (is-eq (get validation-status contribution) "pending") ERR_NOT_VALIDATED)
      
      (map-set research-contributions
        { contribution-id: contribution-id }
        (merge contribution
          {
            peer-reviews: updated-reviews,
            quality-score: updated-quality,
            validation-status: (if (>= updated-reviews (var-get min-peer-reviews)) "validated" "pending")
          }
        )
      )
      
      ;; Reward reviewer
      (try! (ft-mint? exoplanet-research-token PEER_REVIEW_REWARD tx-sender))
      (update-researcher-profile tx-sender "peer-review")
      
      (ok true)
    )
  )
)

(define-public (claim-research-reward (contribution-id uint))
  (let
    (
      (contribution (unwrap! (get-research-contribution contribution-id) ERR_RESEARCH_NOT_FOUND))
      (quality-multiplier (determine-quality-multiplier (get quality-score contribution)))
      (final-reward (/ (* (get reward-amount contribution) quality-multiplier) u100))
      (biosignature-bonus (if (is-biosignature-research (get research-type contribution)) BIOSIGNATURE_BONUS u0))
      (total-reward (+ final-reward biosignature-bonus))
    )
    (begin
      (asserts! (is-eq (get contributor contribution) tx-sender) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get validation-status contribution) "validated") ERR_NOT_VALIDATED)
      (asserts! (not (get claimed contribution)) ERR_ALREADY_CLAIMED)
      
      (map-set research-contributions
        { contribution-id: contribution-id }
        (merge contribution
          {
            claimed: true,
            reward-amount: total-reward,
            multiplier-applied: quality-multiplier
          }
        )
      )
      
      (try! (ft-mint? exoplanet-research-token total-reward tx-sender))
      (update-researcher-profile tx-sender "reward")
      
      (ok total-reward)
    )
  )
)

(define-public (create-governance-proposal
    (title (string-ascii 100))
    (description (string-ascii 500))
    (proposal-type (string-ascii 30))
    (voting-duration uint))
  (let
    (
      (new-proposal-id (+ (var-get proposal-counter) u1))
      (voter-balance (ft-get-balance exoplanet-research-token tx-sender))
    )
    (begin
      (asserts! (>= voter-balance (var-get governance-threshold)) ERR_UNAUTHORIZED)
      (asserts! (and (> voting-duration u100) (< voting-duration u10000)) ERR_INVALID_DATA)
      
      (map-set governance-proposals
        { proposal-id: new-proposal-id }
        {
          proposer: tx-sender,
          title: title,
          description: description,
          proposal-type: proposal-type,
          voting-start: burn-block-height,
          voting-end: (+ burn-block-height voting-duration),
          yes-votes: u0,
          no-votes: u0,
          total-votes: u0,
          status: "active",
          execution-date: u0,
          minimum-participation: u1000 ;; Minimum voters required
        }
      )
      
      (var-set proposal-counter new-proposal-id)
      (ok new-proposal-id)
    )
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote bool))
  (let
    (
      (proposal (unwrap! (get-governance-proposal proposal-id) ERR_RESEARCH_NOT_FOUND))
      (voter-balance (ft-get-balance exoplanet-research-token tx-sender))
      (vote-weight (/ voter-balance u1000000)) ;; 1 vote per 1000 tokens
    )
    (begin
      (asserts! (>= voter-balance (var-get governance-threshold)) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get status proposal) "active") ERR_VOTING_CLOSED)
      (asserts! (<= burn-block-height (get voting-end proposal)) ERR_VOTING_CLOSED)
      
      (map-set governance-proposals
        { proposal-id: proposal-id }
        (merge proposal
          {
            yes-votes: (if vote (+ (get yes-votes proposal) vote-weight) (get yes-votes proposal)),
            no-votes: (if vote (get no-votes proposal) (+ (get no-votes proposal) vote-weight)),
            total-votes: (+ (get total-votes proposal) vote-weight)
          }
        )
      )
      
      ;; Reward governance participation
      (try! (ft-mint? exoplanet-research-token GOVERNANCE_REWARD tx-sender))
      (update-researcher-profile tx-sender "governance")
      
      (ok true)
    )
  )
)

(define-public (create-research-bounty
    (title (string-ascii 100))
    (description (string-ascii 500))
    (reward-amount uint)
    (duration uint)
    (requirements (string-ascii 300))
    (target-planet (string-ascii 50))
    (evaluation-criteria (string-ascii 200)))
  (let
    (
      (new-bounty-id (+ (var-get bounty-counter) u1))
      (creator-balance (ft-get-balance exoplanet-research-token tx-sender))
    )
    (begin
      (asserts! (>= creator-balance reward-amount) ERR_INSUFFICIENT_FUNDS)
      (asserts! (> reward-amount u0) ERR_INVALID_DATA)
      
      ;; Lock the reward amount
      (try! (ft-transfer? exoplanet-research-token reward-amount tx-sender (as-contract tx-sender)))
      
      (map-set research-bounties
        { bounty-id: new-bounty-id }
        {
          creator: tx-sender,
          title: title,
          description: description,
          reward-amount: reward-amount,
          deadline: (+ burn-block-height duration),
          requirements: requirements,
          target-planet: target-planet,
          status: "open",
          claimed-by: none,
          evaluation-criteria: evaluation-criteria
        }
      )
      
      (var-set bounty-counter new-bounty-id)
      (ok new-bounty-id)
    )
  )
)

(define-public (complete-bounty (bounty-id uint) (solution-reference (string-ascii 200)))
  (let
    (
      (bounty (unwrap! (get-research-bounty bounty-id) ERR_RESEARCH_NOT_FOUND))
    )
    (begin
      (asserts! (is-eq (get status bounty) "open") ERR_ALREADY_CLAIMED)
      (asserts! (<= burn-block-height (get deadline bounty)) ERR_INVALID_DATA)
      
      (map-set research-bounties
        { bounty-id: bounty-id }
        (merge bounty
          {
            status: "completed",
            claimed-by: (some tx-sender)
          }
        )
      )
      
      ;; Transfer reward to completer
      (try! (as-contract (ft-transfer? exoplanet-research-token (get reward-amount bounty) (as-contract tx-sender) tx-sender)))
      
      (update-researcher-profile tx-sender "bounty")
      (ok true)
    )
  )
)

(define-public (initialize-reward-pools)
  (let
    (
      (discovery-pool-amount (/ (var-get token-supply) u4)) ;; 25%
      (analysis-pool-amount (/ (var-get token-supply) u5)) ;; 20%
      (validation-pool-amount (/ (var-get token-supply) u10)) ;; 10%
    )
    (begin
      (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
      
      (map-set reward-pool { pool-type: "discovery" }
        { total-allocated: discovery-pool-amount, total-distributed: u0, remaining-balance: discovery-pool-amount, last-updated: burn-block-height, pool-manager: tx-sender })
      
      (map-set reward-pool { pool-type: "analysis" }
        { total-allocated: analysis-pool-amount, total-distributed: u0, remaining-balance: analysis-pool-amount, last-updated: burn-block-height, pool-manager: tx-sender })
      
      (map-set reward-pool { pool-type: "validation" }
        { total-allocated: validation-pool-amount, total-distributed: u0, remaining-balance: validation-pool-amount, last-updated: burn-block-height, pool-manager: tx-sender })
      
      (ok true)
    )
  )
)

;; Private helper functions
(define-private (calculate-base-reward (research-type (string-ascii 30)))
  (if (is-eq research-type "discovery") DISCOVERY_BASE_REWARD
      (if (is-eq research-type "analysis") ANALYSIS_BASE_REWARD
          (if (is-eq research-type "validation") VALIDATION_REWARD
              u50000))) ;; Default reward
)

(define-private (determine-quality-multiplier (quality-score uint))
  (if (>= quality-score u8) HIGH_QUALITY_MULTIPLIER
      (if (>= quality-score u6) MEDIUM_QUALITY_MULTIPLIER
          LOW_QUALITY_MULTIPLIER))
)

(define-private (is-biosignature-research (research-type (string-ascii 30)))
  (or (is-eq research-type "biosignature-analysis")
      (is-eq research-type "atmospheric-biosignature")
      (is-eq research-type "spectral-biosignature"))
)

(define-private (update-researcher-profile (researcher principal) (activity-type (string-ascii 20)))
  (let
    (
      (current-profile (default-to
        { total-contributions: u0, total-rewards-earned: u0, reputation-score: u100, specialization: "general", verified-discoveries: u0, peer-review-count: u0, governance-participation: u0, member-since: burn-block-height, status: "active" }
        (get-researcher-profile researcher)))
    )
    (map-set researcher-profiles
      { researcher: researcher }
      (merge current-profile
        {
          total-contributions: (if (is-eq activity-type "contribution") (+ (get total-contributions current-profile) u1) (get total-contributions current-profile)),
          peer-review-count: (if (is-eq activity-type "peer-review") (+ (get peer-review-count current-profile) u1) (get peer-review-count current-profile)),
          governance-participation: (if (is-eq activity-type "governance") (+ (get governance-participation current-profile) u1) (get governance-participation current-profile)),
          reputation-score: (calculate-reputation-score current-profile activity-type)
        }
      )
    )
  )
)

(define-private (calculate-reputation-score (profile {total-contributions: uint, total-rewards-earned: uint, reputation-score: uint, specialization: (string-ascii 50), verified-discoveries: uint, peer-review-count: uint, governance-participation: uint, member-since: uint, status: (string-ascii 20)}) (activity-type (string-ascii 20)))
  (let
    (
      (base-score (get reputation-score profile))
      (activity-bonus (if (is-eq activity-type "contribution") u5
                          (if (is-eq activity-type "peer-review") u3
                              (if (is-eq activity-type "governance") u2 u1))))
    )
    (+ base-score activity-bonus)
  )
)
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

