;; title: habitability-zone-assessment
;; Planetary system modeling for potentially habitable world identification
;; This contract calculates habitability zones, assesses planetary conditions, and evaluates life potential

;; Constants
(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_INVALID_DATA (err u301))
(define-constant ERR_SYSTEM_NOT_FOUND (err u302))
(define-constant ERR_PLANET_NOT_FOUND (err u303))
(define-constant ERR_ALREADY_EXISTS (err u304))
(define-constant ERR_CALCULATION_ERROR (err u305))
(define-constant CONTRACT_OWNER tx-sender)

;; Physical constants (scaled for integer arithmetic)
(define-constant SOLAR_LUMINOSITY u100) ;; Baseline stellar luminosity
(define-constant EARTH_ORBIT u100) ;; 1 AU baseline
(define-constant HABITABLE_ZONE_INNER u95) ;; Inner edge coefficient
(define-constant HABITABLE_ZONE_OUTER u170) ;; Outer edge coefficient
(define-constant TIDAL_LOCK_THRESHOLD u20) ;; Distance threshold for tidal locking
(define-constant MIN_ATMOSPHERE_PRESSURE u10) ;; Minimum pressure for liquid water
(define-constant MAX_GREENHOUSE_EFFECT u200) ;; Maximum sustainable greenhouse effect

;; Data Structures
(define-map stellar-systems
  { system-id: uint }
  {
    star-name: (string-ascii 50),
    stellar-type: (string-ascii 10),
    stellar-mass: uint, ;; Solar masses * 100
    stellar-luminosity: uint, ;; Solar luminosities * 100
    stellar-temperature: uint, ;; Kelvin
    metallicity: uint, ;; [Fe/H] * 100 + 100
    age: uint, ;; Billion years * 100
    distance-from-earth: uint, ;; Parsecs * 100
    discovered-by: principal,
    verified: bool
  }
)

(define-map exoplanets
  { planet-id: uint }
  {
    system-id: uint,
    planet-designation: (string-ascii 50),
    orbital-distance: uint, ;; AU * 1000
    orbital-period: uint, ;; Days * 100
    planet-mass: uint, ;; Earth masses * 100
    planet-radius: uint, ;; Earth radii * 100
    equilibrium-temperature: uint, ;; Kelvin
    atmospheric-pressure: uint, ;; Earth atmospheres * 100
    surface-gravity: uint, ;; Earth gravity * 100
    magnetic-field-strength: uint, ;; Earth magnetic field * 100
    discovery-method: (string-ascii 30),
    confirmed: bool
  }
)

(define-map habitability-assessments
  { assessment-id: uint }
  {
    planet-id: uint,
    assessor: principal,
    assessment-date: uint,
    in-habitable-zone: bool,
    tidal-locked: bool,
    has-atmosphere: bool,
    liquid-water-possible: bool,
    magnetic-protection: bool,
    stellar-stability: uint, ;; Score 0-100
    planetary-stability: uint, ;; Score 0-100
    habitability-score: uint, ;; Overall score 0-100
    life-potential: (string-ascii 20),
    confidence-level: uint
  }
)

(define-map climate-models
  { model-id: uint }
  {
    planet-id: uint,
    surface-temperature-min: uint, ;; Kelvin
    surface-temperature-max: uint, ;; Kelvin
    greenhouse-warming: uint, ;; Kelvin increase
    albedo-effect: uint, ;; Percentage
    seasonal-variation: uint, ;; Temperature swing in Kelvin
    weather-patterns: (string-ascii 30),
    ocean-coverage: uint, ;; Percentage
    ice-coverage: uint, ;; Percentage
    modeled-by: principal,
    validation-score: uint
  }
)

(define-map biosignature-targets
  { target-id: uint }
  {
    planet-id: uint,
    priority-level: uint, ;; 1-10 priority
    observational-difficulty: uint, ;; 1-10 scale
    expected-observation-time: uint, ;; Hours needed
    recommended-instruments: (string-ascii 100),
    spectral-lines-of-interest: (list 5 uint),
    estimated-biosignature-strength: uint,
    follow-up-required: bool,
    target-added-by: principal,
    research-status: (string-ascii 20)
  }
)

;; Data Variables
(define-data-var system-counter uint u0)
(define-data-var planet-counter uint u0)
(define-data-var assessment-counter uint u0)
(define-data-var model-counter uint u0)
(define-data-var target-counter uint u0)
(define-data-var min-habitability-score uint u50)
(define-data-var observation-priority-threshold uint u7)

;; Read-only functions
(define-read-only (get-stellar-system (system-id uint))
  (map-get? stellar-systems { system-id: system-id })
)

(define-read-only (get-exoplanet (planet-id uint))
  (map-get? exoplanets { planet-id: planet-id })
)

(define-read-only (get-habitability-assessment (assessment-id uint))
  (map-get? habitability-assessments { assessment-id: assessment-id })
)

(define-read-only (get-climate-model (model-id uint))
  (map-get? climate-models { model-id: model-id })
)

(define-read-only (get-biosignature-target (target-id uint))
  (map-get? biosignature-targets { target-id: target-id })
)

(define-read-only (get-system-counter)
  (var-get system-counter)
)

(define-read-only (get-planet-counter)
  (var-get planet-counter)
)

(define-read-only (calculate-habitable-zone (stellar-luminosity uint))
  (let
    (
      (inner-edge (* (/ stellar-luminosity SOLAR_LUMINOSITY) HABITABLE_ZONE_INNER))
      (outer-edge (* (/ stellar-luminosity SOLAR_LUMINOSITY) HABITABLE_ZONE_OUTER))
    )
    { inner: (/ inner-edge u100), outer: (/ outer-edge u100) }
  )
)

;; Public functions
(define-public (register-stellar-system
    (star-name (string-ascii 50))
    (stellar-type (string-ascii 10))
    (stellar-mass uint)
    (stellar-luminosity uint)
    (stellar-temperature uint)
    (metallicity uint)
    (age uint)
    (distance-from-earth uint))
  (let
    (
      (new-system-id (+ (var-get system-counter) u1))
    )
    (begin
      (asserts! (and (> stellar-mass u10) (< stellar-mass u1000)) ERR_INVALID_DATA)
      (asserts! (and (> stellar-temperature u1000) (< stellar-temperature u50000)) ERR_INVALID_DATA)
      
      (map-set stellar-systems
        { system-id: new-system-id }
        {
          star-name: star-name,
          stellar-type: stellar-type,
          stellar-mass: stellar-mass,
          stellar-luminosity: stellar-luminosity,
          stellar-temperature: stellar-temperature,
          metallicity: metallicity,
          age: age,
          distance-from-earth: distance-from-earth,
          discovered-by: tx-sender,
          verified: false
        }
      )
      
      (var-set system-counter new-system-id)
      (ok new-system-id)
    )
  )
)

(define-public (add-exoplanet
    (system-id uint)
    (planet-designation (string-ascii 50))
    (orbital-distance uint)
    (orbital-period uint)
    (planet-mass uint)
    (planet-radius uint)
    (equilibrium-temperature uint)
    (atmospheric-pressure uint)
    (discovery-method (string-ascii 30)))
  (let
    (
      (system (unwrap! (get-stellar-system system-id) ERR_SYSTEM_NOT_FOUND))
      (new-planet-id (+ (var-get planet-counter) u1))
      (surface-gravity (calculate-surface-gravity planet-mass planet-radius))
    )
    (begin
      (asserts! (> orbital-distance u0) ERR_INVALID_DATA)
      (asserts! (> orbital-period u0) ERR_INVALID_DATA)
      
      (map-set exoplanets
        { planet-id: new-planet-id }
        {
          system-id: system-id,
          planet-designation: planet-designation,
          orbital-distance: orbital-distance,
          orbital-period: orbital-period,
          planet-mass: planet-mass,
          planet-radius: planet-radius,
          equilibrium-temperature: equilibrium-temperature,
          atmospheric-pressure: atmospheric-pressure,
          surface-gravity: surface-gravity,
          magnetic-field-strength: (estimate-magnetic-field planet-mass),
          discovery-method: discovery-method,
          confirmed: false
        }
      )
      
      (var-set planet-counter new-planet-id)
      (ok new-planet-id)
    )
  )
)

(define-public (assess-habitability (planet-id uint))
  (let
    (
      (planet (unwrap! (get-exoplanet planet-id) ERR_PLANET_NOT_FOUND))
      (system (unwrap! (get-stellar-system (get system-id planet)) ERR_SYSTEM_NOT_FOUND))
      (new-assessment-id (+ (var-get assessment-counter) u1))
      (habitable-zone (calculate-habitable-zone (get stellar-luminosity system)))
      (in-hz (and (>= (get orbital-distance planet) (get inner habitable-zone))
                  (<= (get orbital-distance planet) (get outer habitable-zone))))
      (tidal-locked (< (get orbital-distance planet) TIDAL_LOCK_THRESHOLD))
      (has-atm (> (get atmospheric-pressure planet) MIN_ATMOSPHERE_PRESSURE))
      (liquid-water (and in-hz has-atm
                         (>= (get equilibrium-temperature planet) u273)
                         (<= (get equilibrium-temperature planet) u373)))
      (magnetic-protection (> (get magnetic-field-strength planet) u50))
      (stellar-stability (calculate-stellar-stability system))
      (planetary-stability (calculate-planetary-stability planet))
      (habitability-score (calculate-habitability-score 
                            in-hz tidal-locked has-atm liquid-water 
                            magnetic-protection stellar-stability planetary-stability))
      (life-potential (determine-life-potential habitability-score))
    )
    (begin
      (asserts! (get confirmed planet) ERR_INVALID_DATA)
      
      (map-set habitability-assessments
        { assessment-id: new-assessment-id }
        {
          planet-id: planet-id,
          assessor: tx-sender,
          assessment-date: burn-block-height,
          in-habitable-zone: in-hz,
          tidal-locked: tidal-locked,
          has-atmosphere: has-atm,
          liquid-water-possible: liquid-water,
          magnetic-protection: magnetic-protection,
          stellar-stability: stellar-stability,
          planetary-stability: planetary-stability,
          habitability-score: habitability-score,
          life-potential: life-potential,
          confidence-level: (calculate-confidence-level planet system)
        }
      )
      
      (var-set assessment-counter new-assessment-id)
      (ok new-assessment-id)
    )
  )
)

(define-public (create-climate-model
    (planet-id uint)
    (surface-temp-min uint)
    (surface-temp-max uint)
    (greenhouse-warming uint)
    (albedo-effect uint)
    (seasonal-variation uint)
    (weather-patterns (string-ascii 30))
    (ocean-coverage uint)
    (ice-coverage uint))
  (let
    (
      (planet (unwrap! (get-exoplanet planet-id) ERR_PLANET_NOT_FOUND))
      (new-model-id (+ (var-get model-counter) u1))
    )
    (begin
      (asserts! (< surface-temp-min surface-temp-max) ERR_INVALID_DATA)
      (asserts! (<= greenhouse-warming MAX_GREENHOUSE_EFFECT) ERR_INVALID_DATA)
      (asserts! (<= albedo-effect u100) ERR_INVALID_DATA)
      (asserts! (<= ocean-coverage u100) ERR_INVALID_DATA)
      (asserts! (<= ice-coverage u100) ERR_INVALID_DATA)
      
      (map-set climate-models
        { model-id: new-model-id }
        {
          planet-id: planet-id,
          surface-temperature-min: surface-temp-min,
          surface-temperature-max: surface-temp-max,
          greenhouse-warming: greenhouse-warming,
          albedo-effect: albedo-effect,
          seasonal-variation: seasonal-variation,
          weather-patterns: weather-patterns,
          ocean-coverage: ocean-coverage,
          ice-coverage: ice-coverage,
          modeled-by: tx-sender,
          validation-score: u0
        }
      )
      
      (var-set model-counter new-model-id)
      (ok new-model-id)
    )
  )
)

(define-public (add-biosignature-target
    (planet-id uint)
    (priority-level uint)
    (observational-difficulty uint)
    (expected-observation-time uint)
    (recommended-instruments (string-ascii 100))
    (spectral-lines (list 5 uint))
    (estimated-biosignature-strength uint))
  (let
    (
      (planet (unwrap! (get-exoplanet planet-id) ERR_PLANET_NOT_FOUND))
      (new-target-id (+ (var-get target-counter) u1))
    )
    (begin
      (asserts! (and (>= priority-level u1) (<= priority-level u10)) ERR_INVALID_DATA)
      (asserts! (and (>= observational-difficulty u1) (<= observational-difficulty u10)) ERR_INVALID_DATA)
      (asserts! (<= estimated-biosignature-strength u100) ERR_INVALID_DATA)
      
      (map-set biosignature-targets
        { target-id: new-target-id }
        {
          planet-id: planet-id,
          priority-level: priority-level,
          observational-difficulty: observational-difficulty,
          expected-observation-time: expected-observation-time,
          recommended-instruments: recommended-instruments,
          spectral-lines-of-interest: spectral-lines,
          estimated-biosignature-strength: estimated-biosignature-strength,
          follow-up-required: (>= priority-level (var-get observation-priority-threshold)),
          target-added-by: tx-sender,
          research-status: "proposed"
        }
      )
      
      (var-set target-counter new-target-id)
      (ok new-target-id)
    )
  )
)

(define-public (verify-stellar-system (system-id uint))
  (let
    (
      (system (unwrap! (get-stellar-system system-id) ERR_SYSTEM_NOT_FOUND))
    )
    (begin
      (asserts! (not (is-eq (get discovered-by system) tx-sender)) ERR_UNAUTHORIZED)
      (map-set stellar-systems
        { system-id: system-id }
        (merge system { verified: true })
      )
      (ok true)
    )
  )
)

(define-public (confirm-exoplanet (planet-id uint))
  (let
    (
      (planet (unwrap! (get-exoplanet planet-id) ERR_PLANET_NOT_FOUND))
      (system (unwrap! (get-stellar-system (get system-id planet)) ERR_SYSTEM_NOT_FOUND))
    )
    (begin
      (asserts! (get verified system) ERR_INVALID_DATA)
      (map-set exoplanets
        { planet-id: planet-id }
        (merge planet { confirmed: true })
      )
      (ok true)
    )
  )
)

(define-public (update-research-status (target-id uint) (new-status (string-ascii 20)))
  (let
    (
      (target (unwrap! (get-biosignature-target target-id) ERR_PLANET_NOT_FOUND))
    )
    (begin
      (asserts! (is-eq (get target-added-by target) tx-sender) ERR_UNAUTHORIZED)
      (map-set biosignature-targets
        { target-id: target-id }
        (merge target { research-status: new-status })
      )
      (ok true)
    )
  )
)

;; Private helper functions
(define-private (calculate-surface-gravity (planet-mass uint) (planet-radius uint))
  (/ (* planet-mass u100) (* planet-radius planet-radius))
)

(define-private (estimate-magnetic-field (planet-mass uint))
  (if (>= planet-mass u50) ;; 0.5 Earth masses
      (/ (* planet-mass u150) u100)
      u25)
)

(define-private (calculate-stellar-stability (system {star-name: (string-ascii 50), stellar-type: (string-ascii 10), stellar-mass: uint, stellar-luminosity: uint, stellar-temperature: uint, metallicity: uint, age: uint, distance-from-earth: uint, discovered-by: principal, verified: bool}))
  (let
    (
      (mass-score (if (and (>= (get stellar-mass system) u80) (<= (get stellar-mass system) u120)) u100 u60))
      (age-score (if (<= (get age system) u1000) u100 u80))
      (type-score (if (is-eq (get stellar-type system) "G") u100 u70))
    )
    (/ (+ mass-score age-score type-score) u3)
  )
)

(define-private (calculate-planetary-stability (planet {system-id: uint, planet-designation: (string-ascii 50), orbital-distance: uint, orbital-period: uint, planet-mass: uint, planet-radius: uint, equilibrium-temperature: uint, atmospheric-pressure: uint, surface-gravity: uint, magnetic-field-strength: uint, discovery-method: (string-ascii 30), confirmed: bool}))
  (let
    (
      (mass-score (if (and (>= (get planet-mass planet) u50) (<= (get planet-mass planet) u200)) u100 u70))
      (gravity-score (if (and (>= (get surface-gravity planet) u80) (<= (get surface-gravity planet) u150)) u100 u60))
      (atmosphere-score (if (> (get atmospheric-pressure planet) u10) u100 u40))
    )
    (/ (+ mass-score gravity-score atmosphere-score) u3)
  )
)

(define-private (calculate-habitability-score 
    (in-hz bool) (tidal-locked bool) (has-atm bool) 
    (liquid-water bool) (magnetic-protection bool) 
    (stellar-stability uint) (planetary-stability uint))
  (let
    (
      (hz-score (if in-hz u30 u0))
      (tidal-penalty (if tidal-locked u10 u0))
      (atmosphere-score (if has-atm u20 u0))
      (water-score (if liquid-water u25 u0))
      (magnetic-score (if magnetic-protection u15 u0))
      (stability-score (/ (+ stellar-stability planetary-stability) u20))
    )
    (- (+ hz-score atmosphere-score water-score magnetic-score stability-score) tidal-penalty)
  )
)

(define-private (determine-life-potential (score uint))
  (if (>= score u80) "high"
      (if (>= score u60) "moderate"
          (if (>= score u40) "low"
              "unlikely")))
)

(define-private (calculate-confidence-level (planet {system-id: uint, planet-designation: (string-ascii 50), orbital-distance: uint, orbital-period: uint, planet-mass: uint, planet-radius: uint, equilibrium-temperature: uint, atmospheric-pressure: uint, surface-gravity: uint, magnetic-field-strength: uint, discovery-method: (string-ascii 30), confirmed: bool}) (system {star-name: (string-ascii 50), stellar-type: (string-ascii 10), stellar-mass: uint, stellar-luminosity: uint, stellar-temperature: uint, metallicity: uint, age: uint, distance-from-earth: uint, discovered-by: principal, verified: bool}))
  (let
    (
      (distance-factor (if (<= (get distance-from-earth system) u10000) u100 u70))
      (method-factor (if (is-eq (get discovery-method planet) "transit") u100 u80))
      (data-quality (if (> (get atmospheric-pressure planet) u0) u100 u60))
    )
    (/ (+ distance-factor method-factor data-quality) u3)
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

