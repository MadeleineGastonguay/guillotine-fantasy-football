# --- Bayesian live-projection model (comparison model) ---------------------------------
# A second way of doing what R/pipeline.R's add_live_projection_columns() does -- blending
# a player's pregame projection with their in-game pace, and shrinking their remaining-score
# uncertainty as the game plays out -- derived from one consistent statistical model instead
# of a hand-picked blend curve (mean) patched together with an unrelated decay curve (SD).
# Surfaced as its own "Standings (Bayesian)" tab so it can be compared side by side with the
# existing model, not as a replacement for it.
#
# THE MODEL
# Treat the player's true full-game rate R as unknown, with a prior matching the pregame
# numbers: R ~ Normal(P, CV^2 * P^2), where P = full_week_projection and CV = UNDERDOG_CV for
# their position. As before, that pregame variance is really two things mixed together: not
# knowing R itself, and real randomness in how points land even at a known R. But instead of
# splitting it by a hand-picked fraction (the original version's BAYESIAN_PHI = 0.5), the split
# is now DERIVED: treat points as a compound Poisson process (events -- plays crediting the
# player with some stat -- arriving at some rate, each worth a random number of points X).
# Standard compound-Poisson variance gives Var(points by time t | true rate R) = R*t*k, where
# k = E[X^2]/E[X] is a per-position constant fitted straight from real replay data
# (COMPOUND_POISSON_K in R/config.R; see analysis/fit_compound_poisson_k.R) -- notably LINEAR
# in R, not quadratic like CV^2*R^2. At t=1 (a full game) that's the "known rate" compounding
# variance the pregame number must already contain, so the REMAINING share -- "we don't yet
# know R" -- falls out by subtraction instead of being guessed:
#   tau0^2(P) = CV^2*P^2 - k*P        (rate-uncertainty share of the pregame variance)
# floored just above zero for very low-P players where the fitted k alone would already exceed
# the full pregame variance (their week-to-week swings look like they're almost entirely
# game-level luck, not rate mis-estimation -- plausible for deep bench/situational players).
#
# Observing L = locked_points by elapsed_fraction t is a noisy read on R, with sampling
# variance k*P/t (plugging in P for the unknown R, same plug-in-the-current-estimate move
# ordinary variance-function GLMs make). That makes updating R from the prior a standard
# Normal-Normal (precision-weighted) update, which has a closed form:
#   posterior weight on the observed rate:  w(t) = (t*tau0^2) / (k*P + t*tau0^2)
#   posterior mean rate (the blended rate): B(t) = (1-w)*P + w*(L/t)
#   posterior variance of R:                tau0^2 * k*P / (k*P + t*tau0^2)
# Then, by the law of total variance, the predictive variance of the player's FINAL score
# combines "compounding randomness in the remaining time, at the current best rate estimate"
# (now the same compound-Poisson form, R*t*k, applied to the remaining time and rate) with "we
# still don't know the true rate, and that uncertainty propagates through the time left":
#   Var(final | data) = k*B(t)*(1-t)  +  (1-t)^2 * tau0^2*k*P / (k*P + t*tau0^2)
#
# This is self-consistent at both ends: at t=0, w=0 (B=P) and Var = k*P + tau0^2 =
# k*P + (CV^2*P^2 - k*P) = CV^2*P^2 exactly (matches the plain pregame number, no
# double-counting); at t=1, both terms carry a (1-t) or (1-t)^2 factor and vanish regardless
# of k or tau0^2, so the final score collapses to locked_points exactly.
#
# WHY THIS REPLACED THE EARLIER (1-phi)/phi VERSION
# The original version used sampling variance (1-phi)*CV^2*P^2/t -- quadratic in P, and phi a
# hand-picked constant (0.5), not fit to anything. analysis/plot_accuracy_summary.R's
# plot_model_accuracy_comparison() showed that version under-covering and more biased than the
# ORIGINAL (non-Bayesian) model for most of the game: with phi=0.5, w(t) is close to LINEAR in
# t for small t, which is exactly the overreaction-to-noise problem R/pipeline.R's own
# elapsed_fraction^3 blend was deliberately cubed to avoid (see its comment) -- the Bayesian
# filter was trusting a tiny, noisy in-game sample too much, too soon. Switching the sampling
# variance to the compound-Poisson form (k*P/t, fit from real event data rather than guessed)
# fixes the functional form directly rather than re-tuning a free constant: k*P grows more
# slowly than CV^2*P^2 for the P ranges most starters/backups fall in (see
# analysis/fit_compound_poisson_k.R's write-up), so w(t) rises more cautiously for small t,
# closer to how R/pipeline.R's own cubic damping behaves.
#
# CAVEATS
# COMPOUND_POISSON_K (R/config.R) is fit from only 3 weeks of one season (2026 wk 1-3) -- real,
# but a small sample, and it folds in real within-game rate inhomogeneity (game script, garbage
# time) along with genuine event-count randomness, which is a deliberate choice (see
# R/config.R's comment) but means it isn't a "pure" Poisson-process constant. Worth refitting as
# more replay weeks accumulate (analysis/fit_compound_poisson_k.R), and revisiting whether it
# should vary by more than position (e.g. workload level) once there's enough data to tell.

add_bayesian_projection_columns <- function(df, k_lookup = COMPOUND_POISSON_K) {
  df |>
    dplyr::mutate(
      bayes_cv = dplyr::coalesce(UNDERDOG_CV[position], DEFAULT_UNDERDOG_CV),
      bayes_k = dplyr::coalesce(k_lookup[position], DEFAULT_COMPOUND_POISSON_K),
      # Rate-uncertainty share of the pregame variance, after subtracting out the
      # compound-Poisson "known rate" compounding variance -- floored just above zero (rather
      # than at zero) so the posterior-weight formula below never divides by exactly zero.
      bayes_tau0_sq = pmax(bayes_cv^2 * full_week_projection^2 - bayes_k * full_week_projection, 1e-6),
      bayes_weight = dplyr::if_else(
        has_live_signal,
        (elapsed_fraction * bayes_tau0_sq) / pmax(bayes_k * full_week_projection + elapsed_fraction * bayes_tau0_sq, 1e-9),
        0
      ),
      bayes_blended_rate = dplyr::if_else(
        has_live_signal,
        (1 - bayes_weight) * full_week_projection + bayes_weight * observed_rate,
        full_week_projection
      ),
      bayes_remaining_mean = bayes_blended_rate * remaining_fraction,
      bayes_posterior_rate_var = bayes_tau0_sq * bayes_k * full_week_projection /
        pmax(bayes_k * full_week_projection + elapsed_fraction * bayes_tau0_sq, 1e-9),
      bayes_compounding_var = bayes_k * pmax(bayes_blended_rate, 0) * remaining_fraction,
      remaining_sd_bayesian = sqrt(pmax(
        bayes_compounding_var + remaining_fraction^2 * bayes_posterior_rate_var, 0
      )),
      # Pre-game (t=0) SD for this model -- identical in value to pregame_sd_underdog (both
      # reduce to CV*P when nothing has been observed yet), kept under its own name so the
      # Bayesian tab doesn't depend on the other model's column existing.
      bayes_pregame_sd = bayes_cv * full_week_projection
    )
}
