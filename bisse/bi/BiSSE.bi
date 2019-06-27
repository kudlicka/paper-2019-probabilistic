class BiSSE < PhyModel {
  delayed:Boolean;

  k_λ:Real;
  θ_λ:Real;
  k_μ:Real;
  θ_μ:Real;
  k_σ:Real;
  θ_σ:Real;

  observation:BiSSETree;

  λ:Random<Real>[2];
  μ:Random<Real>[2];
  σ:Random<Real>;

  fiber run() -> Real {
    λ[1] ~ Gamma(k_λ, θ_λ);
    μ[1] ~ Gamma(k_μ, θ_μ);
    λ[2] ~ Gamma(k_λ, θ_λ);
    μ[2] ~ Gamma(k_μ, θ_μ);
    σ ~ Gamma(k_σ, θ_σ);

    if !delayed {
      Real(λ[1]);
      Real(μ[1]);
      Real(λ[2]);
      Real(μ[2]);
      Real(σ);
    }

    node:BiSSENode! <- observation.depthFirstTraversal();
    while node? {
      pauseYields();

      state:Integer;
      if node!.isRoot() {
        state_rv:Random<Integer>;
        state_rv ~ Categorical([0.5, 0.5]);
        state <- state_rv - 1;
      } else {
        state <- node!.getParent()!.state_end!;
      }

      if node!.isRoot() && node!.noStalk() {
        if node!.hasChildren() {
          yield log(2);
        }
      } else {
        count_sc:Random<Integer>;  // #state changes
        count_sc ~ Poisson(σ * node!.branch_length);

        if node!.state_end? && (
            (state == node!.state_end! && mod(count_sc, 2) != 0) ||
            (state != node!.state_end! && mod(count_sc, 2) != 1)) {
          // State at the end of the branch is not equal to the observed state
          resumeYields();
          yield impossible;
          return;
        }

        Δ:Real <- node!.branch_length;
        t_beg:Real <- node!.t_beg;
        t_end:Real <- node!.t_beg;
        for j:Integer in 1..Integer(count_sc)+1 {
          if j <= count_sc {
            u:Random<Real>;
            u ~ Uniform(0.0, 1.0);
            Δ <- Δ * pow(u, 1.0/(1 + count_sc - j));
          } else {
            Δ <- 0;
          }
          t_end <- node!.t_end + Δ;

          // No extinction event along the branch:
          0 ~> Poisson(μ[1+state] * (t_beg - t_end));

          count_hs:Random<Integer>;  // #simulated hidden speciation events
          count_hs ~ Poisson(λ[1+state] * (t_beg - t_end));

          for i:Integer in 1..Integer(count_hs) {
            t:Random<Real>;
            t ~ Uniform(t_end, t_beg);

            if sidebranch_survives(t, state) {
              resumeYields();
              yield impossible;
              return;
            }

            yield log(2);
          }

          if j <= count_sc {
            state <- 1 - state;
          }
          t_beg <- t_end;
        }

        if node!.hasChildren() {
          0.0 ~> Exponential(λ[1+state]);
          yield log(2);
        }
      }

      node!.state_end <- state;

      resumeYields();
      yield accumulated;
    }
  }

  function sidebranch_survives(t0:Real, state0:Integer) -> Boolean {
    state:Integer <- state0;
    count_sc:Random<Integer>;  // #state changes
    count_sc ~ Poisson(σ * t0);
    t_beg:Real <- t0;
    t_end:Real <- t0;  // [t_end, t_beg] - currently processed segment of the branch (segmented by state change events)
    t_d:Real?;  // Time of death
    for j:Integer in 1..Integer(count_sc)+1 {
      if j == count_sc+1 {
        t_end <- 0;
      } else {
        u:Random<Real>;
        u ~ Uniform(0.0, 1.0);
        t_end <- t_end * pow(u, 1.0/(1 + count_sc - j));
      }

      // Does the death occur in the current segment?
      t_d <- timeOfDeath(state, t_beg, t_end);

      if j == count_sc+1 && !t_d? {
        return true;
      }

      if t_d? {
        t_end <- t_d!;
      }

      Δ_b:Real <- t_beg - t_end;
      count_b:Random<Integer>;  // #births
      count_b ~ Poisson(λ[1+state] * Δ_b);
      for i:Integer in 1..Integer(count_b) {
        t:Random<Real>;
        t ~ Uniform(t_end, t_beg);
        if sidebranch_survives(t, state) {
          return true;
        }
      }

      if t_d? {
        return false;
      }

      state <- 1 - state;
      t_beg <- t_end;
    }

    assert false;
  }

  function timeOfDeath(state:Integer, t_beg:Real, t_end:Real) -> Real? {
    Δ:Random<Real>;
    Δ ~ Exponential(μ[1+state]);
    t:Real <- t_beg - Δ;
    if t < t_end {
        return nil;
    }
    return t;
  }

  function input(reader:Reader) {
    observation.load(reader);
  }
}
