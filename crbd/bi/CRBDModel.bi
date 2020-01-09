class CRBDModel < PhyModel {
  delayed:Boolean;

  k_λ:Real;
  θ_λ:Real;
  k_μ:Real;
  θ_μ:Real;

  observation:PlainPhyTree;

  λ:Random<Real>;
  μ:Random<Real>;

  fiber run() -> Real {
    λ ~ Gamma(k_λ, θ_λ);
    μ ~ Gamma(k_μ, θ_μ);

    if !delayed {
      Real(λ);
      Real(μ);
    }

    node:PhyNode! <- observation.depthFirstTraversal();
    while node? {
      pauseYields();

      if !node!.isRoot() || !node!.noStalk() {
        count_hs:Random<Integer>;
        count_hs ~ Poisson(λ * node!.branch_length);
        for i:Integer in 1..Integer(count_hs) {
          t:Random<Real>;
          t ~ Uniform(node!.t_end, node!.t_beg);
          if branchSurvives(t) {
            resumeYields();
            yield impossible;
            return;
          }
          yield log(2);
        }

        if node!.hasChildren() {
          0.0 ~> Exponential(λ);
        }

        0 ~> Poisson(μ * node!.branch_length);
      }

      resumeYields();
      yield accumulated;
    }
  }

  function branchSurvives(t0:Real) -> Boolean {
    t_d:Real? <- timeOfDeath(t0);
    if !t_d? {
      return true;
    }
    Δ_d:Real <- t0 - t_d!;
    count_b:Random<Integer>;
    count_b ~ Poisson(λ * Δ_d);
    for i:Integer in 1..Integer(count_b) {
      u:Random<Real>;
      u ~ Uniform(t_d!, t0);
      if branchSurvives(u) {
        return true;
      }
    }
    return false;
  }

  function timeOfDeath(t0:Real) -> Real? {
    Δ:Random<Real>;
    Δ ~ Exponential(μ);
    t:Real <- t0 - Δ;
    if t < 0 {
        return nil;
    }
    return t;
  }

  function input(reader:Reader) {
    observation.load(reader);
  }
}
