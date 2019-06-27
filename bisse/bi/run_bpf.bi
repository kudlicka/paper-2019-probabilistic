hpp {{
#include <iostream>
}}

program run_bpf(N:Integer <- 1000, M:Integer <- 1, delayed:Boolean <- false) {
  inputReader:JSONReader <- JSONReader("./input/cetaceans_w_size.phyjson");
  inputReader.load();
  T:Integer <- 173;
  sampler:BPF;
  sampler.sample(inputReader, M, T, N, delayed, 1.0, 1.0, 1.0, 1.0, 1.0, 10.0/820.277262);
}

class BPF < ParticleFilter
{
  function sample(inputReader:Reader?, M:Integer, T:Integer, N:Integer,
      delayed:Boolean, k_λ:Real, θ_λ:Real,
      k_μ:Real, θ_μ:Real, k_σ:Real, θ_σ:Real) {

    verbose:Boolean <- true;

    initialize("BiSSE", inputReader, T, N, 1.0);

    f:BiSSE? <- BiSSE?(f0!);
    f!.delayed <- delayed;
    f!.k_λ <- k_λ;
    f!.θ_λ <- θ_λ;
    f!.k_μ <- k_μ;
    f!.θ_μ <- θ_μ;
    f!.k_σ <- k_σ;
    f!.θ_σ <- θ_σ;

    for (m:Integer in 1..M) {
      tic();
      start();
      for (t:Integer in 1..T) {
        if (T > 1 && verbose) {
          stderr.print(t + " ");
        }
        step(t);
      }
      if verbose {
        stderr.print(Z[T] + "\n");
      }
      stdout.print(N + " " + Z[T] + " " + toc() + "\n");
      cpp {{
      std::cout.flush();
      }}
    }
  }
}
