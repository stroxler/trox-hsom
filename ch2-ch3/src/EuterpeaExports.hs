module EuterpeaExports (
  Music (Prim, (:+:), (:=:), Modify),
  PitchClass (Cff, Cf, C, Cs, Css,
              Dff, Df, D, Ds, Dss,
              Eff, Ef, E, Es, Ess,
              Fff, Ff, F, Fs, Fss,
              Gff, Gf, G, Gs, Gss,
              Aff, Af, A, As, Ass,
              Bff, Bf, B, Bs, Bss),
  Primitive (Note, Rest),
  AbsPitch, Pitch, Octave, Volume, Dur,
  wn, hn, dhn, qn, dqn, ddqn, en, den, sn,
  wnr, hnr, dhnr, qnr, dqnr, ddqnr, enr, denr, snr,
  )
where

import Euterpea (
  Music (Prim, (:+:), (:=:), Modify),
  PitchClass (Cff, Cf, C, Cs, Css,
              Dff, Df, D, Ds, Dss,
              Eff, Ef, E, Es, Ess,
              Fff, Ff, F, Fs, Fss,
              Gff, Gf, G, Gs, Gss,
              Aff, Af, A, As, Ass,
              Bff, Bf, B, Bs, Bss),
  Primitive (Note, Rest),
  AbsPitch, Pitch, Octave, Volume, Dur,
  wn, hn, dhn, qn, dqn, ddqn, en, den, sn,
  wnr, hnr, dhnr, qnr, dqnr, ddqnr, enr, denr, snr,
  )
