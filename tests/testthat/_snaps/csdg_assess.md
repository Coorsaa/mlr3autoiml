# print, format, and as.data.table methods

    Code
      print(imp)
    Output
      <CSDGImportance> held-out PFI, increase in squared error; 4 folds x 3 permutations
      XGBoost: held-out squared error 0.600; model without predictors 1.00
        improvement 0.400; minimum importance (1% of the improvement) 0.0040
        predictor   mean PFI   corrected 95% interval   folds in top 2
        a              0.300           [0.300, 0.300]              4/4
        b              0.200           [0.160, 0.240]              4/4
        c             0.0500         [0.0500, 0.0500]              0/4
        d             0.0100        [-0.0098, 0.0298]              0/4
      ridge logistic regression: held-out squared error 0.600; model without predictors 1.00
        improvement 0.400; minimum importance (1% of the improvement) 0.0040
        predictor   mean PFI   corrected 95% interval   folds in top 2
        a              0.300           [0.300, 0.300]              4/4
        c              0.200           [0.160, 0.240]              4/4
        b             0.0500         [0.0500, 0.0500]              0/4
        d             0.0100        [-0.0098, 0.0298]              0/4
      Folds in top 2: folds in which the predictor has one of the 2 largest PFI
      values (print(x, k = )).
      Intervals are reported descriptively. The minimum importance, the default
      criterion of csdg_check(), is given in squared error units, because PFI values
      are not parts of the improvement.

---

    Code
      print(claim)
    Output
      <CSDG claim> relies_mainly_a_b_m1
        XGBoost relies mainly on a and b.
        Origin: exploratory. Formulated after inspecting the held-out PFI of XGBoost;
        a and b are its two leading predictors on average.

---

    Code
      print(chk)
    Output
      <CSDG check> XGBoost relies mainly on a and b.
      Criteria (defaults, see ?csdg_check): minimum importance 1% of the improvement;
      at least 4 of 4 folds; Monte Carlo rule; corrected 95% interval must clear the
      smallest relevant difference.
      gate  check               status        observation
      G0a   specification       supported     Generated from the analysis: quantity,
                                              model, procedure, data, meaning (model
                                              description), and use.
      G0b   measurement         open          Measurement and data not yet judged;
                                              use csdg_judge("G0b", ...).
      G1    performance         context       Held-out squared error 0.600 versus
                                              1.00 for the model without predictors
                                              (improvement 0.400, a reduction by
                                              40%); 4 of 4 folds improve.
      G2    content             supported     a 0.300 and b 0.200 versus 0.0500 for
                                              c, the largest other predictor (6.00
                                              and 4.00 times); margin (PFI minus 2 x
                                              PFI of c) 0.200 [0.200, 0.200] and
                                              0.100 [0.0603, 0.140]; weakest of the 4
                                              comparisons with other predictors: b
                                              minus 2 x c, 0.100 [0.0603, 0.140].
      G2    minimum importance  supported     a 0.300 and b 0.200 versus the minimum
                                              importance 0.0040 (1% of the
                                              improvement of 0.400, from 1.00 to
                                              0.600).
      G2    procedure           supported     Held-out marginal PFI with squared
                                              error, 3 permutations per fold in 4
                                              folds, as the scope names. No grouped
                                              or conditional comparison was
                                              requested.
      G5    stability           supported     Selected anew in each fold: 4 of 4
                                              folds reproduce the result (cutoff 1: 0
                                              of 4; cutoff 3: 4 of 4).
      G6a   other learner       context       ridge logistic regression: the result
                                              does not hold (a 0.300 and b 0.0500
                                              versus 0.200 for c).

---

    Code
      print(res)
    Output
      <CSDG assessment> XGBoost relies mainly on a and b.
        Unresolved (exploratory)
        Specification (G0a) supported | Measurement and data (G0b) open | Content
        (G2) supported | Minimum importance (G2) supported | Procedure (G2) supported
        | Stability (G5) supported
        Measurement and data (G0b) is open: measurement and data not yet judged; use
        csdg_judge("G0b", ...). The computed properties are supported; pass a
        judgment of G0b with `judgments =` to complete the assessment.
        Decision options: revise or withhold

---

    Code
      print(csdg_assess(csdg_check(claim, imp, judgments = g0b_judged())))
    Output
      <CSDG assessment> XGBoost relies mainly on a and b.
        Met (exploratory)
        Specification (G0a) supported | Measurement and data (G0b) supported |
        Content (G2) supported | Minimum importance (G2) supported | Procedure (G2)
        supported | Stability (G5) supported
        On average, a and b have 6.00 and 4.00 times the PFI of the largest other
        predictor, c; the result recurs in 4 of 4 folds. The claim was formulated
        after the explanation was inspected; it is established only after a test on
        new data (csdg_confirm()).
        Decision options: retain

