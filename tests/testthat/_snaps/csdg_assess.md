# print, format, and as.data.table methods

    Code
      print(imp)
    Output
      <CSDGImportance> held-out PFI, increase in squared error; 4 folds x 3 permutations
      XGBoost: held-out squared error 0.600 versus 1.00 for the model without predictors (improvement 0.400)
        predictor   mean PFI   relative to improvement   folds in top 2
        a              0.300                       75%              4/4
        b              0.200                       50%              4/4
        c             0.0500                       12%              0/4
        d             0.0100                      2.5%              0/4
      ridge logistic regression: held-out squared error 0.600 versus 1.00 for the model without predictors (improvement 0.400)
        predictor   mean PFI   relative to improvement   folds in top 2
        a              0.300                       75%              4/4
        c              0.200                       50%              4/4
        b             0.0500                       12%              0/4
        d             0.0100                      2.5%              0/4
      Folds in top 2: folds in which the predictor has one of the 2 largest PFI values (print(imp, k = )).
      PFI values are not parts of the improvement, so PFI relative to the improvement can exceed 100%.

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
      G0a/b scope               supported     The six scope elements are generated
                                              from the analysis; the claim names
                                              analyzed variables, not constructs.
      G1    performance         context       Held-out squared error 0.600 versus
                                              1.00 for the model without predictors
                                              (improvement 0.400, 40%); 4 of 4 folds
                                              improve.
      G2    content             supported     a 0.300 and b 0.200 versus 0.0500 for
                                              c, the largest other predictor (6.00
                                              and 4.00 times); margin (PFI minus 2 x
                                              PFI of c) 0.200 [0.200, 0.200] and
                                              0.100 [0.0603, 0.140].
      G2    minimum importance  supported     a 0.300 (75%) and b 0.200 (50%) of the
                                              improvement of 0.400 (1.00 to 0.600).
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
        Met (exploratory)
        Specification (G0a) supported | Measurement (G0b) supported | Content (G2)
        supported | Minimum importance (G2) supported | Procedure (G2) supported |
        Stability (G5) supported
        On average, a and b have 6.00 and 4.00 times the PFI of the largest other
        predictor, c; the result recurs in 4 of 4 folds. The claim was formulated
        after the explanation was inspected; it is established only after a test on
        new data (csdg_confirm()).
        Decision options: retain

