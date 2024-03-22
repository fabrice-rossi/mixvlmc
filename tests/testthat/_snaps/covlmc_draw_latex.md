# draw obeys its contract (with vgam) LaTeX

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\textbf{A} (2.639e-10)]
    [\textbf{B} (3.56e-11)]
    [\textbf{C}
    [\textbf{A} (0.002189)]
    [ \textbf{B, C} (2.594e-06) ]
    ]
    ]
    [\textbf{B}
    [\textbf{A}\\\hline {\small (collapsing: 0.001604)}, align={c}
    [\textbf{A} (0.000941)]
    [\textbf{B} (0.0003583)]
    [\textbf{C} (0.0001354)]
    ]
    [\textbf{B}
    [\textbf{C} (0.000957)]
    [ \textbf{A, B} (6.639e-05) ]
    ]
    [\textbf{C}\\\hline {\small (collapsing: 0.007425)}, align={c}
    [\textbf{A} (8.321e-06)]
    [\textbf{B} (0.0001755)]
    [\textbf{C} (3.038e-06)]
    ]
    ]
    [\textbf{C}
    [\textbf{A} (1.463e-10)]
    [\textbf{B}\\\hline {\small merging B and C: 0.001527}, align={c}
    [\textbf{A} (0.001367)]
    [\textbf{B} (0.007996)]
    [\textbf{C} (0.0001326)]
    ]
    [\textbf{C}
    [\textbf{C} (0.005984)]
    [ \textbf{A, B} (4.118e-10) ]
    ]
    ]
    ]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -1.393}&{\small -1.296}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.966}&{\small 0.7838}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2.428}&{\small -1.588}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.935}&{\small 1.07}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -1.083}&{\small -1.469}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.779}&{\small 0.3142}\\
    , align=ccc]
    ]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -3.238}&{\small -3.142}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.005}&{\small 0.5409}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2.033}&{\small -3.865}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.004}&{\small 0.9753}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -4.997}&{\small -22.39}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 4.323}&{\small 4.75}\\
    , align=ccc]
    ]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -3.164}&{\small -2.597}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.954}&{\small 0.9274}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2.166}&{\small -2.165}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.553}&{\small 1.206}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -3.412}&{\small -4.026}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 0.8723}&{\small 0.8833}\\
    , align=ccc]
    ]
    ]
    \end{forest}

---

    \begin{forest}
      for tree={grow'=west}[\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -1.393}&{\small -1.296}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.966}&{\small 0.7838}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2.428}&{\small -1.588}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.935}&{\small 1.07}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -1.083}&{\small -1.469}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.779}&{\small 0.3142}\\
    , align=ccc]
    ]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -3.238}&{\small -3.142}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.005}&{\small 0.5409}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2.033}&{\small -3.865}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.004}&{\small 0.9753}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -4.997}&{\small -22.39}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 4.323}&{\small 4.75}\\
    , align=ccc]
    ]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -3.164}&{\small -2.597}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.954}&{\small 0.9274}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2.166}&{\small -2.165}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.553}&{\small 1.206}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -3.412}&{\small -4.026}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 0.8723}&{\small 0.8833}\\
    , align=ccc]
    ]
    ]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{5}{c}{\textbf{A}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -1.393}&{\small 1.966}&{\small -0.2701}&{\small 1.957}&{\small 1.173}\\
    {\small -1.296}&{\small 0.7838}&{\small 1.216}&{\small -0.942}&{\small 2.738}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{B}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -2.428}&{\small 2.935}&{\small 0.2797}&{\small 2.627}&{\small -15}\\
    {\small -1.588}&{\small 1.07}&{\small 1.347}&{\small 0.8597}&{\small 3.026}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{C}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -1.083}&{\small 1.779}&{\small 0.5446}&{\small 1.275}&{\small -0.5394}\\
    {\small -1.469}&{\small 0.3142}&{\small 2.158}&{\small -0.02489}&{\small 2.407}, align=ccccc]
    ]
    [\textbf{B}
    [\multicolumn{5}{c}{\textbf{A}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -3.238}&{\small 2.005}&{\small 0.938}&{\small 2.525}&{\small 2.54}\\
    {\small -3.142}&{\small 0.5409}&{\small 2.655}&{\small 1.66}&{\small 3.751}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{B}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -2.033}&{\small 2.004}&{\small 0.1771}&{\small 1.809}&{\small 0.9951}\\
    {\small -3.865}&{\small 0.9753}&{\small 2.193}&{\small 2.283}&{\small 4.315}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{C}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -4.997}&{\small 4.323}&{\small 3.73}&{\small 2.829}&{\small 4.266}\\
    {\small -22.39}&{\small 4.75}&{\small 5.124}&{\small 18.4}&{\small 22.28}, align=ccccc]
    ]
    [\textbf{C}
    [\multicolumn{5}{c}{\textbf{A}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -3.164}&{\small 1.954}&{\small 1.545}&{\small 4.954}&{\small 2.35}\\
    {\small -2.597}&{\small 0.9274}&{\small 1.851}&{\small 1.814}&{\small 2.125}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{B}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -2.166}&{\small 1.553}&{\small 1.428}&{\small 2.332}&{\small 1.055}\\
    {\small -2.165}&{\small 1.206}&{\small 2.875}&{\small -0.07224}&{\small 1.419}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{C}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -3.412}&{\small 0.8723}&{\small 0.9043}&{\small 5.503}&{\small 2.849}\\
    {\small -4.026}&{\small 0.8833}&{\small 2.359}&{\small 4.067}&{\small 3.186}, align=ccccc]
    ]
    ]
    \end{forest}

---

    \begin{small}
    \begin{forest}
      for tree={circle, draw}[\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-1.393&-1.296\\
    $\text{y[B]}_{t-1}$&1.966&0.7838\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-2.428&-1.588\\
    $\text{y[B]}_{t-1}$&2.935&1.07\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-1.083&-1.469\\
    $\text{y[B]}_{t-1}$&1.779&0.3142\\
    , align=ccc]
    ]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-3.238&-3.142\\
    $\text{y[B]}_{t-1}$&2.005&0.5409\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-2.033&-3.865\\
    $\text{y[B]}_{t-1}$&2.004&0.9753\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-4.997&-22.39\\
    $\text{y[B]}_{t-1}$&4.323&4.75\\
    , align=ccc]
    ]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-3.164&-2.597\\
    $\text{y[B]}_{t-1}$&1.954&0.9274\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-2.166&-2.165\\
    $\text{y[B]}_{t-1}$&1.553&1.206\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-3.412&-4.026\\
    $\text{y[B]}_{t-1}$&0.8723&0.8833\\
    , align=ccc]
    ]
    ]
    \end{forest}
    \end{small}

---

    \begin{forest}
      for tree={grow'=west}[\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -1.393}&{\scriptsize -1.296}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 1.966}&{\scriptsize 0.7838}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -2.428}&{\scriptsize -1.588}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 2.935}&{\scriptsize 1.07}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -1.083}&{\scriptsize -1.469}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 1.779}&{\scriptsize 0.3142}\\
    , align=ccc]
    ]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -3.238}&{\scriptsize -3.142}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 2.005}&{\scriptsize 0.5409}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -2.033}&{\scriptsize -3.865}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 2.004}&{\scriptsize 0.9753}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -4.997}&{\scriptsize -22.39}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 4.323}&{\scriptsize 4.75}\\
    , align=ccc]
    ]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -3.164}&{\scriptsize -2.597}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 1.954}&{\scriptsize 0.9274}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -2.166}&{\scriptsize -2.165}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 1.553}&{\scriptsize 1.206}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -3.412}&{\scriptsize -4.026}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 0.8723}&{\scriptsize 0.8833}\\
    , align=ccc]
    ]
    ]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -1.4}&{\small -1.3}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.78}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -2.4}&{\small -1.6}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.9}&{\small 1.1}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -1.1}&{\small -1.5}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.8}&{\small 0.31}\\
    , align=ccc]
    ]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -3.2}&{\small -3.1}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.54}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -2}&{\small -3.9}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.98}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -5}&{\small -22}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 4.3}&{\small 4.8}\\
    , align=ccc]
    ]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -3.2}&{\small -2.6}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.93}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -2.2}&{\small -2.2}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.6}&{\small 1.2}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -3.4}&{\small -4}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 0.87}&{\small 0.88}\\
    , align=ccc]
    ]
    ]
    \end{forest}

# draw obeys its contract (with nnet) LaTeX

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\textbf{A} (2.639e-10)]
    [\textbf{B} (3.56e-11)]
    [\textbf{C}
    [\textbf{A} (0.002189)]
    [ \textbf{B, C} (2.594e-06) ]
    ]
    ]
    [\textbf{B}
    [\textbf{A}\\\hline {\small (collapsing: 0.001604)}, align={c}
    [\textbf{A} (0.0009415)]
    [\textbf{B} (0.0003581)]
    [\textbf{C} (0.0001354)]
    ]
    [\textbf{B}
    [\textbf{C} (0.0009274)]
    [ \textbf{A, B} (6.639e-05) ]
    ]
    [\textbf{C}\\\hline {\small (collapsing: 0.007429)}, align={c}
    [\textbf{A} (8.313e-06)]
    [\textbf{B} (0.0001756)]
    [\textbf{C} (3.035e-06)]
    ]
    ]
    [\textbf{C}
    [\textbf{A} (1.463e-10)]
    [\textbf{B}\\\hline {\small merging B and C: 0.001527}, align={c}
    [\textbf{A} (0.001366)]
    [\textbf{B} (0.007996)]
    [\textbf{C} (0.0001326)]
    ]
    [\textbf{C}
    [\textbf{C} (0.005982)]
    [ \textbf{A, B} (4.118e-10) ]
    ]
    ]
    ]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -1.4}&{\small -1.3}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.78}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2.4}&{\small -1.6}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.9}&{\small 1.1}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -1.1}&{\small -1.5}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.8}&{\small 0.31}\\
    , align=ccc]
    ]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -3.2}&{\small -3.1}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.54}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2}&{\small -3.9}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.98}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -5}&{\small -16}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 4.3}&{\small 4.8}\\
    , align=ccc]
    ]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -3.2}&{\small -2.6}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.93}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2.2}&{\small -2.2}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.6}&{\small 1.2}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -3.4}&{\small -4}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 0.87}&{\small 0.88}\\
    , align=ccc]
    ]
    ]
    \end{forest}

---

    \begin{forest}
      for tree={grow'=west}[\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -1}&{\small -1}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.8}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2}&{\small -2}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 3}&{\small 1}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -1}&{\small -1}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.3}\\
    , align=ccc]
    ]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -3}&{\small -3}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.5}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2}&{\small -4}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 1}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -5}&{\small -20}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 4}&{\small 5}\\
    , align=ccc]
    ]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -3}&{\small -3}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.9}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2}&{\small -2}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 1}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -3}&{\small -4}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 0.9}&{\small 0.9}\\
    , align=ccc]
    ]
    ]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{5}{c}{\textbf{A}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -1.4}&{\small 2}&{\small -0.27}&{\small 2}&{\small 1.2}\\
    {\small -1.3}&{\small 0.78}&{\small 1.2}&{\small -0.94}&{\small 2.7}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{B}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -2.4}&{\small 2.9}&{\small 0.28}&{\small 2.6}&{\small -8.3}\\
    {\small -1.6}&{\small 1.1}&{\small 1.3}&{\small 0.86}&{\small 3}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{C}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -1.1}&{\small 1.8}&{\small 0.54}&{\small 1.3}&{\small -0.54}\\
    {\small -1.5}&{\small 0.31}&{\small 2.2}&{\small -0.025}&{\small 2.4}, align=ccccc]
    ]
    [\textbf{B}
    [\multicolumn{5}{c}{\textbf{A}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -3.2}&{\small 2}&{\small 0.94}&{\small 2.5}&{\small 2.5}\\
    {\small -3.1}&{\small 0.54}&{\small 2.7}&{\small 1.7}&{\small 3.8}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{B}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -2}&{\small 2}&{\small 0.18}&{\small 1.8}&{\small 1}\\
    {\small -3.9}&{\small 0.98}&{\small 2.2}&{\small 2.3}&{\small 4.3}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{C}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -5}&{\small 4.3}&{\small 3.7}&{\small 2.8}&{\small 4.3}\\
    {\small -16}&{\small 4.8}&{\small 5.1}&{\small 12}&{\small 16}, align=ccccc]
    ]
    [\textbf{C}
    [\multicolumn{5}{c}{\textbf{A}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -3.2}&{\small 2}&{\small 1.5}&{\small 5}&{\small 2.4}\\
    {\small -2.6}&{\small 0.93}&{\small 1.9}&{\small 1.8}&{\small 2.1}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{B}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -2.2}&{\small 1.6}&{\small 1.4}&{\small 2.3}&{\small 1.1}\\
    {\small -2.2}&{\small 1.2}&{\small 2.9}&{\small -0.072}&{\small 1.4}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{C}}\\\hline
    {\small $(I)$}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -3.4}&{\small 0.87}&{\small 0.9}&{\small 5.5}&{\small 2.8}\\
    {\small -4}&{\small 0.88}&{\small 2.4}&{\small 4.1}&{\small 3.2}, align=ccccc]
    ]
    ]
    \end{forest}

---

    \begin{small}
    \begin{forest}
      for tree={circle, draw}[\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-1.39&-1.3\\
    $\text{y[B]}_{t-1}$&1.97&0.784\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-2.43&-1.59\\
    $\text{y[B]}_{t-1}$&2.93&1.07\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-1.08&-1.47\\
    $\text{y[B]}_{t-1}$&1.78&0.314\\
    , align=ccc]
    ]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-3.24&-3.14\\
    $\text{y[B]}_{t-1}$&2.01&0.541\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-2.03&-3.86\\
    $\text{y[B]}_{t-1}$&2&0.975\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-5&-16.4\\
    $\text{y[B]}_{t-1}$&4.32&4.75\\
    , align=ccc]
    ]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-3.16&-2.6\\
    $\text{y[B]}_{t-1}$&1.95&0.928\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-2.17&-2.17\\
    $\text{y[B]}_{t-1}$&1.55&1.21\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{A}}&\textbf{B}&\textbf{C}\\(I)&-3.41&-4.03\\
    $\text{y[B]}_{t-1}$&0.872&0.883\\
    , align=ccc]
    ]
    ]
    \end{forest}
    \end{small}

---

    \begin{forest}
      for tree={grow'=west}[\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -1.4}&{\scriptsize -1.3}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 2}&{\scriptsize 0.78}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -2.4}&{\scriptsize -1.6}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 2.9}&{\scriptsize 1.1}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -1.1}&{\scriptsize -1.5}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 1.8}&{\scriptsize 0.31}\\
    , align=ccc]
    ]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -3.2}&{\scriptsize -3.1}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 2}&{\scriptsize 0.54}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -2}&{\scriptsize -3.9}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 2}&{\scriptsize 0.98}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -5}&{\scriptsize -16}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 4.3}&{\scriptsize 4.8}\\
    , align=ccc]
    ]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -3.2}&{\scriptsize -2.6}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 2}&{\scriptsize 0.93}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -2.2}&{\scriptsize -2.2}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 1.6}&{\scriptsize 1.2}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{{\scriptsize A}}}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -3.4}&{\scriptsize -4}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 0.87}&{\scriptsize 0.88}\\
    , align=ccc]
    ]
    ]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -1.4}&{\small -1.3}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.78}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -2.4}&{\small -1.6}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.9}&{\small 1.1}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -1.1}&{\small -1.5}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.8}&{\small 0.31}\\
    , align=ccc]
    ]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -3.2}&{\small -3.1}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.54}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -2}&{\small -3.9}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.98}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -5}&{\small -16}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 4.3}&{\small 4.8}\\
    , align=ccc]
    ]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -3.2}&{\small -2.6}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.93}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -2.2}&{\small -2.2}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.6}&{\small 1.2}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {{{\small A}}}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -3.4}&{\small -4}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 0.87}&{\small 0.88}\\
    , align=ccc]
    ]
    ]
    \end{forest}

# draw handles cases when levels have been dropped LaTeX

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{0}
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {\small (I)}&{\small 20}\\
    {\small $\text{y[2]}_{t-1}$}&{\small -20}\\
    {\small $\text{y[3]}_{t-1}$}&{\small -40}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -20}\\
    {\small $\text{z}_{t-1}$}&{\small 4}\\
    {\small $\text{y[2]}_{t-2}$}&{\small 0}\\
    {\small $\text{y[3]}_{t-2}$}&{\small 2}\\
    {\small $\text{y[4]}_{t-2}$}&{\small 0.9}\\
    {\small $\text{z}_{t-2}$}&{\small -1}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{1}}\\\hline
    {\small (I)}&{\small 20}\\
    {\small $\text{y[2]}_{t-1}$}&{\small -20}\\
    {\small $\text{y[3]}_{t-1}$}&{\small -40}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -20}\\
    {\small $\text{z}_{t-1}$}&{\small 4}\\
    {\small $\text{y[2]}_{t-2}$}&{\small 0}\\
    {\small $\text{y[3]}_{t-2}$}&{\small -0.001}\\
    {\small $\text{y[4]}_{t-2}$}&{\small 0.1}\\
    {\small $\text{z}_{t-2}$}&{\small -3}\\, align=cc]
    ]
    [\textbf{1}
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {\small (I)}&{\small -4}\\
    {\small $\text{y[2]}_{t-1}$}&{\small 20}\\
    {\small $\text{y[3]}_{t-1}$}&{\small -1}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -20}\\
    {\small $\text{z}_{t-1}$}&{\small 2}\\
    {\small $\text{y[2]}_{t-2}$}&{\small 1}\\
    {\small $\text{y[3]}_{t-2}$}&{\small 0}\\
    {\small $\text{y[4]}_{t-2}$}&{\small 1}\\
    {\small $\text{z}_{t-2}$}&{\small 3}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{1}}\\\hline
    {\small (I)}&{\small -4}\\
    {\small $\text{y[2]}_{t-1}$}&{\small 20}\\
    {\small $\text{y[3]}_{t-1}$}&{\small 0.8}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -20}\\
    {\small $\text{z}_{t-1}$}&{\small 6}\\
    {\small $\text{y[2]}_{t-2}$}&{\small -0.6}\\
    {\small $\text{y[3]}_{t-2}$}&{\small 2}\\
    {\small $\text{y[4]}_{t-2}$}&{\small 0}\\
    {\small $\text{z}_{t-2}$}&{\small -1}\\, align=cc]
    ]
    ]
    \end{forest}

---

    \begin{forest}
      for tree={grow'=west}[\textbf{$\epsilon$}
    [\textbf{0}
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {{\small 0}}&\textbf{{\small 1}}\\{\small (I)}&{\small 16}\\
    {\small $\text{y[2]}_{t-1}$}&{\small -18}\\
    {\small $\text{y[3]}_{t-1}$}&{\small -40}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -19}\\
    {\small $\text{z}_{t-1}$}&{\small 4.1}\\
    {\small $\text{y[2]}_{t-2}$}&{\small 0}\\
    {\small $\text{y[3]}_{t-2}$}&{\small 1.8}\\
    {\small $\text{y[4]}_{t-2}$}&{\small 0.92}\\
    {\small $\text{z}_{t-2}$}&{\small -1.5}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{1}}\\\hline
    {{\small 0}}&\textbf{{\small 1}}\\{\small (I)}&{\small 18}\\
    {\small $\text{y[2]}_{t-1}$}&{\small -21}\\
    {\small $\text{y[3]}_{t-1}$}&{\small -38}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -20}\\
    {\small $\text{z}_{t-1}$}&{\small 4.2}\\
    {\small $\text{y[2]}_{t-2}$}&{\small 0}\\
    {\small $\text{y[3]}_{t-2}$}&{\small -0.0013}\\
    {\small $\text{y[4]}_{t-2}$}&{\small 0.12}\\
    {\small $\text{z}_{t-2}$}&{\small -3}\\, align=cc]
    ]
    [\textbf{1}
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {{\small 0}}&\textbf{{\small 1}}\\{\small (I)}&{\small -4}\\
    {\small $\text{y[2]}_{t-1}$}&{\small 19}\\
    {\small $\text{y[3]}_{t-1}$}&{\small -1.3}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -20}\\
    {\small $\text{z}_{t-1}$}&{\small 2.4}\\
    {\small $\text{y[2]}_{t-2}$}&{\small 1.3}\\
    {\small $\text{y[3]}_{t-2}$}&{\small 0}\\
    {\small $\text{y[4]}_{t-2}$}&{\small 1.2}\\
    {\small $\text{z}_{t-2}$}&{\small 2.5}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{1}}\\\hline
    {{\small 0}}&\textbf{{\small 1}}\\{\small (I)}&{\small -3.6}\\
    {\small $\text{y[2]}_{t-1}$}&{\small 21}\\
    {\small $\text{y[3]}_{t-1}$}&{\small 0.81}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -20}\\
    {\small $\text{z}_{t-1}$}&{\small 6.2}\\
    {\small $\text{y[2]}_{t-2}$}&{\small -0.57}\\
    {\small $\text{y[3]}_{t-2}$}&{\small 1.9}\\
    {\small $\text{y[4]}_{t-2}$}&{\small 0}\\
    {\small $\text{z}_{t-2}$}&{\small -0.99}\\, align=cc]
    ]
    ]
    \end{forest}

# draw handles cases when multinom is used for two states time series LaTeX

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{0}
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {\small (I)}&{\small 6}\\
    {\small $\text{y[2]}_{t-1}$}&{\small -9}\\
    {\small $\text{y[3]}_{t-1}$}&{\small -20}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -10}\\
    {\small $\text{z}_{t-1}$}&{\small 4}\\
    {\small $\text{y[2]}_{t-2}$}&{\small 1}\\
    {\small $\text{y[3]}_{t-2}$}&{\small 3}\\
    {\small $\text{y[4]}_{t-2}$}&{\small 2}\\
    {\small $\text{z}_{t-2}$}&{\small -1}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{1}}\\\hline
    {\small (I)}&{\small 9}\\
    {\small $\text{y[2]}_{t-1}$}&{\small -10}\\
    {\small $\text{y[3]}_{t-1}$}&{\small -20}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -10}\\
    {\small $\text{z}_{t-1}$}&{\small 4}\\
    {\small $\text{y[2]}_{t-2}$}&{\small 0}\\
    {\small $\text{y[3]}_{t-2}$}&{\small -0.001}\\
    {\small $\text{y[4]}_{t-2}$}&{\small 0.1}\\
    {\small $\text{z}_{t-2}$}&{\small -3}\\, align=cc]
    ]
    [\textbf{1}
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {\small (I)}&{\small -4}\\
    {\small $\text{y[2]}_{t-1}$}&{\small 9}\\
    {\small $\text{y[3]}_{t-1}$}&{\small -1}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -10}\\
    {\small $\text{z}_{t-1}$}&{\small 2}\\
    {\small $\text{y[2]}_{t-2}$}&{\small 1}\\
    {\small $\text{y[3]}_{t-2}$}&{\small 0}\\
    {\small $\text{y[4]}_{t-2}$}&{\small 1}\\
    {\small $\text{z}_{t-2}$}&{\small 3}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{1}}\\\hline
    {\small (I)}&{\small -4}\\
    {\small $\text{y[2]}_{t-1}$}&{\small 10}\\
    {\small $\text{y[3]}_{t-1}$}&{\small 0.8}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -10}\\
    {\small $\text{z}_{t-1}$}&{\small 6}\\
    {\small $\text{y[2]}_{t-2}$}&{\small -0.6}\\
    {\small $\text{y[3]}_{t-2}$}&{\small 2}\\
    {\small $\text{y[4]}_{t-2}$}&{\small 0}\\
    {\small $\text{z}_{t-2}$}&{\small -1}\\, align=cc]
    ]
    ]
    \end{forest}

---

    \begin{small}
    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{0}
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {0}&\textbf{1}\\(I)&6\\
    $\text{y[2]}_{t-1}$&-9\\
    $\text{y[3]}_{t-1}$&-20\\
    $\text{y[4]}_{t-1}$&-10\\
    $\text{z}_{t-1}$&4\\
    $\text{y[2]}_{t-2}$&1\\
    $\text{y[3]}_{t-2}$&3\\
    $\text{y[4]}_{t-2}$&2\\
    $\text{z}_{t-2}$&-1\\, align=cc]
    [\multicolumn{2}{c}{\textbf{1}}\\\hline
    {0}&\textbf{1}\\(I)&9\\
    $\text{y[2]}_{t-1}$&-10\\
    $\text{y[3]}_{t-1}$&-20\\
    $\text{y[4]}_{t-1}$&-10\\
    $\text{z}_{t-1}$&4\\
    $\text{y[2]}_{t-2}$&0\\
    $\text{y[3]}_{t-2}$&-0.001\\
    $\text{y[4]}_{t-2}$&0.1\\
    $\text{z}_{t-2}$&-3\\, align=cc]
    ]
    [\textbf{1}
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {0}&\textbf{1}\\(I)&-4\\
    $\text{y[2]}_{t-1}$&9\\
    $\text{y[3]}_{t-1}$&-1\\
    $\text{y[4]}_{t-1}$&-10\\
    $\text{z}_{t-1}$&2\\
    $\text{y[2]}_{t-2}$&1\\
    $\text{y[3]}_{t-2}$&0\\
    $\text{y[4]}_{t-2}$&1\\
    $\text{z}_{t-2}$&3\\, align=cc]
    [\multicolumn{2}{c}{\textbf{1}}\\\hline
    {0}&\textbf{1}\\(I)&-4\\
    $\text{y[2]}_{t-1}$&10\\
    $\text{y[3]}_{t-1}$&0.8\\
    $\text{y[4]}_{t-1}$&-10\\
    $\text{z}_{t-1}$&6\\
    $\text{y[2]}_{t-2}$&-0.6\\
    $\text{y[3]}_{t-2}$&2\\
    $\text{y[4]}_{t-2}$&0\\
    $\text{z}_{t-2}$&-1\\, align=cc]
    ]
    ]
    \end{forest}
    \end{small}

# draw handles degenerate cases LaTeX

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {{\small (0,1.28]}}&\textbf{{\small (1.28,7.54]}}\\{\small (I)}&{\small -3}\\
    {\small $\text{day\_night}_{t-1}$}&{\small -0.43}\\
    {\small $\text{day\_night}_{t-2}$}&{\small -0.64}\\
    {\small $\text{day\_night}_{t-3}$}&{\small 1.5}\\
    {\small $\text{day\_night}_{t-4}$}&{\small -2.3}\\
    {\small $\text{day\_night}_{t-5}$}&{\small 2.6}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small 0.91}&{\small 0.093}, align=cc]
    ]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small 0.89}&{\small 0.11}, align=cc]
    ]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {{\small (0,1.28]}}&\textbf{{\small (1.28,7.54]}}\\{\small (I)}&{\small -2.7}\\
    {\small $\text{day\_night}_{t-1}$}&{\small 1.3}\\, align=cc]
    ]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small 0.84}&{\small 0.16}, align=cc]
    ]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\small 0.17}&{\small 0.83}, align=cc]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\small 0.15}&{\small 0.85}, align=cc]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {{\small (0,1.28]}}&\textbf{{\small (1.28,7.54]}}\\{\small (I)}&{\small 0.99}\\
    {\small $\text{day\_night}_{t-1}$}&{\small 1.3}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small 0.056}&{\small 0.94}, align=cc]
    ]
    ]
    ]
    ]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {{\tiny (0,1.28]}}&\textbf{{\tiny (1.28,7.54]}}\\{\tiny (I)}&{\tiny -3}\\
    {\tiny $\text{day\_night}_{t-1}$}&{\tiny -0.43}\\
    {\tiny $\text{day\_night}_{t-2}$}&{\tiny -0.64}\\
    {\tiny $\text{day\_night}_{t-3}$}&{\tiny 1.5}\\
    {\tiny $\text{day\_night}_{t-4}$}&{\tiny -2.3}\\
    {\tiny $\text{day\_night}_{t-5}$}&{\tiny 2.6}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny 0.91}&{\tiny 0.093}, align=cc]
    ]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny 0.89}&{\tiny 0.11}, align=cc]
    ]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {{\tiny (0,1.28]}}&\textbf{{\tiny (1.28,7.54]}}\\{\tiny (I)}&{\tiny -2.7}\\
    {\tiny $\text{day\_night}_{t-1}$}&{\tiny 1.3}\\, align=cc]
    ]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny 0.84}&{\tiny 0.16}, align=cc]
    ]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\tiny 0.17}&{\tiny 0.83}, align=cc]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\tiny 0.15}&{\tiny 0.85}, align=cc]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {{\tiny (0,1.28]}}&\textbf{{\tiny (1.28,7.54]}}\\{\tiny (I)}&{\tiny 0.99}\\
    {\tiny $\text{day\_night}_{t-1}$}&{\tiny 1.3}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny 0.056}&{\tiny 0.94}, align=cc]
    ]
    ]
    ]
    ]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {{\small (0,1.28]}}&\textbf{{\small (1.28,7.54]}}\\{\small (I)}&{\small -3}\\
    {\small $\text{day\_night}_{t-1}$}&{\small -0.43}\\
    {\small $\text{day\_night}_{t-2}$}&{\small -0.64}\\
    {\small $\text{day\_night}_{t-3}$}&{\small 1.5}\\
    {\small $\text{day\_night}_{t-4}$}&{\small -2.3}\\
    {\small $\text{day\_night}_{t-5}$}&{\small 2.6}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small 0.91}&{\small 0.093}, align=cc]
    ]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small 0.89}&{\small 0.11}, align=cc]
    ]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {{\small (0,1.28]}}&\textbf{{\small (1.28,7.54]}}\\{\small (I)}&{\small -2.7}\\
    {\small $\text{day\_night}_{t-1}$}&{\small 1.3}\\, align=cc]
    ]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small 0.84}&{\small 0.16}, align=cc]
    ]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\small 0.17}&{\small 0.83}, align=cc]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\small 0.15}&{\small 0.85}, align=cc]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {{\small (0,1.28]}}&\textbf{{\small (1.28,7.54]}}\\{\small (I)}&{\small 0.99}\\
    {\small $\text{day\_night}_{t-1}$}&{\small 1.3}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small 0.056}&{\small 0.94}, align=cc]
    ]
    ]
    ]
    ]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {{\tiny (0,1.28]}}&\textbf{{\tiny (1.28,7.54]}}\\{\tiny (I)}&{\tiny -3}\\
    {\tiny $\text{day\_night}_{t-1}$}&{\tiny -0.43}\\
    {\tiny $\text{day\_night}_{t-2}$}&{\tiny -0.64}\\
    {\tiny $\text{day\_night}_{t-3}$}&{\tiny 1.5}\\
    {\tiny $\text{day\_night}_{t-4}$}&{\tiny -2.3}\\
    {\tiny $\text{day\_night}_{t-5}$}&{\tiny 2.6}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny 0.91}&{\tiny 0.093}, align=cc]
    ]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny 0.89}&{\tiny 0.11}, align=cc]
    ]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {{\tiny (0,1.28]}}&\textbf{{\tiny (1.28,7.54]}}\\{\tiny (I)}&{\tiny -2.7}\\
    {\tiny $\text{day\_night}_{t-1}$}&{\tiny 1.3}\\, align=cc]
    ]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny 0.84}&{\tiny 0.16}, align=cc]
    ]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\tiny 0.17}&{\tiny 0.83}, align=cc]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\tiny 0.15}&{\tiny 0.85}, align=cc]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {{\tiny (0,1.28]}}&\textbf{{\tiny (1.28,7.54]}}\\{\tiny (I)}&{\tiny 0.99}\\
    {\tiny $\text{day\_night}_{t-1}$}&{\tiny 1.3}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny 0.056}&{\tiny 0.94}, align=cc]
    ]
    ]
    ]
    ]
    \end{forest}

