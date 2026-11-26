# draw obeys its contract (with vgam) LaTeX

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\textbf{A} (6.085e-09)]
    [\textbf{B} (8.921e-10)]
    [\textbf{C} (1.23e-05)]]
    [\textbf{B}
    [\textbf{A} (4.786e-05)]
    [\textbf{B}
    [\textbf{C} (0.00761)]
    [\textbf{A, B} (0.000705)]]
    [\textbf{C} (3.541e-11)]]
    [\textbf{C}
    [\textbf{A} (3.459e-09)]
    [\textbf{B} (0.0009756)]
    [\textbf{C} (3.65e-08)]]]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -1.393}&{\small -1.296}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.966}&{\small 0.7838}\\
    {\small $\text{y[C]}_{t-1}$}&{\small -0.2701}&{\small 1.216}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 1.957}&{\small -0.942}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 1.173}&{\small 2.738}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2.428}&{\small -1.588}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.935}&{\small 1.07}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.2797}&{\small 1.347}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2.627}&{\small 0.8597}\\
    {\small $\text{y[C]}_{t-2}$}&{\small -15}&{\small 3.026}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -1.083}&{\small -1.469}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.779}&{\small 0.3142}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.5446}&{\small 2.158}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 1.275}&{\small -0.02489}\\
    {\small $\text{y[C]}_{t-2}$}&{\small -0.5394}&{\small 2.407}\\
    , align=ccc]]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -3.238}&{\small -3.142}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.005}&{\small 0.5409}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.938}&{\small 2.655}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2.525}&{\small 1.66}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 2.54}&{\small 3.751}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2.033}&{\small -3.865}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.004}&{\small 0.9753}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.1771}&{\small 2.193}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 1.809}&{\small 2.283}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 0.9951}&{\small 4.315}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -4.997}&{\small -22.39}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 4.323}&{\small 4.75}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 3.73}&{\small 5.124}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2.829}&{\small 18.4}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 4.266}&{\small 22.28}\\
    , align=ccc]]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -3.164}&{\small -2.597}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.954}&{\small 0.9274}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 1.545}&{\small 1.851}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 4.954}&{\small 1.814}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 2.35}&{\small 2.125}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -0.8267}&{\small -1.163}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.52}&{\small 1.163}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 1.386}&{\small 2.955}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -3.412}&{\small -4.026}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 0.8723}&{\small 0.8833}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.9043}&{\small 2.359}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 5.503}&{\small 4.067}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 2.849}&{\small 3.186}\\
    , align=ccc]]]
    \end{forest}

---

    \begin{forest}
      for tree={grow'=west}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -1.393}&{\small -1.296}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.966}&{\small 0.7838}\\
    {\small $\text{y[C]}_{t-1}$}&{\small -0.2701}&{\small 1.216}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 1.957}&{\small -0.942}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 1.173}&{\small 2.738}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2.428}&{\small -1.588}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.935}&{\small 1.07}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.2797}&{\small 1.347}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2.627}&{\small 0.8597}\\
    {\small $\text{y[C]}_{t-2}$}&{\small -15}&{\small 3.026}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -1.083}&{\small -1.469}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.779}&{\small 0.3142}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.5446}&{\small 2.158}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 1.275}&{\small -0.02489}\\
    {\small $\text{y[C]}_{t-2}$}&{\small -0.5394}&{\small 2.407}\\
    , align=ccc]]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -3.238}&{\small -3.142}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.005}&{\small 0.5409}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.938}&{\small 2.655}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2.525}&{\small 1.66}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 2.54}&{\small 3.751}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2.033}&{\small -3.865}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.004}&{\small 0.9753}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.1771}&{\small 2.193}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 1.809}&{\small 2.283}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 0.9951}&{\small 4.315}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -4.997}&{\small -22.39}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 4.323}&{\small 4.75}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 3.73}&{\small 5.124}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2.829}&{\small 18.4}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 4.266}&{\small 22.28}\\
    , align=ccc]]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -3.164}&{\small -2.597}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.954}&{\small 0.9274}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 1.545}&{\small 1.851}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 4.954}&{\small 1.814}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 2.35}&{\small 2.125}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -0.8267}&{\small -1.163}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.52}&{\small 1.163}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 1.386}&{\small 2.955}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -3.412}&{\small -4.026}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 0.8723}&{\small 0.8833}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.9043}&{\small 2.359}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 5.503}&{\small 4.067}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 2.849}&{\small 3.186}\\
    , align=ccc]]]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{5}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -1.393}&{\small 1.966}&{\small -0.2701}&{\small 1.957}&{\small 1.173}\\
    {\small -1.296}&{\small 0.7838}&{\small 1.216}&{\small -0.942}&{\small 2.738}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -2.428}&{\small 2.935}&{\small 0.2797}&{\small 2.627}&{\small -15}\\
    {\small -1.588}&{\small 1.07}&{\small 1.347}&{\small 0.8597}&{\small 3.026}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -1.083}&{\small 1.779}&{\small 0.5446}&{\small 1.275}&{\small -0.5394}\\
    {\small -1.469}&{\small 0.3142}&{\small 2.158}&{\small -0.02489}&{\small 2.407}, align=ccccc]]
    [\textbf{B}
    [\multicolumn{5}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -3.238}&{\small 2.005}&{\small 0.938}&{\small 2.525}&{\small 2.54}\\
    {\small -3.142}&{\small 0.5409}&{\small 2.655}&{\small 1.66}&{\small 3.751}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -2.033}&{\small 2.004}&{\small 0.1771}&{\small 1.809}&{\small 0.9951}\\
    {\small -3.865}&{\small 0.9753}&{\small 2.193}&{\small 2.283}&{\small 4.315}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -4.997}&{\small 4.323}&{\small 3.73}&{\small 2.829}&{\small 4.266}\\
    {\small -22.39}&{\small 4.75}&{\small 5.124}&{\small 18.4}&{\small 22.28}, align=ccccc]]
    [\textbf{C}
    [\multicolumn{5}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -3.164}&{\small 1.954}&{\small 1.545}&{\small 4.954}&{\small 2.35}\\
    {\small -2.597}&{\small 0.9274}&{\small 1.851}&{\small 1.814}&{\small 2.125}, align=ccccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}\\
    {\small -0.8267}&{\small 1.52}&{\small 1.386}\\
    {\small -1.163}&{\small 1.163}&{\small 2.955}, align=ccc]
    [\multicolumn{5}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -3.412}&{\small 0.8723}&{\small 0.9043}&{\small 5.503}&{\small 2.849}\\
    {\small -4.026}&{\small 0.8833}&{\small 2.359}&{\small 4.067}&{\small 3.186}, align=ccccc]]]
    \end{forest}

---

    \begin{small}
    \begin{forest}
      for tree={circle, draw}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-1.393&-1.296\\
    $\text{y[B]}_{t-1}$&1.966&0.7838\\
    $\text{y[C]}_{t-1}$&-0.2701&1.216\\
    $\text{y[B]}_{t-2}$&1.957&-0.942\\
    $\text{y[C]}_{t-2}$&1.173&2.738\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-2.428&-1.588\\
    $\text{y[B]}_{t-1}$&2.935&1.07\\
    $\text{y[C]}_{t-1}$&0.2797&1.347\\
    $\text{y[B]}_{t-2}$&2.627&0.8597\\
    $\text{y[C]}_{t-2}$&-15&3.026\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-1.083&-1.469\\
    $\text{y[B]}_{t-1}$&1.779&0.3142\\
    $\text{y[C]}_{t-1}$&0.5446&2.158\\
    $\text{y[B]}_{t-2}$&1.275&-0.02489\\
    $\text{y[C]}_{t-2}$&-0.5394&2.407\\
    , align=ccc]]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-3.238&-3.142\\
    $\text{y[B]}_{t-1}$&2.005&0.5409\\
    $\text{y[C]}_{t-1}$&0.938&2.655\\
    $\text{y[B]}_{t-2}$&2.525&1.66\\
    $\text{y[C]}_{t-2}$&2.54&3.751\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-2.033&-3.865\\
    $\text{y[B]}_{t-1}$&2.004&0.9753\\
    $\text{y[C]}_{t-1}$&0.1771&2.193\\
    $\text{y[B]}_{t-2}$&1.809&2.283\\
    $\text{y[C]}_{t-2}$&0.9951&4.315\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-4.997&-22.39\\
    $\text{y[B]}_{t-1}$&4.323&4.75\\
    $\text{y[C]}_{t-1}$&3.73&5.124\\
    $\text{y[B]}_{t-2}$&2.829&18.4\\
    $\text{y[C]}_{t-2}$&4.266&22.28\\
    , align=ccc]]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-3.164&-2.597\\
    $\text{y[B]}_{t-1}$&1.954&0.9274\\
    $\text{y[C]}_{t-1}$&1.545&1.851\\
    $\text{y[B]}_{t-2}$&4.954&1.814\\
    $\text{y[C]}_{t-2}$&2.35&2.125\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-0.8267&-1.163\\
    $\text{y[B]}_{t-1}$&1.52&1.163\\
    $\text{y[C]}_{t-1}$&1.386&2.955\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-3.412&-4.026\\
    $\text{y[B]}_{t-1}$&0.8723&0.8833\\
    $\text{y[C]}_{t-1}$&0.9043&2.359\\
    $\text{y[B]}_{t-2}$&5.503&4.067\\
    $\text{y[C]}_{t-2}$&2.849&3.186\\
    , align=ccc]]]
    \end{forest}
    \end{small}

---

    \begin{forest}
      for tree={grow'=west}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -1.393}&{\scriptsize -1.296}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 1.966}&{\scriptsize 0.7838}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize -0.2701}&{\scriptsize 1.216}\\
    {\scriptsize $\text{y[B]}_{t-2}$}&{\scriptsize 1.957}&{\scriptsize -0.942}\\
    {\scriptsize $\text{y[C]}_{t-2}$}&{\scriptsize 1.173}&{\scriptsize 2.738}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -2.428}&{\scriptsize -1.588}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 2.935}&{\scriptsize 1.07}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize 0.2797}&{\scriptsize 1.347}\\
    {\scriptsize $\text{y[B]}_{t-2}$}&{\scriptsize 2.627}&{\scriptsize 0.8597}\\
    {\scriptsize $\text{y[C]}_{t-2}$}&{\scriptsize -15}&{\scriptsize 3.026}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -1.083}&{\scriptsize -1.469}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 1.779}&{\scriptsize 0.3142}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize 0.5446}&{\scriptsize 2.158}\\
    {\scriptsize $\text{y[B]}_{t-2}$}&{\scriptsize 1.275}&{\scriptsize -0.02489}\\
    {\scriptsize $\text{y[C]}_{t-2}$}&{\scriptsize -0.5394}&{\scriptsize 2.407}\\
    , align=ccc]]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -3.238}&{\scriptsize -3.142}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 2.005}&{\scriptsize 0.5409}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize 0.938}&{\scriptsize 2.655}\\
    {\scriptsize $\text{y[B]}_{t-2}$}&{\scriptsize 2.525}&{\scriptsize 1.66}\\
    {\scriptsize $\text{y[C]}_{t-2}$}&{\scriptsize 2.54}&{\scriptsize 3.751}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -2.033}&{\scriptsize -3.865}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 2.004}&{\scriptsize 0.9753}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize 0.1771}&{\scriptsize 2.193}\\
    {\scriptsize $\text{y[B]}_{t-2}$}&{\scriptsize 1.809}&{\scriptsize 2.283}\\
    {\scriptsize $\text{y[C]}_{t-2}$}&{\scriptsize 0.9951}&{\scriptsize 4.315}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -4.997}&{\scriptsize -22.39}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 4.323}&{\scriptsize 4.75}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize 3.73}&{\scriptsize 5.124}\\
    {\scriptsize $\text{y[B]}_{t-2}$}&{\scriptsize 2.829}&{\scriptsize 18.4}\\
    {\scriptsize $\text{y[C]}_{t-2}$}&{\scriptsize 4.266}&{\scriptsize 22.28}\\
    , align=ccc]]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -3.164}&{\scriptsize -2.597}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 1.954}&{\scriptsize 0.9274}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize 1.545}&{\scriptsize 1.851}\\
    {\scriptsize $\text{y[B]}_{t-2}$}&{\scriptsize 4.954}&{\scriptsize 1.814}\\
    {\scriptsize $\text{y[C]}_{t-2}$}&{\scriptsize 2.35}&{\scriptsize 2.125}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -0.8267}&{\scriptsize -1.163}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 1.52}&{\scriptsize 1.163}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize 1.386}&{\scriptsize 2.955}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -3.412}&{\scriptsize -4.026}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 0.8723}&{\scriptsize 0.8833}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize 0.9043}&{\scriptsize 2.359}\\
    {\scriptsize $\text{y[B]}_{t-2}$}&{\scriptsize 5.503}&{\scriptsize 4.067}\\
    {\scriptsize $\text{y[C]}_{t-2}$}&{\scriptsize 2.849}&{\scriptsize 3.186}\\
    , align=ccc]]]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -1.4}&{\small -1.3}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.78}\\
    {\small $\text{y[C]}_{t-1}$}&{\small -0.27}&{\small 1.2}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2}&{\small -0.94}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 1.2}&{\small 2.7}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -2.4}&{\small -1.6}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.9}&{\small 1.1}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.28}&{\small 1.3}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2.6}&{\small 0.86}\\
    {\small $\text{y[C]}_{t-2}$}&{\small -15}&{\small 3}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -1.1}&{\small -1.5}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.8}&{\small 0.31}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.54}&{\small 2.2}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 1.3}&{\small -0.025}\\
    {\small $\text{y[C]}_{t-2}$}&{\small -0.54}&{\small 2.4}\\
    , align=ccc]]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -3.2}&{\small -3.1}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.54}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.94}&{\small 2.7}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2.5}&{\small 1.7}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 2.5}&{\small 3.8}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -2}&{\small -3.9}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.98}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.18}&{\small 2.2}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 1.8}&{\small 2.3}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 1}&{\small 4.3}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -5}&{\small -22}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 4.3}&{\small 4.8}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 3.7}&{\small 5.1}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2.8}&{\small 18}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 4.3}&{\small 22}\\
    , align=ccc]]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -3.2}&{\small -2.6}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.93}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 1.5}&{\small 1.9}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 5}&{\small 1.8}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 2.4}&{\small 2.1}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -0.83}&{\small -1.2}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.5}&{\small 1.2}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 1.4}&{\small 3}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -3.4}&{\small -4}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 0.87}&{\small 0.88}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.9}&{\small 2.4}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 5.5}&{\small 4.1}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 2.8}&{\small 3.2}\\
    , align=ccc]]]
    \end{forest}

# draw obeys its contract (with nnet) LaTeX

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\textbf{A} (6.085e-09)]
    [\textbf{B} (8.922e-10)]
    [\textbf{C} (1.23e-05)]]
    [\textbf{B}
    [\textbf{A} (4.786e-05)]
    [\textbf{B}
    [\textbf{C} (0.007403)]
    [\textbf{A, B} (0.000705)]]
    [\textbf{C} (3.541e-11)]]
    [\textbf{C}
    [\textbf{A} (3.459e-09)]
    [\textbf{B} (0.0009756)]
    [\textbf{C} (3.65e-08)]]]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -1.4}&{\small -1.3}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.78}\\
    {\small $\text{y[C]}_{t-1}$}&{\small -0.27}&{\small 1.2}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2}&{\small -0.94}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 1.2}&{\small 2.7}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2.4}&{\small -1.6}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.9}&{\small 1.1}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.28}&{\small 1.3}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2.6}&{\small 0.86}\\
    {\small $\text{y[C]}_{t-2}$}&{\small -8.3}&{\small 3}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -1.1}&{\small -1.5}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.8}&{\small 0.31}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.54}&{\small 2.2}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 1.3}&{\small -0.025}\\
    {\small $\text{y[C]}_{t-2}$}&{\small -0.54}&{\small 2.4}\\
    , align=ccc]]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -3.2}&{\small -3.1}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.54}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.94}&{\small 2.7}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2.5}&{\small 1.7}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 2.5}&{\small 3.8}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2}&{\small -3.9}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.98}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.18}&{\small 2.2}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 1.8}&{\small 2.3}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 1}&{\small 4.3}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -5}&{\small -16}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 4.3}&{\small 4.8}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 3.7}&{\small 5.1}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2.8}&{\small 12}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 4.3}&{\small 16}\\
    , align=ccc]]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -3.2}&{\small -2.6}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.93}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 1.5}&{\small 1.9}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 5}&{\small 1.8}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 2.4}&{\small 2.1}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -0.83}&{\small -1.2}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.5}&{\small 1.2}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 1.4}&{\small 3}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -3.4}&{\small -4}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 0.87}&{\small 0.88}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.9}&{\small 2.4}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 5.5}&{\small 4.1}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 2.8}&{\small 3.2}\\
    , align=ccc]]]
    \end{forest}

---

    \begin{forest}
      for tree={grow'=west}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -1}&{\small -1}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.8}\\
    {\small $\text{y[C]}_{t-1}$}&{\small -0.3}&{\small 1}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2}&{\small -0.9}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 1}&{\small 3}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2}&{\small -2}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 3}&{\small 1}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.3}&{\small 1}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 3}&{\small 0.9}\\
    {\small $\text{y[C]}_{t-2}$}&{\small -8}&{\small 3}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -1}&{\small -1}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.3}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.5}&{\small 2}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 1}&{\small -0.02}\\
    {\small $\text{y[C]}_{t-2}$}&{\small -0.5}&{\small 2}\\
    , align=ccc]]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -3}&{\small -3}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.5}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.9}&{\small 3}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 3}&{\small 2}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 3}&{\small 4}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -2}&{\small -4}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 1}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.2}&{\small 2}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2}&{\small 2}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 1}&{\small 4}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -5}&{\small -20}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 4}&{\small 5}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 4}&{\small 5}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 3}&{\small 10}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 4}&{\small 20}\\
    , align=ccc]]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small -3}&{\small -3}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.9}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 2}&{\small 2}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 5}&{\small 2}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 2}&{\small 2}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small -0.8}&{\small -1}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 1}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 1}&{\small 3}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small -3}&{\small -4}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 0.9}&{\small 0.9}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.9}&{\small 2}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 6}&{\small 4}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 3}&{\small 3}\\
    , align=ccc]]]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{5}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -1.4}&{\small 2}&{\small -0.27}&{\small 2}&{\small 1.2}\\
    {\small -1.3}&{\small 0.78}&{\small 1.2}&{\small -0.94}&{\small 2.7}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -2.4}&{\small 2.9}&{\small 0.28}&{\small 2.6}&{\small -8.3}\\
    {\small -1.6}&{\small 1.1}&{\small 1.3}&{\small 0.86}&{\small 3}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -1.1}&{\small 1.8}&{\small 0.54}&{\small 1.3}&{\small -0.54}\\
    {\small -1.5}&{\small 0.31}&{\small 2.2}&{\small -0.025}&{\small 2.4}, align=ccccc]]
    [\textbf{B}
    [\multicolumn{5}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -3.2}&{\small 2}&{\small 0.94}&{\small 2.5}&{\small 2.5}\\
    {\small -3.1}&{\small 0.54}&{\small 2.7}&{\small 1.7}&{\small 3.8}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -2}&{\small 2}&{\small 0.18}&{\small 1.8}&{\small 1}\\
    {\small -3.9}&{\small 0.98}&{\small 2.2}&{\small 2.3}&{\small 4.3}, align=ccccc]
    [\multicolumn{5}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -5}&{\small 4.3}&{\small 3.7}&{\small 2.8}&{\small 4.3}\\
    {\small -16}&{\small 4.8}&{\small 5.1}&{\small 12}&{\small 16}, align=ccccc]]
    [\textbf{C}
    [\multicolumn{5}{c}{\textbf{A}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -3.2}&{\small 2}&{\small 1.5}&{\small 5}&{\small 2.4}\\
    {\small -2.6}&{\small 0.93}&{\small 1.9}&{\small 1.8}&{\small 2.1}, align=ccccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}\\
    {\small -0.83}&{\small 1.5}&{\small 1.4}\\
    {\small -1.2}&{\small 1.2}&{\small 3}, align=ccc]
    [\multicolumn{5}{c}{\textbf{C}}\\\hline
    {\small (I)}&{\small $\text{y[B]}_{t-1}$}&{\small $\text{y[C]}_{t-1}$}&{\small $\text{y[B]}_{t-2}$}&{\small $\text{y[C]}_{t-2}$}\\
    {\small -3.4}&{\small 0.87}&{\small 0.9}&{\small 5.5}&{\small 2.8}\\
    {\small -4}&{\small 0.88}&{\small 2.4}&{\small 4.1}&{\small 3.2}, align=ccccc]]]
    \end{forest}

---

    \begin{small}
    \begin{forest}
      for tree={circle, draw}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-1.39&-1.3\\
    $\text{y[B]}_{t-1}$&1.97&0.784\\
    $\text{y[C]}_{t-1}$&-0.27&1.22\\
    $\text{y[B]}_{t-2}$&1.96&-0.942\\
    $\text{y[C]}_{t-2}$&1.17&2.74\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-2.43&-1.59\\
    $\text{y[B]}_{t-1}$&2.93&1.07\\
    $\text{y[C]}_{t-1}$&0.28&1.35\\
    $\text{y[B]}_{t-2}$&2.63&0.86\\
    $\text{y[C]}_{t-2}$&-8.26&3.03\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-1.08&-1.47\\
    $\text{y[B]}_{t-1}$&1.78&0.314\\
    $\text{y[C]}_{t-1}$&0.545&2.16\\
    $\text{y[B]}_{t-2}$&1.28&-0.0249\\
    $\text{y[C]}_{t-2}$&-0.539&2.41\\
    , align=ccc]]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-3.24&-3.14\\
    $\text{y[B]}_{t-1}$&2.01&0.541\\
    $\text{y[C]}_{t-1}$&0.938&2.66\\
    $\text{y[B]}_{t-2}$&2.53&1.66\\
    $\text{y[C]}_{t-2}$&2.54&3.75\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-2.03&-3.86\\
    $\text{y[B]}_{t-1}$&2&0.975\\
    $\text{y[C]}_{t-1}$&0.177&2.19\\
    $\text{y[B]}_{t-2}$&1.81&2.28\\
    $\text{y[C]}_{t-2}$&0.995&4.31\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-5&-16.4\\
    $\text{y[B]}_{t-1}$&4.32&4.75\\
    $\text{y[C]}_{t-1}$&3.73&5.12\\
    $\text{y[B]}_{t-2}$&2.83&12.4\\
    $\text{y[C]}_{t-2}$&4.27&16.3\\
    , align=ccc]]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-3.16&-2.6\\
    $\text{y[B]}_{t-1}$&1.95&0.928\\
    $\text{y[C]}_{t-1}$&1.54&1.85\\
    $\text{y[B]}_{t-2}$&4.95&1.81\\
    $\text{y[C]}_{t-2}$&2.35&2.13\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-0.827&-1.16\\
    $\text{y[B]}_{t-1}$&1.52&1.16\\
    $\text{y[C]}_{t-1}$&1.39&2.95\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    A&\textbf{B}&\textbf{C}\\(I)&-3.41&-4.03\\
    $\text{y[B]}_{t-1}$&0.872&0.883\\
    $\text{y[C]}_{t-1}$&0.904&2.36\\
    $\text{y[B]}_{t-2}$&5.5&4.07\\
    $\text{y[C]}_{t-2}$&2.85&3.19\\
    , align=ccc]]]
    \end{forest}
    \end{small}

---

    \begin{forest}
      for tree={grow'=west}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -1.4}&{\scriptsize -1.3}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 2}&{\scriptsize 0.78}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize -0.27}&{\scriptsize 1.2}\\
    {\scriptsize $\text{y[B]}_{t-2}$}&{\scriptsize 2}&{\scriptsize -0.94}\\
    {\scriptsize $\text{y[C]}_{t-2}$}&{\scriptsize 1.2}&{\scriptsize 2.7}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -2.4}&{\scriptsize -1.6}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 2.9}&{\scriptsize 1.1}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize 0.28}&{\scriptsize 1.3}\\
    {\scriptsize $\text{y[B]}_{t-2}$}&{\scriptsize 2.6}&{\scriptsize 0.86}\\
    {\scriptsize $\text{y[C]}_{t-2}$}&{\scriptsize -8.3}&{\scriptsize 3}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -1.1}&{\scriptsize -1.5}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 1.8}&{\scriptsize 0.31}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize 0.54}&{\scriptsize 2.2}\\
    {\scriptsize $\text{y[B]}_{t-2}$}&{\scriptsize 1.3}&{\scriptsize -0.025}\\
    {\scriptsize $\text{y[C]}_{t-2}$}&{\scriptsize -0.54}&{\scriptsize 2.4}\\
    , align=ccc]]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -3.2}&{\scriptsize -3.1}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 2}&{\scriptsize 0.54}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize 0.94}&{\scriptsize 2.7}\\
    {\scriptsize $\text{y[B]}_{t-2}$}&{\scriptsize 2.5}&{\scriptsize 1.7}\\
    {\scriptsize $\text{y[C]}_{t-2}$}&{\scriptsize 2.5}&{\scriptsize 3.8}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -2}&{\scriptsize -3.9}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 2}&{\scriptsize 0.98}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize 0.18}&{\scriptsize 2.2}\\
    {\scriptsize $\text{y[B]}_{t-2}$}&{\scriptsize 1.8}&{\scriptsize 2.3}\\
    {\scriptsize $\text{y[C]}_{t-2}$}&{\scriptsize 1}&{\scriptsize 4.3}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -5}&{\scriptsize -16}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 4.3}&{\scriptsize 4.8}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize 3.7}&{\scriptsize 5.1}\\
    {\scriptsize $\text{y[B]}_{t-2}$}&{\scriptsize 2.8}&{\scriptsize 12}\\
    {\scriptsize $\text{y[C]}_{t-2}$}&{\scriptsize 4.3}&{\scriptsize 16}\\
    , align=ccc]]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -3.2}&{\scriptsize -2.6}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 2}&{\scriptsize 0.93}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize 1.5}&{\scriptsize 1.9}\\
    {\scriptsize $\text{y[B]}_{t-2}$}&{\scriptsize 5}&{\scriptsize 1.8}\\
    {\scriptsize $\text{y[C]}_{t-2}$}&{\scriptsize 2.4}&{\scriptsize 2.1}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -0.83}&{\scriptsize -1.2}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 1.5}&{\scriptsize 1.2}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize 1.4}&{\scriptsize 3}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\scriptsize A}&\textbf{{\scriptsize B}}&\textbf{{\scriptsize C}}\\{\scriptsize (I)}&{\scriptsize -3.4}&{\scriptsize -4}\\
    {\scriptsize $\text{y[B]}_{t-1}$}&{\scriptsize 0.87}&{\scriptsize 0.88}\\
    {\scriptsize $\text{y[C]}_{t-1}$}&{\scriptsize 0.9}&{\scriptsize 2.4}\\
    {\scriptsize $\text{y[B]}_{t-2}$}&{\scriptsize 5.5}&{\scriptsize 4.1}\\
    {\scriptsize $\text{y[C]}_{t-2}$}&{\scriptsize 2.8}&{\scriptsize 3.2}\\
    , align=ccc]]]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{A}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -1.4}&{\small -1.3}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.78}\\
    {\small $\text{y[C]}_{t-1}$}&{\small -0.27}&{\small 1.2}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2}&{\small -0.94}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 1.2}&{\small 2.7}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -2.4}&{\small -1.6}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2.9}&{\small 1.1}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.28}&{\small 1.3}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2.6}&{\small 0.86}\\
    {\small $\text{y[C]}_{t-2}$}&{\small -8.3}&{\small 3}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -1.1}&{\small -1.5}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.8}&{\small 0.31}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.54}&{\small 2.2}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 1.3}&{\small -0.025}\\
    {\small $\text{y[C]}_{t-2}$}&{\small -0.54}&{\small 2.4}\\
    , align=ccc]]
    [\textbf{B}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -3.2}&{\small -3.1}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.54}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.94}&{\small 2.7}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2.5}&{\small 1.7}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 2.5}&{\small 3.8}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -2}&{\small -3.9}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.98}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.18}&{\small 2.2}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 1.8}&{\small 2.3}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 1}&{\small 4.3}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -5}&{\small -16}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 4.3}&{\small 4.8}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 3.7}&{\small 5.1}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 2.8}&{\small 12}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 4.3}&{\small 16}\\
    , align=ccc]]
    [\textbf{C}
    [\multicolumn{3}{c}{\textbf{A}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -3.2}&{\small -2.6}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 2}&{\small 0.93}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 1.5}&{\small 1.9}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 5}&{\small 1.8}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 2.4}&{\small 2.1}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{B}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -0.83}&{\small -1.2}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 1.5}&{\small 1.2}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 1.4}&{\small 3}\\
    , align=ccc]
    [\multicolumn{3}{c}{\textbf{C}}\\\hline
    {\small A}&\textbf{{\small B}}&\textbf{{\small C}}\\{\small (I)}&{\small -3.4}&{\small -4}\\
    {\small $\text{y[B]}_{t-1}$}&{\small 0.87}&{\small 0.88}\\
    {\small $\text{y[C]}_{t-1}$}&{\small 0.9}&{\small 2.4}\\
    {\small $\text{y[B]}_{t-2}$}&{\small 5.5}&{\small 4.1}\\
    {\small $\text{y[C]}_{t-2}$}&{\small 2.8}&{\small 3.2}\\
    , align=ccc]]]
    \end{forest}

# draw handles cases when levels have been dropped LaTeX

    \begin{forest}
    [\textbf{$\epsilon$}
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {\small (I)}&{\small 20}\\
    {\small $\text{y[2]}_{t-1}$}&{\small -20}\\
    {\small $\text{y[3]}_{t-1}$}&{\small -40}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -20}\\
    {\small $\text{z}_{t-1}$}&{\small 4}\\, align=cc]
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
    {\small $\text{z}_{t-2}$}&{\small -1}\\, align=cc]]]
    \end{forest}

---

    \begin{forest}
      for tree={grow'=west}
    [\textbf{$\epsilon$}
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {\small 0}&\textbf{{\small 1}}\\{\small (I)}&{\small 17}\\
    {\small $\text{y[2]}_{t-1}$}&{\small -20}\\
    {\small $\text{y[3]}_{t-1}$}&{\small -39}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -19}\\
    {\small $\text{z}_{t-1}$}&{\small 3.6}\\, align=cc]
    [\textbf{1}
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {\small 0}&\textbf{{\small 1}}\\{\small (I)}&{\small -4}\\
    {\small $\text{y[2]}_{t-1}$}&{\small 19}\\
    {\small $\text{y[3]}_{t-1}$}&{\small -1.3}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -20}\\
    {\small $\text{z}_{t-1}$}&{\small 2.4}\\
    {\small $\text{y[2]}_{t-2}$}&{\small 1.3}\\
    {\small $\text{y[3]}_{t-2}$}&{\small 0}\\
    {\small $\text{y[4]}_{t-2}$}&{\small 1.2}\\
    {\small $\text{z}_{t-2}$}&{\small 2.5}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{1}}\\\hline
    {\small 0}&\textbf{{\small 1}}\\{\small (I)}&{\small -3.6}\\
    {\small $\text{y[2]}_{t-1}$}&{\small 21}\\
    {\small $\text{y[3]}_{t-1}$}&{\small 0.81}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -20}\\
    {\small $\text{z}_{t-1}$}&{\small 6.2}\\
    {\small $\text{y[2]}_{t-2}$}&{\small -0.57}\\
    {\small $\text{y[3]}_{t-2}$}&{\small 1.9}\\
    {\small $\text{y[4]}_{t-2}$}&{\small 0}\\
    {\small $\text{z}_{t-2}$}&{\small -0.99}\\, align=cc]]]
    \end{forest}

# draw handles cases when multinom is used for two states time series LaTeX

    \begin{forest}
    [\textbf{$\epsilon$}
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {\small (I)}&{\small 8}\\
    {\small $\text{y[2]}_{t-1}$}&{\small -10}\\
    {\small $\text{y[3]}_{t-1}$}&{\small -30}\\
    {\small $\text{y[4]}_{t-1}$}&{\small -10}\\
    {\small $\text{z}_{t-1}$}&{\small 4}\\, align=cc]
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
    {\small $\text{z}_{t-2}$}&{\small -1}\\, align=cc]]]
    \end{forest}

---

    \begin{small}
    \begin{forest}
    [\textbf{$\epsilon$}
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    0&\textbf{1}\\(I)&8\\
    $\text{y[2]}_{t-1}$&-10\\
    $\text{y[3]}_{t-1}$&-30\\
    $\text{y[4]}_{t-1}$&-10\\
    $\text{z}_{t-1}$&4\\, align=cc]
    [\textbf{1}
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    0&\textbf{1}\\(I)&-4\\
    $\text{y[2]}_{t-1}$&9\\
    $\text{y[3]}_{t-1}$&-1\\
    $\text{y[4]}_{t-1}$&-10\\
    $\text{z}_{t-1}$&2\\
    $\text{y[2]}_{t-2}$&1\\
    $\text{y[3]}_{t-2}$&0\\
    $\text{y[4]}_{t-2}$&1\\
    $\text{z}_{t-2}$&3\\, align=cc]
    [\multicolumn{2}{c}{\textbf{1}}\\\hline
    0&\textbf{1}\\(I)&-4\\
    $\text{y[2]}_{t-1}$&10\\
    $\text{y[3]}_{t-1}$&0.8\\
    $\text{y[4]}_{t-1}$&-10\\
    $\text{z}_{t-1}$&6\\
    $\text{y[2]}_{t-2}$&-0.6\\
    $\text{y[3]}_{t-2}$&2\\
    $\text{y[4]}_{t-2}$&0\\
    $\text{z}_{t-2}$&-1\\, align=cc]]]
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
    {\small (0,1.28]}&\textbf{{\small (1.28,7.54]}}\\{\small (I)}&{\small -3}\\
    {\small $\text{day\_night[TRUE]}_{t-1}$}&{\small -0.43}\\
    {\small $\text{day\_night[TRUE]}_{t-2}$}&{\small -0.64}\\
    {\small $\text{day\_night[TRUE]}_{t-3}$}&{\small 1.5}\\
    {\small $\text{day\_night[TRUE]}_{t-4}$}&{\small -2.3}\\
    {\small $\text{day\_night[TRUE]}_{t-5}$}&{\small 2.6}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small 0.91}&{\small 0.093}, align=cc]]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small 0.89}&{\small 0.11}, align=cc]]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small (0,1.28]}&\textbf{{\small (1.28,7.54]}}\\{\small (I)}&{\small -2.7}\\
    {\small $\text{day\_night[TRUE]}_{t-1}$}&{\small 1.3}\\, align=cc]]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small 0.84}&{\small 0.16}, align=cc]]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\small 0.17}&{\small 0.83}, align=cc]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\small 0.15}&{\small 0.85}, align=cc]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\small (0,1.28]}&\textbf{{\small (1.28,7.54]}}\\{\small (I)}&{\small 0.99}\\
    {\small $\text{day\_night[TRUE]}_{t-1}$}&{\small 1.3}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small 0.056}&{\small 0.94}, align=cc]]]]]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\tiny (0,1.28]}&\textbf{{\tiny (1.28,7.54]}}\\{\tiny (I)}&{\tiny -3}\\
    {\tiny $\text{day\_night[TRUE]}_{t-1}$}&{\tiny -0.43}\\
    {\tiny $\text{day\_night[TRUE]}_{t-2}$}&{\tiny -0.64}\\
    {\tiny $\text{day\_night[TRUE]}_{t-3}$}&{\tiny 1.5}\\
    {\tiny $\text{day\_night[TRUE]}_{t-4}$}&{\tiny -2.3}\\
    {\tiny $\text{day\_night[TRUE]}_{t-5}$}&{\tiny 2.6}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny 0.91}&{\tiny 0.093}, align=cc]]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny 0.89}&{\tiny 0.11}, align=cc]]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny (0,1.28]}&\textbf{{\tiny (1.28,7.54]}}\\{\tiny (I)}&{\tiny -2.7}\\
    {\tiny $\text{day\_night[TRUE]}_{t-1}$}&{\tiny 1.3}\\, align=cc]]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny 0.84}&{\tiny 0.16}, align=cc]]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\tiny 0.17}&{\tiny 0.83}, align=cc]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\tiny 0.15}&{\tiny 0.85}, align=cc]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\tiny (0,1.28]}&\textbf{{\tiny (1.28,7.54]}}\\{\tiny (I)}&{\tiny 0.99}\\
    {\tiny $\text{day\_night[TRUE]}_{t-1}$}&{\tiny 1.3}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny 0.056}&{\tiny 0.94}, align=cc]]]]]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\small (0,1.28]}&\textbf{{\small (1.28,7.54]}}\\{\small (I)}&{\small -3}\\
    {\small $\text{day\_night[TRUE]}_{t-1}$}&{\small -0.43}\\
    {\small $\text{day\_night[TRUE]}_{t-2}$}&{\small -0.64}\\
    {\small $\text{day\_night[TRUE]}_{t-3}$}&{\small 1.5}\\
    {\small $\text{day\_night[TRUE]}_{t-4}$}&{\small -2.3}\\
    {\small $\text{day\_night[TRUE]}_{t-5}$}&{\small 2.6}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small 0.91}&{\small 0.093}, align=cc]]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small 0.89}&{\small 0.11}, align=cc]]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small (0,1.28]}&\textbf{{\small (1.28,7.54]}}\\{\small (I)}&{\small -2.7}\\
    {\small $\text{day\_night[TRUE]}_{t-1}$}&{\small 1.3}\\, align=cc]]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small 0.84}&{\small 0.16}, align=cc]]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\small 0.17}&{\small 0.83}, align=cc]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\small 0.15}&{\small 0.85}, align=cc]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\small (0,1.28]}&\textbf{{\small (1.28,7.54]}}\\{\small (I)}&{\small 0.99}\\
    {\small $\text{day\_night[TRUE]}_{t-1}$}&{\small 1.3}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\small 0.056}&{\small 0.94}, align=cc]]]]]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\textbf{(0,1.28]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\tiny (0,1.28]}&\textbf{{\tiny (1.28,7.54]}}\\{\tiny (I)}&{\tiny -3}\\
    {\tiny $\text{day\_night[TRUE]}_{t-1}$}&{\tiny -0.43}\\
    {\tiny $\text{day\_night[TRUE]}_{t-2}$}&{\tiny -0.64}\\
    {\tiny $\text{day\_night[TRUE]}_{t-3}$}&{\tiny 1.5}\\
    {\tiny $\text{day\_night[TRUE]}_{t-4}$}&{\tiny -2.3}\\
    {\tiny $\text{day\_night[TRUE]}_{t-5}$}&{\tiny 2.6}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny 0.91}&{\tiny 0.093}, align=cc]]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny 0.89}&{\tiny 0.11}, align=cc]]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny (0,1.28]}&\textbf{{\tiny (1.28,7.54]}}\\{\tiny (I)}&{\tiny -2.7}\\
    {\tiny $\text{day\_night[TRUE]}_{t-1}$}&{\tiny 1.3}\\, align=cc]]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny 0.84}&{\tiny 0.16}, align=cc]]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\tiny 0.17}&{\tiny 0.83}, align=cc]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\tiny 0.15}&{\tiny 0.85}, align=cc]
    [\textbf{(1.28,7.54]}
    [\multicolumn{2}{c}{\textbf{(0,1.28]}}\\\hline
    {\tiny (0,1.28]}&\textbf{{\tiny (1.28,7.54]}}\\{\tiny (I)}&{\tiny 0.99}\\
    {\tiny $\text{day\_night[TRUE]}_{t-1}$}&{\tiny 1.3}\\, align=cc]
    [\multicolumn{2}{c}{\textbf{(1.28,7.54]}}\\\hline
    {\tiny 0.056}&{\tiny 0.94}, align=cc]]]]]
    \end{forest}

