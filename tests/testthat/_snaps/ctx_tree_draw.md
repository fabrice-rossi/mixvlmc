# draw obeys its contract for default text output

    *
    +-- 0
    |   +-- 0
    |   '-- 1
    '-- 1
        +-- 0
        '-- 1

---

    * (10)
    +-- 0 (4)
    |   +-- 0 (1)
    |   '-- 1 (2)
    '-- 1 (5)
        +-- 0 (3)
        '-- 1 (2)

---

    * (5,5)
    +-- 0 (1,3)
    |   +-- 0 (0,1)
    |   '-- 1 (1,1)
    '-- 1 (3,2)
        +-- 0 (2,1)
        '-- 1 (1,1)

---

    ⏹
    ├─ 0
    │  ├─ 0
    │  └─ 1
    └─ 1
       ├─ 0
       └─ 1

---

    ⏹ (10)
    ├─ 0 (4)
    │  ├─ 0 (1)
    │  └─ 1 (2)
    └─ 1 (5)
       ├─ 0 (3)
       └─ 1 (2)

---

    ⏹ (5,5)
    ├─ 0 (1,3)
    │  ├─ 0 (0,1)
    │  └─ 1 (1,1)
    └─ 1 (3,2)
       ├─ 0 (2,1)
       └─ 1 (1,1)

---

    x [5,5]
    *-> 0 [1,3]
    ^   *-> 0 [0,1]
    ^   °-> 1 [1,1]
    °-> 1 [3,2]
        *-> 0 [2,1]
        °-> 1 [1,1]

# draw obeys its contract for LaTeX output

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{0}
    [\textbf{0}]
    [\textbf{1}]
    ]
    [\textbf{1}
    [\textbf{0}]
    [\textbf{1}]
    ]
    ]
    \end{forest}

---

    \begin{forest}
    [{\textbf{$\epsilon$}}\\\hline
    {\small 10}, align=c
    [{\textbf{0}}\\\hline
    {\small 4}, align=c
    [{\textbf{0}}\\\hline
    {\small 1}, align=c]
    [{\textbf{1}}\\\hline
    {\small 2}, align=c]
    ]
    [{\textbf{1}}\\\hline
    {\small 5}, align=c
    [{\textbf{0}}\\\hline
    {\small 3}, align=c]
    [{\textbf{1}}\\\hline
    {\small 2}, align=c]
    ]
    ]
    \end{forest}

---

    \begin{forest}
    [\multicolumn{2}{c}{\textbf{$\epsilon$}}\\\hline
    {\small 5}&{\small 5}, align=cc
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {\small 1}&{\small 3}, align=cc
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {\small 0}&{\small 1}, align=cc]
    [\multicolumn{2}{c}{\textbf{1}}\\\hline
    {\small 1}&{\small 1}, align=cc]
    ]
    [\multicolumn{2}{c}{\textbf{1}}\\\hline
    {\small 3}&{\small 2}, align=cc
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {\small 2}&{\small 1}, align=cc]
    [\multicolumn{2}{c}{\textbf{1}}\\\hline
    {\small 1}&{\small 1}, align=cc]
    ]
    ]
    \end{forest}

---

    \begin{forest}
    [{\textbf{$\epsilon$} {\small (5,5)}}
    [{\textbf{0} {\small (1,3)}}
    [{\textbf{0} {\small (0,1)}}]
    [{\textbf{1} {\small (1,1)}}]
    ]
    [{\textbf{1} {\small (3,2)}}
    [{\textbf{0} {\small (2,1)}}]
    [{\textbf{1} {\small (1,1)}}]
    ]
    ]
    \end{forest}

---

    \begin{small}
    \begin{forest}
    [{\textbf{$\epsilon$} (5,5)}
    [{\textbf{0} (1,3)}
    [{\textbf{0} (0,1)}]
    [{\textbf{1} (1,1)}]
    ]
    [{\textbf{1} (3,2)}
    [{\textbf{0} (2,1)}]
    [{\textbf{1} (1,1)}]
    ]
    ]
    \end{forest}
    \end{small}

---

    \begin{small}
    \begin{forest}
      for tree={draw, grow'=west}[{\textbf{$\epsilon$} (5,5)}
    [{\textbf{0} (1,3)}
    [{\textbf{0} (0,1)}]
    [{\textbf{1} (1,1)}]
    ]
    [{\textbf{1} (3,2)}
    [{\textbf{0} (2,1)}]
    [{\textbf{1} (1,1)}]
    ]
    ]
    \end{forest}
    \end{small}

---

    \begin{small}
    \begin{forest}
      for tree={circle, draw, grow'=west}[\multicolumn{2}{c}{\textbf{$\epsilon$}}\\\hline
    {\scriptsize 5}&{\scriptsize 5}, align=cc
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {\scriptsize 1}&{\scriptsize 3}, align=cc
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {\scriptsize 0}&{\scriptsize 1}, align=cc]
    [\multicolumn{2}{c}{\textbf{1}}\\\hline
    {\scriptsize 1}&{\scriptsize 1}, align=cc]
    ]
    [\multicolumn{2}{c}{\textbf{1}}\\\hline
    {\scriptsize 3}&{\scriptsize 2}, align=cc
    [\multicolumn{2}{c}{\textbf{0}}\\\hline
    {\scriptsize 2}&{\scriptsize 1}, align=cc]
    [\multicolumn{2}{c}{\textbf{1}}\\\hline
    {\scriptsize 1}&{\scriptsize 1}, align=cc]
    ]
    ]
    \end{forest}
    \end{small}

