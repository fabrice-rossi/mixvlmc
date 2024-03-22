# draw obeys its contract for default ascii output

    * (0.432, 0.367, 0.201)
    +-- 0 (0.58, 0.1856, 0.2343)
    |   '-- 2 (0.5072, 0.2754, 0.2174)
    |       '-- 2 (0.5833, 0.04167, 0.375)
    '-- 1 (0.3079, 0.5831, 0.109)
    |   +-- 1 (0.3037, 0.5981, 0.09813)
    |   |   '-- 0 (0.2609, 0.6739, 0.06522)
    |   |       '-- 1 (0, 0.625, 0.375)
    |   '-- 2 (0.2917, 0.5417, 0.1667)
    |       '-- 0 (0.2821, 0.5385, 0.1795)
    |           '-- 1 (0.2143, 0.7143, 0.07143)
    |               '-- 1 (0.1667, 0.75, 0.08333)
    |                   '-- 1 (0.2, 0.7, 0.1)
    |                       '-- 0 (1, 0, 0)
    '-- 2 (0.3433, 0.3582, 0.2985)
        '-- 0 (0.3267, 0.3861, 0.2871)
            +-- 0 (0.3333, 0.3889, 0.2778)
            |   '-- 2 (0.2222, 0.7778, 0)
            '-- 1 (0.25, 0.4375, 0.3125)
                '-- 2 (1, 0, 0)

---

    *
    +-- 0
    |   '-- 2
    |       '-- 2
    '-- 1
    |   +-- 1
    |   |   '-- 0
    |   |       '-- 1
    |   '-- 2
    |       '-- 0
    |           '-- 1
    |               '-- 1
    |                   '-- 1
    |                       '-- 0
    '-- 2
        '-- 0
            +-- 0
            |   '-- 2
            '-- 1
                '-- 2

---

    * (432, 367, 201)
    +-- 0 (250, 80, 101)
    |   '-- 2 (35, 19, 15)
    |       '-- 2 (14, 1, 9)
    '-- 1 (113, 214, 40)
    |   +-- 1 (65, 128, 21)
    |   |   '-- 0 (12, 31, 3)
    |   |       '-- 1 (0, 5, 3)
    |   '-- 2 (21, 39, 12)
    |       '-- 0 (11, 21, 7)
    |           '-- 1 (3, 10, 1)
    |               '-- 1 (2, 9, 1)
    |                   '-- 1 (2, 7, 1)
    |                       '-- 0 (2, 0, 0)
    '-- 2 (69, 72, 60)
        '-- 0 (33, 39, 29)
            +-- 0 (18, 21, 15)
            |   '-- 2 (2, 7, 0)
            '-- 1 (8, 14, 10)
                '-- 2 (3, 0, 0)

# draw obeys its contract for latex output

    \begin{forest}
    [\multicolumn{3}{c}{\textbf{$\epsilon$}}\\\hline
    {\small 0.432}&{\small 0.367}&{\small 0.201}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\small 0.58}&{\small 0.1856}&{\small 0.2343}, align=ccc
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\small 0.5072}&{\small 0.2754}&{\small 0.2174}, align=ccc
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\small 0.5833}&{\small 0.04167}&{\small 0.375}, align=ccc]
    ]
    ]
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\small 0.3079}&{\small 0.5831}&{\small 0.109}, align=ccc
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\small 0.3037}&{\small 0.5981}&{\small 0.09813}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\small 0.2609}&{\small 0.6739}&{\small 0.06522}, align=ccc
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\small 0}&{\small 0.625}&{\small 0.375}, align=ccc]
    ]
    ]
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\small 0.2917}&{\small 0.5417}&{\small 0.1667}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\small 0.2821}&{\small 0.5385}&{\small 0.1795}, align=ccc
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\small 0.2143}&{\small 0.7143}&{\small 0.07143}, align=ccc
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\small 0.1667}&{\small 0.75}&{\small 0.08333}, align=ccc
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\small 0.2}&{\small 0.7}&{\small 0.1}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\small 1}&{\small 0}&{\small 0}, align=ccc]
    ]
    ]
    ]
    ]
    ]
    ]
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\small 0.3433}&{\small 0.3582}&{\small 0.2985}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\small 0.3267}&{\small 0.3861}&{\small 0.2871}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\small 0.3333}&{\small 0.3889}&{\small 0.2778}, align=ccc
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\small 0.2222}&{\small 0.7778}&{\small 0}, align=ccc]
    ]
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\small 0.25}&{\small 0.4375}&{\small 0.3125}, align=ccc
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\small 1}&{\small 0}&{\small 0}, align=ccc]
    ]
    ]
    ]
    ]
    \end{forest}

---

    \begin{forest}
    [\textbf{$\epsilon$}
    [\textbf{0}
    [\textbf{2}
    [\textbf{2}]
    ]
    ]
    [\textbf{1}
    [\textbf{1}
    [\textbf{0}
    [\textbf{1}]
    ]
    ]
    [\textbf{2}
    [\textbf{0}
    [\textbf{1}
    [\textbf{1}
    [\textbf{1}
    [\textbf{0}]
    ]
    ]
    ]
    ]
    ]
    ]
    [\textbf{2}
    [\textbf{0}
    [\textbf{0}
    [\textbf{2}]
    ]
    [\textbf{1}
    [\textbf{2}]
    ]
    ]
    ]
    ]
    \end{forest}

---

    \begin{forest}
    [\multicolumn{3}{c}{\textbf{$\epsilon$}}\\\hline
    {\small 432}&{\small 367}&{\small 201}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\small 250}&{\small 80}&{\small 101}, align=ccc
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\small 35}&{\small 19}&{\small 15}, align=ccc
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\small 14}&{\small 1}&{\small 9}, align=ccc]
    ]
    ]
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\small 113}&{\small 214}&{\small 40}, align=ccc
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\small 65}&{\small 128}&{\small 21}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\small 12}&{\small 31}&{\small 3}, align=ccc
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\small 0}&{\small 5}&{\small 3}, align=ccc]
    ]
    ]
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\small 21}&{\small 39}&{\small 12}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\small 11}&{\small 21}&{\small 7}, align=ccc
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\small 3}&{\small 10}&{\small 1}, align=ccc
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\small 2}&{\small 9}&{\small 1}, align=ccc
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\small 2}&{\small 7}&{\small 1}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\small 2}&{\small 0}&{\small 0}, align=ccc]
    ]
    ]
    ]
    ]
    ]
    ]
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\small 69}&{\small 72}&{\small 60}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\small 33}&{\small 39}&{\small 29}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\small 18}&{\small 21}&{\small 15}, align=ccc
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\small 2}&{\small 7}&{\small 0}, align=ccc]
    ]
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\small 8}&{\small 14}&{\small 10}, align=ccc
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\small 3}&{\small 0}&{\small 0}, align=ccc]
    ]
    ]
    ]
    ]
    \end{forest}

---

    \begin{forest}
    [{\textbf{$\epsilon$} {\small (0.432,0.367,0.201)}}
    [{\textbf{0} {\small (0.58,0.1856,0.2343)}}
    [{\textbf{2} {\small (0.5072,0.2754,0.2174)}}
    [{\textbf{2} {\small (0.5833,0.04167,0.375)}}]
    ]
    ]
    [{\textbf{1} {\small (0.3079,0.5831,0.109)}}
    [{\textbf{1} {\small (0.3037,0.5981,0.09813)}}
    [{\textbf{0} {\small (0.2609,0.6739,0.06522)}}
    [{\textbf{1} {\small (0,0.625,0.375)}}]
    ]
    ]
    [{\textbf{2} {\small (0.2917,0.5417,0.1667)}}
    [{\textbf{0} {\small (0.2821,0.5385,0.1795)}}
    [{\textbf{1} {\small (0.2143,0.7143,0.07143)}}
    [{\textbf{1} {\small (0.1667,0.75,0.08333)}}
    [{\textbf{1} {\small (0.2,0.7,0.1)}}
    [{\textbf{0} {\small (1,0,0)}}]
    ]
    ]
    ]
    ]
    ]
    ]
    [{\textbf{2} {\small (0.3433,0.3582,0.2985)}}
    [{\textbf{0} {\small (0.3267,0.3861,0.2871)}}
    [{\textbf{0} {\small (0.3333,0.3889,0.2778)}}
    [{\textbf{2} {\small (0.2222,0.7778,0)}}]
    ]
    [{\textbf{1} {\small (0.25,0.4375,0.3125)}}
    [{\textbf{2} {\small (1,0,0)}}]
    ]
    ]
    ]
    ]
    \end{forest}

---

    \begin{small}
    \begin{forest}
    [{\textbf{$\epsilon$} (0.432,0.367,0.201)}
    [{\textbf{0} (0.58,0.1856,0.2343)}
    [{\textbf{2} (0.5072,0.2754,0.2174)}
    [{\textbf{2} (0.5833,0.04167,0.375)}]
    ]
    ]
    [{\textbf{1} (0.3079,0.5831,0.109)}
    [{\textbf{1} (0.3037,0.5981,0.09813)}
    [{\textbf{0} (0.2609,0.6739,0.06522)}
    [{\textbf{1} (0,0.625,0.375)}]
    ]
    ]
    [{\textbf{2} (0.2917,0.5417,0.1667)}
    [{\textbf{0} (0.2821,0.5385,0.1795)}
    [{\textbf{1} (0.2143,0.7143,0.07143)}
    [{\textbf{1} (0.1667,0.75,0.08333)}
    [{\textbf{1} (0.2,0.7,0.1)}
    [{\textbf{0} (1,0,0)}]
    ]
    ]
    ]
    ]
    ]
    ]
    [{\textbf{2} (0.3433,0.3582,0.2985)}
    [{\textbf{0} (0.3267,0.3861,0.2871)}
    [{\textbf{0} (0.3333,0.3889,0.2778)}
    [{\textbf{2} (0.2222,0.7778,0)}]
    ]
    [{\textbf{1} (0.25,0.4375,0.3125)}
    [{\textbf{2} (1,0,0)}]
    ]
    ]
    ]
    ]
    \end{forest}
    \end{small}

---

    \begin{small}
    \begin{forest}
      for tree={draw, grow'=west}[{\textbf{$\epsilon$} (0.432,0.367,0.201)}
    [{\textbf{0} (0.58,0.1856,0.2343)}
    [{\textbf{2} (0.5072,0.2754,0.2174)}
    [{\textbf{2} (0.5833,0.04167,0.375)}]
    ]
    ]
    [{\textbf{1} (0.3079,0.5831,0.109)}
    [{\textbf{1} (0.3037,0.5981,0.09813)}
    [{\textbf{0} (0.2609,0.6739,0.06522)}
    [{\textbf{1} (0,0.625,0.375)}]
    ]
    ]
    [{\textbf{2} (0.2917,0.5417,0.1667)}
    [{\textbf{0} (0.2821,0.5385,0.1795)}
    [{\textbf{1} (0.2143,0.7143,0.07143)}
    [{\textbf{1} (0.1667,0.75,0.08333)}
    [{\textbf{1} (0.2,0.7,0.1)}
    [{\textbf{0} (1,0,0)}]
    ]
    ]
    ]
    ]
    ]
    ]
    [{\textbf{2} (0.3433,0.3582,0.2985)}
    [{\textbf{0} (0.3267,0.3861,0.2871)}
    [{\textbf{0} (0.3333,0.3889,0.2778)}
    [{\textbf{2} (0.2222,0.7778,0)}]
    ]
    [{\textbf{1} (0.25,0.4375,0.3125)}
    [{\textbf{2} (1,0,0)}]
    ]
    ]
    ]
    ]
    \end{forest}
    \end{small}

---

    \begin{small}
    \begin{forest}
      for tree={circle, draw, grow'=west}[\multicolumn{3}{c}{\textbf{$\epsilon$}}\\\hline
    {\scriptsize 0.432}&{\scriptsize 0.367}&{\scriptsize 0.201}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\scriptsize 0.58}&{\scriptsize 0.1856}&{\scriptsize 0.2343}, align=ccc
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\scriptsize 0.5072}&{\scriptsize 0.2754}&{\scriptsize 0.2174}, align=ccc
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\scriptsize 0.5833}&{\scriptsize 0.04167}&{\scriptsize 0.375}, align=ccc]
    ]
    ]
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\scriptsize 0.3079}&{\scriptsize 0.5831}&{\scriptsize 0.109}, align=ccc
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\scriptsize 0.3037}&{\scriptsize 0.5981}&{\scriptsize 0.09813}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\scriptsize 0.2609}&{\scriptsize 0.6739}&{\scriptsize 0.06522}, align=ccc
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\scriptsize 0}&{\scriptsize 0.625}&{\scriptsize 0.375}, align=ccc]
    ]
    ]
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\scriptsize 0.2917}&{\scriptsize 0.5417}&{\scriptsize 0.1667}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\scriptsize 0.2821}&{\scriptsize 0.5385}&{\scriptsize 0.1795}, align=ccc
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\scriptsize 0.2143}&{\scriptsize 0.7143}&{\scriptsize 0.07143}, align=ccc
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\scriptsize 0.1667}&{\scriptsize 0.75}&{\scriptsize 0.08333}, align=ccc
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\scriptsize 0.2}&{\scriptsize 0.7}&{\scriptsize 0.1}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\scriptsize 1}&{\scriptsize 0}&{\scriptsize 0}, align=ccc]
    ]
    ]
    ]
    ]
    ]
    ]
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\scriptsize 0.3433}&{\scriptsize 0.3582}&{\scriptsize 0.2985}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\scriptsize 0.3267}&{\scriptsize 0.3861}&{\scriptsize 0.2871}, align=ccc
    [\multicolumn{3}{c}{\textbf{0}}\\\hline
    {\scriptsize 0.3333}&{\scriptsize 0.3889}&{\scriptsize 0.2778}, align=ccc
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\scriptsize 0.2222}&{\scriptsize 0.7778}&{\scriptsize 0}, align=ccc]
    ]
    [\multicolumn{3}{c}{\textbf{1}}\\\hline
    {\scriptsize 0.25}&{\scriptsize 0.4375}&{\scriptsize 0.3125}, align=ccc
    [\multicolumn{3}{c}{\textbf{2}}\\\hline
    {\scriptsize 1}&{\scriptsize 0}&{\scriptsize 0}, align=ccc]
    ]
    ]
    ]
    ]
    \end{forest}
    \end{small}

