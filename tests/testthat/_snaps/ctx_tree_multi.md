# multi_ctx_free implements the ctx_tree interface

    Code
      print(mctx)
    Output
      Context tree on 1, 2 
       Number of contexts: 186 
       Maximum context length: 11 

---

    Code
      draw(mctx, frequency = "detailed")
    Output
      * (162,188)
      +-- 1 (75,78)
      |   +-- 1 (35,36)
      |   |   +-- 1 (17,18)
      |   |   |   +-- 1 (7,10)
      |   |   |   |   +-- 1 (1,6)
      |   |   |   |   |   '-- 2 (1,3)
      |   |   |   |   |       +-- 1 (1,1)
      |   |   |   |   |       |   '-- 2 (1,1)
      |   |   |   |   |       '-- 2 (0,2)
      |   |   |   |   '-- 2 (4,4)
      |   |   |   |       +-- 1 (2,1)
      |   |   |   |       |   '-- 2 (2,1)
      |   |   |   |       |       '-- 2 (1,1)
      |   |   |   |       |           '-- 2 (1,1)
      |   |   |   |       '-- 2 (2,3)
      |   |   |   |           +-- 1 (0,2)
      |   |   |   |           '-- 2 (1,1)
      |   |   |   '-- 2 (8,7)
      |   |   |       +-- 1 (3,3)
      |   |   |       |   '-- 2 (3,3)
      |   |   |       |       +-- 1 (1,3)
      |   |   |       |       |   '-- 2 (1,2)
      |   |   |       |       |       '-- 2 (1,2)
      |   |   |       |       |           '-- 2 (1,1)
      |   |   |       |       '-- 2 (2,0)
      |   |   |       |           '-- 2 (2,0)
      |   |   |       '-- 2 (5,2)
      |   |   |           +-- 1 (2,1)
      |   |   |           |   '-- 2 (1,1)
      |   |   |           '-- 2 (2,1)
      |   |   |               '-- 2 (1,1)
      |   |   '-- 2 (15,17)
      |   |       +-- 1 (6,6)
      |   |       |   +-- 1 (0,3)
      |   |       |   |   '-- 1 (0,2)
      |   |       |   '-- 2 (6,2)
      |   |       |       +-- 1 (4,0)
      |   |       |       |   '-- 2 (3,0)
      |   |       |       |       '-- 2 (3,0)
      |   |       |       |           '-- 2 (2,0)
      |   |       |       '-- 2 (2,2)
      |   |       |           '-- 2 (2,1)
      |   |       |               '-- 2 (1,1)
      |   |       '-- 2 (7,10)
      |   |           +-- 1 (3,5)
      |   |           |   +-- 1 (0,2)
      |   |           |   |   '-- 2 (0,2)
      |   |           |   |       '-- 2 (0,2)
      |   |           |   '-- 2 (2,3)
      |   |           |       '-- 2 (1,2)
      |   |           |           '-- 2 (0,2)
      |   |           '-- 2 (3,5)
      |   |               +-- 1 (1,2)
      |   |               |   '-- 2 (1,1)
      |   |               '-- 2 (2,2)
      |   |                   '-- 2 (1,2)
      |   |                       '-- 2 (1,2)
      |   |                           '-- 1 (1,1)
      |   '-- 2 (36,38)
      |       +-- 1 (14,15)
      |       |   +-- 1 (5,10)
      |       |   |   +-- 1 (2,6)
      |       |   |   |   +-- 1 (1,3)
      |       |   |   |   |   '-- 2 (1,2)
      |       |   |   |   |       '-- 2 (1,1)
      |       |   |   |   |           '-- 1 (1,1)
      |       |   |   |   '-- 2 (1,2)
      |       |   |   |       '-- 1 (1,1)
      |       |   |   |           '-- 2 (1,1)
      |       |   |   |               '-- 1 (1,1)
      |       |   |   |                   '-- 2 (1,1)
      |       |   |   |                       '-- 2 (1,1)
      |       |   |   '-- 2 (3,3)
      |       |   |       +-- 1 (1,3)
      |       |   |       |   '-- 1 (1,2)
      |       |   |       |       '-- 1 (1,1)
      |       |   |       '-- 2 (2,0)
      |       |   |           '-- 1 (2,0)
      |       |   '-- 2 (8,4)
      |       |       +-- 1 (4,0)
      |       |       |   '-- 2 (3,0)
      |       |       |       '-- 2 (3,0)
      |       |       |           '-- 2 (2,0)
      |       |       '-- 2 (4,4)
      |       |           +-- 1 (1,1)
      |       |           '-- 2 (3,2)
      |       |               '-- 2 (2,1)
      |       '-- 2 (19,22)
      |           +-- 1 (9,5)
      |           |   +-- 1 (2,2)
      |           |   |   '-- 2 (2,1)
      |           |   |       '-- 2 (2,0)
      |           |   '-- 2 (6,3)
      |           |       +-- 1 (2,2)
      |           |       |   '-- 1 (2,1)
      |           |       |       '-- 1 (1,1)
      |           |       |           '-- 1 (1,1)
      |           |       '-- 2 (3,1)
      |           |           '-- 2 (2,0)
      |           '-- 2 (9,14)
      |               +-- 1 (3,5)
      |               |   +-- 1 (1,2)
      |               |   |   '-- 2 (0,2)
      |               |   '-- 2 (2,2)
      |               |       +-- 1 (1,1)
      |               |       '-- 2 (1,1)
      |               '-- 2 (5,8)
      |                   +-- 1 (1,4)
      |                   |   +-- 1 (0,3)
      |                   |   |   '-- 1 (0,2)
      |                   |   '-- 2 (1,1)
      |                   |       '-- 2 (1,1)
      |                   |           '-- 2 (1,1)
      |                   |               '-- 1 (1,1)
      |                   '-- 2 (4,2)
      |                       '-- 2 (4,1)
      |                           '-- 1 (3,1)
      |                               +-- 1 (1,1)
      |                               |   '-- 1 (1,1)
      |                               |       '-- 2 (1,1)
      |                               '-- 2 (2,0)
      |                                   '-- 2 (2,0)
      '-- 2 (79,98)
          +-- 1 (33,39)
          |   +-- 1 (17,17)
          |   |   +-- 1 (9,8)
          |   |   |   +-- 1 (5,4)
          |   |   |   |   +-- 1 (2,4)
          |   |   |   |   |   '-- 2 (1,2)
          |   |   |   |   |       '-- 2 (1,1)
          |   |   |   |   '-- 2 (3,0)
          |   |   |   |       '-- 2 (2,0)
          |   |   |   |           '-- 1 (2,0)
          |   |   |   '-- 2 (3,4)
          |   |   |       +-- 1 (2,1)
          |   |   |       |   '-- 2 (2,1)
          |   |   |       |       '-- 1 (2,1)
          |   |   |       |           '-- 2 (2,0)
          |   |   |       |               '-- 2 (2,0)
          |   |   |       '-- 2 (1,1)
          |   |   '-- 2 (7,9)
          |   |       +-- 1 (5,1)
          |   |       |   +-- 1 (3,0)
          |   |       |   |   '-- 1 (2,0)
          |   |       |   '-- 2 (2,0)
          |   |       |       '-- 2 (2,0)
          |   |       '-- 2 (2,7)
          |   |           +-- 1 (2,3)
          |   |           |   +-- 1 (1,1)
          |   |           |   |   '-- 2 (1,1)
          |   |           |   |       '-- 2 (1,1)
          |   |           |   '-- 2 (1,2)
          |   |           |       '-- 2 (1,1)
          |   |           |           '-- 2 (1,1)
          |   |           '-- 2 (0,4)
          |   |               '-- 2 (0,2)
          |   |                   '-- 2 (0,2)
          |   |                       '-- 2 (0,2)
          |   '-- 2 (14,20)
          |       +-- 1 (6,8)
          |       |   +-- 1 (3,6)
          |       |   |   +-- 1 (1,5)
          |       |   |   |   +-- 1 (0,3)
          |       |   |   |   |   '-- 2 (0,2)
          |       |   |   |   '-- 2 (1,1)
          |       |   |   '-- 2 (1,1)
          |       |   |       '-- 1 (1,1)
          |       |   |           '-- 1 (1,1)
          |       |   '-- 2 (3,1)
          |       |       '-- 2 (3,1)
          |       |           '-- 2 (2,0)
          |       '-- 2 (8,11)
          |           +-- 1 (2,3)
          |           |   +-- 1 (1,1)
          |           |   '-- 2 (1,2)
          |           |       '-- 1 (1,1)
          |           '-- 2 (5,6)
          |               +-- 1 (1,3)
          |               |   '-- 1 (1,1)
          |               |       '-- 2 (1,1)
          |               '-- 2 (3,3)
          |                   '-- 1 (1,2)
          |                       '-- 1 (1,2)
          |                           '-- 1 (0,2)
          '-- 2 (42,51)
              +-- 1 (14,21)
              |   +-- 1 (4,10)
              |   |   +-- 1 (1,6)
              |   |   |   +-- 1 (1,2)
              |   |   |   |   '-- 1 (1,2)
              |   |   |   |       '-- 2 (1,1)
              |   |   |   '-- 2 (0,4)
              |   |   '-- 2 (3,4)
              |   |       '-- 2 (2,3)
              |   |           +-- 1 (1,1)
              |   |           |   '-- 2 (1,1)
              |   |           '-- 2 (1,2)
              |   |               '-- 2 (1,1)
              |   |                   '-- 2 (1,1)
              |   |                       '-- 2 (1,1)
              |   '-- 2 (9,10)
              |       +-- 1 (4,4)
              |       |   '-- 1 (3,3)
              |       |       '-- 1 (2,3)
              |       |           '-- 1 (2,1)
              |       |               '-- 2 (1,1)
              |       '-- 2 (4,6)
              |           +-- 1 (1,2)
              |           |   '-- 2 (1,1)
              |           '-- 2 (2,3)
              |               +-- 1 (1,2)
              |               '-- 2 (1,1)
              '-- 2 (24,26)
                  +-- 1 (9,12)
                  |   +-- 1 (4,6)
                  |   |   +-- 1 (1,5)
                  |   |   |   +-- 1 (0,2)
                  |   |   |   |   '-- 1 (0,2)
                  |   |   |   '-- 2 (1,3)
                  |   |   '-- 2 (3,1)
                  |   |       '-- 2 (2,1)
                  |   |           '-- 2 (1,1)
                  |   '-- 2 (4,6)
                  |       +-- 1 (2,2)
                  |       |   '-- 1 (1,2)
                  |       |       '-- 1 (1,2)
                  |       '-- 2 (2,4)
                  |           +-- 1 (1,1)
                  |           '-- 2 (1,2)
                  |               '-- 1 (0,2)
                  '-- 2 (13,12)
                      +-- 1 (5,6)
                      |   +-- 1 (3,3)
                      |   |   '-- 1 (2,3)
                      |   |       +-- 1 (1,1)
                      |   |       |   '-- 1 (1,1)
                      |   |       '-- 2 (1,2)
                      |   '-- 2 (2,3)
                      |       '-- 2 (2,2)
                      |           '-- 2 (2,0)
                      |               '-- 1 (2,0)
                      '-- 2 (6,6)
                          +-- 1 (1,5)
                          |   +-- 1 (0,3)
                          |   |   '-- 1 (0,3)
                          |   |       '-- 2 (0,2)
                          |   '-- 2 (1,2)
                          |       '-- 2 (0,2)
                          '-- 2 (5,1)
                              '-- 1 (4,1)
                                  +-- 1 (2,1)
                                  |   '-- 1 (2,1)
                                  |       '-- 2 (2,0)
                                  '-- 2 (2,0)
                                      '-- 2 (2,0)
