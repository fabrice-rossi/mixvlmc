# draw obeys its contract

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

    x [5,5]
    *-> 0 [1,3]
    ^   *-> 0 [0,1]
    ^   °-> 1 [1,1]
    °-> 1 [3,2]
        *-> 0 [2,1]
        °-> 1 [1,1]
