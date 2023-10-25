# context objects are printed as expected

    Code
      print(find_sequence(dts_ctree, c(0, 0)))
    Output
      Context [T]: 0, 0 
       followed by 0 (0), 1 (1)

---

    Code
      print(find_sequence(dts_ctree, c(1, 0)))
    Output
      Sequence [T]: 1, 0 
       followed by 0 (1), 1 (1)

---

    Code
      print(rev(find_sequence(dts_ctree, c(0, 0))))
    Output
      Context [R]: 0, 0 
       followed by 0 (0), 1 (1)

---

    Code
      print(rev(find_sequence(dts_ctree, c(1, 0))))
    Output
      Sequence [R]: 0, 1 
       followed by 0 (1), 1 (1)

