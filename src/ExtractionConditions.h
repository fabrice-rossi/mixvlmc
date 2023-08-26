#ifndef EXTRACTIONCONDITIONS_H
#define EXTRACTIONCONDITIONS_H

// specifies what sequences are looked for
struct ExtractionConditions {
  int min_counts;
  int max_length;
  bool only_ctx;

  ExtractionConditions(int mc, int ml, bool oc)
      : min_counts(mc), max_length(ml), only_ctx(oc) {}
};

#endif
