#ifndef EXTRACTIONCONTENT_H
#define EXTRACTIONCONTENT_H

// specifies what needs to be extracted from a node
struct ExtractionContent {
  bool with_position;
  bool with_cutoff;
  bool with_local_counts;
  bool with_metrics;
  ExtractionContent(bool pos, bool cut, bool lcounts, bool metrics)
      : with_position(pos),
        with_cutoff(cut),
        with_local_counts(lcounts),
        with_metrics(metrics) {}
};

#endif
