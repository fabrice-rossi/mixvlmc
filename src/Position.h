#ifndef POSITION_H
#define POSITION_H
#include "EdgeNode.h"

//' @name Position
//' Subsequence match position representation
//'
//' An of Position gives the complete position of a subsequence matched by
//' a SuffixTree as a pair: the EdgeNode and the position on the edge is the
//' match is implicit.
//'
//' @noRd
struct Position {
  EdgeNode* node;
  int edge;
  Position() : node(nullptr), edge(-1) {}

  Position(EdgeNode* node_, int edge_) : node(node_), edge(edge_) {}

  bool is_valid() const { return node != nullptr; }
};

#endif
