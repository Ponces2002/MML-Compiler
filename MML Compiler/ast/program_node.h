#ifndef __MML_AST_PROGRAM_NODE_H__
#define __MML_AST_PROGRAM_NODE_H__

#include <cdk/ast/basic_node.h>
#include "ast/block_node.h"

namespace mml {

  class program_node: public cdk::basic_node {
    mml::block_node *_main;

  public:
    inline program_node(int lineno, mml::block_node *main) :
        cdk::basic_node(lineno), _main(main) {
    }

  public:
    inline mml::block_node *main() {
      return _main;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_program_node(this, level);
    }

  };

} // mml

#endif
