#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

static bool check_pointer_types(std::shared_ptr<cdk::basic_type> lt, std::shared_ptr<cdk::basic_type> rt) {
  std::shared_ptr<cdk::basic_type> left_type = lt;
  std::shared_ptr<cdk::basic_type> right_type = rt;
  while (right_type != nullptr && left_type->name() == cdk::TYPE_POINTER && right_type->name() == cdk::TYPE_POINTER) {
    left_type = cdk::reference_type::cast(left_type)->referenced();
    right_type = cdk::reference_type::cast(right_type)->referenced();
  }
  return rt == nullptr || lt->name() == rt->name();
}

void mml::type_checker::do_SubAddExpressions(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(node->left()->type());
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_POINTER)) {
    node->type(node->right()->type());
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) && node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("binary expression has wrong types");
  }
}

void mml::type_checker::do_MulDivExpressions(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) && node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong types in binary expression");
  }
}


//---------------------------------------------------------------------------

void mml::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for(size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

void mml::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (node->argument()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong type in unary logical expression");
  }
}

void mml::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}

void mml::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

//---------------------------------------------------------------------------

void mml::type_checker::processUnaryExpression(cdk::unary_operation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in argument of unary expression");
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_neg_node(cdk::neg_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void mml::type_checker::processBinaryExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in left argument of binary expression");

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in right argument of binary expression");

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  do_SubAddExpressions(node, lvl);
}
void mml::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  do_SubAddExpressions(node, lvl);
}
void mml::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  do_MulDivExpressions(node, lvl);
}
void mml::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  do_MulDivExpressions(node, lvl);
}
void mml::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}
void mml::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processBinaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  auto symbol = _symtab.find(id);
  if (symbol) {
    node->type(symbol->type());
  } else {
    throw id;
  }
}

void mml::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

void mml::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl + 4);
  node->rvalue()->accept(this, lvl + 4);

  if (node->lvalue()->is_typed(cdk::TYPE_INT)) {
    if (node->rvalue()->is_typed(cdk::TYPE_INT)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } 
    else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } 
    else {
      throw std::string("wrong assignment to integer");
    }
  }
  else if (node->lvalue()->is_typed(cdk::TYPE_DOUBLE)) {

    if (node->rvalue()->is_typed(cdk::TYPE_DOUBLE) || node->rvalue()->is_typed(cdk::TYPE_INT)) {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } 
    else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      node->rvalue()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    }
    
    else {
      throw std::string("wrong assignment to real");
    }
  } 
  else if (node->lvalue()->is_typed(cdk::TYPE_STRING)) {

    if (node->rvalue()->is_typed(cdk::TYPE_STRING)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
    } 
    else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
    } 
    else {
      throw std::string("wrong assignment to string");
    }

  }
  else if (node->lvalue()->is_typed(cdk::TYPE_FUNCTIONAL)) {

    if (node->rvalue()->is_typed(cdk::TYPE_FUNCTIONAL)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_FUNCTIONAL));
    } 
    else {
      throw std::string("wrong assignment to variable of type functional");
    }
  }
  else if (node->lvalue()->is_typed(cdk::TYPE_POINTER)) {

    if (node->rvalue()->is_typed(cdk::TYPE_POINTER)) {
      if (check_pointer_types(node->lvalue()->type(), node->rvalue()->type())) {
        node->type(node->rvalue()->type());
      }
    } else if (node->rvalue()->is_typed(cdk::TYPE_INT)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_POINTER));
    } else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
    } else {
      throw std::string("wrong assignment to pointer");
    }

  }
  else {
    throw std::string("wrong types in assignment");
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_program_node(mml::program_node *const node, int lvl) {
  _functionTypeR.push(cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_INT)));
  _inBlockReturnType = nullptr;
  node->main()->accept(this, lvl + 2);
}

void mml::type_checker::do_evaluation_node(mml::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void mml::type_checker::do_print_node(mml::print_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_input_node(mml::input_node *const node, int lvl) {
  node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_while_node(mml::while_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_if_node(mml::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

void mml::type_checker::do_if_else_node(mml::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_stop_node(mml::stop_node * const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_next_node(mml::next_node * const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_sizeof_node(mml::sizeof_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->expression()->accept(this, lvl + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_return_node(mml::return_node * const node, int lvl) {
  if (node->retval()) node->retval()->accept(this, lvl + 2);
}

void mml::type_checker::do_address_of_node(mml::address_of_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 2);
  node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}

void mml::type_checker::do_nullptr_node(mml::nullptr_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::reference_type::create(4, nullptr));
}

void mml::type_checker::do_block_node(mml::block_node * const node, int lvl) {
  //EMPTY
}

void mml::type_checker::do_declaration_node(mml::declaration_node * const node, int lvl) {
  if(node->initializer() != nullptr){
    node->initializer()->accept(this, lvl + 2);

    if(node->type() == nullptr) {
      node->type(node->initializer()->type());
    }
    else if (node->is_typed(cdk::TYPE_INT)) {
      if (!node->initializer()->is_typed(cdk::TYPE_INT)){
        throw std::string("initializer has wrong type (integer expected).");
      }
    } 
    else if (node->is_typed(cdk::TYPE_DOUBLE)) {
      if (!node->initializer()->is_typed(cdk::TYPE_INT) && !node->initializer()->is_typed(cdk::TYPE_DOUBLE)) {
        throw std::string("initializer has wrong type (integer or double expected).");
      }
    } 
    else if (node->is_typed(cdk::TYPE_STRING)) {
      if (!node->initializer()->is_typed(cdk::TYPE_STRING)) {
        throw std::string("initializer has wrong type (string expected).");
      }
    }
    else if (node->is_typed(cdk::TYPE_POINTER)) {
      if (!node->initializer()->is_typed(cdk::TYPE_POINTER)) {
        auto in = (cdk::literal_node<int>*)node->initializer();
        if (in == nullptr || in->value() != 0) throw std::string("initializer has wrong type (pointer expected).");
      }
    }
    else if (node->is_typed(cdk::TYPE_FUNCTIONAL)) {
      if (!((node->initializer()->is_typed(cdk::TYPE_FUNCTIONAL)))) {
        throw std::string("wrong type for initializer (function expected).");
      }
    }
    else {
      throw std::string("unknown type for initializer.");
    }
  }
  auto id = node->identifier();
  auto symbol = mml::make_symbol(false, node->qualifier(), node->type(), id, (bool)node->initializer(), false, false);
  std::shared_ptr<mml::symbol> prevDecl = _symtab.find_local(symbol->name());

  if (prevDecl) {
    if (prevDecl->type()->name() == symbol->type()->name()) {
      _symtab.replace(prevDecl->name(), symbol); 
    } 
    else if (prevDecl->type()->name() == cdk::TYPE_POINTER && symbol->type()->name() == cdk::TYPE_POINTER && check_pointer_types(prevDecl->type(), symbol->type())) {
      _symtab.replace(prevDecl->name(), symbol);
    } 
    else if (prevDecl->type()->name() == cdk::TYPE_FUNCTIONAL && symbol->type()->name() == cdk::TYPE_FUNCTIONAL) {
      _symtab.replace(prevDecl->name(), symbol);
    }
    else {
      throw std::string("variable redeclared: " + id);
    }
  } 
  else {
    _symtab.insert(id, symbol);
  }
  _parent->set_new_symbol(symbol);


}

void mml::type_checker::do_function_call_node(mml::function_call_node * const node, int lvl) {
  ASSERT_UNSPEC;
  if(node->function()){
    node->function()->accept(this, lvl + 2);

    if(!node->function()->is_typed(cdk::TYPE_FUNCTIONAL)) throw std::string("Not a function");

    auto function_type = cdk::functional_type::cast(node->function()->type());

    if(function_type->input_length() != node->arguments()->size()) throw std::string("worng size for input");

    node->arguments()->accept(this, lvl + 4);

    for (size_t i = 0; i < node->arguments()->size(); i++) {
      if (node->argument(i)->type() != function_type->input(i) && 
      (!node->argument(i)->is_typed(cdk::TYPE_INT) || function_type->input(i)->name() != cdk::TYPE_DOUBLE)){
          throw std::string("wrong type for argument " + std::to_string(i + 1));
      }
    }
    node->type(function_type->output(0));
  }
  else node->type(cdk::primitive_type::create(4, cdk::TYPE_INT)); //Recursion not done (prevent seg.fault in xml_writter)
}

void mml::type_checker::do_function_definition_node(mml::function_definition_node * const node, int lvl) {
  _inBlockReturnType = nullptr;
  auto funcRet = node->type();
  std::vector<std::shared_ptr<cdk::basic_type>> funcArgs;

  node->arguments()->accept(this, lvl + 2);
  for (size_t i = 0; i < node->arguments()->size(); i++){
    funcArgs.push_back(dynamic_cast<mml::declaration_node*>(node->arguments()->node(i))->type());
  }

  if (funcArgs.size() > 0) node->type(cdk::functional_type::create(funcArgs, funcRet));
  else node->type(cdk::functional_type::create(funcRet));

  _functionTypeR.push(node->type());
  node->block()->accept(this, lvl + 2);
}

void mml::type_checker::do_index_node(mml::index_node * const node, int lvl) {
  ASSERT_UNSPEC;
  std::shared_ptr < cdk::reference_type > btype;

  node->base()->accept(this, lvl + 2);
  btype = cdk::reference_type::cast(node->base()->type());
  if (!node->base()->is_typed(cdk::TYPE_POINTER)){
    throw std::string("pointer expression expected in index left-value");
  }

  node->index()->accept(this, lvl + 2);
  if (!node->index()->is_typed(cdk::TYPE_INT)){
     throw std::string("integer expression expected in left-value index");
  }
  node->type(btype->referenced());
}

void mml::type_checker::do_stack_alloc_node(mml::stack_alloc_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) {
    throw std::string("integer expression expected in allocation expression");
  }
  auto mytype = cdk::reference_type::create(4, cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  node->type(mytype);
}