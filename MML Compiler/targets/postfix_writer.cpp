#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include "targets/frame_size_calculator.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated
#include "mml_parser.tab.h"

//---------------------------------------------------------------------------

void mml::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}

void mml::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}

void mml::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  if(_inFunctionBody){
    _pf.DOUBLE(node->value());
  }
  else{
    _pf.SDOUBLE(node->value());
  }
}
void mml::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2);
  _pf.INT(0);
  _pf.EQ();
}

void mml::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl = ++_lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JZ(mklbl(lbl));
  node->right()->accept(this, lvl + 2);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}

void mml::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl = ++_lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JNZ(mklbl(lbl));
  node->right()->accept(this, lvl + 2);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  if (_inFunctionBody) {
    _pf.INT(node->value());
  } else {
    _pf.SINT(node->value());
  }
}

void mml::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  int lbl1;

  _pf.RODATA();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl1 = ++_lbl));
  _pf.SSTRING(node->value());

  if (_inFunctionBody) {
    _pf.TEXT();
    _pf.ADDR(mklbl(lbl1));
  } else {
    _pf.DATA();
    _pf.SADDR(mklbl(lbl1));
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_neg_node(cdk::neg_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.NEG();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->left()->type()->name() == cdk::TYPE_INT) {
    _pf.I2D();
  } else if (node->type()->name() == cdk::TYPE_POINTER && node->left()->type()->name() == cdk::TYPE_INT) {
    _pf.INT(3);
    _pf.SHTL();
  }

  node->right()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->right()->type()->name() == cdk::TYPE_INT) {
    _pf.I2D();
  } else if (node->type()->name() == cdk::TYPE_POINTER && node->right()->type()->name() == cdk::TYPE_INT) {
    _pf.INT(3);
    _pf.SHTL();
  }

  if (node->type()->name() == cdk::TYPE_DOUBLE)
    _pf.DADD();
  else
    _pf.ADD();
}
void mml::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
   node->left()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->left()->type()->name() == cdk::TYPE_INT) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->right()->type()->name() == cdk::TYPE_INT) {
    _pf.I2D();
  } else if (node->type()->name() == cdk::TYPE_POINTER && node->right()->type()->name() == cdk::TYPE_INT) {
    _pf.INT(3);
    _pf.SHTL();
  }

  if (node->type()->name() == cdk::TYPE_DOUBLE)
    _pf.DSUB();
  else
    _pf.SUB();
}

void mml::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->left()->type()->name() == cdk::TYPE_INT){
     _pf.I2D();
  }
  node->right()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->right()->type()->name() == cdk::TYPE_INT){
    _pf.I2D();
  }
  if (node->type()->name() == cdk::TYPE_DOUBLE){
    _pf.DMUL();
  }
  else{
    _pf.MUL();
  }
}
void mml::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->left()->type()->name() == cdk::TYPE_INT){
    _pf.I2D();
  }
  node->right()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->right()->type()->name() == cdk::TYPE_INT){
    _pf.I2D();
  }
  if (node->type()->name() == cdk::TYPE_DOUBLE){
    _pf.DDIV();
  }
  else{
    _pf.DIV();
  }
}
void mml::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}
void mml::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.LT();
}
void mml::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.LE();
}
void mml::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.GE();
}
void mml::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.GT();
}
void mml::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.NE();
}
void mml::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.EQ();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  const std::string &id = node->name();
  auto symbol = _symtab.find(id);
  if (symbol->offset() == 0) {
    _pf.ADDR(symbol->name());
  } else {
    _pf.LOCAL(symbol->offset());
  }
}

void mml::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.LDDOUBLE();
  } else {
    _pf.LDINT();
  }
}

void mml::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->rvalue()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    if (node->rvalue()->type()->name() == cdk::TYPE_INT) {
      _pf.I2D();
    }
    _pf.DUP64();
  } 
  else {
    _pf.DUP32();
  }

  node->lvalue()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.STDOUBLE();
  }
  else {
    _pf.STINT();
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_program_node(mml::program_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  // Note that MML doesn't have functions. Thus, it doesn't need
  // a function node. However, it must start in the main function.
  // The ProgramNode (representing the whole program) doubles as a
  // main function node.
  std::shared_ptr<mml::symbol> symbol;
  symbol = new_symbol();
  reset_new_symbol();

  _symtab.push();

  _pf.TEXT();
  _pf.ALIGN();
  _pf.GLOBAL("_main", _pf.FUNC());
  _pf.LABEL("_main");

  frame_size_calculator lsc(_compiler, _symtab, symbol);

  _symtab.push();
  node->accept(&lsc, lvl);
  _symtab.pop();

  _pf.ENTER(lsc.localsize());

  _inFunctionBody = true;
  _fType = cdk::primitive_type::create(4, cdk::TYPE_INT);
  _returnPresent = false;
  node->main()->accept(this, lvl);
  _fType = nullptr;
  _inFunctionBody = false;

  // end the main function
  if(!_returnPresent) {
    _pf.INT(0);
    _pf.STFVAL32();
  }
  _symtab.pop();
  _pf.LEAVE();
  _pf.RET();

  for(std::string extFunc : _functions_to_declare){
    _pf.EXTERN(extFunc);
  } 
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_evaluation_node(mml::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.TRASH(node->argument()->type()->size());
}

void mml::postfix_writer::do_print_node(mml::print_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  for (size_t ix = 0; ix < node->argument()->size(); ix++) {
    auto child = dynamic_cast<cdk::expression_node*>(node->argument()->node(ix));
    std::shared_ptr<cdk::basic_type> etype = child->type();
    child->accept(this, lvl);
    if (etype->name() == cdk::TYPE_INT) {
      _functions_to_declare.insert("printi");
      _pf.CALL("printi");
      _pf.TRASH(4);
    } else if (etype->name() == cdk::TYPE_DOUBLE) {
      _functions_to_declare.insert("printd");
      _pf.CALL("printd");
      _pf.TRASH(8);
    } else if (etype->name() == cdk::TYPE_STRING) {
      _functions_to_declare.insert("prints");
      _pf.CALL("prints");
      _pf.TRASH(4);
    } else {
      std::cerr << "cannot print expression of unknown type" << std::endl;
      return;
    }
  }

  if (node->newline()) {
    _functions_to_declare.insert("println");
    _pf.CALL("println");
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_input_node(mml::input_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _functions_to_declare.insert("readd");
    _pf.CALL("readd");
    _pf.LDFVAL64();
  }
  else if (node->is_typed(cdk::TYPE_INT)) {
    _functions_to_declare.insert("readi");
    _pf.CALL("readi");
    _pf.LDFVAL32();
  } else {
    std::cerr << "FATAL: " << node->lineno() << ": cannot read type" << std::endl;
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_while_node(mml::while_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  _whileCondLabels.push_back(++_lbl);
  _whileEndLabels.push_back(++_lbl);

  _pf.LABEL(mklbl(_whileCondLabels.at(_whileCondLabels.size() - 1)));
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(_whileEndLabels.at(_whileEndLabels.size() - 1)));
  node->block()->accept(this, lvl + 2);
  _pf.JMP(mklbl(_whileCondLabels.at(_whileCondLabels.size() - 1)));
  _pf.LABEL(mklbl(_whileEndLabels.at(_whileEndLabels.size() - 1)));

  _whileCondLabels.pop_back();
  _whileEndLabels.pop_back();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_if_node(mml::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_if_else_node(mml::if_else_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl2 = ++_lbl));
  _pf.LABEL(mklbl(lbl1));
  node->elseblock()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1 = lbl2));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_stop_node(mml::stop_node * const node, int lvl) {
  int nWhileEnd = _whileEndLabels.at(_whileEndLabels.size() - node->level());
  _pf.JMP(mklbl(nWhileEnd));
}

void mml::postfix_writer::do_next_node(mml::next_node * const node, int lvl) {
  int nWhileCond = _whileCondLabels.at(_whileCondLabels.size() - node->level());
  _pf.JMP(mklbl(nWhileCond));
}

void mml::postfix_writer::do_sizeof_node(mml::sizeof_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _pf.INT(node->expression()->type()->size());
}

void mml::postfix_writer::do_return_node(mml::return_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_fType->name() != cdk::TYPE_VOID) {
    node->retval()->accept(this, lvl + 2);

    if (_fType->name() == cdk::TYPE_INT || _fType->name() == cdk::TYPE_STRING || _fType->name() == cdk::TYPE_POINTER) {
      _pf.STFVAL32();
    } 
    else if (_fType->name() == cdk::TYPE_DOUBLE) {
      if (node->retval()->type()->name() == cdk::TYPE_INT) _pf.I2D();
      _pf.STFVAL64();
    } 
    else {
      std::cerr << node->lineno() << ": should not happen: unknown return type" << std::endl;
    }
  }
  _returnPresent = true;
}

void mml::postfix_writer::do_address_of_node(mml::address_of_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl + 2);
}

void mml::postfix_writer::do_nullptr_node(mml::nullptr_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_inFunctionBody) {
    _pf.INT(0);
  } else {
    _pf.SINT(0);
  }
}

void mml::postfix_writer::do_block_node(mml::block_node * const node, int lvl) {
  _symtab.push();
  if(node->declarations()->size() > 0) node->declarations()->accept(this, lvl + 2);
  if(node->instructions()->size() > 0) node->instructions()->accept(this, lvl + 2);
  _symtab.pop();
}

void mml::postfix_writer::do_declaration_node(mml::declaration_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  
  auto id = node->identifier();
  int offset = 0, typesize = node->type()->size();

  if(_inFunctionBody) {
    _offset -= typesize;
    offset = _offset;
  }
  else if (_inFunctionArgs) {
    offset = _offset;
    _offset += typesize;
  } 
  else {
    offset = 0;
  }

  auto symbol = new_symbol();
  if (symbol) {
    symbol->set_offset(offset);
    reset_new_symbol();
  }

  if(_inFunctionArgs){
    return;
  }
  else if (_inFunctionBody) {
    if (node->initializer()) {
      node->initializer()->accept(this, lvl);
      if (node->is_typed(cdk::TYPE_INT) || symbol->is_typed(cdk::TYPE_FUNCTIONAL) || node->is_typed(cdk::TYPE_STRING) || node->is_typed(cdk::TYPE_POINTER)) {
        _pf.LOCAL(symbol->offset());
        _pf.STINT();
      } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
        if (node->initializer()->is_typed(cdk::TYPE_INT)){
          _pf.I2D();
        }
        _pf.LOCAL(symbol->offset());
        _pf.STDOUBLE();
      } else {
        std::cerr << "cannot initialize" << std::endl;
      }
    }
  }
  else {
    if(node->qualifier() == tFOREIGN) {
      _functions_to_declare.insert(symbol->name());
    }
    else if (node->qualifier() == tFORWARD && node->is_typed(cdk::TYPE_FUNCTIONAL)){
      return;
    }
    else if (!node->initializer()) {
        _pf.BSS();
        _pf.ALIGN();
        _pf.LABEL(id);
        _pf.SALLOC(typesize);
    } else {
        if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_DOUBLE) || node->is_typed(cdk::TYPE_POINTER) || node->is_typed(cdk::TYPE_STRING)) {
          _pf.DATA();
          _pf.ALIGN();
          _pf.LABEL(id);

          if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_POINTER) || node->is_typed(cdk::TYPE_STRING)) {
            node->initializer()->accept(this, lvl);
          }
          else if (node->is_typed(cdk::TYPE_DOUBLE)) {
            if (node->initializer()->is_typed(cdk::TYPE_DOUBLE)) {
              node->initializer()->accept(this, lvl);
            } 
            else if (node->initializer()->is_typed(cdk::TYPE_INT)) {
              cdk::integer_node *declInt = dynamic_cast<cdk::integer_node*>(node->initializer());
              cdk::double_node dn(declInt->lineno(), declInt->value());
              dn.accept(this, lvl);
            } 
            else {
              std::cerr << node->lineno() << ": '" << id << "' has bad initializer for double value\n";
            }
          }
        }
        else if(node->is_typed(cdk::TYPE_FUNCTIONAL)) {
          node->initializer()->accept(this, lvl);
          _pf.DATA();
          _pf.ALIGN();
          _pf.LABEL(id);
          _pf.SADDR(_funcLabel);
        }
        else {
          std::cerr << node->lineno() << ": '" << id << "' has unexpected initializer\n";
        }
    }
  }
}

void mml::postfix_writer::do_function_call_node(mml::function_call_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  size_t argsSize = 0;
  auto funcType = cdk::functional_type::cast(node->function()->type());
  if (node->arguments()->size() > 0) {
    for (int ax = node->arguments()->size() - 1; ax >= 0; ax--) {
      cdk::expression_node *arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ax));
      arg->accept(this, lvl + 2);
      if (funcType->input(ax)->name() == cdk::TYPE_DOUBLE && arg->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
      }
      argsSize += funcType->input(ax)->size();
    }
  }

  std::string name;
  auto rvalN = dynamic_cast<cdk::rvalue_node*>(node->function());
  if(rvalN != nullptr) {
    auto varN = dynamic_cast<cdk::variable_node*>(rvalN->lvalue());
    name = varN->name();
  }

  auto pos = _functions_to_declare.find(name);
  if(pos != _functions_to_declare.end()) {
    _pf.CALL(name);
  }
  else {
    if(node->function()) node->function()->accept(this, lvl + 2);
    _pf.BRANCH();
  }

  
  if (argsSize != 0) {
    _pf.TRASH(argsSize);
  }

  if (funcType->output(0)->name() == cdk::TYPE_FUNCTIONAL || funcType->output(0)->name() == cdk::TYPE_INT || funcType->output(0)->name() == cdk::TYPE_POINTER || funcType->output(0)->name() == cdk::TYPE_STRING) {
    _pf.LDFVAL32();
  } 
  else if (funcType->output(0)->name() == cdk::TYPE_DOUBLE) {
    _pf.LDFVAL64();
  } 
}

void mml::postfix_writer::do_function_definition_node(mml::function_definition_node * const node, int lvl) {
  //ASSERT_SAFE_EXPRESSIONS;
  auto funcType = cdk::functional_type::cast(node->type());

  std::shared_ptr<mml::symbol> symbol;
  symbol = new_symbol();
  reset_new_symbol();

  _currentBodyRetLabel = mklbl(++_lbl);

  _offset = 8;
  _symtab.push();

  if (node->arguments()->size() > 0) {
    _inFunctionArgs = true;
    for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
      cdk::basic_node *arg = node->arguments()->node(ix);
      if (!arg) break;
      arg->accept(this, 0);
    }
    _inFunctionArgs = false;
  }

  std::string label = mklbl(++_lbl);
  _pf.TEXT();
  _pf.ALIGN();
  _pf.LABEL(label);
  _funcLabel = label;

  frame_size_calculator lsc(_compiler, _symtab, symbol);
  _symtab.push();
  node->accept(&lsc, lvl);
  _symtab.pop();
  _pf.ENTER(lsc.localsize());

  _offset = 0;

  _inFunctionBody = true;
  _fType = funcType->output(0);
  node->block()->accept(this, lvl + 4);
  _inFunctionBody = false;
  _fType = nullptr;

  _pf.LABEL(_currentBodyRetLabel);
  _pf.LEAVE();
  _pf.RET();

  _symtab.pop();
}

void mml::postfix_writer::do_index_node(mml::index_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->base()->accept(this, lvl);
  node->index()->accept(this, lvl);
  _pf.INT(3);
  _pf.SHTL();
  _pf.ADD();
}

void mml::postfix_writer::do_stack_alloc_node(mml::stack_alloc_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.INT(3);
  _pf.SHTL();
  _pf.ALLOC();
  _pf.SP(); 
}