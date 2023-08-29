#ifndef __MML_TARGETS_SYMBOL_H__
#define __MML_TARGETS_SYMBOL_H__

#include <string>
#include <memory>
#include <cdk/types/basic_type.h>

namespace mml {

  class symbol {
    std::shared_ptr<cdk::basic_type> _type;
    std::string _name;
    long _value; // hack!
    int _offset = 0;
    bool _initialized;
    bool _function;
    bool _forward = false;
    int _qualifier;
    std::vector<std::shared_ptr<cdk::basic_type>> _argument_types;
    bool _constant;

  public:
    symbol(bool constant, int qualifier, std::shared_ptr<cdk::basic_type> type, const std::string &name, bool initialized,
           bool function, bool forward = false) :
        _type(type), 
        _name(name), 
        _value(0), 
        _initialized(initialized),
        _function(function),
        _forward(forward),
        _qualifier(qualifier),
        _constant(constant) {
    }

    virtual ~symbol() {
      // EMPTY
    }

    std::shared_ptr<cdk::basic_type> type() const {
      return _type;
    }

    void set_type(std::shared_ptr<cdk::basic_type> t) {
      _type = t;
    }

    bool is_typed(cdk::typename_type name) const {
      return _type->name() == name;
    }

    const std::string &name() const {
      return _name;
    }

    long value() const {
      return _value;
    }

    long value(long v) {
      return _value = v;
    }

    int offset() const {
      return _offset;
    }

    void set_offset(int offset) {
      _offset = offset;
    }

    bool initialized() const {
      return _initialized;
    }

    bool isFunction() const {
      return _function;
    }

    bool isVariable() const {
      return !_function;
    }

    bool forward() const {
      return _forward;
    }

    int qualifier() const {
      return _qualifier;
    }

    void set_argument_types(const std::vector<std::shared_ptr<cdk::basic_type>> &types) {
      _argument_types = types;
    }

    bool argument_is_typed(size_t ax, cdk::typename_type name) const {
      return _argument_types[ax]->name() == name;
    }

    std::shared_ptr<cdk::basic_type> argument_type(size_t ax) const {
      return _argument_types[ax];
    }

    size_t argument_size(size_t ax) const {
      return _argument_types[ax]->size();
    }

    size_t number_of_arguments() const {
      return _argument_types.size();
    }

    bool constant() const{
      return _constant;
    }

  };

  inline auto make_symbol(bool constant, int qualifier, std::shared_ptr<cdk::basic_type> type, const std::string &name,
                          bool initialized, bool function, bool forward = false) {
    return std::make_shared<symbol>(constant, qualifier, type, name, initialized, function, forward);
  }

} // mml

#endif
