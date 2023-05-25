#include "expr.h"

namespace sym 
{
	Expr::Expr(const Const& c) {
        expr_ = new Const(c.get_value());
    }
	Expr::Expr(const Var& v) {
        expr_ = new Var(v.get_variable());
    }
	Expr::Expr(const __expr_t* e) {
        expr_ = e;
    }
	Expr::Expr(const __expr_t& e) {
        expr_ = static_cast<const __expr_t*>(&e);
    }
	Expr::Expr(const Expr& e) {
        expr_ = static_cast<const __expr_t*>(&e);
    }
		
	Expr::~Expr() {
        delete expr_;
    }

	__expr_t* Expr::eval(const var_map_t& var_map) const {
        return new Expr(expr_->eval(var_map));
    }
	__expr_t* Expr::diff(const std::string& v) const {
        return new Expr(expr_->diff(v));
    }
	std::ostream& Expr::operator<< (std::ostream &out) const {
        return out << *expr_;
    }
	bool Expr::operator==(const Expr& other) const {
        return *other.expr_ == *expr_;
    }
	bool Expr::operator==(const __expr_t& other) const {
        return other == *expr_;
    }
}    